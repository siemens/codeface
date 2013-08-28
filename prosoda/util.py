## This file is part of prosoda.  prosoda is free software: you can
## redistribute it and/or modify it under the terms of the GNU General Public
## License as published by the Free Software Foundation, version 2.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
##
## Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.
'''
Utility functions for running external commands
'''
import os
import sys
import re
import math
import shutil
from collections import OrderedDict
from time import sleep
from random import getrandbits
from multiprocessing import Process, Queue, JoinableQueue, current_process
from threading import Thread
from Queue import Empty
from glob import glob
from logging import getLogger; log = getLogger(__name__)
from subprocess import Popen, PIPE
from pkg_resources import resource_filename
from tempfile import NamedTemporaryFile, mkdtemp
from glob import glob

class BatchJob(object):
    '''
    Implementation of a dependency-respecting batch system

    This system uses a pool of N worker processes to run jobs. Since the
    multiprocessing module is used, all functions, args and kwargs must be
    pickleable. Specifically, this means that only functions defined at
    top-level in a module can be used here.

    Jobs can be created using BatchJob.add(function, args, kwargs, deps=deps))
    where deps can be a list of job handles previously returned by
    BatchJob.add.
    Call BatchJob.join() to wait until the execution of all jobs is complete.
    If a work item raises an exception, the BatchJob system terminates and
    calls to BatchJob.join() will return with an Exception.

    Note that BatchJob.add can also be called from another BatchJob.
    '''

    ## Public methods
    @classmethod
    def set_parallel_jobs(cls, n=1):
        '''
        Set the number of parallel processes to use to n.
        This function only has an effect if called from the main process.
        '''
        cls.n_cores = int(n)

    @classmethod
    def add(cls, func, args, kwargs={}, deps=()):
        '''
        Add a job that executes func(*args, **kwargs) and depends on the
        jobs with the ids listed in deps.
        This function returns a job ID which can be used as a dependency
        in other calls to add.
        This function can be called from any process.
        '''
        # Generate a random job ID
        job_id = getrandbits(32)
        cls.add_queue.put((job_id, func, args, kwargs, deps))
        cls.add_queue.join()
        return job_id

    @classmethod
    def join(cls, jobs=None):
        '''
        Wait for all jobs (or all specified jobs) to finish.
        This function can only be called from the main Process.
        '''
        assert cls.main_process is current_process(), ("This function can"
                " only be called from the main process!")
        if jobs is None:
            while not cls.error and not all(j.done for j in cls.jobs.values()):
                sleep(0.1)
        else:
            while not cls.error and not all(cls.jobs[j].done for j in jobs):
                sleep(0.1)
        if cls.error:
            raise Exception("Failure in Batch Job.")

    ## End of public functions
    error = False  # Set to True if a subprocess or thread had an exception
    jobs = OrderedDict() # Dictionary of jobs (ordered for repeatability)
    n_cores = 1 # Number of worker processes to use
    main_process = None # Handle of main process

    # The thread that submits jobs and its queues
    submit_thread = None
    work_queue = Queue()
    done_queue = Queue()

    # Communication for asynchronous out-of-process addition of jobs
    add_thread = None
    add_queue = JoinableQueue()

    # Worker-Manager thread
    worker_manager_thread = None
    workers = []

    @classmethod
    def _add_thread_main(cls):
        '''
        This thread is necessary to allow subprocessesses to add jobs.
        Since they cannot access the job dict directly, they put work requests
        on the add_queue, and wait until task_done() has been called.
        This ensures that the job id will be present in the job
        dictionary once BatchJob.add() returns.
        '''
        log.debug("Starting add thread...")
        while True:
            try:
                job_id, func, args, kwargs, deps = cls.add_queue.get(block=True)
                assert not job_id in cls.jobs, "Duplicate job ID - random number generator faulty"
                cls.jobs[job_id] = cls(job_id, func, args, kwargs, deps)
                cls.add_queue.task_done()
            except EOFError:
                # This exception occurs if the main thread is being shut down.
                # We can therefore just quit.
                break

    @classmethod
    def _submit_thread_main(cls):
        '''
        This thread makes sure that exceptions from work items are handled
        and new jobs are submitted if their dependencies are met.
        The done_queue currently only contains the job id as a return value
        from a successful job; if the BatchJob would be extended to deal with
        return values, a tuple (job_id, return_value) would be necessary.
        '''
        try:
            log.debug("Starting submit main thread...")
            while not cls.error:
                # Put jobs that are ready onto the work queue
                for j in cls.jobs.values():
                    j.submit()
                # Wait for a result from the done_queue
                # The timeout ensures that we regularly check for new jobs
                # which might have been added in the meantime
                try:
                    res = cls.done_queue.get(block=True, timeout=0.1)
                except Empty:
                    continue
                if res is None:
                    log.fatal("Uncaught exception in worker thread! Terminating.")
                    cls.error = True
                    return
                log.debug("Job {} has finished!".format(res))
                cls.jobs[res].done = True
        finally:
            # Since this thread is responsible for stopping the BatchJob system
            # we have to set error to True if there is an exception in here
            # (e.g. KeyboardInterrupt)
            cls.error = True

    @classmethod
    def _worker_manager_thread_main(cls):
        '''
        This thread creates worker processes as necessary.
        '''
        try:
            log.debug("Starting worker management thread....")
            while not cls.error:
                # First, check if all workers are alive
                for w in cls.workers[:]:
                    if not w.is_alive():
                        cls.workers.remove(w)
                # Then, if the work queue is not empty, add workers
                if not cls.work_queue.empty():
                    for i in range(cls.n_cores - len(cls.workers)):
                        w = Process(target=cls._worker_function, args=())
                        w.start()
                        cls.workers.append(w)
                # Finally, sleep.
                sleep(0.1)
        finally:
            for w in cls.workers:
                w.terminate()
            cls.error = True

    @classmethod
    def _start(cls):
        '''
        Internal setup function.
        Set the main_process to the current process, and start the threads.
        '''
        if cls.main_process is None:
            cls.main_process = current_process()
        assert cls.main_process is current_process(), "This function can only be called from the main process!"

        if not cls.add_thread:
            cls.add_thread = Thread(target=cls._add_thread_main, name="BatchJobAsyncAddThread")
            cls.add_thread.setDaemon(True)
            cls.add_thread.start()
        if not cls.worker_manager_thread:
            cls.worker_manager_thread = Thread(target=cls._worker_manager_thread_main, name="BatchJobWorkerManagerThread")
            cls.worker_manager_thread.setDaemon(True)
            cls.worker_manager_thread.start()
        if not cls.submit_thread:
            cls.submit_thread = Thread(target=cls._submit_thread_main, name="BatchJobSubmitThread")
            cls.submit_thread.setDaemon(True)
            cls.submit_thread.start()

    @classmethod
    def _worker_function(cls):
        '''
        Worker function executed in a separate process.
        This function pulls work items off the work queue; terminates if there
        is no item for 0.5s; otherwise executes the work item. Any exception
        is reraised after putting a None onto the done_queue (triggering an
        exception in the main process)
        '''
        while True:
            try:
                workitem = cls.work_queue.get(block=True, timeout=0.5)
            except Empty:
                break
            jobid, func, args, kwargs = workitem
            log.debug("Starting work id {}".format(jobid))
            try:
                func(*args, **kwargs)
                cls.done_queue.put(jobid)
            except Exception as e:
                cls.done_queue.put(None)
                cls.done_queue.close()
                cls.done_queue.join_thread()
                raise

    def __init__(self, job_id, func, args, kwargs, deps):
        '''
        Set up a new BatchJob object
        '''
        self.func, self.args, self.kwargs = func, args, kwargs
        self.deps = deps
        self.id = job_id
        self.submitted = False
        self.done = False

    def submit(self):
        '''
        Put this job onto the work queue if it is ready and not yet submitted
        '''
        if not self.submitted and self.ready:
            log.debug("Submitting Job {} since all deps are OK: {}".
                    format(self.id, [(j, self.__class__.jobs[j].done)
                                     for j in self.deps]))
            workitem = (self.id, self.func, self.args, self.kwargs)
            self.__class__.work_queue.put(workitem)
            self.submitted = True

    @property
    def ready(self):
        '''a job is ready if all its dependencies are done'''
        return all(self.__class__.jobs[j].done for j in self.deps)

    @property
    def running(self):
        '''
        a job is running if it has been submitted to the queue, but is not
        yet done
        '''
        return self.submitted and not self.done

# Initialize the Batch Job system
BatchJob._start()

def execute_command(cmd, ignore_errors=False, direct_io=False, cwd=None):
    '''
    Execute the command `cmd` specified as a list of ['program', 'arg', ...]
    If ignore_errors is true, a non-zero exit code will be ignored, otherwise
    an exception is raised.
    If direct_io is True, do not capture the stdin and stdout of the command
    Returns the stdout of the command.
    '''
    jcmd = " ".join(cmd)
    log.debug("Running command: {}".format(jcmd))
    try:
        if direct_io:
            pipe = Popen(cmd, cwd=cwd)
        else:
            pipe = Popen(cmd, stdout=PIPE, stderr=PIPE, cwd=cwd)
        stdout, stderr = pipe.communicate()
    except OSError:
        log.error("Error executing command {}!".format(jcmd))
        raise

    if pipe.returncode != 0:
        if ignore_errors:
            log.warning("Command '{}' failed with exit code {}. Ignored.".
                    format(jcmd, pipe.returncode))
        else:
            if not direct_io:
                log.info("Command '{}' stdout:".format(jcmd))
                for line in stdout.splitlines():
                    log.info(line)
                log.info("Command '{}' stderr:".format(jcmd))
                for line in stderr.splitlines():
                    log.info(line)
            log.error("Command '{}' failed with exit code {}.".
                    format(jcmd, pipe.returncode))
            raise Exception("Command '{}' failed with exit code {}.".
                    format(jcmd, pipe.returncode))
    return stdout

def _convert_dot_file(dotfile):
    '''
    Convert duplicate edges in the given dot file into edges with
    a larger pen width.
    '''
    res = []
    edges = {}
    edge_spec = re.compile("\s+(\d+) -> (\d+);")

    file = open(dotfile, "r")
    lines = [line.strip("\n") for line in file]
    # Modify the header (copyright line + digraph)
    lines[0] = "digraph {"
    lines[1] = "node[fontsize=30, shape=\"box\"];"

    lines[len(lines)-1] = "" # Skip closing brace

    for line in lines:
        m = re.match(edge_spec, line)
        if m:
            a, b = m.group(1), m.group(2)
            edges[(a,b)] = edges.get((a,b), 0) + 1
        else:
            res.append(line + "\n")

    # sort the edges for reproducibility
    for ((a, b), count) in sorted(edges.items()):
        res.append("{0} -> {1} [weight={2} penwidth={3}];\n".
              format(a,b,count, math.sqrt(float(count))))

    res.append("overlap=prism;\n")
    res.append("splines=true;\n")
    res.append("}\n")
    return res



def layout_graph(filename):
    out = NamedTemporaryFile(mode="w", delete=False)
    out.writelines(_convert_dot_file(filename))
    out.close() # flushes the cache
    cmd = []
    cmd.append("dot")
    cmd.append("-Ksfdp")
    cmd.append("-Tpdf")
    cmd.append("-Gcharset=utf-8")
    cmd.append("-o{0}.pdf".format(os.path.splitext(filename)[0]))
    cmd.append(out.name)
    execute_command(cmd)
    # Manually remove the temporary file
    os.unlink(out.name)

def generate_report(start_rev, end_rev, resdir):
    log.devinfo("  -> Generating report")
    report_base = "report-{0}_{1}".format(start_rev, end_rev)

    # Run perl script to generate report LaTeX file
    cmd = []
    cmd.append(resource_filename(__name__, "perl/create_report.pl"))
    cmd.append(resdir)
    cmd.append("{0}--{1}".format(start_rev, end_rev))
    with open(os.path.join(resdir, report_base + ".tex"), 'w') as f:
        f.write(execute_command(cmd))

    # Compile report with lualatex
    cmd = []
    cmd.append("lualatex")
    cmd.append("-interaction=nonstopmode")
    cmd.append(os.path.join(resdir, report_base + ".tex"))

    # We run latex in a temporary directory so that it's easy to
    # get rid of the log files etc. created during the run that are
    # not relevant for the final result
    orig_wd = os.getcwd()
    tmpdir = mkdtemp()

    os.chdir(tmpdir)
    execute_command(cmd, ignore_errors=True)
    try:
        shutil.copy(report_base + ".pdf", resdir)
    except IOError:
        log.warning("Could not copy report PDF (missing input data?)")

    os.chdir(orig_wd)
    shutil.rmtree(tmpdir)

def generate_reports(start_rev, end_rev, range_resdir):
    files = glob(os.path.join(range_resdir, "*.dot"))
    log.info("  -> Analysing revision range {0}..{1}: Generating Reports...".
        format(start_rev, end_rev))
    dotjobs = [BatchJob.add(layout_graph, (file,)) for file in files]
    return BatchJob.add(generate_report, (start_rev, end_rev, range_resdir), deps=dotjobs)

def check4ctags():
    # check if the appropriate ctags is installed on the system
    prog_name    = 'Exuberant Ctags'
    prog_version = 'Exuberant Ctags 5.9~svn20110310'
    cmd = "ctags-exuberant --version".split()

    res = execute_command(cmd, None)

    if not(res.startswith(prog_name)):
        log.error("program '{0}' does not exist".format(prog_name))
        raise Exception("ctags-exuberant not found")

    if not(res.startswith(prog_version)):
        # TODO: change this to use standard mechanism for error logging
        log.error("Ctags version '{0}' not found".format(prog_version))
        raise Exception("Incompatible ctags-exuberant version")
