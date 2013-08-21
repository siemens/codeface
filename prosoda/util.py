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
import re
import math
import shutil
from time import sleep
from multiprocessing import Process, Queue
from glob import glob
from logging import getLogger; log = getLogger(__name__)
from subprocess import Popen, PIPE
from pkg_resources import resource_filename
from tempfile import NamedTemporaryFile, mkdtemp

class BatchJob(object):
    '''
    Simple implementation of a dependency-respecting batch system

    This implementation spawns a set of n_cores processes, which pop work
    items off the work_queue. If a work item is executed without an exception,
    the worker puts its job id into the done_queue to signal completion. If
    an exception occurs, a None object is put in the done_queue; which
    triggers a RuntimeException in the main Process.

    Jobs can be created using BatchJob.add(function, args, kwargs, deps=deps))
    where deps can be a list of job handles previously returned by
    BatchJob.add. After all jobs have been added, BatchJob.join() keeps up
    execution and returns only after all jobs have finished.
    '''
    work_queue = Queue()
    done_queue = Queue()
    workers = []
    jobs = []
    n_cores = 1

    @classmethod
    def set_parallel_jobs(cls, n=1):
        '''Set the number of parallel processes'''
        cls.n_cores = int(n)

    @classmethod
    def join(cls, jobs=None):
        '''Run until all jobs (or only the specified jobs) are finished.'''
        cls._setup()
        if jobs is None:
            jobs = [j.id for j in jobs]
        while True:
            if all(cls.jobs[j].done for j in jobs):
                break
            for j in cls.jobs:
                j.submit()
            # Wait for a result from the done_queue
            res = cls.done_queue.get(block=True)
            if res is None:
                # sleep for a bit to allow the original processes' exception
                # to actually propagate.
                sleep(1)
                for w in cls.workers:
                    w.terminate()
                raise RuntimeError("Failed process. Terminating.")
            cls.jobs[res].done = True
        cls._shutdown()

    @classmethod
    def add(cls, func, args, kwargs={}, deps=()):
        '''Add a job that exceuted func(*args, **kwargs)'''
        j = cls(func, args, kwargs, deps)
        return j.id

    @classmethod
    def _setup(cls):
        '''
        Internal setup function that spawns the necessary number of worker
        threads. Can be called multiple times with no ill effect.
        Will never decrease the number of worker threads, as jobs might
        already be in progress.
        '''
        for i in range(cls.n_cores - len(cls.workers)):
            w = Process(target=cls._worker_function, args=())
            w.start()
            cls.workers.append(w)

    @classmethod
    def _worker_function(cls):
        '''
        Worker function executed in a separate process.
        This function pulls work items off the work queue; terminates in case
        of None; otherwise executes the work item. Any exception is reraised
        after putting a None onto the done_queue (triggering an exception in
        the main process)
        '''
        while True:
            workitem = cls.work_queue.get(block=True)
            if workitem is None:
                break # regular termination
            jobid, func, args, kwargs = workitem
            try:
                func(*args, **kwargs)
                cls.done_queue.put(jobid)
            except Exception as e:
                cls.done_queue.put(None)
                raise

    @classmethod
    def _shutdown(cls):
        '''Cooperatively shut down the worker processes'''
        for i in range(len(cls.workers)):
            cls.work_queue.put(None)
        for w in cls.workers:
            w.join()
        cls.workers = []

    def __init__(self, func, args, kwargs, deps):
        '''
        Set up a new BatchJob object and insert it into the global job list
        '''
        self.func, self.args, self.kwargs, self.deps = func, args, kwargs, deps
        self.id = len(self.__class__.jobs)
        self.__class__.jobs.append(self)
        self.submitted = False
        self.done = False

    def submit(self):
        '''
        Put this job onto the work queue if it is ready and not yet submitted
        '''
        if not self.submitted and self.ready:
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
