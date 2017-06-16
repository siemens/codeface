## This file is part of Codeface. Codeface is free software: you can
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

import logging; log = logging.getLogger(__name__)
import os
import os.path
import re
import shutil
import signal
import sys
import traceback
from collections import OrderedDict, namedtuple
from glob import glob
from math import sqrt
from multiprocessing import Process, Queue, JoinableQueue, Lock
from pickle import dumps, PicklingError
from pkg_resources import resource_filename
from subprocess import Popen, PIPE
from tempfile import NamedTemporaryFile, mkdtemp
from time import sleep
from threading import enumerate as threading_enumerate
from Queue import Empty
from datetime import timedelta, datetime

# Represents a job submitted to the batch pool.
BatchJobTuple = namedtuple('BatchJobTuple', ['id', 'func', 'args', 'kwargs',
        'deps', 'startmsg', 'endmsg'])
class BatchJob(BatchJobTuple):
    def __init__(self, *args, **kwargs):
        super(BatchJob, self).__init__(*args, **kwargs)
        self.done = False
        self.submitted = False

class BatchJobPool(object):
    '''
    Implementation of a dependency-respecting batch pool

    This system uses a pool of N worker processes to run jobs. Since the
    multiprocessing module is used, all functions, args and kwargs must be
    pickleable. Specifically, this means that only functions defined at
    top-level in a module can be used here.

    Jobs can be created using pool.add(function, args, kwargs, deps=deps))
    where deps can be a list of job handles previously returned by
    pool.add. If multiprocessing is disabled, the functions are run
    immediately and None is returned.

    Call pool.join() to start execution and wait until all jobs are complete.
    If a work item raises an exception, the join() will terminate with
    that exception, if pickleable, or a generic Exception if otherwise.
    '''

    def __init__(self, n_cores):
        self.n_cores = n_cores
        self.next_id = 1
        self.jobs = OrderedDict() # Dictionary of jobs (ordered for repeatability)

        # Initialize workers and their work and done queues
        self.work_queue, self.done_queues, self.workers = Queue(), [], []
        if n_cores > 1:
            # When n_cores is 1 we doen't use the process anyway.
            # However the pycharm debugger goes crasy when we start the
            # process, so as a workaround don't start anything when
            # n_core is 1.
            for i in range(n_cores):
                dq = Queue()
                w = Process(target=batchjob_worker_function, args=(self.work_queue, dq))
                self.done_queues.append(dq)
                self.workers.append(w)
                w.start()

    def _is_ready(self, job):
        '''Returns true if the job is ready for submission'''
        if job.done or job.submitted:
            return False
        return all(self.jobs[j].done for j in job.deps if j is not None)

    def _submit(self, job):
        '''Submit the job if it is ready'''
        if self._is_ready(job):
            self.work_queue.put(job)
            job.submitted = True

    def add(self, func, args, kwargs={}, deps=(), startmsg=None, endmsg=None):
        '''
        Add a job that executes func(*args, **kwargs) and depends on the
        jobs with the ids listed in deps.
        This function returns a job ID which can be used as a dependency
        in other calls to add.
        If n_cores is 1; this call immediately executes the given function
        and returns None
        '''
        if self.n_cores == 1:
            log.info(startmsg)
            func(*args, **kwargs)
            log.info(endmsg)
            return None
        job_id = self.next_id
        self.next_id += 1
        j = BatchJob(job_id, func, args, kwargs, deps, startmsg, endmsg)
        self.jobs[job_id] = j
        return job_id

    def join(self):
        '''
        Submit jobs and wait for all jobs to finish.
        '''
        try:
            while not all(j.done for j in self.jobs.values()):
                # Put jobs that are ready onto the work queue
                for j in self.jobs.values():
                    self._submit(j)
                # Wait for a result from the done_queues
                for dq in self.done_queues:
                    try:
                        res = dq.get(block=False)
                    except Empty:
                        continue
                    if res is None:
                        log.fatal("Uncaught exception in worker thread!")
                        raise Exception("Failure in Batch Pool")
                    if isinstance(res, Exception):
                        log.fatal("Uncaught exception in worker thread:")
                        raise res
                    log.debug("Job {} has finished!".format(res))
                    self.jobs[res].done = True
                # Check if workers died
                for w in self.workers:
                    if not w.is_alive():
                        w.join()
                        raise Exception("A Worker died unexpectedly!")
                sleep(0.01)
        finally:
            # Terminate and join the workers
            # Wait 100ms to allow backtraces to be logged
            sleep(0.1)
            log.devinfo("Terminating workers...")
            for w in self.workers:
                w.terminate()
            log.devinfo("Workers terminated.")

def batchjob_worker_function(work_queue, done_queue):
    '''
    Worker function executed in a separate process.
    This function pulls work items off the work queue; terminates if there
    is no item for 0.5s; otherwise executes the work item. Any exception
    is reraised after putting a None onto the done_queue (triggering an
    exception in the main process)
    '''
    # Silently quit on CTRL+C
    signal.signal(signal.SIGINT, handle_sigint_silent)
    while True:
        try:
            job = work_queue.get(block=True)
        except ValueError as ve:
            # This happens when the main loop stops before we do
            return
        log.debug("Starting job id {}".format(job.id))
        try:
            if job.startmsg:
                log.info(job.startmsg)
            job.func(*job.args, **job.kwargs)
            if job.endmsg:
                log.info(job.endmsg)
            log.debug("Finished work id {}".format(job.id))
            done_queue.put(job.id)
        except Exception as e:
            log.debug("Failed work id {}".format(job.id))
            done_queue.put(Exception(e.__class__.__name__ + ": " +
                    str(e) + "\n" + traceback.format_exc()))

# Function to dump the stacks of all threads
def get_stack_dump():
    id2name = dict([(th.ident, th.name) for th in threading_enumerate()])
    code = ["Stack dump:"]
    for threadId, stack in sys._current_frames().items():
        code.append("")
        code.append("# Thread: %s(%d)" % (id2name.get(threadId,""), threadId))
        for filename, lineno, name, line in traceback.extract_stack(stack):
            code.append('File: "%s", line %d, in %s' % (filename, lineno, name))
            if line:
                code.append("  %s" % (line.strip()))
    return code

# Signal handler that dumps all stacks and terminates
# Lock l dis-interleaves the stack traces of processes
l = Lock()
def handle_sigint(signal, frame):
    with l:
        log.fatal("CTRL-C pressed!")
        for c in get_stack_dump():
            log.devinfo(c)
    # This call raises a SystemExit exception in the
    # stack frame that was interrupted by the signal
    # For the main thread, this is what we want.
    sys.exit(-1)

# Signal handler that dumps all stacks and terminates silently
# Also uses the Lock l to dis-interleave the stack traces
def handle_sigint_silent(signal, frame):
    with l:
        for c in get_stack_dump():
            log.devinfo(c)
    logging.shutdown()
    # Since we want to terminate worker threads with prejudice,
    # we use os._exit, which directly terminates the process.
    # otherwise the worker try/catch will also catch the SystemExit
    os.exit_(-1)

def handle_sigterm(signal, frame):
    # Since we want to terminate worker threads with prejudice,
    # we use os._exit, which directly terminates the process.
    # otherwise the worker try/catch will also catch the SystemExit
    logging.shutdown()
    os._exit(-1)

def handle_sigusr1(signal, frame):
    for c in get_stack_dump():
        log.info(c)

# Dump all the stacks in case of CTRL-C
signal.signal(signal.SIGINT, handle_sigint)
# Also dump on sigterm
signal.signal(signal.SIGTERM, handle_sigterm)
# Also dump on sigusr1, but do not terminate
signal.signal(signal.SIGUSR1, handle_sigusr1)

def execute_command(cmd, ignore_errors=False, direct_io=False, cwd=None, silent_errors=False):
    '''
    Execute the command `cmd` specified as a list of ['program', 'arg', ...]
    If ignore_errors is true, a non-zero exit code will be ignored (and a warning
    messages will be issued), otherwise an exception is raised. If silent_errors is True,
    no messages will be emitted even in case of an error (but exceptions will still be raised).
    If direct_io is True, do not capture the stdin and stdout of the command.
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
            if not(silent_errors):
                log.warning("Command '{}' failed with exit code {}. Ignored.".
                            format(jcmd, pipe.returncode))
        else:
            if not(direct_io) and not(silent_errors):
                log.info("Command '{}' stdout:".format(jcmd))
                for line in stdout.splitlines():
                    log.info(line)
                log.info("Command '{}' stderr:".format(jcmd))
                for line in stderr.splitlines():
                    log.info(line)
            msg = "Command '{}' failed with exit code {}. \n" \
                  "(stdout: {}\nstderr: {})"\
                  .format(jcmd, pipe.returncode, stdout, stderr)
            if not(silent_errors):
                log.error(msg)
            raise Exception(msg)
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
              format(a,b,count, sqrt(float(count))))

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
    cmd.append("-Kfdp")
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
    log.info("  -> Generating Reports...")
    for file in files:
        layout_graph(file)
    generate_report(start_rev, end_rev, range_resdir)

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


def check4cppstats():
    """
    check if the appropriate cppstats is installed on the system.
    """
    # We can not check the version directly as there is no version switch
    # on cppstats We just check if the first line is OK.
    line = "cppstats v0.9."
    cmd = "/usr/bin/env cppstats --version".split()
    res = execute_command(cmd)
    if not (res.startswith(line)):
        error_message = "expected the first line to start with '{0}' but "\
                        "got '{1}'".format(line, res[0])
        log.error("program cppstats does not exist, or it is not working "
                  "as expected ({0}"
                  .format(error_message))
        raise Exception("no working cppstats found ({0})"
                        .format(error_message))


def gen_prefix(i, num_ranges, start_rev, end_rev):
    if (len(start_rev) == 40):
        # When revisions are given by commit hashes, shorten them since
        # they don't carry any meaning
        start_rev = start_rev[0:6]
        end_rev = end_rev[0:6]
    return("  -> Revision range {0}/{1} ({2}..{3}): ".format(i, num_ranges,
                                                             start_rev, end_rev))

def gen_range_path(base_path, i, start_rev, end_rev):
    if (len(start_rev) == 40):
        # Same logic as above, but construct a file system path
        start_rev = start_rev[0:6]
        end_rev = end_rev[0:6]
    return(os.path.join(base_path, "{0}--{1}-{2}".
                        format(str(i).zfill(3), start_rev, end_rev)))


def parse_iso_git_date(date_string):
    # from http://stackoverflow.com/questions/526406/python-time-to-age-part-2-timezones
    try:
        offset = int(date_string[-5:])
    except:
        log.error("could not extract timezone info from \"{0}\""
                  .format(date_string))
        raise
    minutes = (offset if offset > 0 else -offset) % 100
    delta = timedelta(hours=offset / 100,
                      minutes=minutes if offset > 0 else -minutes)
    # In future python versions we can use "%Y-%m-%d %H:%M:%S %z"
    # this way we don't need the above workaround, currently %z isn't
    # working as documented
    fmt = "%Y-%m-%d %H:%M:%S"
    parsed_date = datetime.strptime(date_string[:-6], fmt)
    parsed_date -= delta
    return parsed_date

# Determine settings for the size and amount of analysis windows. If nothing
# specific is provided, use default settings
def get_analysis_windows(conf):
    window_size_months = 3
    num_window = -1

    if "windowSize" in conf.keys():
        window_size_months = conf["windowSize"]
    if "numWindows" in conf.keys():
        num_window = conf["numWindows"]

    return window_size_months, num_window

def generate_analysis_windows(repo, window_size_months):
    """
    Generates a list of revisions (commit hash) in increments of the window_size
    parameter. The window_size parameter specifies the number of months between
    revisions. This function is useful when the git repository has no tags
    referencing releases.
    """
    cmd_date = 'git --git-dir={0} show --format=%ad  --date=iso8601'\
        .format(repo).split()
    latest_date_result = execute_command(cmd_date).splitlines()[0]
    latest_commit = parse_iso_git_date(latest_date_result)

    print_fmt = "%Y-%m-%dT%H:%M:%S+0000"
    month = timedelta(days=30)

    def get_before_arg(num_months):
        date = latest_commit - num_months * month
        return '--before=' + date.strftime(print_fmt)

    revs = []
    start = window_size_months  # Window size time ago
    end = 0  # Present time
    cmd_base = 'git --git-dir={0} log --no-merges --format=%H,%ct,%ci'\
        .format(repo).split()
    cmd_base_max1 = cmd_base + ['--max-count=1']
    cmd = cmd_base_max1 + [get_before_arg(end)]
    rev_end = execute_command(cmd).splitlines()
    revs.extend(rev_end)

    while start != end:
        cmd = cmd_base_max1 + [get_before_arg(start)]
        rev_start = execute_command(cmd).splitlines()

        if len(rev_start) == 0:
            start = end
            cmd = cmd_base + ['--reverse']
            rev_start = [execute_command(cmd).splitlines()[0]]
        else:
            end = start
            start = end + window_size_months

        # Check if any commits occurred since the last analysis window
        if rev_start[0] != revs[0]:
            revs = rev_start + revs
        # else: no commit happened since last window, don't add duplicate
        #       revisions
    # End while

    # Check that commit dates are monotonic, in some cases the earliest
    # first commit does not carry the earliest commit date
    revs = [rev.split(",") for rev in revs]
    rev_len = len(revs)
    if int(revs[0][1]) > int(revs[1][1]):
      del revs[0]

    # Extract hash values and dates intro seperate lists
    revs_hash = [rev[0] for rev in revs]
    revs_date = [rev[2].split(" ")[0] for rev in revs]

    # We cannot detect release canndidate tags in this analysis mode,
    # so provide a list with None entries
    rcs = [None for x in range(len(revs))]

    return revs_hash, rcs, revs_date
