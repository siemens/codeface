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
from glob import glob
from logging import getLogger; log = getLogger(__name__)
from subprocess import Popen, PIPE
from pkg_resources import resource_filename
from tempfile import NamedTemporaryFile, mkdtemp

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

def layout_all_graphs(resdir):
    log.devinfo("  -> Generating cluster graphs")
    files = glob(os.path.join(resdir, "*.dot"))
    for file in files:
        out = NamedTemporaryFile(mode="w", delete=False)
        out.writelines(_convert_dot_file(file))
        out.close() # flushes the cache
        cmd = []
        cmd.append("dot")
        cmd.append("-Ksfdp")
        cmd.append("-Tpdf")
        cmd.append("-Gcharset=utf-8")
        cmd.append("-o{0}.pdf".format(os.path.splitext(file)[0]))
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
