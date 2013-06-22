#! /usr/bin/env python

# This file is part of prosoda.  prosoda is free software: you can
# redistribute it and/or modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation, version 2.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
# All Rights Reserved.

# Dispatcher for the prosoda analysis based on a configuration file
from config import load_config, load_global_config
from subprocess import *
import argparse
import glob
import os
import sys
from conv import convert_dot_file
from tempfile import NamedTemporaryFile, mkdtemp
from dbManager import dbManager
import shutil

def _abort(msg):
    print(msg + "\n")
    sys.exit(-1)

def executeCommand(cmd, dry_run, ignoreErrors=False):
    if dry_run:
        print("{0}".format(" " .join(cmd)))
        return

    try:
        p2 = Popen(cmd, stdout=PIPE)
        res = p2.communicate()[0]
    except OSError:
        _abort("Internal error: Could not execute command '{0}'".
               format(" ".join(cmd)))

    if p2.returncode != 0 and not(ignoreErrors):
        print("\n\n******* Internal error *******")
        print(res)
        print("*******\n\n")
        _abort("Internal error: Command {0} exited with {1}".format(" ".join(cmd),
                                                                    p2.returncode))
    return res

def generate_cluster_graphs(resdir):
    print("  -> Generating cluster graphs")
    files = glob.glob(os.path.join(resdir, "*.dot"))
    for file in files:
        out = NamedTemporaryFile(mode="w")
        out.writelines(convert_dot_file(file))
        # Make sure the information does not stall in caches
        # before dot hits the file
        out.flush()

        cmd = []
        cmd.append("dot")
        cmd.append("-Ksfdp")
        cmd.append("-Tpdf")
        cmd.append("-Gcharset=utf-8")
        cmd.append("-o{0}.pdf".format(os.path.splitext(file)[0]))
        cmd.append(out.name)
        executeCommand(cmd, args.dry_run)

        # NOTE: Only close the temporary file after the graph has
        # been formatted -- the temp file is destroyed after close
        out.close()

def generate_report(revs, i, basedir, resdir, args):
        print("  -> Generating report")
        report_base = "report-{0}_{1}".format(revs[i], revs[i+1])
        cmd = []
        cmd.append(os.path.join(basedir, "cluster", "create_report.pl"))
        cmd.append(resdir)
        cmd.append("{0}--{1}".format(revs[i], revs[i+1]))

        out = open(os.path.join(resdir, report_base + ".tex"), "w")
        res = executeCommand(cmd, args.dry_run)
        if not(args.dry_run):
            out.write(res)

        out.close()

        # Compile report
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
        executeCommand(cmd, args.dry_run, ignoreErrors=True)
        try:
            shutil.copy(report_base + ".pdf", resdir)
        except IOError:
            print("Warning: Could not copy report PDF (missing input data?)")

        os.chdir(orig_wd)
        shutil.rmtree(tmpdir)


def dispatchAnalysis(args):
    conf = load_config(args.conf)
    global_conf = load_global_config("prosoda.conf")
    dbm = dbManager(global_conf)

    revs = conf["revisions"]
    rcs = conf["rcs"]

    if args.basedir == None:
        basedir = "./"
    else:
        basedir = args.basedir

    print("Processing project '{0}'".format(conf["description"]))
    pid = dbm.getProjectID(conf["project"], conf["tagging"])

    # Fill table release_timeline with the release information
    # known so far (date is not yet available)
    for i in range(len(revs)):
        # We need to make sure that no entries are duplicated to avoid
        # creating malformed release databases
        dbm.doExec("SELECT * FROM release_timeline WHERE type='release' " +
                   "AND tag=%s AND projectId=%s", (revs[i], pid))

        if dbm.cur.rowcount < 1:
            dbm.doExecCommit("INSERT INTO release_timeline " +
                             "(type, tag, projectId) " +
                             "VALUES (%s, %s, %s)",
                             ("release", revs[i], pid))

    # Also construct the release ranges, again with the information known
    # so far
    for i in range(len(revs)-1):
        startID = dbm.getRevisionID(pid, revs[i])
        endID = dbm.getRevisionID(pid, revs[i+1])
        rcTag = rcs[i+1]
        rcID = None

        if (rcTag != None):
            dbm.doExec("SELECT * FROM release_timeline WHERE type='rc' " +
                   "AND tag=%s AND projectId=%s", (rcTag, pid))

            if dbm.cur.rowcount < 1:
                dbm.doExecCommit("INSERT INTO release_timeline " +
                                 "(type, tag, projectId) " +
                                 "VALUES (%s, %s, %s)",
                                 ("rc", rcTag, pid))
                rcID = dbm.getRCID(pid, rcTag)

        if (rcID != None):
            dbm.doExecCommit("INSERT INTO release_range " +
                             "(releaseStartId, releaseEndId, "+
                             "projectId, releaseRCStartId) " +
                             "VALUES (%s, %s, %s, %s)",
                             (startID, endID, pid, rcID))
        else:
            dbm.doExecCommit("INSERT INTO release_range " +
                             "(releaseStartId, releaseEndId, "+
                             "projectId) " +
                             "VALUES (%s, %s, %s)",
                             (startID, endID, pid))

    ## Obtain all release range ids created
    dbm.doExec("SELECT id FROM release_range WHERE projectId=%s", (pid))
    releaseRangeIds = dbm.doFetchAll()
    releaseRangeIds = [str(releaseRangeIds[i][0])
                       for i in range(0,len(releaseRangeIds))]

    #############################
    # Analyse all revision ranges
    for i in range(len(revs)-1):
        resdir = os.path.join(args.resdir, conf["project"],
                              conf["tagging"],
                              "{0}-{1}".format(revs[i], revs[i+1]))
        if not(os.path.isabs(resdir)):
            resdir = os.path.abspath(resdir)

        # TODO: Sanity checks (ensure that git repo dir exists)
        if 'proximity' == conf["tagging"]:
            check4ctags()
        
        #######
        # STAGE 1: Commit analysis
        # TODO: Instead of calling an external python script, it
        # would maybe wiser to call a procedure...
        print("  -> Analysing commits {0}..{1}".format(revs[i], revs[i+1]))
        cmd = []
        cmd.append(os.path.join(basedir, "cluster", "cluster.py"))
        cmd.append(os.path.join(args.gitdir, conf["repo"], ".git"))
        cmd.append(args.conf)
        cmd.append(resdir)
        cmd.append(revs[i])
        cmd.append(revs[i+1])

        if rcs[i+1] != None:
            cmd.append("--rc_start")
            cmd.append(rcs[i+1])

        if (not(args.use_db)):
            cmd.append("--create_db")

        executeCommand(cmd, args.dry_run)

        #########
        # STAGE 2: Cluster analysis
        print("  -> Detecting clusters")
        cmd = []
        cmd.append(os.path.join(basedir, "cluster", "persons.r"))
        cmd.append(resdir)
        cmd.append(args.conf)
        cmd.append(releaseRangeIds[i])
        executeCommand(cmd, args.dry_run)

        #########
        # STAGE 3: Generate cluster graphs
        if (not(args.no_report)):
            generate_cluster_graphs(resdir)

        #########
        # STAGE 4: Report generation
        # Stage 4.1: Report preparation
        if (not(args.no_report)):
            generate_report(revs, i, basedir, resdir, args)

    #########
    # Global stage 1: Time series generation
    print("=> Preparing time series data")
    cmd = []
    cmd.append(os.path.join(basedir, "ts.py"))
    cmd.append(args.resdir)
    cmd.append(args.conf)

    executeCommand(cmd, args.dry_run)

    #########
    # Global stage 2: Time series analysis
    print("=> Analysing time series")
    cmd = []
    cmd.append(os.path.join(basedir, "analyse_ts.r"))
    cmd.append(args.resdir)
    cmd.append(args.conf)

    executeCommand(cmd, args.dry_run)

def check4ctags():
    # check if the appropriate ctags is installed on the system
    prog_name    = 'Exuberant Ctags'
    prog_version = 'Exuberant Ctags 5.9~svn20110310'
    cmd = "ctags-exuberant --version".split()
    
    res = executeCommand(cmd, None)
    
    if not(res.startswith(prog_name)):
        print "Fatal Error: program {0} does not exist".format("ctags-exuberant")
        
    if not(res.startswith(prog_version)):
        # TODO: change this to use standard mechanism for error logging
        print "Fatal Error: Ctags version {0} not found".format(prog_version)
        exit()
    
if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('resdir',
                        help="Base directory to store analysis results in")
    parser.add_argument('gitdir',
                        help="Base directory for git repositories")
    parser.add_argument('conf', help="Project specific configuration file")
    parser.add_argument('--use_db', action='store_true',
                        help="Re-use existing database")
    parser.add_argument('--basedir',
                        help="Base directory where the prosoda infrastructure is found")
    parser.add_argument('--dry-run', action="store_true",
                        help="Just show the commands called, don't perform any work")
    parser.add_argument('--no-report', action="store_true",
                        help="Skip LaTeX report generation (and dot compilation)")
    # TODO: Use tag as argument here, not in the configuration file
    # (better include information about signed-off or not in the configuration
    # file)

    args = parser.parse_args()
    dispatchAnalysis(args)
