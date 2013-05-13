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

# Create time series from a sequence of VCS objects

from VCS import gitVCS
from commit_analysis import createCumulativeSeries, createSeries, \
    writeToFile, getSeriesDuration
import os.path
from subprocess import *
from datetime import datetime
import kerninfo
import pickle
import argparse
from config import load_config, load_global_config
from dbManager import dbManager, tstamp_to_sql;
import yaml
import sys

def doAnalysis(dbfilename, destdir, revrange=None, rc_start=None):
    pkl_file = open(dbfilename, 'rb')
    vcs = pickle.load(pkl_file)
    pkl_file.close()

    if revrange:
        sfx = "{0}-{1}".format(revrange[0], revrange[1])
    else:
        sfx = "{0}-{1}".format(vcs.rev_start, vcs.rev_end)

    res = createSeries(vcs, "__main__", revrange, rc_start)
    writeToFile(res, os.path.join(destdir, "raw_{0}.dat".format(sfx)))
    return res
    
def writeReleases(dbm, tstamps, conf):
    pid = dbm.getProjectID(conf["project"], conf["tagging"])

    for tstamp in tstamps:
        if tstamp[0] == "release":
            dbm.doExec("UPDATE release_timeline SET date=%s WHERE " +
                       "projectId=%s AND type=%s AND tag=%s",
                       (tstamp_to_sql(int(tstamp[2])), pid, tstamp[0], tstamp[1]))
            print("Trying to update {0} with {1}".format(tstamp[0],
                                                         tstamp_to_sql(int(tstamp[2]))))
        else:
            dbm.doExec("INSERT INTO release_timeline " +
                       "(type, tag, date, projectId) " +
                       "VALUES (%s, %s, %s, %s)",
                       (tstamp[0], tstamp[1], tstamp_to_sql(int(tstamp[2])),
                        pid))
    dbm.doCommit()

def dispatch_ts_analysis(resdir, conf_file):
    conf = load_config(conf_file)
    global_conf = load_global_config("prosoda.conf")

    dbm = dbManager(global_conf)
    dbpath = os.path.join(resdir, conf["project"], conf["tagging"])
    destdir = os.path.join(dbpath, "ts")

    if not(os.path.exists(destdir)):
        os.mkdir(destdir)

    ## Stage 1: Create the individual time series (and record all time
    ## stamps for the boundaries)
    tstamps = []
    for i in range(1, len(conf["revisions"])):
        dbfilename = os.path.join(dbpath, "{0}-{1}".format(conf["revisions"][i-1],
                                                           conf["revisions"][i]),
                                  "vcs_analysis.db")
        
        ts = doAnalysis(dbfilename, destdir, revrange=[conf["revisions"][i-1],
                                                       conf["revisions"][i]],
                        rc_start=conf["rcs"][i])

        if (i==1):
            tstamps.append(("release", conf["revisions"][i-1], ts.get_start()))

        if (ts.get_rc_start()):
            tstamps.append(("rc", conf["revisions"][i], ts.get_rc_start()))

        tstamps.append(("release", conf["revisions"][i], ts.get_end()))

    ## Stage 2: Insert time stamps for all releases considered into the database
    writeReleases(dbm, tstamps, conf)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('resdir')
    parser.add_argument('conf_file')
    args = parser.parse_args()
    
    dispatch_ts_analysis(args.resdir, args.conf_file)

