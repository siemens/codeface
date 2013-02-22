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
import kerninfo
import pickle
import argparse
from config import load_config
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
    
def dispatch_ts_analysis(resdir, conf_file):
    conf = load_config(conf_file)

    dbpath = os.path.join(resdir, conf["project"], conf["tagging"])
    destdir = os.path.join(dbpath, "ts")

    if not(os.path.exists(destdir)):
        os.mkdir(destdir)

    for i in range(1, len(conf["revisions"])):
        dbfilename = os.path.join(dbpath, "{0}-{1}".format(conf["revisions"][i-1],
                                                           conf["revisions"][i]),
                                  "vcs_analysis.db")
        
        doAnalysis(dbfilename, destdir, revrange=[conf["revisions"][i-1],
                                                  conf["revisions"][i]],
                   rc_start=conf["rcs"][i])

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('resdir')
    parser.add_argument('conf_file')
    args = parser.parse_args()
    
    dispatch_ts_analysis(args.resdir, args.conf_file)

