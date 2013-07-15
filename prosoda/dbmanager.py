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

# Thin sql database wrapper

import MySQLdb as mdb
from datetime import datetime
from logging import getLogger; log = getLogger(__name__)
from contextlib import contextmanager

@contextmanager
def _log_db_error(action):
    try:
        yield
    except mdb.Error as e:
        log.critical('MySQL error {e[0]} during "{action}": {e[1]}'
                ''.format(e=e.args, action=action))
        raise

class DBManager:
    """This class provides an interface to the prosoda sql database."""

    def __init__(self, conf):
        try:
            self.con = None
            self.con = mdb.Connection(host=conf["dbhost"],
                                      user=conf["dbuser"],
                                      passwd=conf["dbpwd"],
                                      db=conf["dbname"])
            log.debug("Establishing MySQL connection to {c[dbuser]}@{c[dbhost]}"
                    ", DB '{c[dbname]}'".format(c=conf))
        except mdb.Error as e:
            log.critical("Failed to establish MySQL connection to "
                    "{c[dbuser]}@{c[dbhost]}, DB '{c[dbname]}': {e[1]} ({e[0]})"
                    "".format(c=conf, e=e.args))
            raise
        self.cur = self.con.cursor()

    def __del__(self):
        if self.con != None:
            self.con.close()

    def doExec(self, stmt, args=None):
        with _log_db_error(stmt):
            return self.cur.execute(stmt, args)

    def doFetchAll(self):
        with _log_db_error("fetchall"):
            return self.cur.fetchall()

    def doCommit(self):
        with _log_db_error("commit"):
            return self.con.commit()

    def doExecCommit(self, stmt, args=None):
        self.doExec(stmt, args)
        self.doCommit()

    # NOTE: We don't provide any synchronisation since by assumption,
    # a single project is never analysed from two threads.
    # TODO: This does not consider the case that one project can
    # be analysed with different methods
    def getProjectID(self, name, analysisMethod):
        self.doExec("SELECT id FROM project WHERE name=%s", name)

        if self.cur.rowcount < 1:
            # Project is not contained in the database
            self.doExec("INSERT INTO project (name, analysisMethod) " +
                        "VALUES (%s, %s);", (name, analysisMethod))
            self.doCommit()
            self.doExec("SELECT id FROM project WHERE name=%s;", name)

        res = self.doFetchAll()[0]
        return res[0]

    def getTagID(self, projectID, tag, type):
        """Determine the ID of a tag, given its textual form and the type"""
        self.doExec("SELECT id FROM release_timeline WHERE projectId=%s " +
                    "AND tag=%s AND type=%s", (projectID, tag, type))

        res = self.doFetchAll()[0]
        return(res[0])

    def getRevisionID(self, projectID, tag):
        return(self.getTagID(projectID, tag, "release"))

    def getRCID(self, projectID, tag):
        return(self.getTagID(projectID, tag, "rc"))

    def getReleaseRange(self, projectID, revisionIDs):
        """Given a pair of release IDs, determine the release range ID"""
        self.doExec("SELECT id FROM release_range WHERE projectId=%s " +
                    "AND releaseStartId=%s AND releaseEndId=%s",
                    (projectID, revisionIDs[0], revisionIDs[1]))

        res = self.doFetchAll()[0]
        return(res[0])

def tstamp_to_sql(tstamp):
    """Convert a Unix timestamp into an SQL compatible DateTime string"""
    return(datetime.utcfromtimestamp(tstamp).strftime("%Y-%m-%d %H:%M:%S"))
