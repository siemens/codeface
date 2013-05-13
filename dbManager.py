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
import sys

class dbManager:
    """This class provides an interface to the prosoda sql database."""

    def __init__(self, conf):
        try:
            self.con = None
            self.con = mdb.Connection(host=conf["dbhost"], user=conf["dbuser"],
                                      passwd=conf["dbpwd"], db=conf["dbname"])
        except mdb.Error, e:
            print "Could not initialise database connection: %s (%d)" % \
                    (e.args[1], e.args[0])
            sys.exit(-1)
        self.cur = self.con.cursor()

    def __del__(self):
        if self.con != None:
            self.con.close()

    def doExec(self, stmt, args=None):
        try:
            return(self.cur.execute(stmt, args))
        except mdb.Error, e:
            print "Encountered mysql error %d during statement %s: %s" % \
                    (e.args[0], stmt, e.args[1])
            sys.exit(-1)

    def doFetchAll(self):
        try:
            return(self.cur.fetchall())
        except mdb.Error, e:
            print "Encountered mysql error %d during fetchall: %s" % \
                    (e.args[0], e.args[1])
            sys.exit(-1)

    def doCommit(self):
        try: 
            return(self.con.commit())
        except mdb.Error, e:
            print "Encountered mysql error %d during commit: %s" % \
                (e.args[0], e.args[1])
            sys.exit(-1)

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
        return(res[0])

##### Test cases #####
if __name__ == "__main__":
    import config
    conf = config.load_global_config("prosoda.conf")
    dbman = dbManager(conf)
    project="Twitter Bootstrap2"
    print("Found ID {0} for {1}\n".format(dbman.getProjectID(project, "tag"),
                                          project))
