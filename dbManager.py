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

class dbManager:
    """This class provides an interface to the prosoda sql database."""

    def __init__(self, host, user, passwd, db):
        self.con = mdb.Connection(host=host, user=user,
                                  passwd=passwd, db=db)
        self.cur = self.con.cursor()

    def __del__(self):
        if self.con:
            self.con.close()


    # NOTE: We don't provide any synchronisation since by assumption,
    # a single project is never analysed from two threads.
    # TODO: This does not consider the case that one project can
    # be analysed with different methods
    def getProjectID(self, name, analysisMethod):
        self.cur.execute("SELECT id FROM project WHERE name=%s", name)

        if self.cur.rowcount < 1:
            # Project is not contained in the database
            print("Registering project")
            self.cur.execute("INSERT INTO project (name, analysisMethod) " +
                             "VALUES (%s, %s);", (name, analysisMethod))
            res = self.con.commit()
            self.cur.execute("SELECT id FROM project WHERE name=%s;", name)

        res = self.cur.fetchall()[0]
        return(res[0])

##### Test cases #####
dbman = dbManager("localhost", "quantarch", "quantarch", "quantarch")
project="Twitter Bootstrap2"
print("Found ID {0} for {1}\n".format(dbman.getProjectID(project, "tag"),
                                      project))
