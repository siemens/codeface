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
# Copyright 2010, 2011 by Wolfgang Mauerer <wm@linux-kernel.net>
# Copyright 2012, 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
# All Rights Reserved.

import re
from email.Utils import parseaddr
from PersonInfo import PersonInfo
from dbManager import dbManager
import httplib
import urllib
import json
import sys

class idManager:
    """Provide unique IDs for developers.

    This class provides an interface to the REST id server. Heuristics to
    detect developers who operate under multiple identities are included
    in the server."""
    def __init__(self, project):
        self.subsys_names = []

        # Map IDs to an instance of PersonInfo
        self.persons = {}

        # Map a name, email address, or a combination of both to the numeric ID
        # assigned to the developer
        self.person_ids = {}

        self.fixup_emailPattern = re.compile(r'([^<]+)\s+<([^>]+)>')
        self.commaNamePattern = re.compile(r'([^,\s]+),\s+(.+)')

        # Initialise the REST connection to the id server
        # TODO: The URL needs to be configurable and must not be hard-coded
        self._idMgrServer = "localhost"
        self._idMgrPort = 8080
        self._conn = httplib.HTTPConnection(self._idMgrServer, self._idMgrPort)

        # Create a project ID
        # TODO: The credentials should be made configurable
        self._dbm = dbManager("localhost", "quantarch",
                              "quantarch", "quantarch")
        # TODO: Pass the analysis method to idManager via the configuration
        # file. However, the method should not influence the id scheme so
        # that the results are easily comparable.
        self._projectID = self._dbm.getProjectID(project, "tag")

    # We need the subsystem names because PersonInfo instances
    # are created from this class -- and we want to know in which
    # subsystem(s) a developer is active
    def setSubsysNames(self, subsys_names):
        self.subsys_names = subsys_names

    def getSubsysNames(self):
        return self.subsys_names

    def _decompose_addr(self, addr):
        (name, email) = parseaddr(addr)

        # The eMail parser cannot handle Surname, Name <email@domain.tld> properly.
        # Provide a fixup hack for this case
        if (name == "" and email.count("@") == 0):
            m = re.search(self.fixup_emailPattern, addr)
            if m:
                name = m.group(1)
                email = m.group(2)
                m2 = re.search(self.commaNamePattern, name)
                if m2:
                    # Replace "Surname, Name" by "Name Surname"
                    name = "{0} {1}".format(m2.group(2), m2.group(1))

#                print "Fixup for addr {0} required -> ({1}/{2})".format(addr, name, email)
            else:
                # In this case, no eMail address was specified.
#                print("Fixup for email required, but FAILED for {0}".format(addr))
                name = addr
                email = "could.not@be.resolved.tld"

        email = email.lower()
        # TODO: Strip trailing and pending spaces off the names
        name = self._cleanName(name)

        return (name, email)

    def _query_user_id(self, name, email):
        """Query the ID database for a contributor ID"""

        params = urllib.urlencode({'projectID': self._projectID,
                                   'name': name,
                                   'email': email})
        headers = { "Content-type":
                        "application/x-www-form-urlencoded; charset=utf-8",
                    "Accept": "text/plain" }

        try:
            self._conn.request("POST", "/post_user_id", params, headers)
            res = self._conn.getresponse()
        except:
            print("Could not reach ID service. Is the server running?\n")
            sys.exit(-1)

        # TODO: We should handle errors by throwing an exception instead
        # of silently ignoring them
        id = json.loads(res.read())["id"]
        return(id)

    def getPersonID(self, addr):
        """Obtain a unique ID from contributor identity credentials.

        The IDs are managed by a central database accessed via REST.
        Managing multiple identities for the same person is also
        handled there. Safety against concurrent access is provided by
        the database.
        """

        (name, email) = self._decompose_addr(addr)

        ID = self._query_user_id(name, email)

        # Construct a local instance of PersonInfo for the contributor
        # if it is not yet available
        if (not(self.persons.has_key(ID))):
            self.persons[ID] = PersonInfo(self.subsys_names, ID, name, email)

        return ID

    def getPersons(self):
        return self.persons

    def getPI(self, ID):
        return self.persons[ID]
    
    def _cleanName(self, name):
        # Remove or replace characters in names that are known
        # to cause parsing problems in later stages
        nameClean = name.replace('\"', "")
        
        return nameClean
############################ Test cases #########################
if __name__ == "__main__":
    idm = idManager()
    for addr in ["Luck, Tony <tony.luck@intel.com>", "Tony Luck <tony.luck@intel.com>"]:
        id = idm.getPersonID(addr)
        print("ID for {0}: {1}".format(addr, id))

    persons = idm.getPersons()
    for id in sorted(persons.keys()):
        pi = persons[id]
        print "{0} -> {1}".format(id, pi.getName())
