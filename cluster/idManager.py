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

class idManager:
    """This class provides unique IDs for developers identified by their name and eMail address.

    We use heuristics to assign a single ID to each developer even if he appears under
    multiple eMail addresses and/or name variations."""
    def __init__(self):
        self.subsys_names = []
        self.ID = 0

        # Map IDs to an instance of PersonInfo
        self.persons = {}

        # Map a name, email address, or a combination of both to the numeric ID
        # assigned to the developer
        self.person_ids = {}

        self.fixup_emailPattern = re.compile(r'([^<]+)\s+<([^>]+)>')
        self.commaNamePattern = re.compile(r'([^,\s]+),\s+(.+)')

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

        return (name, email)

    def getPersonID(self, addr):
        """Create a unique ID from a contributor

        Multiple identities are detected using heuristics. We do not use
        an explicit database because the method is supposed to be applicable for
        a wide range of projects.
        """
        # TODO: This should be made thread-safe
        # lock = threading.Lock()

        (name, email) = self._decompose_addr(addr)

        if name != "":
            
            #clean name for troublesome characters 
            name = self.cleanName(name)
            
            name_known = self.person_ids.has_key(name)
        else:
            name_known = False

        if email != "":
            email_known = self.person_ids.has_key(email)
        else:
            print("WARNING: Developer {0} has no email address?!".format(name))
            email_known = False

#        print("EMAIL: ({0}/{1}) ({2}/{3})".format(name, email, name_known, email_known))
        
        if not(name_known) and not(email_known):
            self.person_ids[name] = self.ID
            self.person_ids[email] = self.ID
            self.persons[self.ID] = PersonInfo(self.subsys_names, self.ID, name, email)
            self.ID += 1
        elif name_known and not(email_known):
            # Person with multiple eMail addresses (we assume that the name of
            # each developer is unique. Resolving the general case would require extra
            # information)
            self.person_ids[email] = self.person_ids[name]
        elif  email_known and not(name_known):
            # Different orthographic variants of the name
            self.person_ids[name] = self.person_ids[email]

        return self.person_ids[name]

    def getPersons(self):
        return self.persons

    def getPI(self, ID):
        return self.persons[ID]
    
    def cleanName(self, name):
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
