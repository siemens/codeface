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

import unittest
import argparse
import sys
import os
import yaml
from tempfile import NamedTemporaryFile

from prosoda.configuration import Configuration, ConfigurationError

class TestConfiguration(unittest.TestCase):
    '''Test that the configuration object behaves in a sane way'''

    def testDefaults(self):
        '''Check that the defaults are set correctly'''
        c = Configuration()
        self.assertEqual(c['nodejsHostname'], '127.0.0.1')
        self.assertEqual(c['nodejsPort'], 8080)

    def testLoad(self):
        '''Test that an example configuration is loaded correctly'''
        global_conf = NamedTemporaryFile(delete=False)
        global_conf.write("""
# Foo commentary! A:B
---
# Fake Database access information
dbhost: remotehost
dbuser: 'theuser'
dbpwd: thepassword
dbname: 'thedb'
# intermediate comment
nodejsPort: 4242
nodejsHostname: foohost
        """)
        global_conf.close()
        project_conf = NamedTemporaryFile(delete=False)
        project_conf.write("""
# Fake commentary!

---
project: theproject
repo: therepo # Relative to git-dir as specified on the command line
description: the description
ml: the mailing list
revisions: [ "v1", "v2", "v3",
            "v4",
            "v5"]
rcs : ["v1rc0", "v2rc0", "v3rc0", "v4rc0",
"v5rc0"
]
new_tag: newvalue
tagging: tag
""")
        project_conf.close()
        c = Configuration.load(global_conf.name, project_conf.name)
        self.assertEqual(c["dbhost"], "remotehost")
        self.assertEqual(c["dbuser"], "theuser")
        self.assertEqual(c["dbpwd"], "thepassword")
        self.assertEqual(c["dbname"], "thedb")
        self.assertEqual(c["project"], "theproject")
        self.assertEqual(c["nodejsPort"], 4242)
        self.assertEqual(c["nodejsHostname"], "foohost")
        self.assertEqual(c["repo"], "therepo")
        self.assertEqual(c["description"], "the description")
        self.assertEqual(c["ml"], "the mailing list")
        self.assertEqual(c["revisions"], ["v1", "v2", "v3", "v4", "v5"])
        self.assertEqual(c["rcs"],  ["v1rc0", "v2rc0", "v3rc0", "v4rc0", "v5rc0"])
        self.assertEqual(c["tagging"], "tag")
        self.assertEqual(c["new_tag"], "newvalue")
        os.unlink(global_conf.name)
        os.unlink(project_conf.name)
        # Check that the configuration is valid YAML
        yaml_conf = NamedTemporaryFile(delete=False)
        yaml_conf.write(str(c))
        yaml_conf.close()
        c2 = Configuration.load(yaml_conf.name)
        self.assertEqual(dict(c), dict(c2))
        os.unlink(yaml_conf.name)

    def testDict(self):
        '''Quick test if a Configuration object behaves like a dict'''
        c = Configuration()
        expected_keys = set(("nodejsPort", "nodejsHostname"))
        self.assertEqual(set(c.keys()), expected_keys)
        print(str(c))
        for k in c:
            self.assertIn(k, expected_keys)
            c[k]

    def testFail(self):
        '''Test the failure modes of configuration'''
        global_conf = NamedTemporaryFile(delete=False)
        global_conf.write("""
# Foo commentary! A:B
---
# Fake Database access information
dbhost: remotehost
dbuser: theuser
dbpwd: thepassword
dbname: thedb
# intermediate comment
nodejsPort: 4242
nodejsHostname: foohost
        """)
        global_conf.close()
        project_conf_1 = NamedTemporaryFile(delete=False)
        project_conf_1.write("""
# Fake commentary!

---
project: theproject
Parse error! ""
""")
        project_conf_1.close()

        # project conf wirh wrong number of rcs tags
        project_conf_2 = NamedTemporaryFile(delete=False)
        project_conf_2.write("""
# Fake commentary!

---
project: theproject
repo: therepo # Relative to git-dir as specified on the command line
description: the description
ml: the mailing list
revisions: [ "v1", "v2", "v3",
            "v4",
            "v5"]
rcs : ["v1rc0", "v3rc0", "v4rc0",

"v5rc0"
]
tagging: tag
""")
        project_conf_2.close()

        self.assertRaises(IOError, Configuration.load,
                "/tmp/does_not_exist_42_42", "/tmp/does_not_exist_42_42")
        self.assertRaises(yaml.YAMLError, Configuration.load,
                global_conf.name, project_conf_1.name)
        self.assertRaises(ConfigurationError, Configuration.load,
                global_conf.name, project_conf_2.name)
        os.unlink(global_conf.name)
        os.unlink(project_conf_1.name)
        os.unlink(project_conf_2.name)
