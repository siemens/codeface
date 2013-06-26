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

# Configuration file parser 
import yaml
import sys

def replace_none(val):
    if val == "":
        return None
    return val

def load_config(file):
    f = open(file, "r")
    conf = yaml.load(f)
    f.close()

    # Some elementary sanity checks
    if not(conf.has_key("project")) or not(conf.has_key("repo")) or \
            not(conf.has_key("tagging")) or not(conf.has_key("revisions")):
        print("Malformed configuration: Mandatory tags missing!")
        sys.exit(-1)
        
    if conf["tagging"] != "tag" and conf["tagging"] != "committer2author" \
            and conf["tagging"] != "proximity":
        print("Unsupported tagging mechanism specified!")
        sys.exit(-1)

    if len(conf["revisions"]) < 2:
        print("Malformed configuration: At least 2 revisions required")
        sys.exit(-1)

    if (not(conf.has_key("rcs"))):
        conf["rcs"] = [None for x in range(len(conf["revisions"]))]

    if (not(conf.has_key("nodejsHostname"))):
        conf["nodejsHostname"] = "127.0.0.1"

    if (not(conf.has_key("nodejsPort"))):
        conf["nodejsPort"] = 8080
    else:
        conf["nodejsPort"] = int(conf["nodejsPort"])

    if (len(conf["rcs"]) > 0) & (len(conf["revisions"]) != len(conf["rcs"])):
        print("Malformed configuration: revision and rcs list lengths differ!")
        print("Found {0} revisions and {1} release candidates.".
              format(len(conf["revisions"]), len(conf["rcs"])))
        sys.exit(-1)

    conf["rcs"] = [replace_none(x) for x in conf["rcs"]]

    return conf

def load_global_config(file):
    f = open(file, "r")
    conf = yaml.load(f)
    f.close()

    # Some elementary sanity checks
    if not(conf.has_key("dbname")) or not(conf.has_key("dbhost")) or \
            not(conf.has_key("dbuser")) or not(conf.has_key("dbpwd")):
        print("Malformed configuration: Database information missing!")
        sys.exit(-1)

    return conf
