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
'''
Configuration module for prosoda

Encapsulates a configuration as an immutable dict
'''

import yaml
import sys
from collections import Mapping
from logging import getLogger; log = getLogger(__name__)

class ConfigurationError(Exception):
    pass

class Configuration(Mapping):
    '''
    Encapsulates the prosoda configuration
    '''
    def __init__(self):
        '''
        Initialize an empty configuration object with the default values
        '''
        self._conf = {
                'nodejsHostname' : '127.0.0.1',
                'nodejsPort' : 8080
                }

    @classmethod
    def load(self, global_conffile, local_conffile):
        '''
        Load configuration from global/local files
        '''
        c = Configuration()
        self._global_conf = c._load(global_conffile)
        self._project_conf = c._load(local_conffile)

        c._conf.update(c._global_conf)
        c._conf.update(c._project_conf)
        c._initialize()
        c._check_sanity()
        return c

    def _load(self, filename):
        try:
            return yaml.load(open(filename))
        except IOError:
            log.exception("Could not open configuration file '{}'".
                    format(filename))
            raise
        except yaml.YAMLError:
            log.exception("Could not parse configuration file '{}'".
                    format(filename))
            raise

    def _initialize(self):
        if "rcs" not in self:
            self._conf["rcs"] = [None for x in range(len(self["revisions"]))]

    def _check_sanity(self):
        global_keys = ('dbname', 'dbhost', 'dbuser', 'dbpwd',
                'nodejsHostname', 'nodejsPort')
        other_keys = ('project', 'repo', 'tagging', 'revisions', 'rcs')
        optional_keys = ('description', 'ml')
        # Some elementary sanity checks
        for key in global_keys:
            if key in self._project_conf:
                log.critical("The key '{}' may not be overridden in the "
                        "project configuration file".format(key))
                raise ConfigurationError('Invalid configuration key.')

        for key in global_keys + other_keys:
            if not key in self:
                log.critical("Required key '{}' missing in configuration!"
                        ''.format(key))
                raise ConfigurationError('Missing configuration key.')

        if not self['tagging'] in ('tag', 'committer2author', 'proximity'):
            log.critical('Unsupported tagging mechanism specified!')
            raise ConfigurationError('Unsupported tagging mechanism.')

        if len(self["revisions"]) < 2:
            log.critical("Malformed configuration: At least 2 revisions required")
            raise ConfigurationError('Malformed configuration.')

        if len(self["revisions"]) != len(self["rcs"]):
            log.critical("Malformed configuration: revision and rcs list "
                "lengths differ! Found {0} revisions and {1} release "
                "candidates.".format(len(self["revisions"]), len(self["rcs"])))
            raise ConfigurationError('Malformed configuration.')

        unknown_keys = [k for k in self if k not in global_keys + other_keys + optional_keys]
        for key in unknown_keys:
            log.warning("Unknown key '{}' in configuration.".format(key))

    def __getitem__(self, key):
        return self._conf[key]

    def __len__(self):
        return len(self._conf)

    def __iter__(self):
        return iter(self._conf)

    def __keys__(self):
        return self._conf.keys()
