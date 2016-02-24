# This file is part of Codeface. Codeface is free software: you can
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
# Copyright 2013 by Siemens AG, Johannes Ebke <johannes.ebke.ext@siemens.com>
# All Rights Reserved.
"""Configuration module for codeface.

Encapsulates a configuration as an immutable dict."""

import yaml
from collections import Mapping
from logging import getLogger
from codeface.linktype import LinkType
from tempfile import NamedTemporaryFile

log = getLogger(__name__)


class ConfigurationError(Exception):
    """Raised if any part of the configuration is malformed."""
    pass


class Configuration(Mapping):
    """Encapsulates the codeface configuration.

    Configuration parameters can be accessed in dict style. Further includes
    helper methods to load a configuration from file, including validation and
    merging settings from multiple files, as well as storing a configuration set
    to a file.
    """

    GLOBAL_KEYS = ('dbname', 'dbhost', 'dbuser', 'dbpwd',
                   'idServiceHostname', 'idServicePort')
    """Mandatory global configuration keys."""
    GLOBAL_OPTIONAL_KEYS = ('dbport',)
    """Optional global configuration keys."""
    PROJECT_KEYS = ('project', 'repo', 'tagging', 'revisions', 'rcs')
    """Project configuration specific mandatory keys."""
    OPTIONAL_KEYS = ('description', 'ml', 'mailinglists', 'sleepTime',
                     'proxyHost', 'proxyPort', 'bugsProjectName',
                     'productAsProject', 'issueTrackerType',
                     'issueTrackerURL', 'understand', 'sloccount')
    """Optional configuration keys."""
    ALL_KEYS = set(GLOBAL_KEYS + GLOBAL_OPTIONAL_KEYS + PROJECT_KEYS +
                   OPTIONAL_KEYS)

    def __init__(self):
        """Initialize an empty configuration object with the default values."""
        self._conf = {
            'idServiceHostname': '127.0.0.1',
            'idServicePort': 8080
        }

        self._conf_file_loc = None
        self._project_conf = None
        self._global_conf = None

    @classmethod
    def load(cls, global_conf_file, local_conf_file=None):
        """Load configuration from global/local files.

        Args:
            global_conf_file (str): Project independent configuration.
            local_conf_file (str): Project specific configuration, overrides
                global settings.

        Returns:
            Configuration: Instance of Configuration with settings from both
            configuration files merged.
        """
        c = Configuration()
        log.devinfo("Loading global configuration file '{}'".
                    format(global_conf_file))
        c._global_conf = c._load(global_conf_file)
        c._conf.update(c._global_conf)
        if local_conf_file:
            log.devinfo("Loading project configuration file '{}'".
                        format(local_conf_file))
            c._project_conf = c._load(local_conf_file)
            c._conf.update(c._project_conf)
        else:
            log.devinfo("Not loading project configuration file!")
        c._initialize()
        c._check_sanity()
        return c

    @staticmethod
    def _load(filename):
        """Helper function that checks loading errors and logs them"""
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
        """Infer missing values in the configuration"""
        if "rcs" not in self:
            self._conf["rcs"] = [None for _ in range(len(self["revisions"]))]

        if "mailinglists" not in self:
            self._conf["mailinglists"] = []
            if "ml" in self:
                self._conf["mailinglists"].append({"name": self["ml"]})
        for ml in self._conf["mailinglists"]:
            ml.setdefault("type", "dev")
            ml.setdefault("source", "gmane")

        if "dbport" not in self:
            self._conf["dbport"] = 3306
        else:
            self._conf["dbport"] = int(self._conf["dbport"])

    def _check_sanity(self):
        """Check that the configuration makes sense.

        Raises:
            ConfigurationError:
        """

        # Some elementary sanity checks
        for key in self.GLOBAL_KEYS:
            if self._project_conf and key in self._project_conf:
                log.critical("The key '{}' may not be overridden in the "
                             "project configuration file".format(key))
                raise ConfigurationError('Invalid configuration key.')

        for key in self.GLOBAL_KEYS + self.PROJECT_KEYS:
            if key not in self:
                log.critical("Required key '{}' missing in configuration!"
                             ''.format(key))
                raise ConfigurationError('Missing configuration key.')

        if not self['tagging'] in LinkType.get_all_link_types():
            log.critical('Unsupported tagging mechanism specified!')
            raise ConfigurationError('Unsupported tagging mechanism.')

        if len(self["revisions"]) < 2:
            log.info(
                    "No revision range specified in configuration, "
                    "analyzing history in 3 month increments")

        if len(self["revisions"]) != len(self["rcs"]):
            log.critical("Malformed configuration: revision and rcs list "
                         "lengths differ! Found {0} revisions and {1} release "
                         "candidates.".format(len(self["revisions"]),
                                              len(self["rcs"])))
            raise ConfigurationError('Malformed configuration.')

        unknown_keys = [k for k in self if k not in self.ALL_KEYS]
        for key in unknown_keys:
            log.warning("Unknown key '{}' in configuration.".format(key))

    def write(self):
        """Write dump of current configuration to temporary file.

        Access conf_file_loc after calling this method to get the filename.
        """
        conf_file = NamedTemporaryFile(mode='w', prefix=self._conf['project'],
                                       delete=False)
        yaml.dump(self._conf, conf_file)
        self._conf_file_loc = conf_file.name
        conf_file.close()

    def get_conf_file_loc(self):
        """Location of the config file generated by a call to write()

        Returns:
            str: Location the config was written to last.
        """
        return self._conf_file_loc

    # Function for the Configuration object to function as a dict
    def __getitem__(self, key):
        return self._conf[key]

    def __setitem__(self, key, value):
        # TODO Key is not validated?
        self._conf[key] = value

    def __len__(self):
        return len(self._conf)

    def __iter__(self):
        return iter(self._conf)

    def __keys__(self):
        return self._conf.keys()

    def __str__(self):
        """Return a pretty string for display and logging."""
        r = ["--- # global codeface configuration"]
        for key in self.GLOBAL_KEYS:
            if key in self:
                r.append("{}: {}".format(key, repr(self[key])))
        r.append("# codeface project configuration")
        for key in self.PROJECT_KEYS + self.OPTIONAL_KEYS:
            if key in self:
                r.append("{}: {}".format(key, repr(self[key])))
        unknown = [k for k in self if k not in self.ALL_KEYS]
        if unknown:
            r.append("# Unknown keys")
            for key in unknown:
                r.append("{}: {}".format(key, repr(self[key])))
        return "\n".join(r)
