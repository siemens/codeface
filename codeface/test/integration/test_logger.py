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

import unittest
import sys
from os import unlink
from logging import getLogger, INFO, DEBUG
from tempfile import NamedTemporaryFile
from StringIO import StringIO

from codeface.logger import (set_log_level, start_logfile, stop_logfile,
        console_handler)


class TestLogger(unittest.TestCase):
    '''Integration tests for the logger module'''

    def testLoglevel(self):
        '''Check that the log level on the console logger is set correctly
        as well as propagated to derived logger'''
        log = getLogger("codeface.test.integration.test_logger")
        handler = getLogger("codeface").handlers[0]
        self.assertIs(handler, console_handler)
        old_stream = handler.stream
        try:
            set_log_level('warning')
            handler.stream = StringIO()
            log.warning(":-)")
            self.assertIn(":-)", handler.stream.getvalue())
            handler.stream = StringIO()
            log.info(":-(")
            log.devinfo(":-(")
            self.assertNotIn(":-(", handler.stream.getvalue())
            handler.stream = StringIO()
            log.warning(":-)")
            self.assertIn(":-)", handler.stream.getvalue())
            set_log_level('info')
            handler.stream = StringIO()
            log.info(":-)")
            self.assertIn(":-)", handler.stream.getvalue())
            handler.stream = StringIO()
            log.devinfo(":-(")
            self.assertNotIn(":-(", handler.stream.getvalue())
            set_log_level('devinfo')
            handler.stream = StringIO()
            log.devinfo(":-)")
            self.assertIn(":-)", handler.stream.getvalue())
            handler.stream = StringIO()
            log.debug(":-(")
            self.assertNotIn(":-(", handler.stream.getvalue())
            set_log_level('debug')
            log.debug(":-)")
            self.assertIn(":-)", handler.stream.getvalue())
        finally:
            set_log_level('debug')
            handler.stream = old_stream

    def testLogfile(self):
        '''Test logging into a logfile'''
        f = NamedTemporaryFile(delete=False)
        filename = f.name
        try:
            set_log_level('error') # avoid using the console logger
            f.write(":-P\n")
            f.close()
            start_logfile(f.name, 'devinfo')
            log = getLogger("codeface.test.integration.test_logger")
            log.debug("Should not be in logfile! :-( ")
            log.info("Should be in logfile :-) ")
            log.devinfo("Should be in logfile :-) ")
            log.warning("Should really be in logfile :-D ")
            stop_logfile(f.name)
            contents = file(f.name).read()
            self.assertNotIn(":-(", contents)
            self.assertNotIn(":-P", contents)
            self.assertIn(":-)", contents)
            self.assertIn(":-D", contents)
            # Make sure no colour codes are leaked into the logfile
            self.assertNotIn("\033", contents)
        finally:
            set_log_level('debug')
            unlink(filename)
