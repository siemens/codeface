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
# Copyright 2013 by Siemens AG, Johannes Ebke <johannes.ebke.ext@siemens.com>
# All Rights Reserved.

import unittest
import logging

from StringIO import StringIO
from prosoda.logger import (_insert_seqs, _remove_seqs,
        _loglevel_from_string, _get_log_handler, console_handler, log)

def get_test_record(level, msg):
    return logging.LogRecord("packagename", level, "/fake/path", 42,
            msg, [], None)

class TestLogger(unittest.TestCase):
    '''Tests for the logger module'''

    def testInsertRemoveSeqs(self):
        '''Check that the internal insertSeqs function works as expected'''
        # null hypothesis test
        for s in ['Goo$fooStr\nBoo', '$RESTFUL $TEST']:
            self.assertEqual(_insert_seqs(s), s)
            self.assertEqual(_remove_seqs(s), s)
        # positive test
        RESET_SEQ = "\033[0m"
        COLOR_SEQ = "\033[1;%dm"
        BOLD_SEQ = "\033[1m"
        s = "Go\n$BOLDProsoda$RESET And do stuff"
        expected_ins = "Go\n\033[1mProsoda\033[0m And do stuff"
        expected_rem = "Go\nProsoda And do stuff"
        # Repeat the string five times to test repeated replacements
        self.assertEqual(_insert_seqs(s*5), expected_ins*5)
        self.assertEqual(_remove_seqs(s*5), expected_rem*5)


    def testColoredFormatter(self):
        io = StringIO()
        handler = _get_log_handler(io)
        handler.setLevel(10)
        handler.emit(get_test_record(10, "Test Message"))
        handler.flush()
        res = io.getvalue()
        self.assertIn("[packagename", res)
        self.assertIn("Test Message", res)
        # assert that no colour codes are in the message, since the StringIO
        # is not a TTY
        self.assertNotIn('\033', res)
