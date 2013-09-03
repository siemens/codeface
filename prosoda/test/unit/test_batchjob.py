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
import exceptions
from logging import getLogger; log = getLogger("prosoda.test.unit.batchjob")
from time import sleep
from random import random
from prosoda.util import BatchJobPool

def test_function(i):
    sleep(0.05*random())
    log.info(i)

def fast_function(i):
    pass

def dep_function(i):
    log.info(i)
    pass

def typeerror_function(missing_argument):
    pass

def ioerror_function():
    open("foo"*42)

def unpickleable_error_function():
    class MyEx(Exception):
        def __init__(self):
            self.handle = open("tmpfile", "wb")
    raise MyEx()

class Testpool(unittest.TestCase):
    '''Tests for the pool multiprocessing'''
    def testBasicFunctionality(self):
        '''Check that basic processing works'''
        pool = BatchJobPool(24)
        for i in range(10):
            pool.add(test_function, [i], {}, deps="")
        pool.join()

    def testFastFunctions(self):
        '''Check that fast functions work'''
        pool = BatchJobPool(24)
        for i in range(240):
            pool.add(fast_function, [i], {}, deps="")
        pool.join()

    def testDependencies(self):
        '''Check that dependencies work'''
        pool = BatchJobPool(24)
        last_id = None
        for i in range(10):
            last_id = pool.add(dep_function, [i], {}, deps=[last_id])
        pool.join()

    def testNormalFailure(self):
        '''Check that normal exceptions are reported'''
        pool = BatchJobPool(24)
        pool.add(typeerror_function, [], {}, deps=[])
        raised = False
        # For some reason assertRaises does not accept this
        # exception. Maybe because it has been pickled?
        try:
            pool.join()
        except TypeError:
            raised = True
        self.assertEqual(raised, True)

    def testIOFailure(self):
        '''Check that IO failures are reported'''
        pool = BatchJobPool(24)
        pool.add(ioerror_function, [], {}, deps=[])
        raised = False
        # For some reason assertRaises does not accept this
        # exception. Maybe because it has been pickled?
        try:
            pool.join()
        except IOError:
            raised = True
        self.assertEqual(raised, True)

    def testUnpickleableFailure(self):
        '''Check that unpickleable Exceptions are reported'''
        pool = BatchJobPool(24)
        pool.add(unpickleable_error_function, [], {}, deps=[])
        raised = False
        # For some reason assertRaises does not accept this
        # exception. Maybe because it has been pickled?
        try:
            pool.join()
        except Exception as e:
            self.assertIn("MyEx", str(e))
            raised = True
        self.assertEqual(raised, True)
