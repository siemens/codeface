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

from contextlib import contextmanager
import sys

@contextmanager
def hide_stderr():
    '''
    Context manager to temporarily hide stderr
    '''
    old_stderr = sys.stderr
    class Redirector(object):
        def write(self, *args):
            pass
    sys.stderr = Redirector()
    yield
    sys.stderr = old_stderr

@contextmanager
def hide_output():
    '''
    Context manager to temporarily hide stderr and stdout
    '''
    old_stderr = sys.stderr
    old_stdout = sys.stdout
    class Redirector(object):
        def write(self, *args):
            pass
    sys.stderr = Redirector()
    sys.stdout = Redirector()
    yield
    sys.stderr = old_stderr
    sys.stdout = old_stdout
