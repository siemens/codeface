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
# Copyright 2013 by Siemens AG< Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
# All Rights Reserved.
"""Provide a generic wrapper for time series of commits."""


class TimeSeries(object):
    """Simple wrapper for a time series.

    The raw data are stored in a plain array, augmented with information
    about start end end date of the series. It's not sufficient to take
    the first end final list element for this purpose because there may
    be data points outside the intended range.

    Attributes:
        series (list): A list of dicts, using the following pattern:
            [{'commit': Commit ID, 'value': Payload}, ...]
        start (int): Unix timestamp of range start.
        end (int): Unix timestamp of range end.
        rc_start (int): Unix timestamp first RC inside the range or None.
    """
    # TODO This class is not using properties, but explicit getters and setters

    def __init__(self, subsys_names=[], ID=None, name="", email=""):
        self.series = []
        self.start = -1
        self.end = -1

        # None is a valid value since a time series does not necessarily
        # contain a rc start date
        self.rc_start = None

    def set_start(self, _start):
        self.start = _start

    def get_start(self):
        if self.start == -1:
            raise Exception("Time series start date is undefined")
        return self.start

    def set_end(self, _end):
        self.end = _end

    def get_end(self):
        if self.end == -1:
            raise Exception("Time series end date is undefined")
        return self.end

    def set_rc_start(self, _start):
        self.rc_start = _start

    def get_rc_start(self):
        return self.rc_start
