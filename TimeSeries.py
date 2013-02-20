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
# Copyright 2013 by Siemens AG< Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
# All Rights Reserved.

class TimeSeries:
    """Simple wrapper for a time series.

    The raw data are stored in a plain array, augmented with information
    about start end end date of the series. It's not sufficient to take
    the first end final list element for this purpose because there may
    be data points outside the intended range."""
    
    def __init__(self, subsys_names = [], ID=None, name="", email=""):
        self.series = []
        self.start = -1
        self.end = -1

    def set_start(self, _start):
        self.start = _start

    def get_start(self):
        if (self.start == -1):
            raise Exception("Time series start date is undefined")
        return self.start

    def set_end(self, _end):
        self.end = _end

    def get_end(self):
        if (self.end == -1):
            raise Exception("Time series end date is undefined")
        return self.end
