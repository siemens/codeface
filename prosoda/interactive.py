# Commands that are useful after adist.yp has been
# run in ipython

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
# Copyright 2010, 2011, 2012 by Wolfgang Mauerer <wm@linux-kernel.net>
# All Rights Reserved.

initialiseR()
git = shelve.open("/home/wolfgang/linux-14-33")["git"]
res = createSeries(git, "__main__", ["v2.6.24", "v2.6.25"])
writeToFile(res, "/home/wolfgang/raw.dat")
runR('raw = as.xts(read.zoo(file="/home/wolfgang/raw.dat", FUN=tstamp_to_date))')
runR('reg = to.regts(raw[,1], 250)')
reg = RtoPython(runR('reg'))
raw = RtoPython(runR('raw'))

# ... and then commence with the analysis as desired
