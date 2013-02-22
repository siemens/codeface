#! /usr/bin/env python
# Convert the dot output of igraph into something suitable for neato

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

import sys
import re
import math

edges = {}
edge_spec = re.compile("\s+(\d+) -> (\d+);")

lines = [line.strip("\n") for line in sys.stdin]
# Modify the header (copyright line + digraph)
lines[0] = "digraph {"
lines[1] = "node[fontsize=30, shape=\"box\"];"
#lines[1] = "node[fontsize=30];"

lines[len(lines)-1] = "" # Skip closing brace

for line in lines:
    m = re.match(edge_spec, line)
    if m:
        a = m.group(1)
        b = m.group(2)
        if not edges.has_key((a,b)):
            edges[(a,b)] = 1
        else:
            edges[(a,b)] += 1
    else:
        print line

for ((a, b), count) in edges.iteritems():
    print("{0} -> {1} [weight={2} penwidth={3}];".format(a,b,count, math.sqrt(float(count))))

print "overlap=prism;"
print "splines=true;"
print "}"
