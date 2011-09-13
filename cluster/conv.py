#! /usr/bin/env python
# Convert the dot output of igraph into something suitable for neato
# Use, for instance, like
# for file in `ls prefix*.dot`; do
#    basefile=`basename $file .dot`;
#    echo "Processing $file";
#    cat $file | ../../conv.py | sfdp -Tpdf > ${basefile}.pdf;
# done
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
