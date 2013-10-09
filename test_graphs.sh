#!/bin/bash
# Copyright Siemens AG 2013
#
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

echo 'Dynamic graph interactive testing script'
echo '----------------------------------------'
echo 'Please open http://localhost:8100/?projectid=X in a web browser (where X is the project ID of a project in the database.'
echo 'This script will serve each dynamic graph in sequence.'
echo 'Press CTRL-C to advance to the next graph; refresh the browser, and check if the graph is working'
echo
echo
for graph in $(prosoda dynamic --list | tail -n +2 | cut -b4-); do 
    echo "Showing graph: $graph"
    prosoda dynamic $graph
done
echo 'All graphs served.'
