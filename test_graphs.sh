#!/bin/bash
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
