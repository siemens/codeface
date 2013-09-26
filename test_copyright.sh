#!/bin/bash

for f in $(find prosoda -name '*.py' -or -iname '*.r' -or -iname '*.css' -or -iname '*.js' -or -iname '*.html'); do
    if [[ -z $(grep "Copyright " $f) ]]; then 
        echo "Missing Copyright: $f"
    fi
done;

