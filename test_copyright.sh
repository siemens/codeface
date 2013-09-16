#!/bin/bash

for f in $(find prosoda -name '*.py' -or -iname '*.r'); do
    if [[ -z $(grep "# Copyright " $f) ]]; then 
        echo "Missing Copyright: $f"
    fi
done;

