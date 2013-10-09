#!/bin/bash
# Copyright Siemens AG 2013
#
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

for f in $(find prosoda -name '*.py' -or -iname '*.sh' -or -iname '*.r' \
                        -or -iname '*.css' -or -iname '*.js' \
                        -or -iname '*.html'); do
    if [[ -z $(grep "Copyright " $f) ]]; then 
        echo "Missing Copyright: $f"
    fi
done;

