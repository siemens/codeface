#!/bin/bash

# Copyright Siemens AG 2013
#
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

export WD=$PWD
export MYNODEJS=$WD/node-v0.10.13-linux-???/bin/node
$MYNODEJS $WD/node-v0.10.13-linux-???/lib/node_modules/shiny-server/lib/main.js $WD/shiny-server.config
