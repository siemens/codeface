#!/bin/bash
export WD=$PWD
export MYNODEJS=$WD/node-v0.10.13-linux-???/bin/node
$MYNODEJS $WD/node-v0.10.13-linux-???/lib/node_modules/shiny-server/lib/main.js $WD/shiny-server.config
