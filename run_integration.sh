#!/bin/bash

cd "id_service"
node id_service.js ../codeface.conf &
node_job=$!
cd ..

PYTHONPATH=$PWD ./codeface/runCli.py test -c codeface.conf
codeface_exit=$?
kill $node_job
exit $codeface_exit