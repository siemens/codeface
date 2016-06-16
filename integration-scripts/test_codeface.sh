#!/bin/bash
# Copyright Matthias Dittrich <matthi.d@gmail.com>
# SPDX-License-Identifier:	Apache-2.0 BSD-2-Clause GPL-2.0+ MIT WTFPL

cd "id_service"
node id_service.js ../codeface.conf &
node_job=$!
cd ..

codeface test -c codeface.conf
codeface_exit=$?
kill $node_job
exit $codeface_exit
