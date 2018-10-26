#!/bin/sh
# Copyright Roger Meier <roger@bufferoverflow.ch>
# SPDX-License-Identifier:	Apache-2.0 BSD-2-Clause GPL-2.0+ MIT WTFPL

echo "Providing id_service"

sudo DEBIAN_FRONTEND=noninteractive apt-get -qqy install npm
cd id_service
sudo mkdir node_modules
sudo chown vagrant.vagrant node_modules
npm set ca null
npm install --no-bin-links
cd ..
