#!/bin/sh
# Copyright Roger Meier <roger@bufferoverflow.ch>
# Copyright Claus Hunsen <hunsen@fim.uni-passau.de>
# SPDX-License-Identifier:	Apache-2.0 BSD-2-Clause GPL-2.0+ MIT WTFPL

echo "Providing codeface database"

sudo mysql -e "CREATE DATABASE codeface;" -uroot
sudo mysql -e "CREATE DATABASE codeface_testing;" -uroot
sudo mysql -e "CREATE USER 'codeface'@'localhost' IDENTIFIED BY 'codeface';" -uroot
sudo mysql -e "GRANT ALL PRIVILEGES ON * . * TO 'codeface'@'localhost';" -uroot

DATAMODEL="datamodel/codeface_schema.sql"
mysql -ucodeface -pcodeface < ${DATAMODEL}
cat ${DATAMODEL} | sed -e 's/codeface/codeface_testing/g' | mysql -ucodeface -pcodeface

