#! /bin/sh
# Copyright 2016, Wolfgang Mauerer <wm@linux-kernel.net>
# -*- yaml -*-
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

# Helper script to start the id service

if [ -d /vagrant ]; then
    # We can use absolute paths in a vagrant based install
    # so that the script can be used from any location
    nodejs /vagrant/id_service/id_service.js /vagrant/codeface.conf
else
    # Otherwise, assume we're in the same directory as id_service.js,
    # and the copnfiguration file is supposed to be one level above
    if [ -e ../codeface.conf ]; then
	nodejs id_service.js ../codeface.conf
    else
	echo "Could not find codeface configutation file, please"
	echo "run the script from directory $CODEFACE/id_service/"
	exit 1
    fi
fi
