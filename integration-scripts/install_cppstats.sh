#!/bin/bash
# NOTE: bash instead of sh is required for pushd/popd

# Copyright Roger Meier <roger@bufferoverflow.ch>
# Copyright Wolfgang Mauerer <wolfgang.mauerer@oth-regensburg.de>
# SPDX-License-Identifier:	Apache-2.0 BSD-2-Clause GPL-2.0+ MIT WTFPL

export CPPSTATS_VERSION=0.9.3

echo "Providing cppstats $CPPSTATS_VERSION"

pushd .
TMPDIR=`mktemp -d` || exit 1
cd ${TMPDIR}

CPPSTATS_URL="https://codeload.github.com/clhunsen/cppstats/tar.gz/v${CPPSTATS_VERSION}"
wget --quiet ${CPPSTATS_URL} -O ${TMPDIR}/cppstats.tar.gz
if [ ! -e ${TMPDIR}/cppstats.tar.gz ]
then
    echo "Could not download cppstats from ${CPPSTATS_URL}"
    exit 1
fi
(cd ${TMPDIR} &&
    tar -xvf ${TMPDIR}/cppstats.tar.gz &&
    cd cppstats-${CPPSTATS_VERSION} &&
    sudo python setup.py install)

echo "Providing srcML"
SRCML_URL="http://131.123.42.38/lmcrs/beta/srcML-Ubuntu14.04-64.deb"
wget --quiet ${SRCML_URL} -O ${TMPDIR}/srcML.deb
if [ ! -e ${TMPDIR}/srcML.deb ]
then
    echo "Could not download srcML from ${SRCML_URL}"
    exit 1
fi
sudo dpkg -i ${TMPDIR}/srcML.deb

popd
