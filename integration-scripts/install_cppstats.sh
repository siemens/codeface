#!/bin/bash
# NOTE: bash instead of sh is required for pushd/popd

export CPPSTATS_VERSION=0.8.4

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
(cd /opt; sudo tar -xvf ${TMPDIR}/cppstats.tar.gz)
export CPPSTATS=/opt/cppstats-$CPPSTATS_VERSION/
echo '#!/bin/bash' > ${TMPDIR}/cppstats
echo "cd $CPPSTATS" >> ${TMPDIR}/cppstats
echo "PYTHONPATH=\"\$PYTHONPATH:/opt/$CPPSTATS/lib\" ./cppstats.py \"\$@\"" >> ${TMPDIR}/cppstats
chmod +x ${TMPDIR}/cppstats
sudo cp ${TMPDIR}/cppstats /usr/local/bin/cppstats

echo "Providing srcML"
SRCML_URL="http://131.123.42.38/lmcrs/srcML-Ubuntu14.04-64.tar.gz"
wget --quiet ${SRCML_URL} -O ${TMPDIR}/srcML.tar.gz
if [ ! -e ${TMPDIR}/srcML.tar.gz ]
then
    echo "Could not download srcML from ${SRCML_URL}"
    exit 1
fi
tar -xvf ${TMPDIR}/srcML.tar.gz
sudo cp -rf $PWD/srcML/* $CPPSTATS/lib/srcml/linux/

popd
