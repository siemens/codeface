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
tar -xvf cppstats.tar.gz
export CPPSTATS=$PWD/cppstats-$CPPSTATS_VERSION/
echo '#!/bin/bash' > $CPPSTATS/cppstats
echo "cd $CPPSTATS" >> $CPPSTATS/cppstats
echo "PYTHONPATH=\"\$PYTHONPATH:$CPPSTATS/lib\" ./cppstats.py \"\$@\"" >> $CPPSTATS/cppstats
chmod +x $CPPSTATS/cppstats

echo "Providing srcML"
SRCML_URL="http://131.123.42.38/lmcrs/srcML-Ubuntu12.04-64.tar.gz"
wget --quiet ${SRCML_URL} -O ${TMPDIR}/srcML.tar.gz
if [ ! -e ${TMPDIR}/srcML.tar.gz ]
then
    echo "Could not download srcML from ${SRCML_URL}"
    exit 1
fi
tar -xvf ${TMPDIR}/srcML.tar.gz
cp -rf $PWD/srcML/* $CPPSTATS/lib/srcml/linux/

sudo ln -sf $CPPSTATS/cppstats /usr/local/bin/cppstats

popd
