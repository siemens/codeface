#!/bin/sh

export CPPSTATS_VERSION=0.8.4

echo "Providing cppstats $CPPSTATS_VERSION"

mkdir -p vendor/
cd vendor/

wget --quiet https://codeload.github.com/clhunsen/cppstats/tar.gz/v$CPPSTATS_VERSION -O /tmp/cppstats.tar.gz
tar -xvf /tmp/cppstats.tar.gz
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

cd ..
