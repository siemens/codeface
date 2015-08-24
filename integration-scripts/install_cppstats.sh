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
wget --quiet http://sdml.info/lmcrs/srcML-Ubuntu12.04-64.tar.gz -O /tmp/srcML.tar.gz
tar -xvf /tmp/srcML.tar.gz
cp -rf $PWD/srcML/* $CPPSTATS/lib/srcml/linux/

sudo ln -sf $CPPSTATS/cppstats /usr/local/bin/cppstats

cd ..
