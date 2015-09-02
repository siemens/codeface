#!/bin/sh

echo "Providing codeface python"

sudo pip install --upgrade -q setuptools mock

# Only development mode works
# install fails due to R scripts accessing unbundled resources!
# TODO Fix the R scripts
sudo python2.7 setup.py -q develop

