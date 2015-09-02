#!/bin/sh

sudo apt-get update -qq
sudo DEBIAN_FRONTEND=noninteractive apt-get -qqy install software-properties-common python-software-properties

echo "Adding R cran repositories"
sudo add-apt-repository -y ppa:marutter/rrutter
sudo add-apt-repository -y ppa:marutter/c2d4u
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

echo "Adding node.js repository"
sudo add-apt-repository -y ppa:chris-lea/node.js

sudo apt-get update -qq

