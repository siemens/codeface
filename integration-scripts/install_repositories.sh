#!/bin/sh

sudo apt-get update -qq
sudo DEBIAN_FRONTEND=noninteractive apt-get -qqy install software-properties-common python-software-properties

echo "Adding R cran repositories"
echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list
gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -

#echo "Adding node.js repository"
#sudo add-apt-repository -y ppa:chris-lea/node.js

sudo apt-get update -qq

