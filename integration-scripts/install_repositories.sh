#!/bin/sh

sudo apt-get update -qq
sudo DEBIAN_FRONTEND=noninteractive apt-get -qqy install software-properties-common python-software-properties

echo "Adding R cran repositories"
version=`lsb_release -r | awk '{ print $2;}'`

case ${version} in
    "14.04")
	echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" | sudo tee -a /etc/apt/sources.list
	;;
    "16.04")
	echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list
	;;
    *) echo "Unsupported version of Ubuntu detected, aborting"
       exit 1;;
esac

gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -

#echo "Adding node.js repository"
#sudo add-apt-repository -y ppa:chris-lea/node.js

sudo apt-get update -qq

