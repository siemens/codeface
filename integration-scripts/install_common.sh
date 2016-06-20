#!/bin/sh
# Copyright Roger Meier <roger@bufferoverflow.ch>
# SPDX-License-Identifier:	Apache-2.0 BSD-2-Clause GPL-2.0+ MIT WTFPL

echo "Providing common binaries and libraries"

echo "mysql-server mysql-server/root_password password" | sudo debconf-set-selections
echo "mysql-server mysql-server/root_password_again password" | sudo debconf-set-selections

sudo DEBIAN_FRONTEND=noninteractive apt-get -qqy install sinntp texlive default-jdk \
	mysql-common mysql-client npm \
	mysql-server python-dev exuberant-ctags nodejs git subversion \
	sloccount graphviz doxygen libxml2-dev libcurl4-openssl-dev \
	libmysqlclient-dev libcairo2-dev libxt-dev libcairo2-dev libmysqlclient-dev \
	astyle xsltproc libxml2 libxml2-dev python build-essential libyaml-dev \
	gfortran python-setuptools python-pkg-resources python-numpy python-matplotlib \
	python-libxml2 python-lxml python-notify python-lxml gcc libarchive13 python-pip \
	libxml2-dev libcurl4-openssl-dev xorg-dev libx11-dev libgles2-mesa-dev \
	libglu1-mesa-dev libxt-dev libpoppler-dev libpoppler-glib-dev python-mock screen

