#!/bin/bash
# Copyright Roger Meier <roger@bufferoverflow.ch>
# SPDX-License-Identifier:	Apache-2.0 BSD-2-Clause GPL-2.0+ MIT WTFPL

echo "Providing common binaries and libraries"

echo "mysql-server mysql-server/root_password password" | sudo debconf-set-selections
echo "mysql-server mysql-server/root_password_again password" | sudo debconf-set-selections

sudo DEBIAN_FRONTEND=noninteractive apt-get -qqy install sinntp texlive default-jdk \
	mysql-common mysql-client libgraphviz-dev libarchive13 libhunspell-dev \
	mysql-server python-dev exuberant-ctags nodejs git subversion libxslt1-dev \
	sloccount graphviz doxygen libxml2-dev libcurl4-openssl-dev \
	libmysqlclient-dev libcairo2-dev libxt-dev libcairo2-dev libmysqlclient-dev \
	astyle xsltproc libxml2 libxml2-dev python build-essential libyaml-dev \
	gfortran python-setuptools python-pkg-resources python-numpy python-matplotlib \
	python-libxml2 python-lxml python-notify python-lxml gcc python-pip \
	libxml2-dev libcurl4-openssl-dev xorg-dev libx11-dev libgles2-mesa-dev \
	libglu1-mesa-dev libxt-dev libpoppler-dev libpoppler-glib-dev python-mock

# Make sure that the mysql socket file is available as /var/run/mysqld/mysqld.sock
# and /tmp/mysql.sock.
# HACK: Remove this hack once R, python and node.js try to access the same file.
version=`lsb_release -r | awk '{ print $2;}'`
if [ "${version}" = "16.04" ]; then
    sudo sed -i "s/\/var\/run\/mysqld\/mysqld.sock/\/tmp\/mysql.sock/g;" /etc/mysql/mysql.conf.d/mysqld.cnf
    sudo service mysql restart # Deleted /var/run/mysqld, so restart before creating the link
    sudo ln -s /tmp/mysql.sock /var/run/mysqld/mysqld.sock
fi

## Make sure that the nodejs binary exists (older distributions
## might name the binary just node), and create an appropriate
## symbolic link if required
if [ ! -f /usr/bin/nodejs ]; then
    sudo ln -s /usr/bin/node /usr/bin/nodejs
fi
