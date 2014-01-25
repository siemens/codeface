# -*- mode: ruby -*-
# vi: set ft=ruby :

# Copyright Roger Meier <roger@bufferoverflow.ch>
# SPDX-License-Identifier:	Apache-2.0 BSD-2-Clause GPL-2.0+ MIT WTFPL

$build_and_test = <<SCRIPT
echo "Provisioning system to compile, test and develop."
cd /vagrant

sudo -E apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
echo deb http://cran.r-project.org/bin/linux/ubuntu precise/ > /tmp/r-project
sudo mv /tmp/r-project /etc/apt/sources.list.d/r-project.list

sudo apt-get update -qq -y
sudo DEBIAN_FRONTEND=noninteractive apt-get -qq -y -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" upgrade

# GNU R
sudo apt-get -qq -y install r-base r-base-dev

# MySQL with vagrant password
sudo debconf-set-selections <<< 'mysql-server mysql-server/root_password password vagrant'
sudo debconf-set-selections <<< 'mysql-server mysql-server/root_password_again password vagrant'
sudo apt-get -y install mysql-server mysql-common mysql-client

# setup Database
sudo mysql --user=root --password=vagrant -e "create database codeface; GRANT ALL PRIVILEGES ON codeface.* TO codeface@localhost IDENTIFIED BY 'codeface'"
sudo mysql -ucodeface -pcodeface < datamodel/codeface_schema.sql
 

# Generic packages
sudo apt-get -qq -y install python-mysqldb sinntp texlive default-jdk \
                            python-dev \
                            exuberant-ctags git subversion \
                            libgles2-mesa python-pip sloccount

# Devel packages required to build the R packages below from source
sudo apt-get -qq -y install libxml2-dev libcurl4-openssl-dev xorg-dev \
                            libx11-dev libgles2-mesa-dev libglu1-mesa-dev \
                            libmysqlclient-dev libcairo2-dev libxt-dev \
                            libcairo2-dev libmysqlclient-dev

# Devel packages required for python packages
sudo apt-get -qq -y install libyaml-dev
sudo -E pip install pyyaml progressbar python-ctags

# install a recent node.js version
sudo apt-get install -qq -y python-software-properties python g++ make
sudo add-apt-repository -y ppa:chris-lea/node.js
sudo apt-get update -qq -y
sudo apt-get install -qq -y nodejs
sudo npm install addressparser express js-yaml mysql -g

# install R packages
sudo R CMD javareconf
sudo cp .Rprofile ~/.Rprofile
sudo R --save < packages.R 

sudo python setup.py develop --user
sudo -E npm install -g \
        https://github.com/JohannesEbke/shiny-server/archive/no-su.tar.gz
shiny-server shiny-server.config &

# TODO: setup the missing stuff

echo "provisioning of Codeface done."
echo "see http://siemens.github.io/codeface/ for further info"
SCRIPT

Vagrant.configure("2") do |config|
  # Hmm... no Debian image available yet, let's use a derivate
  # Ubuntu 12.04 LTS (Precise Pangolin)
  config.vm.box = "precise64"
  config.vm.box_url = "http://files.vagrantup.com/precise64.box"

  config.vm.provider :virtualbox do |vbox|
    vbox.customize ["modifyvm", :id, "--memory", "1024"]
    vbox.customize ["modifyvm", :id, "--cpus", "2"]
  end

  config.vm.network :forwarded_port, guest: 8081, host: 8081

  # call the script
  config.vm.provision :shell, :inline => $build_and_test

end
