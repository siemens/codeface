# -*- mode: ruby -*-
# vi: set ft=ruby :

# Copyright Roger Meier <roger@bufferoverflow.ch>
# SPDX-License-Identifier:	Apache-2.0 BSD-2-Clause GPL-2.0+ MIT WTFPL

$build = <<SCRIPT
cd /vagrant

integration-scripts/install_repositories.sh
integration-scripts/install_common.sh
integration-scripts/install_codeface_R.sh
integration-scripts/install_codeface_node.sh
integration-scripts/install_codeface_python.sh

integration-scripts/install_cppstats.sh

integration-scripts/setup_database.sh

# Ensure that logs can actually be written to the log directory
sudo chmod a+rw log
SCRIPT

Vagrant.configure("2") do |config|
  # Hmm... no Debian image available yet, let's use a derivate
  # Ubuntu 12.04 LTS (Precise Pangolin)

 config.vm.provider :virtualbox do |vbox, override|
   config.vm.box = "ffuenf/ubuntu-16.04-server-amd64"

    vbox.customize ["modifyvm", :id, "--memory", "4096"]
    vbox.customize ["modifyvm", :id, "--cpus", "2"]
  end

  config.vm.provider :lxc do |lxc, override|
     override.vm.box = "vagrant-lxc-xenial-amd64.box"
     override.vm.box_url = "http://terminal.lfd.sturhax.de/~wolfgang/vagrant-lxc-xenial-amd64.box"
  end

  # Forward main web ui (8081) and testing (8100) ports
  config.vm.network :forwarded_port, guest: 8081, host: 8081
  config.vm.network :forwarded_port, guest: 8100, host: 8100

  config.vm.provision "fix-no-tty", type: "shell" do |s|
    s.privileged = true
    s.inline = "sed -i '/tty/!s/mesg n/tty -s \\&\\& mesg n/' /root/.profile"
  end

  config.vm.provision "local-mirror", type: "shell" do |s|
    s.privileged = true
    s.inline = "sed -i 's|http://[a-z\.]*\.ubuntu\.com/ubuntu|mirror://mirrors\.ubuntu\.com/mirrors\.txt2|' /etc/apt/sources.list"
  end

  config.vm.provision "build", type: "shell" do |s|
    s.privileged = false
    s.inline = $build
  end

  config.vm.provision "test", type: "shell" do |s|
    s.privileged = false
    s.inline = "cd /vagrant && integration-scripts/test_codeface.sh"
  end

end
