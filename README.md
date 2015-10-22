# Installation notes for Codeface

## Installing Codeface
The recommended way to set up a Codeface instance is via
vagrant. Clone the repository and run

	vagrant up

to obtain a fully provisioned Codeface machine. Vagrant defaults to
Virtualbox as provider, which may cause large performance impacts
especially for I/O heavy tasks. You can /alternatively/ use

	vagrant up --provider=lxc

if you have the corresponding LXC provider for vagrant installed on
your system. To access the machine in each case, use

	vagrant ssh

## Analysis Setup

To get a `codeface` executable in your `$PATH`; go to `$CFDIR` and run:

        python setup.py develop --user

To analyse a project:

* Clone the desired git repositories into some directory
* Download the desired mailing lists into some directory
* Start the ID server: `cd $CFDIR/id_service/; nodejs id_service.js ../codeface.conf`
* Run `codeface`, see the command line help for usage examples

## Web server setup
There are two options to set up an instance of the web frontend server:

* Using a self-contained tarball prepared on a machine with proper
  internet connection (to be deployed on machines without network
  access or behind restrictive corporate firewalls):
  Run `bash shiny-server-pack.sh`, copy the resulting
  `shiny-server-pack.tar.gz` to the destination machine and unpack
  it into $CFDIR. Start the server with `shiny-server.sh`.

* Global installation: Run

        sudo -E npm install -g \
        https://github.com/JohannesEbke/shiny-server/archive/no-su.tar.gz

  to install shiny server (respectively the customised version which
  supports operation without root privileges) into the global
  node package repo. Start with

        shiny-server shiny-server.config

  in `$CFDIR`.

  In the default configuration, the web frontend is available
  on http://localhost:8081/.

## Generate HTML Documentation

* To generate the Sphinx documentation for the codeface python classes, go
  to $CFDIR and run:

        python setup.py build_sphinx

The resulting documentation is found in `$CFDIR/build/sphinx/html`
* To generate the python HTML documentation, run `python setup.py`.
