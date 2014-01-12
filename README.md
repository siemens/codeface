# Installation notes for Codeface

Note that only the sections /Git Clone/ and /Analysis Setup/ are relevant if
your machine is already set up for Codeface. If you want to use your own
database instance, you can search/replace 'quantarch' with 'my_database_name'
in the step "Database Setup", and modify codeface.conf accordingly.

## Required Programs
* Install GNU R, mysql, mysql-workbench 6 (see below), node.js and npm from the distribution repository

* Install mysql-workbench 6
  Version 5.x is not sufficient because there are subtle differences
  when it comes to handling the binary ER models that make life fairly
  hard. Since the latest revision is not included in Ubuntu 12.04, you
  need to download the package directly from Oracle
  (http://dev.mysql.com/downloads/tools/workbench/) and install it
  via the usual distribution mechanisms.

* Graphviz often comes in ancient versions with distributions. For Ubuntu
  12.04, use the recent packages fro AT&T:
  http://www.graphviz.org/Download_linux_ubuntu.php
  (>= 2.30 is fine; download and install the main package and
  libgraphviz4{,-dev}. There may be some additional prerequisites for the
  packages that can be satisfied from the distro repo)

* Make sure that GNU R is available in a sufficiently new release. Not sure
  which one is exactly the oldest possible one, but Ubuntu tends to have
  fairly vintage stuff available. To install the packages from CRAN, use

        sudo -E apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

  Add `deb http://cran.r-project.org/bin/linux/ubuntu precise/`
  to `/etc/apt/sources.list`, and execute

        sudo -E apt-get update
        sudo -E apt-get install r-base r-base-dev

  Codeface has been tested to work with R 2.15.x and 3.0.x.

* Make sure that the following distribution packages are installed (the
  list is for sufficient for a pristine default installation of
  Ubuntu 12.04 Desktop):

         # Generic packages
         sudo apt-get install python-mysqldb sinntp texlive default-jdk \
                              mysql-common mysql-client mysql-server python-dev \
                              exuberant-ctags nodejs npm git subversion \
                              libgles2-mesa python-pip sloccount graphviz

         # Devel packages required to build the R packages below from source
         sudo apt-get install libxml2-dev libcurl4-openssl-dev xorg-dev \
                              libx11-dev libgles2-mesa-dev libglu1-mesa-dev \
                              libmysqlclient-dev libcairo2-dev libxt-dev \
                              libcairo2-dev libmysqlclient-dev

         # Devel packages required for python packages
         sudo apt-get install libyaml-dev

## Preparing the R installation

* Run `sudo R CMD javareconf`; make sure that the tool reports success in
  finding a java version and compiling programs with the native interface.

* Install RGraphviz from bioconductor. In an R shell, execute

        source("http://bioconductor.org/biocLite.R")
        biocLite("Rgraphviz")

* Install the required R packages in an R shell with

        install.packages(c("statnet", "ggplot2", "tm", "tm.plugin.mail", "optparse",
                           "igraph", "zoo", "xts", "lubridate", "xtable",
                           "reshape", "wordnet", "stringr", "yaml", "plyr",
                           "scales", "gridExtra", "scales", "RMySQL",
                           "RCurl", "mgcv", "shiny", "dtw", "httpuv", "devtools",
                           "corrgram", "logging", "png", "rjson", "lsa"), dependencies=T)

  If necessary, make sure _before_ the installation that
  `/usr/local/lib/R/site-library/` is writeable by the current user
  so that the packages are made available system-wide.

* Some packages for R need to be installed from github resp. r-forge:

        devtools::install_github("shiny-gridster", "wch")

* Currently, the development versions of `tm.plugin.mail` and `snatm` need to
  be installed. In a R session, use

        install.packages(c("snatm", "tm-plugin-mail",
                         repos="http://R-Forge.R-project.org")

  Should an installable package be unavailable on R-Forge (which can
  happen from time to time), clone the source manually

        svn checkout svn://r-forge.r-project.org/svnroot/tm-plugin-mail/
        svn checkout svn://r-forge.r-project.org/svnroot/snatm

  and install each package with `cd pkg; R CMD INSTALL .`.

## Installing Python packages

* Install the required python packages using pip:

        sudo -E pip install pyyaml progressbar python-ctags

## Clone the git repository

* Create a base directory `$BASEDIR` for the software.

* Clone the Codeface repository into `$BASEDIR` with

        git clone https://github.com/siemens/codeface

  which results in the directory `$BASEDIR/codeface` (referred to as `$CFDIR`
  in the following)

## Database Setup

NOTE: Updating the database schema after analyses have been performed
will naturally delete all existing data stored in the schema.

* Create a database user quantarch with sufficient privileges
  to create and modify tables: Start mysql-workbench and connect
  to the database.

  * Select Management->Users and Privileges
  * Click "Add Account", and create a new user (you may want to limit
    the connectivity to localhost). Click apply.
  * Select tab "Schema Privileges", click "Add Entry", and
    click "Select ALL". Click Apply.
  * Select tab "Administrative Roles", and select DBDesigner and
    DBManager. Click Apply.

* For a fresh setup, install the database schema from
  `$CFDIR/datamodel/codeface.mwb` respectively
  `$CFDIR/datamodel/codeface_schema.sql`:

        mysql -ucodeface -pcodeface < codeface_schema.sql

## Build the Bug extractor
See `bugextractor/INSTALL` for all java-related details.

## Prerequisites for the ID service
* Ubuntu 12.04 comes with a fairly outdated node.js release.
  Make sure to install a more recent one (binary packages
  are provided at http://nodejs.org/dist/), and pick a stable
  release (even version numbers, 0.10.x at the time of writing).
* The ID service requires a few node.js packages. Install them by running

        npm install addressparser express js-yaml mysql

  in the `id_service` directory.

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
