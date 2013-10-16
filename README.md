# Installation notes for Codeface

Note that only the sections /Git Clone/ and /Analysis Setup/ are relevant if
your machine is already set up for Codeface. If you want to use your own
database instance, you can search/replace 'quantarch' with 'my_database_name'
in the step "Database Setup", and modify codeface.conf accordingly.

## Required Programs
* Install GNU R, mysql, mysql workbench, node.js and npm from the distribution repository

* Graphviz often comes in ancient versions with distributions. For Ubuntu
  12.04, use the recent packages fro AT&T:
  http://www.graphviz.org/Download_linux_ubuntu.php/x86_64/graphviz_2.30.1-1~precise_amd64.deb
  (2.30 is fine; download and install the main package,
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

  Codeface has been tested to work with R 2.15.3 and 3.0.1.

* Make sure that the following distribution packages are installed (for
  Ubuntu, most likely non-exhaustive):

        libopenmpi-dev, openmpi-bin, libxml2-dev,
        libcurl4-openssl-dev, xorg-dev, libx11-dev, libgles2-mesa-dev,
        libglu1-mesa-dev, graphviz, libmysqlclient-dev, python-mysqldb, sinntp,
        texmf-all, exuberant-ctags (v5.9), libcairo2-dev, libxt-dev, default-jdk,
        libcairo2-dev, libmysqlclient-dev, python-dev, exuberant-ctags

  (NOTE: texmf-all can likely be replaced with a smaller package, and it's only for
  producing the textual reports.  However, keeping the possibility to generate
  static reports would actually be a feature).

## Preparing the R installation

* Run `sudo R CMD javareconf`; make sure that the tool reports success in
  finding a java version and compiling programs with the native interface.

* Install RGraphviz from bioconductor. In an R shell, execute

        source("http://bioconductor.org/biocLite.R")
        biocLite("Rgraphviz")

* Install the required R packages in an R shell with

        install.packages(c("statnet", "ggplot2", "tm", "tm.plugin.mail", "optparse",
                           "igraph", "lsa", "zoo", "xts", "lubridate", "xtable",
                           "reshape", "wordnet", "stringr", "yaml", "plyr",
                           "scales", "gridExtra", "scales", "RMySQL",
                           "RCurl", "mgcv", "shiny", "dtw", "httpuv", "devtools",
                           "corrgram"), dependencies=T)

  If necessary, make sure _before_ the installation that
  `/usr/local/lib/R/site-library/` is writeable by the current user
  so that the packages are made available system-wide.

  NOTE: In case problems with old versions of installed packages are
  encountered when the additional packages are installed, it can be helpful
  to run `update.packages(ask="graphics")` before `install.packages()`.

  NOTE2: Installing some of the packages and dependent sub-packages
  can fail because development headers are missing on the base
  system. This can be mended by installing the appropriate distribution
  packages, and the re-trying the R package installation.

* Some packages for R need to be installed from github resp. r-forge:

        devtools::install_github("shiny-gridster", "wch")

* Currently, the development versions of `tm.plugin.mail` and `snatm` need to
  be installed. Clone with

        svn checkout svn://r-forge.r-project.org/svnroot/tm-plugin-mail/
        svn checkout svn://r-forge.r-project.org/svnroot/snatm

  and install each package with `cd pkg; R CMD INSTALL .`.

## Installing Python packages

* Install the required python packages using pip resp. easy_install:

        sudo -E pip install pyyaml progressbar python-ctags
        sudo -E easy_install python-ctags

## Clone the git repository

* Create a base directory `$BASEDIR` for the software.

* Clone the Codeface repository into `$BASEDIR` with

        git clone https://github.com/siemens/codeface

  which results in the directory `$BASEDIR/codeface` (referred to as `$CFDIR`
  in the following)

## Database Setup

WARNING: This step will irrevocably delete any data already stored in the database!

* Create a database user quantarch with sufficient privileges
  to create tables and modify these
  mysql-workbench -> Manage Security -> Users and Privileges ->
  Add Account. Then go to Schema Privileges, select quantarch,
  Choose "Select ALL". Finally, limit the connectivity to localhost.

* For a fresh setup, install the database schema from
  `$CFDIR/datamodel/QuantArch.mwb` respectively
  `$CFDIR/datamodel/quantarchSchema.sql`:

        mysql -uquantarch -pquantarch < quantarchSchema.sql

## Build the Bug extractor
See `bugextractor/INSTALL` for all java-related details.

## Prerequisites for the ID service
* Make sure to follow the instructions in `id_service/README` to obtain the
  required node.js packages.

## Analysis Setup

To get a `codeface` executable in your `$PATH`; go to `$CFDIR` and run:

        python setup.py develop --user

To analyse a project:

* Clone the desired git repositories into some directory
* Download the desired mailing lists into some directory
* Start the ID server: `cd $CFDIR/id_service/; nodejs id_service.js ../codeface.conf`
* Run `codeface`, see the command line help for usage examples

## Generate HTML Documentation

* To generate the Sphinx documentation for the codeface python classes, go
  to $CFDIR and run:

        python setup.py build_sphinx

The resulting documentation is found in `$CFDIR/build/sphinx/html`
* To generate the python HTML documentation, run `python setup.py`.
