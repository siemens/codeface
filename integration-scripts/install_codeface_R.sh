#!/bin/sh
# Copyright Roger Meier <roger@bufferoverflow.ch>
# SPDX-License-Identifier:	Apache-2.0 BSD-2-Clause GPL-2.0+ MIT WTFPL

echo "Providing R libraries"

sudo DEBIAN_FRONTEND=noninteractive apt-get -qqy install r-base r-base-dev r-cran-ggplot2 r-cran-tm \
	r-cran-tm.plugin.mail r-cran-optparse r-cran-igraph r-cran-zoo r-cran-xts \
	r-cran-lubridate r-cran-xtable r-cran-reshape r-cran-wordnet \
	r-cran-stringr r-cran-yaml r-cran-plyr r-cran-scales r-cran-gridExtra \
	r-cran-scales r-cran-RMySQL r-cran-RJSONIO r-cran-RCurl r-cran-mgcv \
	r-cran-shiny r-cran-dtw r-cran-httpuv r-cran-png \
	r-cran-rjson r-cran-lsa r-cran-testthat r-cran-arules r-cran-data.table \
	r-cran-ineq libx11-dev libssh2-1-dev r-bioc-biocinstaller

sudo Rscript packages.R

