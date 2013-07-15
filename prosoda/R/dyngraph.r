## This file is part of prosoda.  prosoda is free software: you can
## redistribute it and/or modify it under the terms of the GNU General Public
## License as published by the Free Software Foundation, version 2.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
##
## Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

## Common Setup and command line arguments for Shiny dispatcher for commit statistics

suppressPackageStartupMessages(library(optparse))

source("config.r")
source("db.r")

## This function parses the command line and requires a single argument to be
## present, the prosoda configuration file. It then creates a conf object and
## amends it with a database connection.
dyngraph.config <- function() {
    parser <- OptionParser(usage = "%prog prosodaconfig")
    arguments <- parse_args(parser, positional_arguments = TRUE)
    if (length(arguments$args) != 1) {
        cat("Please specify configuration file\n")
        print_help(parser)
        stop()
    }
    conf <- load.global.config(arguments$args[1])
    conf <- init.db.global(conf)
    return(conf)
}
