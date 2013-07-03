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

## Load configuration and apply some (yet uncomprehensive) sanity checks
library(yaml)

load.config <- function(file) {
  conf <- yaml.load_file(file)

  if (is.null(conf$project) || is.null(conf$repo)) {
    stop("Malformed configuration: Specify project and repository!\n")
  }
  
  if (is.null(conf$nodejsHostname)) {
    conf$nodejsHostname <- "127.0.0.1"
  }

  if (is.null(conf$nodejsPort)) {
    conf$nodejsPort <- 8080
  } else {
    conf$nodejsPort <- as.integer(conf$nodejsPort)
  }

  if (conf$tagging != "tag" && conf$tagging != "committer2author" &&
      conf$tagging != "proximity") {
    stop("Malformed configuration: Invalid tagging mode specified!")
  }

  if(length(conf$revisions) < 2) {
    stop("Malformed configuration: Revision list must include at least 2 commits!")
  }

  if (length(conf$rcs) > 0 && (length(conf$revisions) != length(conf$rcs))) {
    stop("Malformed configuration: Revision and rcs lists must have same length!")
  }

  return(conf)
}

load.global.config <- function(file) {
  conf <- yaml.load_file(file)

  if (is.null(conf$dbhost) || is.null(conf$dbname) ||
      is.null(conf$dbuser) || is.null(conf$dbpwd)) {
    stop("Malformed global configuration: Database information is incomplete!\n")
  }

  return(conf)
}
