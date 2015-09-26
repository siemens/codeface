## This file is part of Codeface. Codeface is free software: you can
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
## Copyright 2013 by Wolfgang Mauerer <wolfgang.mauerer@oth-regensburg.de>
## All Rights Reserved.

## Definitions to simplify interacting with the Codeface database,
## usually for explorative research

## Use create.conf() to create a configuration object.
## Then, use standard query functions and the associated processing
## mechanisms to work with the database content.

source("config.r")
source("query.r")

create.conf <- function(codeface.conf, project.conf=NULL) {
  ## Load configuration file(s)
  conf <- load.config(codeface.conf, project.conf)

  ## Open up the corresponding database connection
  if(is.null(project.conf)) {
    conf <- init.db.global(conf)
  } else {
    conf <- init.db(conf)
  }

  return(conf)
}


## Example session: Plot the initiation-response network
########################################################

### Create a configuration object for project busybox
### Run this in codeface/R (or setwd() appropriately)
#source(interactive.r)
#conf <- create.conf("/path/to/codeface.conf", "/path/to/busybox.conf")
#
### Load definitions of interest
#source("ml/init.response.r")
#
### Obtain a list of range ids. Evert analysed release range is
### uniquely identified by a single numeric id
#range.ids <- query.range.ids(conf)
#
### Query data of interest (in this case, the initate-response
### structure for the developer mailing list). The analysis range
### is chosen arbitrarily
#ml.id <- query.ml.id(conf, "dev")
#dat <- query.initiate.response(conf$con, ml.id, range.ids[[3]])
#dat <- prepare.initiate.response(dat)
#g <- plot.init.response(g, title="Initate-response structure for busybox")
#
### Store the resulting graphs
#ggsave("/tmp/ir1.pdf", g[[1]])
#ggsave("/tmp/ir2.pdf", g[[2]])
