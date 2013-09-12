#! /usr/bin/env Rscript

# This file is part of prosoda.  prosoda is free software: you can
# redistribute it and/or modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation, version 2.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# Copyright 2013 by Siemens AG, Johannes Ebke <johannes.ebke.ext@siemens.com>

library(testthat)
source("shiny/apps/common.server.r", chdir=TRUE)
source("shiny/widgets.r", chdir=TRUE)

# Only look at the first project in the database
projects.selected = 1:1

for(pid.num in projects.list$id[projects.selected]) {
  #print(paste("Testing PID", pid.num))
  pid <- reactive({ pid.num })
  pid.num <- isolate(pid())
  #observe({ print(paste("Observing PID", pid())) })
  for(widget in widget.list) {
    name <- widget$widget.classes[[1]]
    #print(paste("PID:", pid.num, "Testing widget:", widget$widget.classes[[1]]))
    test_that(paste(name, "can be created for project", pid.num), {
      w <- newWidget(widget, pid)
    })
    test_that(paste(name, "can be initialized for project", pid.num), {
      w <- initWidget(newWidget(widget, pid))
    })
    test_that(paste(name, "can be listed for project", pid.num), {
      w <- initWidget(newWidget(widget, pid))
      l <- listViews(w)
      isolate(l())
    })
  }
}

