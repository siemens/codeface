#! /usr/bin/env Rscript

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

source("../common.server.r", chdir=TRUE)
source("../../widgets/commit.structure.r", chdir=TRUE)

shinyServer(function(input, output, clientData, session) {
  pid = common.server.init(output, session, "commit.structure")
  output$princompPlot <- commit.structure.plot.princomp(pid)
  output$mdsPlot <- commit.structure.plot.mds(pid)
  output$quantarchContent <- renderUI({
    div(
      tabsetPanel(
        tabPanel("Principal components", plotOutput("princompPlot")),
        tabPanel("Multi-Dimensional Scaling", plotOutput("mdsPlot"))
      )
    )
  })
})

