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
## Copyright 2013 by Siemens AG, Johannes Ebke <johannes.ebke.ext@siemens.com>
## All Rights Reserved.

## This file should contain the overview widgets for the main dashboard

## Plan for status indicators for the different sections:
## * Collaboration ()
##   - G
## * Communication
##   - Very good: 
## * Complexity
## *

## Useful symbols
## 1F3AF -- direct hit
## 1F40C, 1f422 -- snail, turtle
## 1F41C -- ant (for bugs)
## 1f44d/e -- thumbs up/down
## 1F4C8/9/A -- chart up/down/barchart
## 1F4C8 -- cheering megaphone
## 2388 -- steering wheel
## 2B43 -- alternative collaboration

overview.html <- function(title, link, subtitle.size="100%") {
  html = function(id) {
    tagList(
      tags$div(class="grid_title", style="margin-bottom: 0px", title),
      tags$div(id=id,
              class="status_output",
              tags$div(class = 'grid_inserttext grid_bigtext', style="font-size:150px"),
              tags$p(style=paste("font-size:", subtitle.size, "; line-height: 30px"))
      ),
      p(a(href=link, "details..."))
    )
  }
}

createWidgetClass(
  "widget.overview.project",
  "Project Summary", "Short, one-widget project summary",
  1, 1,
  html=htmlOutput
)

initWidget.widget.overview.project <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$project.name <- reactive({query.project.name(conf$con, w$pid())})
  w$cycles <- reactive({get.cycles.con(conf$con, w$pid())})
  return(w)
}

renderWidget.widget.overview.project <- function(w) {
  renderUI({
    pid <- w$pid()
    cycles <- w$cycles()
    date.start <- as.Date(min(cycles$date.start))
    date.end <- as.Date(max(cycles$date.end))
    n.releases <- nrow(cycles)
    n.commits <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM commit WHERE projectId=", pid))[[1]]
    n.persons <- nrow(dbGetQuery(conf$con, str_c("SELECT 1 FROM person WHERE projectId=", pid)))
    n.issues <- nrow(dbGetQuery(conf$con, str_c("SELECT 1 FROM issue WHERE projectId=", pid)))
    n.mail.threads <- nrow(dbGetQuery(conf$con, str_c("SELECT 1 FROM mail_thread WHERE projectId=", pid)))

    make.indicator <- function(symbol, color) {
      div(style=paste("margin: auto; ", "width: 50px; ", "height: 50px;",
                      " -webkit-border-radius: 25px;",
                      "-moz-border-radius: 25px;",
                      "border-radius: 25px;",
                      #"border-width:0.1px; border-style:solid; border-color:black;",
                      "box-shadow: 2px 2px 2.5px rgb(0,0,0);",
                      "line-height: 50px; text-align: center; vertical-align: middle;",
                      "font-size: 25px;",
                      "background-color:", color, ";"),
                      symbol)
    }
    ## Collaboration indicator color
    indicator.summary <- intToUtf8(0x2705)

    if (n.commits > 1000) {
      color.collab <- color.good;
    } else if (n.commits > 10) {
      color.collab <- color.warn;
    } else {
      color.collab <- color.bad;
    }

    if (n.mail.threads > 1000) {
      color.comm <- color.good;
    } else if (n.mail.threads > 10) {
      color.comm <- color.warn;
    } else {
      color.comm <- color.bad;
    }

    color.complex <- color.warn;

    indicator.collaboration <- make.indicator(intToUtf8(0x21c4), color.collab)
    indicator.communication <- make.indicator(intToUtf8(0x1f4e7), color.comm)
    indicator.complexity <- make.indicator(intToUtf8(0x2102), color.complex)


    subtitle.size <- "100%"
    link <- paste("?projectid=", pid, sep="")
    tagList(
      tags$div(class="grid_title", style="margin-top: 10px; margin-bottom: 10px;", w$project.name()),
      tags$div(class='grid_bigtext', style="font-size:130px; text-align: center",
               indicator.summary
              ),
      tags$p(#style=paste("text-align: center; vertical-align: middle;"),
              tags$table(width="100%", tags$tr(
                                 tags$td(indicator.collaboration),
                                 tags$td(indicator.communication),
                                 tags$td(indicator.complexity)
                                 ))
            ),
      p(a(href=link, "details..."))
    )
  })
}

widgetColor.widget.overview.project <- function(w) {
  reactive({color.neutral})
}

createWidgetClass(
  "widget.overview.communication",
  "Communication", "Information on how developers communicate",
  1, 1,
  html=overview.html("Communication", "?topic=communication", "250%")
)

createWidgetClass(
  "widget.overview.collaboration",
  "Collaboration", "Information on how developers communicate",
  1, 1,
  html=overview.html("Collaboration", "?topic=collaboration", "500%")
)

createWidgetClass(
  "widget.overview.complexity",
  "Complexity", "Information on how developers communicate",
  1, 1,
  html=overview.html("Complexity", "?topic=complexity")
)

renderWidget.widget.overview.communication <- function(w) {
  #reactive({list(text=intToUtf8(0x2197), subtext="++")})
  reactive({list(text=intToUtf8(0x3020), subtext="++")})
}
widgetColor.widget.overview.communication <- function(w) {
  reactive({color.good})
}

renderWidget.widget.overview.collaboration <- function(w) {
  reactive({list(text=intToUtf8(0x219D), subtext=intToUtf8(0x21DD))})
}
widgetColor.widget.overview.collaboration <- function(w) {
  reactive({color.warn})
}

renderWidget.widget.overview.complexity <- function(w) {
  reactive({list(text=intToUtf8(0x21AF), subtext="not good")})
}
widgetColor.widget.overview.complexity <- function(w) {
  reactive({color.bad})
}

