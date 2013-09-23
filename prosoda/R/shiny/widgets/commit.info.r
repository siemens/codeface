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

## Get a commit.info.splom widget
createWidgetClass(
  class = c("widget.commit.info.splom", "widget.commit.info", "widget.rangeid"),
  name = "Commit Information - Scatterplot",
  description = "Shows the commit information in a scatterplot.",
  topics = c("basics", "construction"),
  size.x = 2,
  size.y = 1,
  compareable = FALSE
)

## Get a commit.info.corrgram widget
createWidgetClass(
  class = c("widget.commit.info.corrgram", "widget.commit.info", "widget.rangeid"),
  name = "Commit Information - Correlations",
  description = "Shows correlations in the commits.",
  topics = c("construction"),
  size.x = 1,
  size.y = 1,
  compareable = FALSE
)

## Common initialization of commit.info widgets
initWidget.widget.commit.info <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$data <- reactive({
    gen.commits.info(conf$con, w$pid(), w$view())
  })
  return(w)
}

# Render a Scatterplot
renderWidget.widget.commit.info.splom <- function(w) {
  renderPlot({
    gen.commits.splom(w$data()$cmt.info, w$data()$plot.types)
  })
}

# Render Correlations
renderWidget.widget.commit.info.corrgram <- function(w) {
  renderPlot({
    gen.commits.corrgram(w$data()$cmt.info, w$data()$plot.types)
  })
}

createWidgetClass(
  class = c("widget.commit.doc"),
  name = "Commit Documentation",
  description = "Shows the commit documentation quality. The red line gives the percentage of commits which have a commit message of five or more lines (currently including git-svn conversion comments). The blue line gives the percentage of commits with at least one signoff.",
  topics = c("basics", "construction"),
  size.x = 1,
  size.y = 1,
  compareable = FALSE
)

initWidget.widget.commit.doc <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$cycles <- reactive({get.cycles.con(conf$con, w$pid())})
  ## Get commit count per author, release range and weekend/not-weekend
  ## NOTE: Once the authorDate and authorTimezone are available,
  ## they should be used instead of WEEKDAY to get more accurate estimates
  w$dat <- reactive({
    sqlr <- dbGetQuery(conf$con, str_c("SELECT releaseRangeId,",
                                       " AVG(CmtMsgLines >= 5) as percentdoc,",
                                       " AVG(NumSignedOffs >= 1) as percentsignoffs",
                                       " FROM commit where projectId=", w$pid(),
                                       " GROUP BY releaseRangeId"))
    names(sqlr)[1] <- "range.id"
    merge(w$cycles(), sqlr)
  })
  return(w)
}

renderWidget.widget.commit.doc <- function(w) {
  renderPlot({
    dat <- melt(data.frame(cycle=w$dat()$cycle, commitmsg=w$dat()$percentdoc, signoffs=w$dat()$percentsignoffs))
    dat$variable <- revalue(dat$variable, c(commitmsg="% documented", signoffs="% signed off"))
    g <- ggplot(dat, aes(x=cycle, y=value, group=variable, colour=variable)) +
                geom_line() +
                scale_y_continuous(labels = percent_format()) +
                expand_limits(y=1) +
                expand_limits(y=0) +
                scale_colour_manual(values=c("red","blue"), name="") +
                xlab("Release range") +
                ylab("Percent") +
                theme(axis.text.x = element_text(angle = 30, hjust = 1, size=7), legend.position="bottom")
    print(g)
  })
}
