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

detailpage <- list(name="widget.contributions.overview,widget.contributions.multiCycles,widget.contributions.authorsPerCycle",
                   title="Authors / Contributions")

suppressPackageStartupMessages(library(reshape))

## Determine how many authors repeatedly contributed to how many cycles
gen.authors.multi.cycles <- function(project.stats) {
  tab <- tabulate(table(project.stats$author.id))
  tab <- data.frame(num.cycles=ordered(1:length(tab)),  count=tab)

  return(tab)
}

## Determine how many authors contributed per cycle
gen.authors.per.cycle <- function(project.stats, cycles) {
  tab <- table(project.stats$range.id)
  tab <- data.frame(range.id=as.integer(names(tab)),
                    developers=as.integer(tab))
  tab <- cbind(tab, cycle=cycles$cycle[cycles$range.id==tab$range.id])

  return(tab)
}

## Provide a cycle-resolved overview about how many commits and/or
## code changes authors performed in the cycle
gen.contrib.overview <- function(project.stats, cycles) {
  project.stats <- cbind(project.stats, cycle=mapvalues(project.stats$range.id,
                                          cycles$range.id, cycles$cycle))
  stats.molten <- melt(project.stats, id=c("range.id", "cycle", "author.id"))

  return(stats.molten)
}

do.contrib.overview.plot <- function(project.stats, cycles) {
  dat <- gen.contrib.overview(project.stats, cycles)

  g <- ggplot(dat, aes(x=cycle, y=value)) + geom_boxplot() + scale_y_log10() +
    facet_grid(variable~., scales="free_y") + xlab("Release range") +
      ylab("Contributions per author")

  print(g)
}

## There seem to be two patterns: Very frequent one-time contributors
## (git, qemu, kernel), and a fairly level structure (clang, openssl)
do.multi.cycle.plot <- function(project.stats) {
  dat <- gen.authors.multi.cycles(project.stats)
  g <- ggplot(dat, aes(x=num.cycles, y=count)) + geom_bar(stat="identity") +
    xlab("Number of cycles contributed to") + ylab("Number of developers")

  print(g)
}

do.authors.per.cycle.plot <- function(project.stats, cycles) {
  dat <- gen.authors.per.cycle(project.stats, cycles)
  g <- ggplot(dat, aes(x=cycle, y=developers)) + geom_bar(stat="identity") +
    xlab("Release cycle") + ylab("Number of contributors")

  print(g)
}

createWidgetClass(
  c("widget.contributions.multiCycles", "widget.contributions"),
  "Author Turnover", "Time Authors stayed on the Project",
  c("basics", "collaboration"),
  1, 1,
  detailpage = detailpage
)

createWidgetClass(
  c("widget.contributions.authorsPerCycle", "widget.contributions"),
  "Authors per Cycle", "Number of Authors in each cycle",
  c("basics", "collaboration", "complexity"),
  1, 1,
  detailpage = detailpage
)

createWidgetClass(
  c("widget.contributions.overview", "widget.contributions"),
  "Contributions - Overview", "Numerical overview of contributions",
  c("basics", "collaboration"),
  2, 1,
  detailpage = detailpage
)

initWidget.widget.contributions <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$project.stats <- reactive({
    query.contributions.stats.project(conf$con, w$pid())
  })
  w$cycles <- reactive({get.cycles.con(conf$con, w$pid())})
  #NextMethod(w)
  return(w)
}

renderWidget.widget.contributions.multiCycles = function(w) {
  renderPlot({do.multi.cycle.plot(w$project.stats())})
}

renderWidget.widget.contributions.authorsPerCycle = function(w) {
  renderPlot({do.authors.per.cycle.plot(w$project.stats(), w$cycles())})
}

renderWidget.widget.contributions.overview = function(w) {
  renderPlot({do.contrib.overview.plot(w$project.stats(), w$cycles())})
}

