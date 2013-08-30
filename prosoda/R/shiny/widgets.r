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

##
## This file contains the support structure for "widgets". A widget represents
## a plot that can be displayed either on its own page, or in a box on the
## project dashboard.
##
## For this application, "S3 classes" are used, very informal constructs with
## great flexibility. In practice, S3 classes are only defined by their names,
## which are used for resolving generic functions (see renderWidget)
##
## To provide 'class methods' and 'constructors', a further list() is used as
## an analogon to a 'class' in other languages. This class c must contain a
## c$new function that returns a new widget object given a project ID,
## (..which is just a list w with a class for which renderWidget and listViews
## functions are defined) and a c$html function which calls the correct plotX
## function.
##
## All such 'classes' are stored under their name in the global widget.list,
## which can be inspected by the user interface routines to provide graph
## selections to the analyst.
##
## To show any plot on the page, shiny requires two things: A renderPlot (or
## renderTable, or renderXYZ) function, whose return value is written into the
## output$plotname variable; and a plotOutput (or tableOutput,..) function
## which is put into the UI (HTML) of the page. Since the latter, the HTML
## output, does not change much, it is directly a member of the widget class
## (c$html). The renderPlot function, however, actually acquires the data and
## renders the plot; so it has been defined as a generic function on the class,
## and is called as renderWidget(w); where w <- c$new(pid).
##
## Widgets can have different 'views', the most common ones are the different
## release cycles. Views are shown by calling renderWidget with a second
## argument; an identifier previously returned by listViews.
##
## To create a new Widget with no views, just call createWidgetClass with the
## name, title and gridster (dashboard) size; and write renderWidget.name.
##
## For widgets with views either use createRangeIdWidgetClass or look at the
## example e.g. in widgets/plots.r

## Global list of widget 'classes' (lists with $new, $html, $title, ...)
widget.list <<- list()

## Generic function - a call to renderWidget(w) will try to call
## renderWidget.class1(w), renderWidget.class2(w) for all class1, class2 in
## the list of classes of w (class(w)).
renderWidget <- function(x, view=NULL) UseMethod("renderWidget", x)

## Generic function listing different subsets of Data for a given widget
## The return value must be a vector x of identifiers; with names(x) set
## to a human-readable description of the specific view.
listViews <- function(x) UseMethod("listViews", x)

## Constructor function for a generic widget
make.widget <- function(pid) {
  list(
    class = "widget",
    pid = pid
  )
}

## Constructor function for a generic range.id-using widget
make.widget.range.id <- function(pid) {
  widget <- make.widget(pid)
  widget$class <- c("widget.rangeid", widget$class)
  widget$range.ids <- query.range.ids.con(conf$con, pid)
  # Also save names of range.ids
  cycles <- get.cycles.con(conf$con, pid)
  names(widget$range.ids) <- cycles$cycle
  return(widget)
}

## Helper function that creates a new widget 'class' with a generic constructor
createWidgetClass <- function(class.name, title, size.x=1, size.y=1) {
  maker <- function(pid) {
    w <- make.widget(pid)
    class(w) <- c(class.name, w$class)
    return (w)
  }
  widget.list[[class.name]] <<- list(
      new = maker,
      title = title,
      size.x = size.x,
      size.y = size.y,
      html = function(id) { plotOutput(id, width="100%", height="100%") }
  )
  return(widget.list[[class.name]])
}

## Helper function that creates a new widget 'class' with a constructor
## that initializes w$range.ids
createRangeIdWidgetClass <- function(class.name, title, size.x=1, size.y=1) {
  maker <- function(pid) {
    w <- make.widget.range.id(pid)
    class(w) <- c(class.name, w$class)
    return (w)
  }
  widget.list[[class.name]] <<- list(
      new = maker,
      title = title,
      size.x = size.x,
      size.y = size.y,
      html = function(id) { plotOutput(id, width="100%", height="100%") }
  )
  return(widget.list[[class.name]])
}

## Default implementation for listViews (called if no other function matches)
## The Default for a widget is to have one view, which is called using NULL
listViews.default <- function(x) {
  return(list(NULL))
}

## Implementation for listViews for widgets with data per range.id.
listViews.widget.rangeid <- function(w) {
  return(w$range.ids)
}

## Load all the widgets so that widget.list is populated
source("widgets/commit.info.r", chdir=TRUE)
source("widgets/commit.structure.r", chdir=TRUE)
source("widgets/contributions.r", chdir=TRUE)
source("widgets/contributors.r", chdir=TRUE)
source("widgets/general.info.r", chdir=TRUE)
source("widgets/plots.r", chdir=TRUE)
source("widgets/punchcard_ml.r", chdir=TRUE)
source("widgets/punchcard.r", chdir=TRUE)
source("widgets/release_distance.r", chdir=TRUE)
source("widgets/timeseries.r", chdir=TRUE)
source("widgets/vis.clusters.r", chdir=TRUE)

