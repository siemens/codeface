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
## Classes and Instances of Widgets
## --------------------------------
## For this application, "S3 classes" are used, very informal constructs with
## great flexibility. In practice, S3 classes are only defined by their names,
## which are used for resolving generic functions (see renderWidget)
##
## To provide 'class methods' and 'constructors', a further list() is used as
## an analogon to a 'class' in other languages. This class c must be useable
## in the newWidget function; and should be created with the createWidgetClass
## function. This function also inserts the class into the global widget.list.
## This list can be inspected by the user interface routines to
## provide graph selections to the analyst.
##
## Reactivity
## ----------
## A core element of the Shiny output structure is the concept of reactive
## variables. In implementation; reactive variables are closures, which, if
## called with braces in a reactive() or observe() context (e.g. pid()) yield
## their current value, and add themselves to the dependency list of the
## context. Any time a reactive value is updated upstream, all code in
## dependent contexts is re-run.
##
## newWidget requires reactive values for all parameters; this includes the
## project ID (so that implementing a project switcher will be easier).
##
## Output necessary for Shiny
## --------------------------
## To show any plot on the page, shiny requires two things: A renderPlot (or
## renderTable, or renderXYZ) function, whose return value is written into the
## output$plotname variable; and a plotOutput (or tableOutput,..) function
## which is put into the UI (HTML) of the page. Since the latter, the HTML
## output, does not change much, it is directly a member of the widget class
## (c$html). The renderPlot function, however, actually acquires the data and
## renders the plot; so it has been defined as a generic function on the
## class, and is called as renderWidget(w);
## where w <- initWidget(newWidget(pid)).
##
## Views of a Widget
## -----------------
## Widgets can have different 'views', the most common ones are the different
## release cycles. Views are set by calling newWidget with an additional
## argument, a reactive value taking on either NULL or a value returned by
## listViews.
##
## Note: For new Widgets: listViews should be available in the initWidget
## function, so initialization of data necessary for listviews must be done
## before calling NextMethod()
##
## Adding New Widgets
## ------------------
## To create a new Widget with no views, call createWidgetClass with the
## class, human-readable name, description, gridster (dashboard) sizes,
## set compareable to TRUE if the widget supports comparing projects
## set html to a different widget.xyzOutput.html function if necessary,
## and write initWidget.mywidget, renderWidget.mywidget, listViews.mywidget,
## and so on.
##
## To get widget$range.ids and widget$cycles already initialized, specify
## c("widget.mywidget", "widget.rangeid") as a class in createWidgetClass,
## this will also call initWidget.widget.rangeid in the NextMethod() chain
##
## Object-oriented overview
## --------------------
## "Widget Class" cls in the widget.list:
##   - name = "My Widget X"
##   - widget.classes = c("widget.mywidget", "widget")
##   - description = "This widget does X"
##   - size.x, size.y = 1,2
##   - compareable = TRUE/FALSE # Can accept additional PIDs
##   - html = function(id) { plotOutput(id) }
##
## A "Widget class" cls can be passed to newWidget(cls, pid, view, com.pid)
## returns a widget object. (Note: Technically, the Widget class is not any
## kind of R class, it's just a list containing information usually found in
## classes.
##
## "Widget object" widget returned from newWidget(cls, pid):
##   - name = cls$name
##   - class = cls$widget.classes
## Generic functions accepting widget objects:
##   - initWidget # must be called after newWidget
##   - listViews # list possible settings for "view"
##   - renderWidget # render the widget into a format suitable for shiny output
##   - widgetColor # get a reactive string specifying the color
##   - widgetTitle # human-readable title for the current view


## Global list of widget 'classes'
widget.list <<- list()

## Generic function - a call to renderWidget(w) will try to call
## renderWidget.class1(w), renderWidget.class2(w) for all class1, class2 in
## the list of classes of w (class(w)).
## Returns the result of renderPlot, renderTable, etc. as expected by the HTML
renderWidget <- function(x) UseMethod("renderWidget", x)

## Generic function listing different subsets of Data for a given widget
## The return value must be a reactive vector x of identifiers; with
## names(x) set to a human-readable description of the specific view.
listViews <- function(x) UseMethod("listViews", x)

## Generic function returning the background color of the widget
## as a reactive string.
widgetColor <- function(x) UseMethod("widgetColor", x)

## Generic function returning the current title of the widget
## as a reactive string.
widgetTitle <- function(x) UseMethod("widgetTitle", x)

## Generic function calling widget initialization routines.
## If you write a initialization routine that is shared between widget classes,
## use NextMethod() to continue calling all initialization routines.
## Note that this function MUST return an instance of the widget!
initWidget <- function(x) UseMethod("initWidget", x)

## Output HTML element for printing plots as widgets
widget.plotOutput.html <- function(id) {
  plotOutput(id, width="100%", height="100%")
}

## Output HTML element for printing tables as widgets
widget.tableOutput.html <- function(id) {
  div(style="display: block; height: 100%; overflow: auto; width: 100%",
      tableOutput(id))
}

## Helper function that creates a new widget 'class' with a constructor
## that initializes w$range.ids
createWidgetClass <- function(class, name, description,
                                     size.x=1, size.y=1,
                                     compareable=FALSE,
                                     html=widget.plotOutput.html) {
  ## Create a widget class and save it under the last name in the class list
  widget.list[[ class[[1]] ]] <<- list(
    widget.classes = c(class, "widget"),
    name = name,
    description = description,
    size.x = size.x,
    size.y = size.y,
    compareable = compareable,
    html = html
  )
}

assert.reactive <- function(x, what) {
  if (typeof(x) != "closure") {
    stop(paste(what, "is not a reactive value"))
  }
}

## Create a new widget instance, given a widget class, and reactive variables
## pid, view and pids.compare.
newWidget = function(cls, pid, view=reactive({NULL}), pids.compare=reactive({list()})) {
  if (is.null(cls$widget.classes)) {
    logfatal("newWidget called with something that is missing widget.classes:")
    str(cls)
    stop("Invalid call to newWidget")
  }
  main.class <- cls$widget.classes[[1]]

  assert.reactive(pid, "Project ID 'pid' passed to newWidget")
  assert.reactive(pids.compare, "Project ID list 'pids.compare'")

  loginfo(paste("newWidget:", main.class))

  widget <- list(
    main.class = main.class,
    name = cls$name,
    pid = pid,
    pids.compare = pids.compare,
    view.raw = view
  )
  class(widget) <- cls$widget.classes
  return(widget)
}

## Initialization function for widgets which do not need special initialization
initWidget.widget <- function(w) {
  ## Create a variable that automatically takes on a default
  ## if it is NULL
  view.with.default <- reactive({
    if (is.null(w$view.raw())) {
      l <- listViews(w)()
      l[[max(1, length(l)/2)]]
      #listViews(w)()[[1]]
    } else {
      w$view.raw()
    }
  })
  w$view <- view.with.default
  return(w)
}

## Initialization for widgets which have range.id views
initWidget.widget.rangeid <- function(widget) {
  widget$cycles <- reactive({get.cycles.con(conf$con, widget$pid())})
  widget$range.ids <- reactive({
    range.ids <- query.range.ids.con(conf$con, widget$pid())
    # Set the names of the range IDs to human readable values
    names(range.ids) <- widget$cycles()$cycle
    range.ids
  })
  ## Call the superclass here, since the views are now defined
  w <- NextMethod(w)
  ## Call the next initialization method in the sequence
  return(w)
}

## Default implementation for listViews (called if no other function matches)
## The Default for a widget is to have one view, which is called using NULL
listViews.default <- function(x) {
  reactive({list(NULL)})
}

## Implementation for listViews for widgets with data per range.id.
listViews.widget.rangeid <- function(w) {
  reactive({ w$range.ids() })
}

## Implementation for widgets which do not change colour
widgetColor.default <- function(w) {
  reactive({"white"})
}

## Default widget title is the class human-readable name
widgetTitle.default <- function(w) {
  reactive({w$name})
}

## Include color scheme
source("color.r")

## Load all the widgets so that widget.list is populated
source("widgets/commit.info.r", chdir=TRUE)
source("widgets/commit.structure.r", chdir=TRUE)
source("widgets/contributions.r", chdir=TRUE)
source("widgets/contributors.r", chdir=TRUE)
source("widgets/general.info.r", chdir=TRUE)
source("widgets/overview.r", chdir=TRUE)
source("widgets/plots.r", chdir=TRUE)
source("widgets/punchcard_ml.r", chdir=TRUE)
source("widgets/punchcard.r", chdir=TRUE)
source("widgets/release_distance.r", chdir=TRUE)
source("widgets/timeseries.r", chdir=TRUE)
source("widgets/vis.clusters.r", chdir=TRUE)

