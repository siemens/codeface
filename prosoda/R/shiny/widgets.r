
widget.list <- list()
renderWidget <- function(x, view=NULL) UseMethod("renderWidget", x)
listViews <- function(x) UseMethod("listViews", x)

## A generic widget
make.widget <- function(pid) {
  list(
    class = "widget",
    pid = pid
  )
}

## A generic range.id-using widget
make.widget.range.id <- function(pid) {
  widget <- make.widget(pid)
  widget$class <- c("widget.rangeid", widget$class)
  widget$range.ids <- query.range.ids.con(conf$con, pid)
  # Also save names of range.ids
  cycles <- get.cycles.con(conf$con, pid)
  names(widget$range.ids) <- cycles$cycle
  return(widget)
}

## Create a new widget class with the generic constructor
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
      size.y = size.y
  )
  return(maker)
}

## Create a new widget class with the generic constructor
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
      size.y = size.y
  )
  return(maker)
}

## The Default for a widget is to have one view, which is called using NULL
listViews.default <- function(x) {
  return(list(NULL))
}

## Generic view listing for range.id widgets
listViews.widget.rangeid <- function(w) {
  return(w$range.ids)
}

## Load all the widgets
source("widgets/commit.info.r", chdir=TRUE)
source("widgets/commit.structure.r", chdir=TRUE)
source("widgets/contributions.r", chdir=TRUE)
source("widgets/contributors.r", chdir=TRUE)
source("widgets/punchcard_ml.r", chdir=TRUE)
source("widgets/punchcard.r", chdir=TRUE)
source("widgets/release_distance.r", chdir=TRUE)
source("widgets/timeseries.r", chdir=TRUE)
source("widgets/vis.clusters.r", chdir=TRUE)

