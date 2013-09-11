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


createWidgetClass(
  c("widget.general.info.overview", "widget.general.info"),
  "General Information",
  "General information",
  1, 1,
  html=htmlOutput
)

## Common queries for all widgets in the following go here
initWidget.widget.general.info <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$project.name <- reactive({query.project.name(conf$con, w$pid())})
  w$cycles <- reactive({get.cycles.con(conf$con, w$pid())})
  return(w)
}

renderWidget.widget.general.info.overview <- function(w) {
  renderUI({
    pid <- w$pid()
    project.name <- w$project.name()
    loginfo(paste("project.name=",project.name," (renderWidget.widget.general.info.overview)."))
    cycles <- w$cycles()
    date.start <- as.Date(min(cycles$date.start))
    date.end <- as.Date(max(cycles$date.end))
    n.releases <- nrow(cycles)
    n.releases.text <- paste("Analysed", n.releases, "Release cycles.")
    n.commits <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM commit WHERE projectId=", pid))[[1]]
    n.persons <- nrow(dbGetQuery(conf$con, str_c("SELECT 1 FROM person WHERE projectId=", pid)))
    n.issues <- nrow(dbGetQuery(conf$con, str_c("SELECT 1 FROM issue WHERE projectId=", pid)))
    n.mail.threads <- nrow(dbGetQuery(conf$con, str_c("SELECT 1 FROM mail_thread WHERE projectId=", pid)))
    if (n.releases == 1) {
      n.releases.text <- paste("Analysed one release cycle.")
    }
    tagList(
      div(class="grid_title", HTML(paste("The <em>", project.name, "</em> project"))),
      HTML(paste("<pre>\n",n.releases.text,
                 "\n", paste("Analysis range: ", month.name[month(date.start)], year(date.start), "until",  month.name[month(date.end)], year(date.end)),
                 "\n", paste(n.commits, "commits."),
                 "\n", paste(n.persons, "persons."),
                 "\n", paste(n.issues, "bugtracker entries."),
                 "\n", paste(n.mail.threads, "mailing list threads."),
                 "</pre>")),
      p(a(href=paste("?projectid=", w$pid(), "&topic=basics", sep=""), "details..."))
    )
  })
}

widgetColor.widget.general.info.overview <- function(w) { reactive({color.neutral}) }

## ----------------------------

createWidgetClass(
  "widget.gauge.commits",
  "Commits",
  "Number of commits in this project",
  1, 1,
  html = function(id) {
    tagList(
    tags$div(class="grid_title", "Total Commits"),
    tags$div(id=id,
             class="status_output",
             tags$div(class = 'grid_inserttext grid_bigtext'),
             tags$p()
    )
    )
  }
)

initWidget.widget.gauge.commits <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$commits.alltime <- reactive({
    dbGetQuery(conf$con, str_c("SELECT COUNT(*) as count FROM commit WHERE projectId=", w$pid()))
  })
  return(w)
}

renderWidget.widget.gauge.commits <- function(w) {
  reactive({
    count <- w$commits.alltime()$count
    if (count > 100000) {
      subtext <- "A lot"
    } else if (count > 10000) {
      subtext <- "Many"
    } else if (count > 1000) {
      subtext <- "Average"
    } else if (count > 10) {
      subtext <- "A few"
    } else {
      subtext <- "Possible error!"
    }
    list(text=as.character(count), subtext=subtext)
  })
}

widgetColor.widget.gauge.commits <- function(w) {
  reactive({
    if (w$commits.alltime()$count > 100) {
      color.good
    } else {
      color.bad
    }
  })
}

# ----------------------

createWidgetClass(
  "widget.gauge.commitspeed",
  "Commit Speed",
  "Number of commits last month compared to the average number of commits per month",
  1, 1,
  html = function(id) {
    tags$div(id = id,
             class = "justgage_output",
             title="Commits last month",
             units="% of average",
             min=0,
             max=200,
             style="width:250px; height:200px")
  }
)

initWidget.widget.gauge.commitspeed <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$scaled.rate <- reactive({
    commit.first <- dbGetQuery(conf$con, str_c("SELECT commitDate FROM commit WHERE projectId=", w$pid(), " ORDER BY commitDate LIMIT 1"))
    commit.last <- dbGetQuery(conf$con, str_c("SELECT commitDate FROM commit WHERE projectId=", w$pid(), " ORDER BY commitDate DESC LIMIT 1"))
    commits.alltime <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) as count FROM commit WHERE projectId=", w$pid()))
    commits.lastmonth <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) as count FROM commit WHERE projectId=", w$pid(), " AND commitDate >= (SELECT DATE_SUB(commitDate, INTERVAL 28 DAY) FROM commit WHERE projectId=", w$pid(), " ORDER BY commitDate DESC LIMIT 1)"))
    length <- as.Date(commit.last$commitDate) - as.Date(commit.first$commitDate)
    avg.commits <- commits.alltime$count*28.0/as.integer(length)
    relative.commits <- commits.lastmonth$count/avg.commits
    relative.commits * 100.0
  })
  return(w)
}

renderWidget.widget.gauge.commitspeed <- function(w) {
  reactive({
    print("Gauge value: ")
    str(w$scaled.rate())
    as.integer(w$scaled.rate())
  })
}

widgetColor.widget.gauge.commitspeed <- function(w) {
  reactive({
    if (w$scaled.rate() < 10) {
      color.bad
    } else if (w$scaled.rate() < 60) {
      color.warn
    } else if (w$scaled.rate() < 150) {
      "white"
    } else {
      color.good
    }
  })
}

## ---------------------------

createWidgetClass(
  c("widget.gauge.current.cycle", "widget.rangeid"),
  "Current Release Range",
  "Specifies the release range currently viewed",
  1, 1,
  html = function(id) {
    tagList(
    tags$div(class="grid_title", "Release Range"),
    tags$div(id=id,
             class="status_output",
             tags$div(class = 'grid_inserttext grid_medtext'),
             tags$p()
    )
    )
  }
)

renderWidget.widget.gauge.current.cycle <- function(w) {
  reactive({
    range.id <- w$view()
    cycles <- w$cycles()
    cycle.index <- which(cycles$range.id == as.integer(range.id))
    date.start <- as.Date(cycles$date.start[[cycle.index]])
    date.end <- as.Date(cycles$date.end[[cycle.index]])
    range.id.name <- cycles$cycle[[cycle.index]]
    subtext <- paste(cycle.index, "of", nrow(cycles), "-",
                          date.start, "to", date.end)

    print("range id is")
    str(range.id.name)
    list(text=as.character(range.id.name), subtext=subtext)
  })
}

## ---------------------------

createWidgetClass(
  c("widget.gauge.commits.per.cycle", "widget.rangeid"),
  "Commit Speed in Cycle",
  "Rate of commits in this release range compared to the average commit rate",
  1, 1,
  html = function(id) {
    tags$div(id = id,
             class = "justgage_output",
             title="Commits this cycle",
             units="% of average",
             min=0,
             max=200,
             style="width:250px; height:200px")
  }
)

initWidget.widget.gauge.commits.per.cycle <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$scaled.rate <- reactive({
    commit.first <- dbGetQuery(conf$con, str_c("SELECT commitDate FROM commit WHERE projectId=", w$pid(), " ORDER BY commitDate LIMIT 1"))
    commit.last <- dbGetQuery(conf$con, str_c("SELECT commitDate FROM commit WHERE projectId=", w$pid(), " ORDER BY commitDate DESC LIMIT 1"))
    commits.alltime <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) as count FROM commit WHERE projectId=", w$pid()))

    r.commit.first <- dbGetQuery(conf$con, str_c("SELECT commitDate FROM commit WHERE projectId=", w$pid(), " AND releaseRangeId=", w$view(), " ORDER BY commitDate LIMIT 1"))
    r.commit.last <- dbGetQuery(conf$con, str_c("SELECT commitDate FROM commit WHERE projectId=", w$pid(), " AND releaseRangeId=", w$view(), " ORDER BY commitDate DESC LIMIT 1"))
    r.commits.alltime <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) as count FROM commit WHERE projectId=", w$pid(), " AND releaseRangeId=", w$view()))

    length <- as.Date(commit.last$commitDate) - as.Date(commit.first$commitDate)
    r.length <- as.Date(r.commit.last$commitDate) - as.Date(r.commit.first$commitDate)
    avg.commits <- commits.alltime$count*as.integer(r.length)*1.0/as.integer(length)
    relative.commits <- r.commits.alltime$count/avg.commits
    relative.commits * 100.0
  })
  return(w)
}

renderWidget.widget.gauge.commits.per.cycle <- renderWidget.widget.gauge.commitspeed
widgetColor.widget.gauge.commits.per.cycle <- widgetColor.widget.gauge.commitspeed
