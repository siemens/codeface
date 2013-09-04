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


widget.general.info.overview <- createWidgetClass(
  "widget.general.info.overview",
  "Overview",
  1, 1,
  html=htmlOutput
)


renderWidget.widget.general.info.overview <- function(w, view=NULL) {
  renderUI({
    project.name <- query.project.name(conf$con, w$pid)
    loginfo(paste("project.name=",project.name," (renderWidget.widget.general.info.overview)."))
    cycles <- get.cycles.con(conf$con, w$pid)
    date.start <- as.Date(min(cycles$date.start))
    date.end <- as.Date(max(cycles$date.end))
    n.releases <- nrow(cycles)
    n.releases.text <- paste("Analysed", n.releases, "Release cycles.")
    n.commits <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM commit WHERE projectId=", w$pid))[[1]]
    n.persons <- nrow(dbGetQuery(conf$con, str_c("SELECT 1 FROM person WHERE projectId=", w$pid)))
    n.issues <- nrow(dbGetQuery(conf$con, str_c("SELECT 1 FROM issue WHERE projectId=", w$pid)))
    n.mail.threads <- nrow(dbGetQuery(conf$con, str_c("SELECT 1 FROM mail_thread WHERE projectId=", w$pid)))
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
                 "</pre>"))
    )
  })
}

widgetColor.widget.general.info.overview <- function(w) { "lightblue" }

widget.gauge.commits <- createWidgetClass(
  "widget.gauge.commits",
  "Commits",
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

renderWidget.widget.gauge.commits <- function(w, view=NULL) {
  commits.alltime <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) as count FROM commit WHERE projectId=", w$pid))
  #reactive({as.character(commits.alltime$count)})
  if (commits.alltime$count > 100000) {
    subtext <- "A lot"
  } else if (commits.alltime$count > 10000) {
    subtext <- "Many"
  } else if (commits.alltime$count > 1000) {
    subtext <- "Average"
  } else if (commits.alltime$count > 10) {
    subtext <- "A few"
  } else {
    subtext <- "Possible error!"
  }
  reactive({list(text=as.character(commits.alltime$count), subtext=subtext)})
}

widgetColor.widget.gauge.commits <- function(w) {
  commits.alltime <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) as count FROM commit WHERE projectId=", w$pid))
  if (commits.alltime$count > 100) {
    return("lightgreen")
  } else {
    return("red")
  }
}

widget.gauge.commitspeed <- createWidgetClass(
  "widget.gauge.commitspeed",
  "Commit Speed",
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

renderWidget.widget.gauge.commitspeed <- function(w, view=NULL) {
  commit.first <- dbGetQuery(conf$con, str_c("SELECT commitDate FROM commit WHERE projectId=", w$pid, " ORDER BY commitDate LIMIT 1"))
  commit.last <- dbGetQuery(conf$con, str_c("SELECT commitDate FROM commit WHERE projectId=", w$pid, " ORDER BY commitDate DESC LIMIT 1"))
  commits.alltime <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) as count FROM commit WHERE projectId=", w$pid))
  commits.lastmonth <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) as count FROM commit WHERE projectId=", w$pid, " AND commitDate >= (SELECT DATE_SUB(commitDate, INTERVAL 28 DAY) FROM commit WHERE projectId=", w$pid, " ORDER BY commitDate DESC LIMIT 1)"))

  length <- as.Date(commit.last$commitDate) - as.Date(commit.first$commitDate)
  avg.commits <- commits.alltime$count*28.0/as.integer(length)
  relative.commits <- commits.lastmonth$count/avg.commits
  scaled <- relative.commits * 50.0
  reactive({
      as.integer(scaled)
  })
}

widgetColor.widget.gauge.commitspeed <- function(w) {
  commit.first <- dbGetQuery(conf$con, str_c("SELECT commitDate FROM commit WHERE projectId=", w$pid, " ORDER BY commitDate LIMIT 1"))
  commit.last <- dbGetQuery(conf$con, str_c("SELECT commitDate FROM commit WHERE projectId=", w$pid, " ORDER BY commitDate DESC LIMIT 1"))
  commits.alltime <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) as count FROM commit WHERE projectId=", w$pid))
  commits.lastmonth <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) as count FROM commit WHERE projectId=", w$pid, " AND commitDate >= (SELECT DATE_SUB(commitDate, INTERVAL 28 DAY) FROM commit WHERE projectId=", w$pid, " ORDER BY commitDate DESC LIMIT 1)"))

  length <- as.Date(commit.last$commitDate) - as.Date(commit.first$commitDate)
  avg.commits <- commits.alltime$count*28.0/as.integer(length)
  relative.commits <- commits.lastmonth$count/avg.commits
  scaled <- relative.commits * 50.0
  if (scaled < 10) {
    return("red")
  } else if (scaled < 50) {
    return("lightyellow")
  } else if (scaled < 150) {
    return("white")
  } else {
    return("lightgreen")
  }
}


widget.gauge.current.cycle <- createRangeIdWidgetClass(
  "widget.gauge.current.cycle",
  "Current Release Range",
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

renderWidget.widget.gauge.current.cycle <- function(w, view=NULL) {
  reactive({
    cycle.index <- which(w$cycles$range.id == as.integer(view))
    date.start <- as.Date(w$cycles$date.start[[cycle.index]])
    date.end <- as.Date(w$cycles$date.end[[cycle.index]])
    range.id.name <- w$cycles$cycle[[cycle.index]]
    subtext <- paste(cycle.index, "of", nrow(w$cycles), "-",
                          date.start, "to", date.end)

    list(text=as.character(range.id.name), subtext=subtext)
  })
}

widget.gauge.commits.per.cycle <- createRangeIdWidgetClass(
  "widget.gauge.commits.per.cycle",
  "Commit Speed in Cycle",
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

renderWidget.widget.gauge.commits.per.cycle <- function(w, view=NULL) {
  commit.first <- dbGetQuery(conf$con, str_c("SELECT commitDate FROM commit WHERE projectId=", w$pid, " ORDER BY commitDate LIMIT 1"))
  commit.last <- dbGetQuery(conf$con, str_c("SELECT commitDate FROM commit WHERE projectId=", w$pid, " ORDER BY commitDate DESC LIMIT 1"))
  commits.alltime <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) as count FROM commit WHERE projectId=", w$pid))

  r.commit.first <- dbGetQuery(conf$con, str_c("SELECT commitDate FROM commit WHERE projectId=", w$pid, " AND releaseRangeId=", view, " ORDER BY commitDate LIMIT 1"))
  r.commit.last <- dbGetQuery(conf$con, str_c("SELECT commitDate FROM commit WHERE projectId=", w$pid, " AND releaseRangeId=", view, " ORDER BY commitDate DESC LIMIT 1"))
  r.commits.alltime <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) as count FROM commit WHERE projectId=", w$pid, " AND releaseRangeId=", view))

  length <- as.Date(commit.last$commitDate) - as.Date(commit.first$commitDate)
  r.length <- as.Date(r.commit.last$commitDate) - as.Date(r.commit.first$commitDate)
  avg.commits <- commits.alltime$count*as.integer(r.length)*1.0/as.integer(length)
  relative.commits <- r.commits.alltime$count/avg.commits
  scaled <- relative.commits * 50.0
  reactive({
      as.integer(scaled)
  })
}

