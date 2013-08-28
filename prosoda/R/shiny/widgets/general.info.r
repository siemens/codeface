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
  1, 1
)
widget.general.info.overview$html = htmlOutput


renderWidget.widget.general.info.overview = function(w, view=NULL) {
  renderUI({
    project.name <- query.project.name(conf$con, w$pid)
    cycles <- get.cycles.con(conf$con, w$pid)
    date.start <- as.Date(min(cycles$date.start))
    date.end <- as.Date(max(cycles$date.end))
    n.releases <- nrow(cycles)
    n.releases.text <- paste("Analysed", n.releases, "Release cycles.")
    n.commits <- nrow(dbGetQuery(conf$con, str_c("SELECT 1 FROM commit WHERE projectId=", w$pid)))
    n.persons <- nrow(dbGetQuery(conf$con, str_c("SELECT 1 FROM person WHERE projectId=", w$pid)))
    n.issues <- nrow(dbGetQuery(conf$con, str_c("SELECT 1 FROM issue WHERE projectId=", w$pid)))
    n.mail.threads <- nrow(dbGetQuery(conf$con, str_c("SELECT 1 FROM mail_thread WHERE projectId=", w$pid)))
    if (n.releases == 1) {
      n.releases.text <- paste("Analysed one release cycle.")
    }
    list(
      h3(HTML(paste("The <em>", project.name, "</em> project"))),
      HTML(paste("<ul>",
                 "<li>", n.releases.text,
                 "<li>", paste("Analysis range: ", month.name[month(date.start)], year(date.start), "until",  month.name[month(date.end)], year(date.end)),
                 "<li>", paste(n.commits, "commits."),
                 "<li>", paste(n.persons, "persons."),
                 "<li>", paste(n.issues, "bugtracker entries."),
                 "<li>", paste(n.mail.threads, "mailing list threads."),
                 "</ul>"))
    )
  })
}

