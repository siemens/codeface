#! /usr/bin/env Rscript

## This file is part of Codeface. Codeface is free software: you can
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
## Copyright 2013, Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

## Example server component to dynamically compare inter-release similarities
## for projects

get.release.distance.data <- function(con, name.list) {
  pid.list <- lapply(name.list, function(name) {
    return(projects.list[projects.list$name==name,]$id)
  })

  plot.ids <- lapply(pid.list, function(pid) {
    return(get.plot.id.con(con, pid, "Release TS distance"))
  })

  ts.list <- lapply(plot.ids, function(plot.id) {
    return(query.timeseries(con, plot.id))
  })

  name.list <- lapply(pid.list, function(pid) {
    return(query.project.name(con, pid))
  })

  dat <- do.call(rbind, lapply(1:length(name.list), function(i) {
    data.frame(project=name.list[[i]], distance=ts.list[[i]]$value,
               date=ts.list[[i]]$time)
  }))

  return(dat)
}

do.release.distance.plot <- function(con, names.list) {
  dat <- get.release.distance.data(con, names.list)

  g <- ggplot(dat, aes(x=project, y=distance)) +
    geom_boxplot(outlier.colour="red") +
    geom_jitter(alpha=0.4, size=1.1) + xlab("Project") +
      ylab("Distribution of distance values")

  return(g)
}

createWidgetClass(
  "widget.release.distance",
  "Release distance",
  "Time distance of releases",
  c("construction"),
  size.x = 1,
  size.y = 1,
  compareable=TRUE
)

renderWidget.widget.release.distance <- function(w) {
  renderPlot({
    projectname <- projects.list$name[[which(projects.list$id == as.integer(w$pid()))]]
    if(is.null(w$pids.compare())) {
      compare.projectnames <- list()
      if (!is.null(w$name2) && !is.null(w$name2())) {
        compare.projectnames <- c(compare.projectnames, w$name2())
      }
      if (!is.null(w$name3) && !is.null(w$name3())) {
        compare.projectnames <- c(compare.projectnames, w$name3())
      }
    } else {
      compare.projectnames <- lapply(w$pids.compare(), function(pid) {
        return(projects.list[projects.list$id==pid,]$name)
      })
    }
    print(do.release.distance.plot(conf$con, c(projectname, compare.projectnames)))
  })
}

