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

detailpage <- list(name="widget.complexity.loc.per.author.summary,widget.complexity.loc.per.author.weighted,widget.complexity.loc",
                   title="Lines of Code Metrics")

detailpage2 <- list(name="widget.complexity.diff.size,widget.complexity.relative.diff.size",
                   title="Diff Size Metrics")

gen.loc.per.author <- function(pid) {
  ## Get basic release cycle information
  cycles <- get.cycles.con(conf$con, pid)
  ## Get sum of added and deleted lines of code per range
  dat <- dbGetQuery(conf$con, str_c("SELECT releaseRangeId as rangeid,",
                                    " SUM(AddedLines-DeletedLines) as deltaLines",
                                    " FROM commit where projectId=", pid,
                                    " GROUP BY releaseRangeId"))
  ## Fix range.id not being a valid identifier in MySQL
  names(dat)[1] <- "range.id"
  ## Merge the cycles and the lines. This ensures the ranges are in order!
  info <- merge(cycles, dat)
  ## Construct the per-cycle size of the project by doing a cumulative sum
  info$loc <- cumsum(info$deltaLines)
  ## Get author information
  author.info <- query.contributions.stats.project(conf$con, pid)
  ## Get number of authors per release range and merge into info
  a.info <- aggregate(author.id ~ range.id, data=author.info, FUN=length)
  names(a.info)[2] <- "n.authors"
  ## Get total sum of diff sizes in a release cycle
  a.info$cycle.diff.size <- aggregate(total~range.id, data=author.info, FUN=sum)$total
  info <- merge(info, a.info)
  ## Fix the cycle sorting
  info$cycle <- factor(info$cycle, levels=unique(as.character(info$cycle)) )
  info$loc.per.author <- info$loc/info$n.authors
  #return(info)
  ## return at this point for plain loc/authors

  ## Get list of individual author weights by diff size
  author.info <- merge(author.info, info)
  author.info$weight <- author.info$n.authors * author.info$total/author.info$cycle.diff.size
  author.info$value <- author.info$weight * (author.info$loc/author.info$n.authors)
  return(list(summary=info, by.author=author.info))
}

initWidget.widget.complexity.loc.per.author <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$dat <- reactive({gen.loc.per.author(w$pid())})
  return(w)
}

createWidgetClass(
  c("widget.complexity.loc.per.author.summary", "widget.complexity.loc.per.author"),
  "Lines of code per Author",
  "Number of lines of code in the project divided by the number of Authors for a given cycle. Gives an indication of the maintenance burden of an individual author.",
  c("complexity"),
  1, 1,
  detailpage=detailpage
)

renderWidget.widget.complexity.loc.per.author.summary = function(w) {
  renderPlot({
    str(w$dat()$summary)
    g <- ggplot(w$dat()$summary, aes(x=cycle, y=loc.per.author)) +
                geom_bar(stat="identity") +
                xlab("Release range") +
                ylab("Lines of Code per Author")
    print(g)
  })
}

createWidgetClass(
  c("widget.complexity.loc.per.author.weighted", "widget.complexity.loc.per.author"),
  "Lines of code per weighted Author",
  "Number of lines of code in the project divided by the number of Authors for a given cycle, multiplied for each author by the authors weight. The authors weight is calculated by the fraction of lines she has added, removed or modified in this release. For example, if one author makes all changes for one release, the number of lines of code for her will be equal to the number of lines of code in the project.",
  c("complexity"),
  2, 1,
  detailpage=detailpage
)

renderWidget.widget.complexity.loc.per.author.weighted = function(w) {
  renderPlot({
    g <- ggplot(w$dat()$by.author, aes(x=cycle, y=value)) +
                geom_boxplot(outlier.colour="red") +
                geom_jitter(alpha=0.4, size=1.1) +
                #scale_y_log10() +
                #expand_limits(y = 1000) +
                #expand_limits(y = 10000) +
                xlab("Release range") +
                ylab("Lines of Code per weighted Author")
    print(g)
  })
}

createWidgetClass(
  c("widget.complexity.loc", "widget.complexity.loc.per.author"),
  "Lines of code",
  "Number of lines of code in the project",
  c("complexity"),
  1, 1,
  detailpage=detailpage
)

renderWidget.widget.complexity.loc = function(w) {
  renderPlot({
    str(w$dat()$summary)
    g <- ggplot(w$dat()$summary, aes(x=cycle, y=loc)) +
                geom_bar(stat="identity") +
                xlab("Release range") +
                ylab("Lines of Code")
    print(g)
  })
}

createWidgetClass(
  c("widget.complexity.diff.size", "widget.complexity.loc.per.author"),
  "Cumulative diff size in a release",
  "Number of lines added, removed or changed in all edits in the release cycle",
  c("complexity"),
  1, 1,
  detailpage=detailpage2
)

renderWidget.widget.complexity.diff.size = function(w) {
  renderPlot({
    str(w$dat()$summary)
    g <- ggplot(w$dat()$summary, aes(x=cycle, y=cycle.diff.size)) +
                geom_bar(stat="identity") +
                xlab("Release range") +
                ylab("Lines added, removed or changed")
    print(g)
  })
}

createWidgetClass(
  c("widget.complexity.relative.diff.size", "widget.complexity.loc.per.author"),
  "Cumulative diff size compared to lines of code",
  "Compares the length of the sum of all diffs of all edits in the cycle with the number of lines of code",
  c("complexity"),
  1, 1,
  detailpage=detailpage2
)

renderWidget.widget.complexity.relative.diff.size = function(w) {
  renderPlot({
    str(w$dat()$summary)
    g <- ggplot(w$dat()$summary, aes(x=cycle, y=100*cycle.diff.size/loc)) +
                geom_bar(stat="identity") +
                expand_limits(y=10) +
                expand_limits(y=100.0) +
                scale_y_log10(breaks=c(0.5, 1, 2.5, 5, 10, 25, 50, 100, 250, 500, 1000, 10000)) +
                xlab("Release range") +
                ylab("Diff size / Lines of Code in %")
    print(g)
  })
}
