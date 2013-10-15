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
## Copyright 2013 by Siemens AG, Johannes Ebke <johannes.ebke.ext@siemens.com>
## All Rights Reserved.

## Also see punchcard.r
detailpage <- list(name="widget.weekend.fraction,widget.weekend.fraction.type,widget.punchcard",
                   title="Authors / Time of Activity")

gen.weekend.fraction <- function(pid) {
  ## Get basic release cycle information
  cycles <- get.cycles.con(conf$con, pid)
  ## Get commit count per author, release range and weekend/not-weekend
  ## NOTE: Once the authorDate and authorTimezone are available,
  ## they should be used instead of WEEKDAY to get more accurate estimates
  sqlr <- dbGetQuery(conf$con, str_c("SELECT releaseRangeId as rangeid,",
                                    " author,",
                                    " WEEKDAY(commitDate)>=5 as weekend,",
                                    " COUNT(*) as count,",
                                    " SUM(DiffSize) as diffsize", # Alternate measure
                                    " FROM commit where projectId=", pid,
                                    " GROUP BY releaseRangeId,",
                                    "  WEEKDAY(commitDate)>=5, author"))
  ## Fix range.id not being a valid identifier in MySQL
  names(sqlr)[1] <- "range.id"
  ## Aggregate the counts and counts on weekends
  agg <- aggregate(cbind(count, count*weekend, diffsize, diffsize*weekend)
                         ~ author + range.id, sqlr, FUN=sum)
  dat <- merge(cycles, agg)

  ## Also aggreate the total commit count for normalization
  agg <- aggregate(count ~ range.id, dat, FUN=sum)
  names(agg)[2] <- "count.all"
  dat <- merge(dat, agg)

  ## Calculate weekend fraction of authors
  #dat$weekend.fraction.count <- dat$V2 / dat$count
  ## Weekend fraction by diff size
  dat$weekend.fraction <- dat$V4 / dat$diffsize
  return(dat)
}

createWidgetClass(
  "widget.weekend.fraction",
  "Weekend fraction",
  str_c("Show percentage of work done (measured by diff size) on weekdays vs weekends per author. ",
  "Each author is represented by a dot, the size of which is proportional to her number of commits. ",
  "The colour is proportional to the total size of diffs the author contributed that cycle."),
  c("basics"),
  2, 1,
  detailpage=detailpage
)

initWidget.widget.weekend.fraction <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$dat <- reactive({gen.weekend.fraction(w$pid())})
  return(w)
}

renderWidget.widget.weekend.fraction = function(w) {
  renderPlot({
    dat <- w$dat()
    dat$colour <- factor(dat$count)
    g <- ggplot(dat, aes(x=cycle, y=weekend.fraction)) +
                geom_violin(aes(weight=diffsize), scale="width", adjust = .5) +
                geom_jitter(aes(size=count, colour=diffsize)) +
                scale_colour_gradient(low="darkgray", high="red") +
                scale_y_continuous(labels = percent_format()) +
                xlab("Release range") +
                ylab("Fraction of commits on wekeends per Author") +
                theme(axis.text.x = element_text(angle = 30, hjust = 1, size=7))
    print(g)
  })
}

createWidgetClass(
  c("widget.weekend.fraction.type", "widget.weekend.fraction"),
  "Weekend, continuous or weekday worker",
  str_c("Show percentage of authors that work mostly (> 50%) on weekends, that work ",
  "continuously (> 25% weekends) or that work mostly on weekdays, measured by diff size."),
  c("basics"),
  2, 1,
  detailpage=detailpage
)

renderWidget.widget.weekend.fraction.type = function(w) {
  renderPlot({
    dat <- w$dat()
    dat$classify <- factor(sapply(dat$weekend.fraction, function(f) {
            ## Rationale:
            ## A person working only on local time workdays might have
            ## some commits on "UTC workdays"; we therefore classify
            ## all people with < 0.25 weekend fraction as professionals
            if (is.na(f)) {
              ""
            } else if (f < 0.25) {
              "workdays"
            } else if (f < 0.5) {
              "continuous"
            } else {
              "weekend"
            }
          }), levels=c("workdays", "continuous", "weekend"))
    g <- ggplot(dat, aes(x=cycle, fill=classify)) + geom_bar(aes(weight=diffsize), position="fill") +
                scale_y_continuous(labels = percent_format()) +
                xlab("Release range") +
                ylab("Author type") +
                theme(axis.text.x = element_text(angle = 30, hjust = 1, size=7))
    print(g)
  })
}
