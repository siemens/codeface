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


## Test widget: Each timezone is a view
createWidgetClass(
  "widget.weekend.fraction",
  "Weekend fraction",
  "Show percentage of work done on weekdays vs weekends",
  c("basics"),
  1, 1
)

gen.weekend.fraction <- function(pid) {
  ## Get basic release cycle information
  cycles <- get.cycles.con(conf$con, pid)
  ## Get commit count per author, release range and weekend/not-weekend
  dat <- dbGetQuery(conf$con, str_c("SELECT releaseRangeId as rangeid,",
                                    " author,",
                                    " WEEKDAY(commitDate)>=5 as weekend,",
                                    " COUNT(*) as count",
                                    " FROM commit where projectId=", pid,
                                    " GROUP BY releaseRangeId,",
                                    "  WEEKDAY(commitDate)>=5, author"))
  ## Fix range.id not being a valid identifier in MySQL
  names(dat)[1] <- "range.id"
  ## Aggregate the counts and counts on weekends
  dat2 <- aggregate(cbind(count, count*weekend) ~ author + range.id, dat, FUN=sum)
  dat <- merge(cycles, dat2)
  ## Calculate weekend fraction of authors
  dat$weekend.fraction <- dat$V2 / dat$count
  return(dat)
}

initWidget.widget.weekend.fraction <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$dat <- reactive({gen.weekend.fraction(w$pid())})
  return(w)
}

renderWidget.widget.weekend.fraction = function(w) {
  renderPlot({
    g <- ggplot(w$dat(), aes(x=cycle, y=weekend.fraction)) +
                geom_boxplot(outlier.colour="red") +
                geom_jitter(alpha=0.4, size=1.1) +
                #scale_y_log10() +
                #expand_limits(y = 1000) +
                #expand_limits(y = 10000) +
                xlab("Release range") +
                ylab("Fraction of commits on wekeends per Author")
    print(g)
  })
}
