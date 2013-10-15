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
## Copyright 2013 by Siemens AG, Johannes Ebke <johannes.ebke.ext@siemens.com>
## All Rights Reserved.

library(tm)
library(slam)

createWidgetClass(
  c("widget.tagcloud.mlsubjects"),
  "Frequent mailing list subjects",
  "Tag cloud of frequent mailing list subjects",
  c("communication"),
  2, 1,
  html=htmlOutput
)

initWidget.widget.tagcloud.mlsubjects <- function(w) {
  w <- NextMethod()
  w$data <- reactive({dbGetQuery(conf$con, str_c("SELECT subject, count FROM",
    " freq_subjects WHERE projectId=", w$pid(),
    " ORDER BY count DESC LIMIT 100"))})
  w$frequencies <- reactive({
    d <- w$data()
    lapply(1:length(d$subject), function(i) {
           freqs <- termFreq(PlainTextDocument(stripWhitespace(removeWords(
                             removeNumbers(removePunctuation(d$subject[[i]])),
                             c(stopwords("english"), "V","v", "PATCH", "PATCHv",
                               "are", "one")))))
           freqs <- freqs * d$count[[i]]
    })
  })
  w$frequency <- reactive({
    tdm <- do.call(c, w$frequencies())
    row_sums(tdm)
  })
  return(w)
}

renderWidget.widget.tagcloud.mlsubjects <- function(w) {
  renderUI({
    toString(names(w$frequency()))
    toString(w$frequency())
    v <- w$frequency()
    max.v <- max(v)
    scale.f <- function(x) {
      10 + 30 * 5**log10(x/max.v)
    }
    n <- names(v)
    objs <- lapply(1:length(v), function(i) {
      paste('{ "text": "', n[[i]], '", "size": ', scale.f(v[[i]]), '}', sep="")
    })
    arr <- do.call(paste, c(objs, sep=", "))
    div.id <- paste("wordcloudcontainer", w$pid(), sep="")
    tagList(
      tags$div(id=div.id, style="margin-top: 20px"),
      ## Remove ~~ from rotate function to get a non-rectangular layiut
      tags$script(paste('
var fill = d3.scale.category20b();

d3.layout.cloud().size([560, 240])
    .words([', arr, '])
    .padding(0)
    .rotate(function() { return (Math.random() * 2) * 90 - 90; })
    .font("Impact")
    .fontSize(function(d) { return d.size; })
    .on("end", draw)
    .start();

function draw(words) {
d3.select("div#', div.id, '").append("svg")
    .attr("width", 560)
    .attr("height", 240)
    .append("g")
    .attr("transform", "translate(280,120)")
    .selectAll("text")
    .data(words)
    .enter().append("text")
    .style("font-size", function(d) { return d.size + "px"; })
    .style("font-family", "Impact")
    .style("fill", function(d, i) { return fill(i); })
    .attr("text-anchor", "middle")
    .attr("transform", function(d) {
        return "translate(" + [d.x, d.y] + ")rotate(" + d.rotate + ")";
    })
    .text(function(d) { return d.text; });
}', sep=""))
  )
  })
}

