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
## Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

## Helper functions to process/visualise mailing list thread information
## The functions work on the data frame returned by query.thread.info()

suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(reshape))

plot.author.msg.scatter <- function(thread.info, title="") {
  ## How focused are discussions, respectively how does the number
  ## of authors scale with the number of messages per thread?
  ## (former plot auth_msg_scatter.pdf)
  g <- ggplot(thread.info, aes(x=authors, y=messages)) + geom_point() +
    xlab("Authors per thread") + ylab("Messages per thread") + geom_smooth() +
    ggtitle(title)

  return(g)
}

plot.author.msg.dist <- function(thread.info, title="") {
  ## Distribution of authors and messages per thread
  ## (former plot aut_msg_dist.pdf)

  thread.info.molten <- melt(thread.info[,c("authors", "messages", "tid")],
                             id.vars="tid")
  g <- ggplot(thread.info.molten, aes(x=variable, y=value)) + geom_boxplot() +
    scale_y_log10() + xlab("Type") + ylab("Number per thread") +
    ggtitle(title)

  return(g)
}

plot.thread.contributions <- function(thread.info, title="") {
  ## Former plot thread_contributions.pdf
  thread.combined <- rbind(data.frame(num=thread.info$authors,
                                      type="Authors"),
                           data.frame(num=thread.info$messages,
                                      type="Messages"))
  g <- ggplot(thread.combined, aes(x=num, colour=type, fill=type)) +
    geom_histogram(binwidth=1, position="dodge") + scale_y_sqrt() +
    xlab("Amount of thread contributions") +
    ylab("Number of threads (sqrt transformed)") +
    scale_size("Type of contribution") + ggtitle(title)

  return(g)
}


plot.thread.densities <- function(thread.info, title="") {
  ## Former plot thread_densities.pdf
  d.auth <- density(thread.info$authors)
  d.msg <- density(thread.info$messages)
  thread.densities <- rbind(data.frame(num=d.auth$x, density=d.auth$y,
                                       type="Authors"),
                            data.frame(num=d.msg$x, density=d.msg$y,
                                       type="Messages"))

  g <- ggplot(thread.densities, aes(x=num, y=density)) + geom_line() +
       scale_y_sqrt() + facet_grid(type~.) + xlab("Number per thread") +
       ylab("Density") + ggtitle(title)

  return(g)
}
