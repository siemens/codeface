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
## Copyright 2015 by Siemens AG, Mitchell Joblin <mitchell.joblin.ext@siemens.com>
## All Rights Reserved.
    
suppressMessages(library(igraph))
suppressMessages(library(markovchain))
suppressMessages(library(psych))

source("db.r")
source("query.r")

## Classify a set of developers based on the number of commits made withing a
## time range, core developers are those which are responsible for a given
## percentage of the work load with a default threshold set a 80% according
## to Ref: Terceiro A, Rios LR, Chavez C (2010) An empirical study on
##         the structural complexity introduced by core and peripheral
##         developers in free software projects.
get.developer.class.con <- function(con, project.id, start.date, end.date,
                                    source, count.type) {

  if (source=="VCS") {
    db.dat <- get.commits.by.date.con(con, project.id, start.date, end.date,
                                      count.type=count.type)
  } else if (source=="mail") {
    db.dat <- query.author.mail.count(con, project.id, start.date, end.date)
  }

  developer.class <- get.developer.class(db.dat, count.type)

  return(developer.class)
}



## Low-level function to compute classification
get.developer.class <- function(author.commit.count, count.type, threshold=0.8,
                                quantile.threshold=TRUE) {
  if (nrow(author.commit.count)==0) {
    return(NULL)
  }

  developer.class <- author.commit.count[order(-author.commit.count$freq),]
  num.commits <- sum(developer.class$freq)
  if (quantile.threshold) {
    commit.threshold <- quantile(developer.class$freq, probs=threshold)
    core.test <- developer.class$freq > commit.threshold
  } else {
    commit.threshold <- round(threshold * num.commits)
    core.test <- cumsum(developer.class$freq) < commit.threshold
  }

  developer.class[core.test, "class"] <- "core"
  developer.class[!core.test, "class"] <- "peripheral"
  developer.class$metric <- paste(count.type, "count", sep=".")

  return(developer.class)
}

## Determine developer class based on vertex centrality
get.developer.class.centrality <- function(edgelist, vertex.ids, source,
                                           threshold=0.8, metric="degree",
                                           quantile.threshold=TRUE) {
  if(nrow(edgelist)==0) {
    return(NULL)
  }

  if (metric=="degree") FUN <- igraph::degree
  if (metric=="evcent") FUN <- evcent.named
  if (metric=="page.rank") FUN <- page.rank.named
  if (metric=="hierarchy") FUN <- vertex.hierarchy

  graph <- graph.data.frame(edgelist, directed=FALSE,
                            vertices=data.frame(vertex.ids))
  centrality.vec <- sort(FUN(graph), decreasing=T)
  developer.class <- data.frame(author=names(centrality.vec),
                                centrality=as.vector(centrality.vec))
  if (quantile.threshold) {
    centrality.threshold <- quantile(centrality.vec, probs=threshold)
    core.test <- developer.class$centrality > centrality.threshold
  } else{
    centrality.threshold <- threshold * sum(centrality.vec)
    core.test <- cumsum(developer.class$centrality) < centrality.threshold
  }

  developer.class[core.test, "class"] <- "core"
  developer.class[!core.test, "class"] <- "peripheral"
  developer.class$metric <- paste(source, metric, sep=" ")

  return(developer.class)
}


## Compare aggreement between developer classes
compare.classification <- function(developer.class.1, developer.class.2,
                                   similarity.metric=c("cohen")) {
  classes.merged <- merge(developer.class.1, developer.class.2, by="author")
  classes.merged$match <- classes.merged$class.x == classes.merged$class.y

  res <- list()
  if ("jaccard" %in% similarity.metric) {
    res[["total.jaccard"]] <- nrow(subset(classes.merged, match==T)) /
                              nrow(classes.merged)

    res[["core.jaccard"]] <- nrow(subset(classes.merged, match==T &
                                         class.x=="core")) /
                             (nrow(subset(classes.merged, class.x=="core")) +
                             nrow(subset(classes.merged, class.y=="core")) -
                             nrow(subset(classes.merged, match==T & class.x=="core")))

    res[["peripheral.jaccard"]] <- nrow(subset(classes.merged, match==T &
                                               class.x=="peripheral")) /
                                   (nrow(subset(classes.merged, class.x=="peripheral")) +
                                   nrow(subset(classes.merged, class.y=="peripheral")) -
                                   nrow(subset(classes.merged, match==T & class.x=="peripheral")))
  }

  if ("cohen" %in% similarity.metric) {
    if (all(classes.merged$match)) {
      res[["cohen"]] <- 1
    } else if (nrow(classes.merged) > 2) {
      res[["cohen"]] <- cohen.kappa(classes.merged[, c("class.x", "class.y")])$kappa
    } else {
      res[["cohen"]] <- NA
    }
  }

  return(res)
}

## Compare aggreement accross a set of developer classes
compare.classification.all <- function(developer.classifications) {
  classification.types <- names(developer.classifications)
  classification.pairs <- combn(classification.types, m=2, simplify=F)

  all.agreement <- list()
  i <- 0
  for (pair in classification.pairs) {
    classification.1 <- developer.classifications[[pair[[1]]]]
    classification.2 <- developer.classifications[[pair[[2]]]]
    classes.merged <- merge(classification.1, classification.2,
        by=c("author", "L1"))
    dates <- unique(classes.merged$L1)

    ## Compute classification agreement over for all dates
    class.match <- lapply(dates,
        function(date) {
          compare.classification(subset(classes.merged, L1==date)[, c("author", "class.x")],
              subset(classes.merged, L1==date)[, c("author", "class.y")])
        })
    class.match.df <- melt(class.match)
    class.match.df$L1 <- dates[class.match.df$L1]
    correlation.type <- "spearman"
    colnames(class.match.df) <- c("value", "metric", "date")
    class.cor <- lapply(dates,
        function(date) {
          metric.values <- subset(classes.merged, L1==date)[, c("value.x",
                  "value.y")]
          class.rank.cor <- cor(metric.values$value.x,
              metric.values$value.y,
              method=correlation.type)
        })
    class.cor.df <- melt(class.cor)
    class.cor.df$metric <- correlation.type
    class.cor.df$L1 <- dates[class.cor.df$L1]
    colnames(class.cor.df) <- c("value", "date", "metric")

    agreement.df <- rbind(class.cor.df, class.match.df)
    agreement.df[, "class1"] <- unique(classification.1$metric)
    agreement.df[, "class2"] <- unique(classification.2$metric)
    all.agreement[[paste(pair, collapse="-")]] <- agreement.df
  }

  all.agreement <- do.call(rbind, all.agreement)
  all.agreement$date <- as.Date(all.agreement$date)

  return(all.agreement)
}

## Compute edge probabilities between core and peripheral groups
compute.edge.probs <- function(developer.class.list, edgelist, vertex.ids) {
  developer.class.list$author <- as.character(developer.class.list$author)
  edgelist[, 1:2] <- sapply(edgelist[, 1:2], as.character)
  graph <- graph.data.frame(edgelist, directed=FALSE,
                            vertices=data.frame(as.character(vertex.ids)))
  graph <- simplify(graph)

  ## Compute core edge probability
  core.ids <- subset(developer.class.list, class=="core")$author
  g.core <- induced.subgraph(graph, as.character(core.ids))
  core.prob <- ecount(g.core) / choose(vcount(g.core),2)

  ## Compute peripheral edge probability
  peri.ids <- subset(developer.class.list, class=="peripheral")$author
  g.peri <- induced.subgraph(graph, as.character(peri.ids))
  peri.prob <- ecount(g.peri) / choose(vcount(g.peri),2)

  ## Compute core-peripheral edge probability
  inter.edges <- E(graph)[core.ids %--% peri.ids]
  inter.prob <- length(inter.edges) / choose(vcount(graph),2)

  res <- c("core.prob"=core.prob,
           "peripheral.prob"=peri.prob,
           "inter.prob"=inter.prob)

  return(res)
}


## Compute markov chain for developer class transitions
compute.markov.chain <- function(developer.class.list, type="class") {
  index <- names(developer.class.list)
  turnover <-
      lapply(index,
          function(t) {
            class.state.t <- developer.class.list[[t]]
            res <- class.state.t[, c("author", type)]
            state.t <- paste("state.", t, sep="")
            colnames(res) <- c("id", state.t)
            return(res)
          })
  turnover.all <- join_all(turnover, by="id", type="full")
  turnover.all[is.na(turnover.all)] <- "absent"
  markov.chain <- markovchainFit(data=turnover.all[, -1], method="mle")$estimate
  return(markov.chain)
}

compute.class.markov.chain <- function(developer.class.list) {
    return(compute.markov.chain(developer.class.list, "class"))
}

## Centrality metric wrappers to generate named vectors
page.rank.named <- function(g) {
  cent.vec <- page.rank(g)$vector
  names(cent.vec) <- V(g)$name
  return(cent.vec)
}

evcent.named <- function(g) {
  cent.vec <- evcent(g)$vector
  names(cent.vec) <- V(g)$name
  return(cent.vec)
}

vertex.hierarchy <- function (g) {
  v.ids <- V(g)
  cc <- igraph::transitivity(g, type="local", vids=)
  deg <- igraph::degree(g, v=)
  hier <- deg/cc
  hier[!is.finite(hier)] <- 0
  return(hier)
}

## Plot agreement between classifications
plot.agreement <- function(dat) {
  ## Sort based on number of occurences
  freq <- ave(rep(1, times=nrow(dat)), c(dat$class1, dat$class2), FUN=sum)
  dat <- dat[sort.list(freq), ]
  labels <- unique(c(dat$class2, dat$class1))

  p.matrix <- ggplot(dat, aes(x=class1, y=class2, fill=value)) +
      geom_tile(stat="identity", color="white") +
      geom_text(aes(Var1=class1, Var2=class2, label=signif(value,2)),
                    color="black", size=4) +
      facet_wrap(~ metric, ncol=2) +
      scale_fill_gradient(low="#32cd32", high="#145214", na.value="white",
                          limit=c(0,1)) +
      scale_x_discrete(limits=labels) +
      scale_y_discrete(limits=labels) +
      coord_fixed() +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.text.x=element_text(angle=90, vjust=1),
            axis.title.y = element_blank(),
            #panel.grid.major = element_blank(),
            panel.border = element_blank(),
            #panel.background = element_blank(),
            axis.ticks = element_blank())

  return(p.matrix)
}
