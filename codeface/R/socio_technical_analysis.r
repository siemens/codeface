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
## Copyright 2016 by Mitchell Joblin <mitchell.joblin.ext@siemens.com>
## Copyright 2016 by Wolfgang Mauerer <wolfgang.mauerer@oth-regensburg.de>
## All Rights Reserved.

s <- suppressPackageStartupMessages
s(library(ggplot2))
s(library(igraph))
s(library(BiRewire))
s(library(GGally))
s(library(lubridate))
s(library(corrplot))

source("query.r")
source("config.r")
source("dependency_analysis.r")
source("process_dsm.r")
source("process_jira.r")
source("quality_analysis.r")
source("ml/ml_utils.r", chdir=T)
source("id_manager.r")

#################################################################################
plot.to.file <- function(g, outfile) {
    g <- simplify(g,edge.attr.comb="first")
    g <- delete.vertices(g, names(which(degree(g)<2)))
    E(g)[is.na(E(g)$color)]$color <- "#0000001A"
    png(file=outfile, width=10, height=10, units="in", res=300)
    plot(g, layout=layout.kamada.kawai, vertex.size=2,
         vertex.label.dist=0.5, edge.arrow.size=0.5,
         vertex.label=NA)
    dev.off()
}

motif.generator <- function(type, person.role, artifact.type, vertex.coding, anti=FALSE) {
    ensure.supported.artifact.type(artifact.type)

    motif <- graph.empty(directed=FALSE)
    if (type=="square") {
        motif <- add.vertices(motif, 4)
        motif <- add.edges(motif, c(1,2, 1,3, 2,4, 3,4))
        if (anti) motif <- delete.edges(motif, c(1))
        V(motif)$kind <- c(person.role, person.role, artifact.type, artifact.type)
        V(motif)$color <- vertex.coding[V(motif)$kind]
    }
    else if (type=="triangle") {
        motif <- add.vertices(motif, 3)
        motif <- add.edges(motif, c(1,2, 1,3, 2,3))
        if (anti) motif <- delete.edges(motif, c(1))
        V(motif)$kind <- c(person.role, person.role, artifact.type)
        V(motif)$color <- vertex.coding[V(motif)$kind]
    }
    else {
        motif <- NULL
    }

    return(motif)
}

preprocess.graph <- function(g, person.role) {
    ## Remove loops and multiple edges
    g <- simplify(g, remove.multiple=TRUE, remove.loops=TRUE,
                  edge.attr.comb="first")

    ## Remove low degree artifacts
    ##artifact.degree <- degree(g, V(g)[V(g)$kind==artifact.type])
    ##low.degree.artifact <- artifact.degree[artifact.degree < 2]
    ##g <- delete.vertices(g, v=names(low.degree.artifact))

    ## Remove isolated developers
    dev.degree <- degree(g, V(g)[V(g)$kind==person.role])
    isolated.dev <- dev.degree[dev.degree==0]
    g <- delete.vertices(g, v=names(isolated.dev))

    return(g)
}

cor.mtest <- function(mat, conf.level = 0.95) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    diag(lowCI.mat) <- diag(uppCI.mat) <- 1
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level, method="spearman")
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
                                        #lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
                                        #uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
        }
    }
    return(list(p.mat, lowCI.mat, uppCI.mat))
}

## Some helper functions to ensure that functions receive correct
## parameter values
ensure.supported.artifact.type <- function(artifact.type) {
    if(!(artifact.type %in% c("function", "file", "feature"))) {
        stop(str_c("Internal error: Artifact type ", artifact.type,
                   " is unsupported!"))
    }
}

ensure.supported.dependency.type <- function(dependency.type) {
    if(!(dependency.type %in% c("co-change", "dsm", "feature_call", "none"))) {
        stop(str_c("Internal error: Dependency type ", dependency.type,
                   " is unsupported!"))
    }
}

ensure.supported.quality.type <- function(quality.type) {
    if(!(quality.type %in% c("corrective", "defect"))) {
        stop(str_c("Internal error: Quality type ", quality.type,
                   " is unsupported!"))
    }
}

ensure.supported.communication.type <- function(communication.type) {
    if(!(communication.type %in% c("mail", "jira"))) {
        stop(str_c("Internal error: Communication type ", communication.type,
                   " is unsupported!"))
    }
}

do.conway.analysis <- function(conf, resdir, srcdir, titandir) {
    project.name <- conf$project
    project.id <- conf$pid

    ## Analysis window
    ## TODO: Select these automatically from the database
    window.size <- 12 # months
    end.date <- list(flink="2015-11-05",
                     cassandra="2015-11-06",
                     thrift="2015-09-25",
                     storm="2015-11-20",
                     camel="2015-11-08",
                     solr="2016-02-20",
                     hbase="2016-02-23",
                     lucene="2016-02-20",
                     accumulo="2015-12-03")[[project.name]]
    start.date <- as.character(ymd(end.date) - months(window.size))

    dsm.filename <- file.path(titandir, "sdsm", "project.sdsm")
    feature.call.filename <- "/home/mitchell/Documents/Feature_data_from_claus/feature-dependencies/cg_nw_f_1_18_0.net"
    jira.filename <- file.path(srcdir, "jira-comment-authors-with-email.csv")
    defect.filename <- file.path(srcdir, "file_metrics.csv")


    ## Analysis
    motif.type <- list("triangle", "square")[[2]]
    artifact.type <- list("function", "file", "feature")[[2]]
    dependency.type <- list("co-change", "dsm", "feature_call", "none")[[2]]
    quality.type <- list("corrective", "defect")[[2]]
    communication.type <- list("mail", "jira")[[2]]

    ## Constants
    person.role <- "developer"
    file.limit <- 30
    historical.limit <- ddays(365)

    ## Compute dev-artifact relations
    vcs.dat <- query.dependency(conf$con, project.id, artifact.type, file.limit,
                                start.date, end.date, impl=FALSE, rmv.dups=FALSE)
    vcs.dat$entity <- sapply(vcs.dat$entity,
                             function(filename) filename <- gsub("/", ".", filename, fixed=T))
    vcs.dat$author <- as.character(vcs.dat$author)

    ## Save to csv TODO: Why do we need to save that?
    write.csv(vcs.dat, file.path(resdir, "commit_data.csv"))

    ## Compute communication relations
    if (communication.type=="mail") {
        comm.dat <- query.mail.edgelist(conf$con, project.id, start.date, end.date)
        colnames(comm.dat) <- c("V1", "V2", "weight")
    } else if (communication.type=="jira") {
        comm.dat <- load.jira.edgelist(conf, jira.filename, start.date, end.date)
    }
    comm.dat[, c(1,2)] <- sapply(comm.dat[, c(1,2)], as.character)

    ## Compute entity-entity relations
    relevant.entity.list <- unique(vcs.dat$entity)
    if (dependency.type == "co-change") {
        start.date.hist <- as.Date(start.date) - historical.limit
        end.date.hist <- start.date

        commit.df.hist <- query.dependency(conf$con, project.id, artifact.type, file.limit,
                                           start.date.hist, end.date.hist)

        commit.df.hist <- commit.df.hist[commit.df.hist$entity %in% relevant.entity.list, ]

        ## Compute co-change relationship
        freq.item.sets <- compute.frequent.items(commit.df.hist)
        ## Compute an edgelist
        dependency.dat <- compute.item.sets.edgelist(freq.item.sets)
        names(dependency.dat) <- c("V1", "V2")

    } else if (dependency.type == "dsm") {
        dependency.dat <- load.sdsm(dsm.filename, relevant.entity.list)
        dependency.dat <-
            dependency.dat[dependency.dat[, 1] %in% relevant.entity.list &
                           dependency.dat[, 2] %in% relevant.entity.list, ]
    } else if (dependency.type == "feature_call") {
        graph.dat <- read.graph(feature.call.filename, format="pajek")
        V(graph.dat)$name <- V(graph.dat)$id
        dependency.dat <- get.data.frame(graph.dat)
        dependency.dat <-
            dependency.dat[dependency.dat[, 1] %in% relevant.entity.list &
                           dependency.dat[, 2] %in% relevant.entity.list, ]
        names(dependency.dat) <- c("V1", "V2")
    } else {
        dependency.dat <- data.frame()
    }

    ## Compute node sets
    node.function <- unique(vcs.dat$entity)
    node.dev <- unique(c(vcs.dat$author))

    ## Generate bipartite network
    g.nodes <- graph.empty(directed=FALSE)
    g.nodes <- add.vertices(g.nodes, nv=length(node.dev),
                            attr=list(name=node.dev, kind=person.role,
                                      type=TRUE))
    g.nodes  <- add.vertices(g.nodes, nv=length(node.function),
                             attr=list(name=node.function, kind=artifact.type,
                                       type=FALSE))

    ## Add developer-entity edges
    vcs.edgelist <- with(vcs.dat, ggplot2:::interleave(author, entity))
    g.bipartite <- add.edges(g.nodes, vcs.edgelist, attr=list(color="#00FF001A"))

    ## Add developer-developer communication edges
    g <- graph.empty(directed=FALSE)
    ## Remove persons that don't appear in VCS data
    comm.inter.dat <- comm.dat[comm.dat$V1 %in% node.dev & comm.dat$V2 %in% node.dev, ]
    comm.edgelist <- as.character(with(comm.inter.dat, ggplot2:::interleave(V1, V2)))
    g <- add.edges(g.bipartite, comm.edgelist, attr=list(color="#FF00001A"))

    ## Add entity-entity edges
    if(nrow(dependency.dat) > 0) {
        dependency.edgelist <- as.character(with(dependency.dat,
                                                 ggplot2:::interleave(V1, V2)))
        g <- add.edges(g, dependency.edgelist)
    }

    ## Apply filters
    g <- preprocess.graph(g, person.role)

    ## Define a numeric encoding scheme for vertices
    vertex.coding <- c()
    vertex.coding[person.role] <- 1
    vertex.coding[artifact.type] <- 2
    V(g)$color <- vertex.coding[V(g)$kind]

    ## Save graph
    write.graph(g, file.path(resdir, "network_data.graphml"), format="graphml")

    ## Define motif
    motif <- motif.generator(motif.type, person.role, artifact.type, vertex.coding)
    motif.anti <- motif.generator(motif.type, person.role, artifact.type, vertex.coding, anti=TRUE)

    ## Count subgraph isomorphisms
    motif.count <- count_subgraph_isomorphisms(motif, g, method="vf2")
    motif.anti.count <- count_subgraph_isomorphisms(motif.anti, g, method="vf2")

    ## Extract subgraph isomorphisms
    motif.subgraphs <- subgraph_isomorphisms(motif, g, method="vf2")
    motif.subgraphs.anti <- subgraph_isomorphisms(motif.anti, g, method="vf2")

    ## Compute null model
    niter <- 100
    motif.count.null <- c()

    motif.count.null <-
        mclapply(seq(niter),
                 function(i) {
                     ## Rewire dev-artifact bipartite
                     g.bipartite.rewired <- birewire.rewire.bipartite(simplify(g.bipartite), verbose=FALSE) #g.bipartite

                     ## Add rewired edges
                     g.null <- add.edges(g.nodes,
                                         as.character(with(get.data.frame(g.bipartite.rewired),
                                                           ggplot2:::interleave(from, to))))

                     ## Aritfact-artifact edges
                     if (nrow(dependency.dat) > 0) {
                         g.null <- add.edges(g.null, dependency.edgelist)
                     }

                     ## Test degree dist
                                        #if(!all(sort(as.vector(degree(g.null))) ==
                                        #        sort(as.vector(degree(g.bipartite))))) stop("Degree distribution not conserved")

                     ## Rewire dev-dev communication graph
                     g.comm <- graph.data.frame(comm.inter.dat)
                     g.comm.null <- birewire.rewire.undirected(simplify(g.comm),
                                                               verbose=FALSE)

                     ## Test degree dist
                     if(!all(sort(as.vector(degree(g.comm.null))) ==
                             sort(as.vector(degree(g.comm))))) stop("Degree distribution not conserved")

                     g.null <- add.edges(g.null,
                                         as.character(with(get.data.frame(g.comm.null),
                                                           ggplot2:::interleave(from, to))))

                     ## Code and count motif
                     V(g.null)$color <- vertex.coding[V(g.null)$kind]

                     g.null <- preprocess.graph(g.null, person.role)

                     count.positive <- count_subgraph_isomorphisms(motif, g.null, method="vf2")
                     count.negative <- count_subgraph_isomorphisms(motif.anti, g.null, method="vf2")

                     res <- data.frame(count.type=c("positive", "negative"),
                                       count=c(count.positive, count.negative))

                     return(res)}, mc.cores=2) # TODO: Use codefaces multiprocessing infrastructure!
    print("...finished")

    null.model.dat <- do.call(rbind, motif.count.null)
    null.model.dat[null.model.dat$count.type=="positive", "empirical.count"] <- motif.count
    null.model.dat[null.model.dat$count.type=="negative", "empirical.count"] <- motif.anti.count

    ## Save plots
    networks.dir <- file.path(resdir, "motif_analysis", motif.type, communication.type)
    dir.create(networks.dir, recursive=T, showWarnings=T)

    ## Null model
    p.null <- ggplot(data=null.model.dat, aes(x=count)) +
        geom_histogram(aes(y=..density..), colour="black", fill="white") +
        geom_point(aes(x=empirical.count), y=0, color="red", size=5) +
        geom_density(alpha=.2, fill="#AAD4FF") +
    facet_wrap(~count.type, scales="free")

    ggsave(plot=p.null,
           filename=file.path(networks.dir, "motif_null_model.png"))

    ## Communication degree distribution
    p.comm <- ggplot(data=data.frame(degree=degree(graph.data.frame(comm.dat))), aes(x=degree)) +
        geom_histogram(aes(y=..density..), colour="black", fill="white") +
        geom_density(alpha=.2, fill="#AAD4FF")

    ggsave(plot=p.comm,
           filename=file.path(networks.dir, "communication_degree_dist.png"))

    ## Complete network
    plot.to.file(g, file.path(networks.dir, "socio_technical_network.png"))

    ## Perform quality analysis
    if (quality.type=="defect") {
        quality.dat <- load.defect.data(defect.filename, relevant.entity.list,
                                        start.date, end.date)
    } else {
        quality.dat <- get.corrective.count(conf$con, project.id, start.date, end.date,
                                            artifact.type)
    }

    artifacts <- count(data.frame(entity=unlist(lapply(motif.subgraphs,
                                                       function(i) i[[3]]$name))))
    anti.artifacts <- count(data.frame(entity=unlist(lapply(motif.subgraphs.anti,
                                                            function(i) i[[3]]$name))))

    ## Get file developer count
    file.dev.count.df <- ddply(vcs.dat, .(entity),
                               function(df) data.frame(entity=unique(df$entity),
                                                       dev.count=length(unique(df$id))))

    compare.motifs <- merge(artifacts, anti.artifacts, by='entity', all=TRUE)

    compare.motifs[is.na(compare.motifs)] <- 0
    names(compare.motifs) <- c("entity", "motif.count", "motif.anti.count")

    artifacts.dat <- merge(quality.dat, compare.motifs, by="entity")
    artifacts.dat <- merge(artifacts.dat, file.dev.count.df, by="entity")

    ## Add features
    artifacts.dat$motif.percent.diff <- 2 * abs(artifacts.dat$motif.anti.count - artifacts.dat$motif.count) /
        (artifacts.dat$motif.anti.count + artifacts.dat$motif.count)
    artifacts.dat$motif.ratio <- artifacts.dat$motif.anti.count / artifacts.dat$motif.count
    artifacts.dat$motif.ratio[is.infinite(artifacts.dat$motif.ratio)] <- NA
    artifacts.dat$bug.density <- artifacts.dat$BugIssueCount / (artifacts.dat$CountLineCode+1)
    artifacts.dat$motif.count.norm <- artifacts.dat$motif.count / artifacts.dat$dev.count
    artifacts.dat$motif.anti.count.norm <- artifacts.dat$motif.anti.count / artifacts.dat$dev.count

    ## Generate correlation plot (omitted correlatipn quantitied: BugIsseChurn, IssueCommits)
    corr.cols <- c("motif.count",      "motif.anti.count",
                   "motif.count.norm", "motif.anti.count.norm",
                   "motif.ratio",      "motif.percent.diff",
                   "dev.count",        "bug.density",
                   "BugIssueCount",    "Churn",
                   "CountLineCode")

    correlation.dat <- ggpairs(artifacts.dat,
                               columns=corr.cols,
                               lower=list(continuous=wrap("points",
                                                          alpha=0.33,
                                                          size=0.5)),
                               upper=list(continuous=wrap('cor',
                                                          method='spearman'))) +
        theme(axis.title.x = element_text(angle = 90, vjust = 1, color = "black"))

    corr.mat <- cor(artifacts.dat[, corr.cols], use="pairwise.complete.obs", method="spearman")
    corr.test <- cor.mtest(artifacts.dat[, corr.cols])

    ## Write correlations and raw data to file
    corr.plot.path <- file.path(resdir, "quality_analysis", motif.type, communication.type)
    dir.create(corr.plot.path, recursive=T, showWarnings=T)
    png(file.path(corr.plot.path, "correlation_plot.png"), width=1200, height=1200)
    print(correlation.dat)
    dev.off()

    png(file.path(corr.plot.path, "correlation_plot_color.png"), width=700, height=700)
    corrplot(corr.mat, p.mat=corr.test[[1]],
             insig = "p-value", sig.level=0.05, method="color",
             type="upper")
    dev.off()

    write.csv(artifacts.dat, file.path(corr.plot.path, "quality_data.csv"))
}

######################### Dispatcher ###################################
config.script.run({
    conf <- config.from.args(positional.args=list("resdir", "srcdir"),
                             require.project=TRUE)
    resdir <- file.path(conf$resdir, conf$project, "conway")
    srcdir <- file.path(conf$srcdir, conf$project)
    titandir <- file.path(srcdir, "titan")

    logdevinfo(paste("Directory for storing conway results is", resdir), logger="conway")
    dir.create(resdir, showWarnings=FALSE, recursive=TRUE)

    if (conf$profile) {
        ## R cannot store line number profiling information before version 3.
        if (R.Version()$major >= 3) {
            Rprof(filename="ts.rprof", line.profiling=TRUE)
        } else {
            Rprof(filename="ts.rprof")
        }
    }

    do.conway.analysis(conf, resdir, srcdir, titandir)
})