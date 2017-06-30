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
source("utils.r")
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
    pdf(file=outfile, width=10, height=10)
    plot(g, layout=layout.kamada.kawai, vertex.size=2,
         vertex.label.dist=0.5, edge.arrow.size=0.5,
         vertex.label=NA)
    dev.off()
}

motif.generator <- function(type, person.role, artifact.type, vertex.coding,
                            anti=FALSE) {
    ensure.supported.artifact.type(artifact.type)

    motif <- graph.empty(directed=FALSE)
    if (type=="square") {
        motif <- add.vertices(motif, 4)
        motif <- add.edges(motif, c(1,2, 1,3, 2,4, 3,4))
        if (anti) motif <- delete.edges(motif, c(1))
        V(motif)$kind <- c(person.role, person.role, artifact.type,
                           artifact.type)
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

## Compute communication relations between contributors
compute.communication.relations <- function(conf, jira.filename,
                                            start.date, end.date) {
    communication.type <- conf$communicationType

    if (communication.type=="mail") {
        comm.dat <- query.mail.edgelist(conf$con, conf$pid, start.date, end.date)

        ## If there are no usable communication relationships within the
        ## analysed time range, make sure to exit the analysis early)
        if (nrow(comm.dat) == 0) {
            comm.dat <- NULL
        } else {
            colnames(comm.dat) <- c("V1", "V2", "weight")
        }
    } else if (communication.type=="jira") {
        ## If there are no jira data, load.jira.edgelist will return NULL
        comm.dat <- load.jira.edgelist(conf, jira.filename, start.date, end.date)
    }

    if (is.null(comm.dat)) {
        return(NULL)
    }
    comm.dat[, c(1,2)] <- sapply(comm.dat[, c(1,2)], as.character)

    return(comm.dat)
}

## Compute entity-entity relations, that is, couplings between non-person
## artefacts (functions with functions, files with files, etc.)
## Returns a data frame with columns V1 and V2 (for two entities that
## are connected), and optionally a weight column (if weights for
## the connections are available)
compute.ee.relations <- function(conf, vcs.dat, start.date, end.date,
                                 dsm.filename, params) {
    dependency.type <- conf$dependencyType
    artifact.type <- conf$artifactType
    ensure.supported.dependency.type(dependency.type)
    ensure.supported.artifact.type(artifact.type)

    ## Compute a list of relevant files (i.e., all files touched in the release
    ## range)
    relevant.entity.list <- unique(vcs.dat$entity)
    if (dependency.type == "co-change") {
        start.date.hist <- as.Date(start.date) - params$historical.limit
        end.date.hist <- start.date

        commit.df.hist <- query.dependency(conf, params$file.limit,
                                           start.date.hist, end.date.hist)

        commit.df.hist <- commit.df.hist[commit.df.hist$entity %in%
                                         relevant.entity.list,]

        ## Compute co-change relationship
        freq.item.sets <- compute.frequent.items(commit.df.hist)
        ## Compute an edgelist
        dependency.dat <- compute.item.sets.edgelist(freq.item.sets)
        if (nrow(dependency.dat) == 0) {
            logwarning("Could not find any co-change dependencies",
                       logger="conway")
            return(data.frame())
        }
        names(dependency.dat) <- c("V1", "V2", "weight")
    } else if (dependency.type == "dsm") {
        dependency.dat <- load.sdsm(dsm.filename)
        if (is.null(dependency.dat)) {
            logwarning(str_c("Could not obtain any dependencies from the SDSM! ",
                             "Trying to continue without dependencies.\n",
                             "Is the implementation language supported?"), logger="conway")
            return(data.frame())
        }

        dependency.dat <-
            dependency.dat[dependency.dat[, 1] %in% relevant.entity.list &
                           dependency.dat[, 2] %in% relevant.entity.list,]
    } else if (dependency.type == "feature_call") {
        graph.dat <- read.graph(feature.call.filename, format="pajek")
        V(graph.dat)$name <- V(graph.dat)$id
        dependency.dat <- get.data.frame(graph.dat)
        dependency.dat <-
            dependency.dat[dependency.dat[, 1] %in% relevant.entity.list &
                           dependency.dat[, 2] %in% relevant.entity.list,]
        names(dependency.dat) <- c("V1", "V2")
    } else {
        dependency.dat <- data.frame()
    }

    dependency.dat$V1 <- as.character(dependency.dat$V1)
    dependency.dat$V2 <- as.character(dependency.dat$V2)
    return(dependency.dat)
}

MOTIF.SEARCH.LIMIT=10*60 # Allow for trying up to 10 minutes of motif searching
do.motif.search <- function(motif, g, count.only=TRUE) {
    ## Compute the matching domains (i.e., which vertices in the pattern are
    ## allowed to be matched with which vertices in the larger graph)
    dom <- lapply(V(motif)$color, function(col) { V(g)[V(g)$color==col] })

    if (count.only) {
        ## Count subgraph isomorphisms in the collaboration graph with
        ## the given motif (this is faster than actually computing the subgraphs)
        motif.count <- count_subgraph_isomorphisms(motif, g, method="lad", domain=dom,
                                                   induced=TRUE, time.limit=MOTIF.SEARCH.LIMIT)
        return(motif.count)
    }

    motif.subgraphs <- subgraph_isomorphisms(motif, g, method="lad", domain=dom,
                                             induced=TRUE, time.limit=MOTIF.SEARCH.LIMIT)
    return(motif.subgraphs)
}


do.null.model <- function(g.bipartite, g.nodes, person.role, dependency.dat,
                          dependency.edgelist, comm.inter.dat, vertex.coding,
                          motif, motif.anti) {
    ## Rewire dev-artifact bipartite
    g.bipartite.rewired <- birewire.rewire.bipartite(simplify(g.bipartite),
                                                     verbose=FALSE)

    ## Add rewired edges
    gbr.df <- get.data.frame(g.bipartite.rewired)
    g.null <- add.edges(g.nodes,
                        as.character(do.interleave(gbr.df$from, gbr.df$to)))

    ## Aritfact-artifact edges
    if (nrow(dependency.dat) > 0) {
        g.null <- add.edges(g.null, dependency.edgelist)
    }

    ## Rewire dev-dev communication graph
    g.comm <- graph.data.frame(comm.inter.dat)

    ## birewire.rewire.undirected goes into an infinite loop
    ## if |E|/(|V|^{2}/2)=1 (see the calculation in birewire.rewire.sparse),
    ## and more generally if (1-|E|/(|V|^{2}/2)) <= 0.
    ## This condition can arise when a graph with very few edges is considered,
    ## but also for non-pathological inputs.
    ## Bound the maximal number of iterations to a (large) value to avoid the
    ## infinite loop that arises in the rewiring code in these cases.
    g.simplified <- simplify(g.comm)

    ## The following quantities are required to reproduce calculations from the
    ## birewiring code
    n <- as.numeric(length(V(g.simplified)))
    e <- as.numeric(length(E(g.simplified)))

    if (1-e/(n^2/2) <= 0) {
        logwarning("Warning: Preventing infinite birewiring loop by bounding the maximal iteration count",
                   logger="conway")
        g.comm.null <- birewire.rewire.undirected(g.simplified, max.iter=50000,
                                                  verbose=FALSE)
    } else {
        g.comm.null <- birewire.rewire.undirected(g.simplified, verbose=FALSE)
    }

    ## Ensure degree distribution is identical for graph and null model
    if(!all(sort(as.vector(degree(g.comm.null))) ==
            sort(as.vector(degree(g.comm))))) {
        stop("Internal error: degree distribution not conserved!")
    }

    g.null <- add.edges(g.null,
                        as.character(with(get.data.frame(g.comm.null),
                                          do.interleave(from, to))))

    ## Code and count motif
    V(g.null)$color <- vertex.coding[V(g.null)$kind]
    g.null <- preprocess.graph(g.null, person.role)

    count.positive <- do.motif.search(motif, g.null)
    count.negative <- do.motif.search(motif.anti, g.null)
    ratio <- count.positive/(count.positive+count.negative)

    res <- data.frame(count.type=c("positive", "negative", "ratio"),
                      count=c(count.positive, count.negative, ratio))

    return(res)
}

## Generate a text string with some information about the range under analysis
gen.plot.info <- function(stats) {
    return(str_c(stats$project, " (", stats$start.date, "--", stats$end.date,
                 ")\n", "Devs: ", stats$num.devs,
                 " Funs: ", stats$num.functions,
                 " M: ", stats$num.motifs,
                 " A-M: ", stats$num.motifs.anti, sep=""))
}

do.quality.analysis <- function(conf, vcs.dat, defect.filename, start.date, end.date,
                                motif.type, motif.dat, stats, range.resdir) {
    ## Perform quality analysis
    if (length(motif.dat$motif.subgraphs) == 0 ||
        length(motif.dat$motif.anti.subgraphs) == 0) {
        loginfo("Quality analysis lacks motifs or anti-motifs, exiting early")
        return(NULL)
    }

    artifact.type <- conf$artifactType
    quality.type <- conf$qualityType
    communication.type <- conf$communicationType

    relevant.entity.list <- unique(vcs.dat$entity)
    if (quality.type=="defect") {
        quality.dat <- load.defect.data(defect.filename, relevant.entity.list,
                                        start.date, end.date)
    } else {
        quality.dat <- get.corrective.count(conf$con, conf$pid, start.date,
                                            end.date, artifact.type)
    }

    artifacts <- count(data.frame(entity=unlist(lapply(motif.dat$motif.subgraphs,
                                                       function(i) i[[3]]$name))))
    anti.artifacts <- count(data.frame(entity=unlist(lapply(motif.dat$motif.anti.subgraphs,
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
    artifacts.dat$motif.percent.diff <- 2 * abs(artifacts.dat$motif.anti.count -
                                                artifacts.dat$motif.count) /
                    (artifacts.dat$motif.anti.count + artifacts.dat$motif.count)
    artifacts.dat$motif.percent.diff.sign <- 2 * (artifacts.dat$motif.anti.count -
                                                 artifacts.dat$motif.count) /
                    (artifacts.dat$motif.anti.count + artifacts.dat$motif.count)
    artifacts.dat$motif.ratio <- artifacts.dat$motif.anti.count /
        (artifacts.dat$motif.count + artifacts.dat$motif.anti.count)
    artifacts.dat$motif.count.norm <- artifacts.dat$motif.count /
        artifacts.dat$dev.count
    artifacts.dat$motif.anti.count.norm <- artifacts.dat$motif.anti.count /
        artifacts.dat$dev.count

    if (quality.type=="defect") {
        artifacts.dat$bug.density <- artifacts.dat$BugIssueCount /
            (artifacts.dat$CountLineCode+1)
    }

    corr.elements <- gen.correlation.columns(quality.type)

    ## Do not plot correlations for all quantities, but only for combinations
    ## that are of particular interest.
    artifacts.subset <- artifacts.dat[, corr.elements$names]
    colnames(artifacts.subset) <- corr.elements$labels

    ## NOTE: We deliberately use artifacts.dat and then subset the columns because
    ## working directly with artifacts.subset does not work -- the column names
    ## are not in a suitable format for ggpairs()
    correlation.plot <- ggpairs(artifacts.dat,
                                columns=corr.elements$names,
                                lower=list(continuous=wrap("points",
                                               alpha=0.33, size=0.5)),
                                upper=list(continuous=wrap('cor',
                                               method='spearman')),
                               title=gen.plot.info(stats)) + theme_bw() +
                  theme(axis.title.x = element_text(angle = 90, vjust = 1,
                            color = "black"))

    corr.mat <- cor(artifacts.subset, use="pairwise.complete.obs",
                    method="spearman")
    if (nrow(artifacts.subset) == 0) {
        loginfo(str_c("Conway analysis: No correlation data for time interval ",
                      start.date, "--", end.date, ", exiting early", sep=""),
                startlogger="conway")
        return()
    }

    ## Compute the p values for each correlation tests
    corr.test <- cor.mtest(artifacts.subset)

    ## Write correlations and raw data to file
    corr.plot.path <- file.path(range.resdir, "quality_analysis", motif.type,
                                communication.type)
    dir.create(corr.plot.path, recursive=T, showWarnings=T)
    pdf(file.path(corr.plot.path, "correlation_plot.pdf"), width=10, height=10)
    print(correlation.plot)
    dev.off()

    if (!is.null(corr.test)) {
        pdf(file.path(corr.plot.path, "correlation_plot_color.pdf"),
            width=7, height=7)
        corrplot(corr.mat, p.mat=corr.test[[1]],
                 insig = "p-value", sig.level=0.05, method="pie", type="lower")
        title(gen.plot.info(stats))
        dev.off()
    }

    write.csv(artifacts.dat, file.path(corr.plot.path, "quality_data.csv"))
}

search.motifs <- function(graphs, motif.type, params, dependency.dat,
                          dependency.edgelist, comm.inter.dat,
                          artifact.type, vertex.coding) {
    ## Generate motif and anti-motif that we want to find in the data
    motif <- motif.generator(motif.type, params$person.role, artifact.type,
                             vertex.coding)
    motif.anti <- motif.generator(motif.type, params$person.role, artifact.type,
                                  vertex.coding, anti=TRUE)

    ## Find motif and anti-motifs in the collaboration graph
    logdevinfo("Searching for motif and anti-motif", logger="conway")
    motif.subgraphs <- do.motif.search(motif, graphs$g, count.only=FALSE)
    motif.count <- length(motif.subgraphs)

    motif.anti.subgraphs <- do.motif.search(motif.anti, graphs$g, count.only=FALSE)
    motif.anti.count <- length(motif.anti.subgraphs)

    ## Compute a null model
    NITER <- 100

    logdevinfo("Computing null models", logger="conway")
    motif.count.null <- lapply(seq(NITER), function(i) {
        do.null.model(graphs$g.bipartite, graphs$g.nodes, params$person.role,
                      dependency.dat, dependency.edgelist, comm.inter.dat,
                      vertex.coding, motif, motif.anti)
    })

    null.model.dat <- do.call(rbind, motif.count.null)
    null.model.dat[null.model.dat$count.type=="positive", "empirical.count"] <-
        motif.count
    null.model.dat[null.model.dat$count.type=="negative", "empirical.count"] <-
        motif.anti.count
    null.model.dat[null.model.dat$count.type=="ratio", "empirical.count"] <-
        motif.count/(motif.count+motif.anti.count)

    logdevinfo("Cleaning up null models", logger="conway")
    ## When we did neither find motifs nor anti-motifs, set the fraction
    ## M/(M+A-M) to one (instead of NaN), since there are exactly as many motifs as
    ## anti-motifs. NaNs would also break the plotting code below.
    if (any(is.nan(null.model.dat$count))) {
        null.model.dat[is.nan(null.model.dat$count),]$count <- 1
    }
    if (any(is.nan(null.model.dat$empirical.count))) {
        null.model.dat[is.nan(null.model.dat$empirical.count),]$empirical.count <- 1
    }

    return(list(null.model.dat=null.model.dat, motif.subgraphs=motif.subgraphs,
                motif.anti.subgraphs=motif.anti.subgraphs))
}

do.motif.plots <- function(motif.dat, comm.dat, stats, networks.dir) {
    ## Visualise the null model
    labels <- c(negative = "Anti-Motif", positive = "Motif", ratio = "Ratio")
    p.null <- ggplot(data=motif.dat$null.model.dat, aes(x=count)) +
        geom_histogram(aes(y=..density..), colour="black", fill="white", bins=30) +
        geom_point(aes(x=empirical.count), y=0, color="red", size=5) +
        geom_density(alpha=.2, fill="#AAD4FF") +
        facet_wrap(~count.type, scales="free", labeller=labeller(count.type=labels)) +
        xlab("Count") + ylab("Density [a.u.]") + ggtitle(gen.plot.info(stats))

    ggsave(plot=p.null,
           filename=file.path(networks.dir, "motif_null_model.pdf"),
           width=8, height=4)

    ## Compute the communication degree distribution
    p.comm <- ggplot(data=data.frame(degree=degree(graph.data.frame(comm.dat))),
                     aes(x=degree)) +
        geom_histogram(aes(y=..density..), colour="black", fill="white", bins=30) +
        geom_density(alpha=.2, fill="#AAD4FF") + xlab("Node degree") +
        ylab("Density [a.u.]") + ggtitle(gen.plot.info(stats))

    ggsave(plot=p.comm,
           filename=file.path(networks.dir, "communication_degree_dist.pdf"),
           width=7, height=8)
}

do.conway.analysis <- function(conf, global.resdir, range.resdir, start.date, end.date,
                               titandir) {
    project.name <- conf$project
    project.id <- conf$pid

    dsm.filename <- file.path(titandir, "sdsm", "project.sdsm")
    ## TODO: How can this file be constructed?
    feature.call.filename <- file.path(global.resdir,
                                       "feature-dependencies/cg_nw_f_1_18_0.net")
    jira.filename <- file.path(global.resdir, "jira_issue_comments.csv")
    defect.filename <- file.path(range.resdir, "changes_and_issues_per_file.csv")

    ## The configuration object contains several variables that describe
    ## the "flavour" of the Conway analysis:
    ## artifactType: Which artefact to consider (function, file, feature)
    ## dependencyType: Mechanism to generate dependencies between files or functions
    ##                 (co-change, dsm, feature_call, none)
    ## qualityType: corrective or defect
    ## communicationType: Data source to capture developer communication (mail, jira)
    artifact.type <- conf$artifactType

    ## Constant parameters
    params <- list(person.role="developer", file.limit=30,
                   historical.limit <- ddays(365))

    ## Compute developer-artifact relationships that are obtained
    ## from the revision control system
    vcs.dat <- query.dependency(conf, params$file.limit, start.date, end.date)
    vcs.dat$author <- as.character(vcs.dat$author)

    ## Determine which function/file artifacts (node.artifact) and developers
    ## (node.dev) contribute to the developer-artifact relationships
    nodes.artifact <- unique(vcs.dat$entity)
    nodes.dev <- unique(c(vcs.dat$author))

    ## Compute various other relationships between contributors and/or entities
    comm.dat <- compute.communication.relations(conf, jira.filename,
                                                start.date, end.date)

    if (is.null(comm.dat)) {
        loginfo(str_c("Conway analysis: No usable communication relationships available ",
                      "for time interval ", start.date, "--", end.date, ", exiting early",
                      sep=""), startlogger="conway")
        return(NULL)
    }
    dependency.dat <- compute.ee.relations(conf, vcs.dat, start.date, end.date,
                                           dsm.filename, params)

    ## Generate a bipartite network that describes the socio-technical structure
    ## of a development project. This data structure is the core of the Conway
    ## analysis, and to make statements about relations between the technical
    ## and the social structure of a project.

    ## g.nodes is the basis for the constructed graph. It contains all
    ## relevant nodes, persons (developers) and artefacts, but has
    ## no edges.
    g.nodes <- graph.empty(directed=FALSE)
    g.nodes <- add.vertices(g.nodes, nv=length(nodes.dev),
                            attr=list(name=nodes.dev, kind=params$person.role,
                                      type=TRUE))
    g.nodes  <- add.vertices(g.nodes, nv=length(nodes.artifact),
                             attr=list(name=nodes.artifact, kind=artifact.type,
                                       type=FALSE))

    ## Graph g.bipartite based on g.nodes that contains developer-artifact edges
    ## (#00ff00 is green)
    vcs.edgelist <- with(vcs.dat, do.interleave(author, entity))
    g.bipartite <- add.edges(g.nodes, vcs.edgelist, attr=list(color="#00FF001A"))

    ## Create a graph g that is based on g.bipartite and that additionally
    ## captures developer-developer communication

    ## * First, remove persons that don't appear in VCS data (if they
    ##   didn't perform a technical contribution, there is nothing
    ##   socio-technical to be inferred about them), and transfer the
    ##   remaining edges of g.bipartite into g
    g <- graph.empty(directed=FALSE)
    comm.inter.dat <- comm.dat[comm.dat$V1 %in% nodes.dev &
                                   comm.dat$V2 %in% nodes.dev,]

    if (nrow(comm.inter.dat) == 0) {
        loginfo("No overlap between VCS and communication data, exiting analysis early!",
                logger="conway")
        return(NULL)
    }

    comm.edgelist <- as.character(with(comm.inter.dat,
                                       do.interleave(V1, V2)))
    ## (#ff0000 is red)
    g <- add.edges(g.bipartite, comm.edgelist, attr=list(color="#FF00001A"))

    ## * Second, add entity-entity edges
    if(nrow(dependency.dat) > 0) {
        dependency.edgelist <- with(dependency.dat,
                                    do.interleave(V1, V2))
        g <- add.edges(g, dependency.edgelist)
    }

    ## * Third, remove some undesired outlier contributions of the graph
    ##   that complicate further processing.
    g <- preprocess.graph(g, params$person.role)

    ## * Fourth, define a numeric encoding scheme for vertices, and color
    ##   the different vertices
    vertex.coding <- c()
    vertex.coding[params$person.role] <- 1
    vertex.coding[artifact.type] <- 2
    V(g)$color <- vertex.coding[V(g)$kind]

    ## * Last, save the resulting graph for external processing
    ## (TODO: This should go into the database)
    ## The graph has the following node attributes:
    ## * name (person ID (integer) or artifact name (string))
    ## * kind (string: developer, function, or file)
    ## * type (boolean: true for person, false for artifact)
    ## * color (colour as hex value)
    write.graph(g, file.path(range.resdir, "network_data.graphml"), format="graphml")

    ## Plot the complete network
    plot.to.file(g, file.path(range.resdir, "socio_technical_network.pdf"))

    ## ############### Analyse the socio-technical graph ##############
    ## ############### (motif specific computations) ##################
    graphs <- list(g=g, g.bipartite=g.bipartite, g.nodes=g.nodes)

    motif.types <- c("triangle", "square")
    if (conf$dependencyType == "none") {
        ## When no dependencies between artefacts are available,
        ## the square motif cannot be found
        motif.types <- c("triangle")
    }
                                       
    for (motif.type in motif.types) {
        loginfo(str_c("Performing ", motif.type, " motif calculations"),
                      logger="conway")
        motif.dat <- search.motifs(graphs, motif.type, params, dependency.dat,
                                   dependency.edgelist, comm.inter.dat,
                                   artifact.type, vertex.coding)

        logdevinfo("Computing null model stats", logger="conway")
        stats <- list(num.devs=length(nodes.dev), num.functions=length(nodes.artifact),
                      num.motifs=length(motif.dat$motif.subgraphs),
                      num.motifs.anti=length(motif.dat$motif.anti.subgraphs),
                      start.date=start.date, end.date=end.date, project=conf$project)

        ## Visualise the results in some graphs
        logdevinfo("Writing null model data", logger="conway")
        networks.dir <- file.path(range.resdir, "motif_analysis", motif.type,
                                  conf$communicationType)
        dir.create(networks.dir, recursive=TRUE, showWarnings=TRUE)

        ## The file contains the following columns:
        ## * count.type: string (positive, negative, ratio)
        ## * count: integer (the count or ratio obtained from the
        ##                   various rewired grapsh)
        ## * empirical.count: integer (count or ratio for the observed data)
        write.table(motif.dat$null.model.dat,
                    file=file.path(networks.dir, "raw_motif_results.txt"))

        do.motif.plots(motif.dat, comm.dat, stats, networks.dir)

        ## Finally, statistically analyse the quality of the results
        do.quality.analysis(conf, vcs.dat, defect.filename, start.date, end.date,
                            motif.type, motif.dat, stats, range.resdir)
    }
}

######################### Dispatcher ###################################
config.script.run({
    conf <- config.from.args(positional.args=list("project_resdir", "range_resdir",
                                                  "start_date", "end_date"),
                             require.project=TRUE)
    global.resdir <- conf$project_resdir
    range.resdir <- conf$range_resdir
    start.date <- conf$start_date
    end.date <- conf$end_date
    titandir <- file.path(range.resdir, "titan")

    logdevinfo(paste("Directory for storing conway results is", range.resdir),
               logger="conway")
    dir.create(global.resdir, showWarnings=FALSE, recursive=TRUE)
    dir.create(range.resdir, showWarnings=FALSE, recursive=TRUE)

    if (conf$profile) {
        ## R cannot store line number profiling information before version 3.
        if (R.Version()$major >= 3) {
            Rprof(filename="ts.rprof", line.profiling=TRUE)
        } else {
            Rprof(filename="ts.rprof")
        }
    }

    do.conway.analysis(conf, global.resdir, range.resdir,
                       start.date, end.date, titandir)
})
