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
source("semantic_dependency.r")
source("process_dsm.r")
source("process_jira.r")
source("quality_analysis.r")
source("conway_data.r")
source("ml/ml_utils.r", chdir=T)
source("id_manager.r")
source("st_relations.r")

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
                 " Art: ", stats$num.artefacts,
                 " M: ", stats$num.motifs,
                 " A-M: ", stats$num.motifs.anti, sep=""))
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

## Conway analysis first combines social and technical data derived in earlier steps,
## and then performs some statistical and visual analyses
## i is the release range under consideration
do.conway.analysis <- function(conf, global.resdir, i) {
    project.name <- conf$project
    project.id <- conf$pid

    ## TODO: How can this file be constructed?
    feature.call.filename <- file.path(global.resdir,
                                       "feature-dependencies/cg_nw_f_1_18_0.net")
    jira.filename <- file.path(global.resdir, "jira_issue_comments.csv")

    cycles <- get.cycles(conf)
    cycle <- cycles[i,]
    range.resdir <- file.path(global.resdir, gen.range.path(i, cycle$cycle))
    logdevinfo(paste("Directory for storing conway results is", range.resdir),
               logger="conway")
    dir.create(range.resdir, showWarnings=FALSE, recursive=TRUE)

    start.date <- cycle$date.start
    end.date <- cycle$date.end

    ## The configuration object contains several variables that describe
    ## the "flavour" of the Conway analysis:
    ## artifactType: Which artefact to consider (function, file, feature)
    ## dependencyType: Mechanism to generate dependencies between files or functions
    ##                 (co-change, dsm, feature_call, none)
    ## qualityType: corrective or defect
    ## communicationType: Data source to capture developer communication (mail, jira)
    types <- types.from.conf(conf)
    params <- get.conway.params()
    output.dir.graphs <- file.path(range.resdir,
                                   gen.conway.path(types, omit.motif=TRUE))
    dir.create(output.dir.graphs, showWarnings=FALSE, recursive=TRUE)

    ## Compute developer-artifact relationships that are obtained
    ## from the revision control system
    vcs.dat <- query.dependency(conf, params$file.limit, start.date, end.date)
    if (is.null(vcs.dat) || nrow(vcs.dat) == 0) {
        loginfo(str_c("Conway analysis: No usable VCS relationships available ",
                      "for time interval ", start.date, "--", end.date, ", exiting early",
                      sep=""), startlogger="conway")
        return()
    }
    vcs.dat$author <- as.character(vcs.dat$author)

    ## Determine which function/file artifacts (node.artifact) and developers
    ## (node.dev) contribute to the developer-artifact relationships
    nodes.artifact <- unique(vcs.dat$entity)
    nodes.dev <- unique(c(vcs.dat$author))

    ## Determine communication relations between contributors
    comm.dat <- compute.communication.relations(conf, jira.filename,
                                                start.date, end.date)

    if (is.null(comm.dat)) {
        loginfo(str_c("Conway analysis: No usable communication relationships available ",
                      "for time interval ", start.date, "--", end.date, ", exiting early",
                      sep=""), startlogger="conway")
        return()
    }

    ## Determine connections between entities (aka artefacts)
    dependency.dat <- compute.ee.relations(conf, vcs.dat, start.date, end.date,
                                           range.resdir, params, types)

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
                             attr=list(name=nodes.artifact, kind=types$artifact,
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
        return()
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
    vertex.coding[types$artifact] <- 2
    V(g)$color <- vertex.coding[V(g)$kind]

    ## * Last, save the resulting graph for external processing
    ## (TODO: This should go into the database)
    ## The graph has the following node attributes:
    ## * name (person ID (integer) or artifact name (string))
    ## * kind (string: developer, function, or file)
    ## * type (boolean: true for person, false for artifact)
    ## * color (colour as hex value)
    write.graph(g, file.path(output.dir.graphs, "network_data.graphml"), format="graphml")

    ## Plot the complete network
    plot.to.file(g, file.path(output.dir.graphs, "socio_technical_network.pdf"))

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

        types$motif <- motif.type
        output.dir <- file.path(range.resdir, gen.conway.path(types))
        dir.create(output.dir, recursive=TRUE, showWarnings=TRUE)

        motif.dat <- search.motifs(graphs, motif.type, params, dependency.dat,
                                   dependency.edgelist, comm.inter.dat,
                                   types$artifact, vertex.coding)

        logdevinfo("Computing null model stats", logger="conway")
        stats <- list(num.devs=length(nodes.dev), num.artefacts=length(nodes.artifact),
                      num.motifs=length(motif.dat$motif.subgraphs),
                      num.motifs.anti=length(motif.dat$motif.anti.subgraphs),
                      start.date=start.date, end.date=end.date, project=conf$project)

        ## Visualise the results in some graphs
        logdevinfo("Writing null model data", logger="conway")

        ## The file contains the following columns:
        ## * count.type: string (positive, negative, ratio)
        ## * count: integer (the count or ratio obtained from the
        ##                   various rewired grapsh)
        ## * empirical.count: integer (count or ratio for the observed data)
        write.table(motif.dat$null.model.dat,
                    file=file.path(output.dir, "raw_motif_results.txt"))


        ## Combine the previously computed data sets if motifs and anti-motifs
        ## were detected in the network
        if (length(motif.dat$motif.subgraphs) == 0 ||
            length(motif.dat$motif.anti.subgraphs) == 0) {
            loginfo(str_c("No ", motif.type, " motifs or anti-motifs found for cycle ", i,
                          ", exiting early"))
            return()
        }

        ## Prepare a list of motif and anti-motif counts for each artefact
        ## and save the result for later use
        ## The file contains the following columns:
        ## * entity: Name of the artefact
        ## * motif.count: (empirical) number of motifs
        ## * motif.anti.count: (empirical) number of anti-motifs
        artifacts <- count(data.frame(entity=unlist(lapply(motif.dat$motif.subgraphs,
                                          function(i) i[[3]]$name))))
        anti.artifacts <- count(data.frame(entity=unlist(lapply(motif.dat$motif.anti.subgraphs,
                                               function(i) i[[3]]$name))))
        compare.motifs <- merge(artifacts, anti.artifacts, by='entity', all=TRUE)
        compare.motifs[is.na(compare.motifs)] <- 0
        names(compare.motifs) <- c("entity", "motif.count", "motif.anti.count")

        do.motif.plots(motif.dat, comm.dat, stats, output.dir)

        write.table(compare.motifs,
                    file=file.path(output.dir, "entity_motifs.txt"))
        merge.conway.data(conf, cycles, i, types, global.resdir)
    }
}

######################### Dispatcher ###################################
config.script.run({
    conf <- config.from.args(positional.args=list("project_resdir", "release_range"),
                             require.project=TRUE)
    global.resdir <- conf$project_resdir
    i <- conf$release_range

    dir.create(global.resdir, showWarnings=FALSE, recursive=TRUE)

    if (conf$profile) {
        ## R cannot store line number profiling information before version 3.
        if (R.Version()$major >= 3) {
            Rprof(filename="ts.rprof", line.profiling=TRUE)
        } else {
            Rprof(filename="ts.rprof")
        }
    }

    do.conway.analysis(conf, global.resdir, i)
})
