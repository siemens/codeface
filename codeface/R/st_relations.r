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
    } else if (communication.type=="mail+jira") {
        ## Combine both communication types
        comm.dat.mail <- query.mail.edgelist(conf$con, conf$pid, start.date, end.date)
        if (nrow(comm.dat.mail) == 0) {
            comm.dat.mail <- NULL
        } else {
            colnames(comm.dat.mail) <- c("V1", "V2", "weight")
        }
        comm.dat.jira <- load.jira.edgelist(conf, jira.filename, start.date, end.date)

        if (is.null(comm.dat.mail) || is.null(comm.dat.jira)) {
            return(NULL)
        }

        comm.dat <- merge(comm.dat.mail, comm.dat.jira, by=c("V1", "V2"))
        comm.dat$weight <- comm.dat$weight.x + comm.dat$weight.y
        comm.dat <- comm.dat[,c("V1", "V2", "weight")]
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
                                 range.resdir, params, types) {
    ## Compute a list of relevant files (i.e., all files touched in the release
    ## range)
    relevant.entity.list <- unique(vcs.dat$entity)

    ## Then, compute the the actual dependencies with the chosen mechanism
    if (types$dependency == "co-change") {
        start.date.hist <- as.Date(start.date) - params$historical.limit
        end.date.hist <- start.date

        commit.df.hist <- query.dependency(conf, params$file.limit,
                                           start.date.hist, end.date.hist)

        commit.df.hist <- commit.df.hist[commit.df.hist$entity %in%
                                         relevant.entity.list,]

        ## Compute co-change relationships, and transfer the results
        ## into an edgelist that connects co-changing artifacts
        freq.item.sets <- compute.frequent.items(commit.df.hist)
        dependency.dat <- compute.item.sets.edgelist(freq.item.sets)

        if (nrow(dependency.dat) == 0) {
            logwarning("Could not find any co-change dependencies",
                       logger="conway")
            return(data.frame())
        }
        names(dependency.dat) <- c("V1", "V2", "weight")
    } else if (types$dependency == "dsm") {
        dsm.filename <- file.path(range.resdir, "static_file_dependencies.csv")

        dependency.dat <- load.dsm(dsm.filename)
        if (is.null(dependency.dat)) {
            logwarning(str_c("Could not obtain any dependencies from the SDSM! ",
                             "Trying to continue without dependencies.\n",
                             "Is the implementation language supported?"), logger="conway")
            return(data.frame())
        }

        dependency.dat <-
            dependency.dat[dependency.dat[, 1] %in% relevant.entity.list &
                           dependency.dat[, 2] %in% relevant.entity.list,]
    } else if (types$dependency == "feature_call") {
        graph.dat <- read.graph(feature.call.filename, format="pajek")
        V(graph.dat)$name <- V(graph.dat)$id
        dependency.dat <- get.data.frame(graph.dat)
        dependency.dat <-
            dependency.dat[dependency.dat[, 1] %in% relevant.entity.list &
                           dependency.dat[, 2] %in% relevant.entity.list,]
        names(dependency.dat) <- c("V1", "V2")
    } else if (types$dependency == "semantic") {
        commit.df <- query.dependency(conf, params$file.limit, start.date, end.date,
                                      impl=TRUE)

        ## Compute the semantic coupling between entities in the commit.df
        semantic.rel <- computeSemanticCoupling(commit.df, threshold=0.7)

        ## Map vertex id to an entity name
        vertex.data <- semantic.rel$vertex.data
        V1 <- vertex.data[semantic.rel$edgelist$X1, "id"]
        V2 <- vertex.data[semantic.rel$edgelist$X2, "id"]
        dependency.dat <- data.frame(V1=V1, V2=V2, stringsAsFactors=F)
    } else {
        dependency.dat <- data.frame()
    }

    dependency.dat$V1 <- as.character(dependency.dat$V1)
    dependency.dat$V2 <- as.character(dependency.dat$V2)
    return(dependency.dat)
}
