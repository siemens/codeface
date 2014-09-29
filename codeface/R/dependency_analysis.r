require(arules)
require(plyr)
require(parallel)
require(ggplot2)
require(reshape)

source("db.r")
source("config.r")

## Global Constants
cores <- 4
rule.support <- 2.5
entity.threshold <- 20

## Experiments
## Naviation
navigation <- 'navigation'
## Prevention
prevention <- 'prevention'
## Closure
closure <- 'closure'

## Define the list of experiments to perform
experiments <- c(navigation)#, prevention, closure)

query.dependency.sem <- function(con, project.id, type, limit) {
  ## Query for dependencies between entities edited by a common commit

  ## Type: Type of dependency of which 'File' or 'Function' are possible.
  ## Limit: Integer to specify the maximum number of files edited by
  ##        a single commit. Often one would want to eliminate commits
  ##        that touch a very large number of files because the nature
  ##        of them is unique (e.g., change licence information)

  query <- str_c("SELECT commit.id, entityName, entityType, impl ",
                 "FROM commit, commit_dependency ",
                 "WHERE commit.id = commit_dependency.commitId ",
                 "AND commit.projectId=", project.id, " ",
                 "AND commit_dependency.entityType=", sq(type), " ",
                 "AND commit.ChangedFiles <= ", limit, " ",
                 "ORDER BY commit.id ASC")
  dat <- dbGetQuery(con, query)

  return(dat)
}

query.dependency <- function(con, project.id, type, limit, start.date, end.date,
                             impl=FALSE) {
  ## Query for dependencies between entities edited by a common commit

  ## Type: Type of dependency of which 'File' or 'Function' are possible.
  ## Limit: Integer to specify the maximum number of files edited by
  ##        a single commit. Often one would want to eliminate commits
  ##        that touch a very large number of files because the nature
  ##        of them is unique (e.g., change licence information)
  select.str <- "SELECT author, commitDate, commit.id, file, entityId,
                 entityType, size "

  if (impl == TRUE) {
    select.str <- paste(select.str, ", impl ", sep="")
  }

  query <- str_c(select.str,
                 "FROM commit_dependency INNER JOIN commit ",
                 "ON commit.id = commit_dependency.commitId ",
                 "WHERE commit.projectId=", project.id, " ",
                 "AND commit_dependency.entityType=", sq(type), " ",
                 "AND commit.ChangedFiles <= ", limit, " ",
                 "AND commit.commitDate >=", sq(start.date), " ",
                 "AND commit.commitDate <", sq(end.date), " ",
                 "ORDER BY commit.id ASC")

  dat <- dbGetQuery(con, query)
  cols <- c('file', 'entityId')

  if(nrow(dat) > 0) {
    dat$entity <- apply(dat[,cols], 1, paste, collapse="/")
    dat$cycle <- paste(start.date, end.date, sep="-")
  }

  dat <- dat[, !names(dat) %in% cols]

  return(dat)
}


query.commit.date.range <- function(con, project.id) {
  query <- str_c("SELECT min(commitDate) ",
                 "FROM commit ",
                 "WHERE projectId=", project.id)
  start.date <- dbGetQuery(con, query)[[1]]

  query <- str_c("SELECT max(commitDate) ",
                 "FROM commit ",
                 "WHERE projectId=", project.id)
  end.date <- dbGetQuery(con, query)[[1]]

  res <- list(start.date=start.date, end.date=end.date)

  return(res)
}


aggregate.commit.dependencies <- function(depend.df) {
  if(nrow(depend.df) == 0) {
    depend.list <- list()
  } else {
    ## Handle duplicate rows
    ## under some cases we get duplicate dependencies for a single
    ## commit. This can happen when an entity with the same identifier
    ## is defined in two places in the same file.
    depend.df.no.dups <- ddply(depend.df, .(id, entity), function(r) sum(r['size']))

    depend.list <- split(depend.df.no.dups$entity, depend.df.no.dups$id)
  }

  return(depend.list)
}


generate.rules <- function(depend.list) {
  ## create transations object
  entity.trans <- as(depend.list, "transactions")

  ## Confidence=0.8, Support=0.02, rationale in
  ## "An Empirical Study on the Developers Perception of Software Coupling"
  ## Bavota et al.
  support.norm <- min(1,rule.support/length(depend.list))
  rules <- apriori(entity.trans,
                   parameter=list(sup=support.norm, conf=0.5, minlen=2,
                                  target="rules"),
                   control=list(verbose=FALSE))

  ## Prune redundant rules
  #sorted.rules <- sort(rules, by='lift')
  #if(length(rules) > 0) {
  #subset.matrix <- is.subset(sorted.rules, sorted.rules)
  #subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
  #redundant <- colSums(subset.matrix, na.rm=T) >= 1
  #rules.pruned <- sorted.rules[!redundant]
  #}

  return(rules)
}


check.rule <- function(rules, query, expected) {
  ## Check the consequents for a query on the rule set

  ## A rule matches a query when the item set composing the
  ## query is equal to the antecedent of the rule. The prediction
  ## for a query is then the union of the consequents of all matching
  ## rules.

  ## Check if query items are in the training table
  items.present <- query %in% labels(rules@lhs)$items

  if(!is.null(query) && length(rules) != 0 && all(items.present)) {
    #rules.subset <- subset(rules, subset = lhs %in% query)
    match.lhs <- sapply(as(rules@lhs, 'list'), function(lhs) setequal(lhs, query))
    consequent <- unlist(as(rules@rhs, 'list')[match.lhs])

    #rules.subset <- subset(rules, subset=match.lhs)

    #consequent <- unlist(match.rhs) #unlist(as(rules.subset@rhs, 'list'))

    intersect <- intersect(expected, consequent)

    diff <- setdiff(consequent, expected)

    ## Recall
    exp.card <- length(expected)
    if (exp.card == 0) {
      recall <- 1
    } else {
      recall <- length(intersect) / exp.card
    }

    ## Precision
    consequent.card <- length(consequent)
    if (consequent.card == 0) {
      ## The case where the prediction is an empty set
      ## distorts the further analysis, thus we throw
      ## this case out completely
      precision <- NA
    } else{
      precision <- length(intersect) / consequent.card
    }

    antecedent_str <- paste(query, collapse=' ')
    consequent_str <- paste(consequent, collapse=' ')
    expected_str <- paste(expected, collapse=' ')
  } else {
    ## Query contains items that have never been seen before,
    ## therefore we cannot assign reasonable values
    precision <- NA
    recall <- NA
    consequent_str <- NA
    antecedent_str <- NA
    expected_str <- NA
  }

  res <- list(precision=precision, recall=recall, antecedent=antecedent_str,
              consequent=consequent_str, expected=expected_str)

  return(res)
}


generate.query <- function(transaction) {
  ## Navigation
  ## Generate one query per transaction item where the query is one item and
  ## the expected value is all remaining items
  nav.query <- lapply(transaction, function(item) list(query=item, exp=c(setdiff(transaction,item))))

  ## Prevention
  ## Generate one query for each transation item where the query is the transation minus one item
  ## one outcome per query where the expected values is the one removed
  prev.query <- lapply(transaction, function(item) {
                                      query <- setdiff(transaction,item)
                                      exp <- setdiff(transaction, query)
                                      if(length(query) == 0){
                                        NA
                                      } else {
                                        list(query=query, exp=exp)
                                      }
                                    })
  prev.query <- prev.query[!is.na(prev.query)]

  ## Closure
  ## Generate one query, query is the entire transaction, expected value is the
  ## empty set
  clos.query <- list(list(query=transaction, exp=list()))

  res <- list(nav.query, prev.query, clos.query)
  names(res) <- c(navigation, prevention, closure)

  return(res)
}


generate.all.queries <- function(all.transactions) {
  ## computes the queries over all transactions
  res <- lapply(all.transactions, function(t) generate.query(t))
  return(res)
}

compute.precision.recall <- function(rules, all.queries, type){
  ## get all navigation queries
  query.list <- sapply(all.queries, function(q) q[type])
  query.list <- unlist(query.list, recursive=FALSE)
  ## compute precisions and recall
  prec.rec <- mclapply(query.list, mc.cores=cores,
                       function(x) check.rule(rules, unlist(x$query), x$exp))

  return(prec.rec)
}

perform.analysis <- function(project.id, start.date, training.span, evaluation.span, con) {
  ## compute timelines
  training.start.date <- start.date
  training.end.date <- training.start.date + training.span
  evaluation.start.date <- training.end.date
  evaluation.end.date <- evaluation.start.date + evaluation.span
  evaluation.period <- paste(evaluation.start.date, evaluation.end.date, sep="-")

  ## Query database for dependency training data and limit maximium number
  ## of files edited by one commit to less than or equal to 30
  commit.file.edit.limit <- 30
  depend.training.df <- query.dependency(con, project.id, 'Function', commit.file.edit.limit,
                                         training.start.date, training.end.date)

  ## Query database for dependency evaluation date
  depend.evaluation.df <- query.dependency(con, project.id, 'Function', commit.file.edit.limit,
                                           evaluation.start.date, evaluation.end.date)
  #depend.evaluation.df <- depend.evaluation.df[sample(nrow(depend.evaluation.df),
  #                                             min(nrow(depend.evaluation.df),50)), ]

  ## Trim the training set to relavent transactions
  depend.training.trimmed.df <- remove.extraneous.commits(depend.training.df,
                                                          depend.evaluation.df)

  ## Compute transations
  depend.training.list <- aggregate.commit.dependencies(depend.training.trimmed.df)
  depend.evaluation.list <- aggregate.commit.dependencies(depend.evaluation.df)
  ## Remove large commits
  depend.training.list <- remove.large.commits(depend.training.list, entity.threshold)
  depend.evaluation.list <- remove.large.commits(depend.evaluation.list, entity.threshold)

  ## Check preconditions
  ## The training data and evaluation data must satisfy a mimumum set
  ## of requirements for it to be sensible to continue the analysis
  proceed <- check.conditions(depend.training.list, depend.evaluation.list)

  res.df <- data.frame()

  ## Check that both evaluation and training data is not empty
  if(proceed) {
    ## Compute association rules
    rules <- generate.rules(depend.training.list)

    ## Generate the evaluation queries and expected values
    queries <- generate.all.queries(depend.evaluation.list)

    save(rules, file='/home/mitchell/workspace/depend/temp_rules.dat')
    ## Compute precision and recall for different experiments
    res <- lapply(experiments, function(ex) compute.precision.recall(rules, queries, ex))
    names(res) <- experiments
    save(res,file='/home/mitchell/workspace/depend/temp_recall_precision.dat')

    ## Add results to dataframe
    for (ex in experiments) {
      recall <- sapply(unlist(res[ex], recursive=FALSE, use.names=FALSE),
                       function(i) i[['recall']])
      precision <- sapply(unlist(res[ex], recursive=FALSE, use.names=FALSE),
                          function(i) i[['precision']])
      antecedent <- sapply(unlist(res[ex], recursive=FALSE, use.names=FALSE),
                          function(i) i[['antecedent']])
      consequent <- sapply(unlist(res[ex], recursive=FALSE, use.names=FALSE),
                           function(i) i[['consequent']])
      expected <- sapply(unlist(res[ex], recursive=FALSE, use.names=FALSE),
                         function(i) i[['expected']])
      ex.df <- data.frame(experiment=ex, evaluation.period=evaluation.period,
                          recall=recall, precision=precision, antecedent=antecedent,
                          consequent=consequent, expected=expected)
      res.df <- rbind(res.df, ex.df)
    }
  }

  return(res.df)
}


compute.sliding.window <- function(start.date, end.date, step.size,
                                   window.size, pre.window.size=0) {
  ## Calculates the time intervals for a sliding window

  ## start.date: beginning of first window
  ## end.date: ending of final window
  ## window.size: width of the sliding window in days
  ## step.size: step size for each increment of the sliding the window
  ## pre.window.size: window before main window, useful for when a training
  ##                  set is used
  step.size.days = ddays(step.size) #days
  window.size.days = ddays(window.size) #days
  pre.window.size = ddays(pre.window.size)

  latest.possible.start.date <- end.date - window.size.days - pre.window.size
  n.inter <- floor((latest.possible.start.date - start.date) / step.size.days)
  intervals <- lapply(0:n.inter, function(x) {
                 next.date <- start.date + (x*step.size.days)
                 round(next.date, "day")})

  sliding.window <- ldply(intervals, function(start.date) {
                      data.frame(start.date=start.date,
                                 end.date=start.date + window.size.days)})

  return(sliding.window)
}


run.analysis <- function(project.id) {

  if(!exists('dbcon')) {
    con <- connect.db("../../codeface.conf")$con
    dbcon <- TRUE
  }
  ## Time constants
  ## TODO: check how long of a span is needed, using the
  ## remove.extraneous.commits function to see the span
  ## where the training commits overlap with evaluation
  ## commits, if the overlap is eliminated after a few
  ## months then there will be no possibility of generating
  ## useful rules for the additional commits. Make a formal
  ## analysis of the potential for useful rules using the
  ## overlap principle.
  training.span <- 365 # days
  evaluation.span <- 1 #days
  window.delta <- 1 #days

  ## Query commit date range
  date <- query.commit.date.range(con, project.id)
  start.date <- ymd_hms(date$start.date)
  start.date <- round_date(start.date, 'day')
  end.date <- ymd_hms(date$end.date)
  end.date <- round_date(end.date, 'day')

  ## Calculate time intervals
  intervals <- compute.sliding.window(start.date, end.date, window.delta,
                                      evaluation.span, training.span)$start.date

  ## Perform recall and precision computation for all intervals
  res.list <- lapply_pb(intervals, function(start.date) {
                perform.analysis(project.id, start.date,
                                 ddays(training.span),
                                 ddays(evaluation.span),
                                 con)})

  ## combine list of data.frame
  res.df <- do.call(rbind, res.list)

  return(res.df)
}


lapply_pb <- function(X, FUN, ...) {
  env <- environment()
  pb_Total <- length(X)
  counter <- 0
  pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)

 # wrapper around FUN
 wrapper <- function(...){
    curVal <- get("counter", envir = env)
    assign("counter", curVal +1 ,envir=env)
    setTxtProgressBar(get("pb", envir=env), curVal +1)
    FUN(...)
  }
  res <- lapply(X, wrapper, ...)
  close(pb)
  res
}


check.conditions <- function(training.data, evaluation.data) {
  ## The training set cannot be two small otherwise the support
  ## for the rules will be high even when it appears only once
  training.size.condition <- length(training.data) > 10

  ## There must be at least one evaluation
  evaluation.size.condition <- length(evaluation.data) > 0

  ## The evaluation cannot be too small otherwise the result
  ## will contain empty expectations or empty queries
  if(evaluation.size.condition) {
    dependency.count <- sapply(evaluation.data, length)
    min.dependency.condition <- max(dependency.count) > 1
  } else {
    min.dependency.condition <- FALSE
  }

  res <- all(training.size.condition, evaluation.size.condition,
             min.dependency.condition)

  return(res)
}


remove.large.commits <- function(commit.list, threshold) {
  res <- list()

  if(length(commit.list) != 0) {
    keep <- sapply(commit.list, function(c) length(c) <= threshold)
    res <- commit.list[keep]
  }

  return(res)
}


remove.extraneous.commits <- function(training.df, evaluation.df) {
  ## Remove commits that will not result in producing rules
  ## that contribute to preductions on the evaluation commits.
  ## By trimming the training set we reduce the compute time.
  if(nrow(evaluation.df) == 0) {
    training.trimmed.df <- data.frame()
  } else {
    evaluation.entity <- unique(evaluation.df[,'entity'])

    training.trimmed.df <- ddply(training.df, .(id),
                                 function(r) {
                                   keep <- any(r[,'entity'] %in% evaluation.entity)
                                   if(keep) res <- r
                                   else res <- data.frame()})
  }
  return(training.trimmed.df)
}


get.frequent.item.sets <- function(con, project.id, start.date, end.date) {
  ## Get commits for time period
  item.sets.list <- list()
  commit.file.edit.limit <- 30
  commit.depends.df <- query.dependency(con, project.id, 'Function', commit.file.edit.limit,
                                      start.date, end.date)
  ## Compute transactions
  commit.list <- aggregate.commit.dependencies(commit.depends.df)
  commit.list <- remove.large.commits(commit.list, entity.threshold)

  ## Mine frequent change sets
  if (length(commit.list) != 0) {
    trans.list <- as(commit.list, 'transactions')
    support.norm <- min(1,rule.support/length(trans.list))
    freq.change.sets <- apriori(trans.list,
                                parameter=list(sup=support.norm, minlen=2,
                                               target='frequent'),
                                control=list(verbose=FALSE))

    ## Coerce itemsets to list
    item.sets.list <- as(items(freq.change.sets), 'list')
  }

  return(item.sets.list)
}


compute.item.sets.edgelist <- function(item.sets) {
  combs.list <- lapply(item.sets, function(item.set) {
                                        combs <- t(combn(item.set,2))
                                        data.frame(combs)})
  edge.list <- do.call('rbind', combs.list)

  return(edge.list)
}


get.co.change.edgelist <- function(con, project.id, start.date, end.date) {
  freq.items <- get.frequent.item.sets(con, project.id, start.date,
                                       end.date)
  freq.items.edgelist <- compute.item.sets.edgelist(freq.items)

  return(freq.items.edgelist)
}


save.results <- function(pr.df, outdir) {

  ## Compute x-axis ticks for time series
  times <- unique(pr.df[,'evaluation.period'])
  step <- floor(length(times)/10)
  labels <- sapply(seq(1,length(times), step), function(x) times[[x]])

  ## Plot precision and recall values for all experiments
  reca.plot <- ggplot(aes(y=recall, x=evaluation.period, fill=experiment), data=res) +
                      geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=0.5)) +
                      ylab('Recall') +
                      xlab('Evaluation Period') +
                      scale_x_discrete(breaks=labels)

  prec.plot <- ggplot(aes(y=precision, x=evaluation.period, fill=experiment), data=res) +
                      geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=0.5)) +
                      ylab('Precision') +
                      xlab('Evaluation Period') +
                      scale_x_discrete(breaks=labels)

  ## Get macro evaluation for recal and precision
  ## avaerage precision, NA in the precision indicates
  ## the consequent was the empty set
  ## compute sum precision
  desc.stats <- ddply(pr.df, .(evaluation.period,experiment),
                     function(df) {
                       ## Remove rows with NA in recall and precision
                       keep.row <- !(is.na(df[,'recall']) & is.na(df[,'precision']))
                       df <- df[keep.row,]
                       experiment <- df[1,'experiment']
                       period <- df[1,'evaluation.period']
                       precision <- df[,'precision']
                       recall <- df[,'recall']
                       non.empty <- !is.na(precision)
                       sum.pre <- sum(precision[non.empty], na.rm=TRUE)
                       sum.rec <- sum(recall[non.empty], na.rm=TRUE)
                       z_nonempty <- sum(non.empty)
                       z_total <- nrow(df)
                       avg.pre <- sum.pre / z_nonempty
                       avg.rec <- sum.rec / z_nonempty
                       feedback <- z_nonempty / z_total
                       data.frame(evaluation.period=period,
                                  experiment=experiment,
                                  avg.precision=avg.pre,
                                  avg.recall=avg.rec,
                                  feedback=feedback)})


  desc.stats <- melt(desc.stats, id=c('evaluation.period','experiment'))

  desc.stats.plot <-  ggplot(desc.stats,
                             aes(y=value, x=evaluation.period, shape=experiment,
                                  color=variable)) +
                             geom_point() +
                             scale_x_discrete(breaks=labels) +
                             theme(axis.text.x=element_text(angle=0, hjust=0.5))

  ## Get percentage of unknown queries
  compute.percent.na <- function(df) {
                          pr <- is.na(df[,'precision'])
                          re <- is.na(df[,'recall'])
                          ## If both precision and recall are NA
                          ## then we know at least one item in the
                          ## query was never seen before
                          tot <- pr & re
                          c(percent=sum(tot) / nrow(df))}

  percent.na.df <- ddply(pr.df, ~evaluation.period, compute.percent.na)
  percent.na.plot <- ggplot(percent.na.df, aes(x=evaluation.period, y=percent)) +
                            geom_point(size=3, color='red') +
                            theme(axis.text.x=element_text(angle=45,hjust=0.5)) +
                            ylab('Percent Unseen Development') +
                            xlab('Evaluation Period') +
                            scale_x_discrete(breaks=labels)

   ## Save plots to files
   ggsave(filename=paste(outdir,precision.png, sep="/"), plot=prec.plot,
          width=11, height=8)
   ggsave(filename=paste(outdir, recall.png, sep="/"), plot=reca.plot,
          width=11, height=8)
   ggsave(filename=paste(outdir, unseen.png, sep="/"), plot=percent.na.plot,
          width=11, height=8)
   ggsave(filename=paste(outdir, means.png, sep="/"), plot=desc.stats.plot,
          width=20, height=8)
}

## Main ##
run <- function() {
  res <- run.analysis(3)
  save.results(res, outdir)
  non.na.rows <- !(is.na(res[,'precision']) | is.na(res[,'recall']))
  save(res[non.na.rows,],file='/home/mitchell/workspace/depend/rule_analysis.dat')
}
