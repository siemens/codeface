source("dependency_analysis.r")

test.remove.extraneous.commits <- function() {
  eval.df <- data.frame(entityName=c('f1', 'f2', 'f3', 'f2'),
                        id=c(1,2,3,4))
  train.df <- data.frame(entityName=c('f2', 'f5', 'f3', 'f7', 'f19','f10'),
                         id=c(5,6,7,8,9,5))

  trim.df <- remove.extraneous.commits(train.df, eval.df)

  id.test <- trim.df[,'id'] %in% c(5,7)
  entity.test <- trim.df[,'entityName'] %in% c('f3','f2','f10')
  pass <- all(id.test,entity.test)

  return(pass)
}


test.aggregate.commit.dependencies  <- function() {
  commit.df <- data.frame(entityName=c('f2', 'f5', 'f3', 'f7', 'f19','f10'),
                          id=c(5,6,6,8,7,5),
                          size=c(10,4,6,8,3,4))

  commit.ag <- aggregate.commit.dependencies(commit.df)

  expected.ag <- list('5'=c('f2', 'f10'), '6'=c('f5','f3'),
                      '7'=c('f10'), '8'=c('f7'))

  commit.id <- names(commit.ag)
  pass <- all(sapply(commit.id, 
              function(id) setequal(expected.ag$id,as.character(commit.ag$id))))

  return(pass)
}


test.generate.query <- function() {
  commit.df <- data.frame(entityName=c('f2', 'f5', 'f3', 'f7', 'f19','f10'),
                          id=c(1,1,1,2,2,2),
                          size=c(10,4,6,8,3,4))

  commit.list <- aggregate.commit.dependencies(commit.df)

  queries <- generate.all.queries(commit.list)
 
  commit.id <- names(commit.list)
  
  ## Thoughly check that the logic is correct for one example
  test.nav.q <- setequal(queries$'1'[navigation][[1]][[1]]$query, c('f2'))
  test.nav.e <- setequal(queries$'1'[navigation][[1]][[1]]$exp, c('f3','f5'))
  test.prev.q <- setequal(queries$'1'[prevention][[1]][[1]]$query, c('f3','f5'))
  test.prev.e <- setequal(queries$'1'[prevention][[1]][[1]]$exp, c('f2'))  
  test.clos.q <- setequal(queries$'1'[closure][[1]][[1]]$query, c('f2','f3','f5'))
  test.clos.e <-setequal(queries$'1'[closure][[1]][[1]]$exp, c())  

  ex.pass <- all(test.nav.q, test.nav.e, test.prev.q, test.prev.q, test.clos.q, test.clos.e)

  ## Sanity check that all configurations are being generated
  query.count <- sum(sapply(commit.list, function(x) 2*length(x) + 1))  
  sane.pass <- length(unlist(queries)) == query.count*3

  pass <- all(ex.pass, sane.pass)
  
  return(pass)
}
