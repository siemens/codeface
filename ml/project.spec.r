# Project-specific functions for pre- and postprocessing the corpora.
# TODO: Make this systematic, for instance via filter classes
linux.kernel.preprocess <- function(corp) {
  # Remove anything sent by a tip bot
  authors <- sapply(corp, Author)
  tip.bot <- grep("tip-bot for ", authors, fixed=T, useBytes=T)
  if (length(tip.bot) > 0)
      corp <- corp[-tip.bot]
  
  # Remove all git pull requests (don't remove any follow-up messages,
  # they likely contain discussions).
  headings <- tolower(sapply(corp, Heading))
  pull.req <- grep("^\\[git pull", headings)
  if (length(pull.req) > 0)
    corp <- corp[-pull.req]
  
  return(corp)
}

# Entries to remove from the keyword lists
# ML specific terms
terms.clang <- c("use", "defin")
terms.d <- c("jonathan", "alex", "davi", "steve")
