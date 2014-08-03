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

## Project-specific functions for pre- and postprocessing the corpora.
## TODO: Make this systematic, for instance via filter classes
linux.kernel.preprocess <- function(corp) {
  # Remove anything sent by a tip bot
  authors <- sapply(corp, meta, tag='author')
  tip.bot <- grep("tip-bot for ", authors, fixed=TRUE, useBytes=TRUE)
  if (length(tip.bot) > 0)
      corp <- corp[-tip.bot]

  # Remove all git pull requests (don't remove any follow-up messages,
  # they likely contain discussions).
  headings <- tolower(sapply(corp, meta, tag='heading'))
  pull.req <- grep("^\\[git pull", headings)
  if (length(pull.req) > 0)
    corp <- corp[-pull.req]

  return(corp)
}

## Entries to remove from the keyword lists
## ML specific terms
terms.clang <- c("use", "defin")
terms.d <- c("jonathan", "alex", "davi", "steve")
