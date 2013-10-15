# Tests for utils.r

# This file is part of Codeface. Codeface is free software: you can
# redistribute it and/or modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation, version 2.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# Copyright 2013 by Siemens AG, Johannes Ebke <johannes.ebke.ext@siemens.com>

library(testthat)
source("utils.r")

test_that("tstamp.to.POSIXct works as expected", {
  expect_that(tstamp.to.POSIXct(42), is_a("POSIXct"))
})


test_that("scale.data returns expected values", {
  expect_that(scale.data(c(-100, 100, 0)), equals(c(0,1,0.5)))
  expect_that(scale.data(c(42,42,42)), equals(c(1,1,1)))
  expect_that(scale.data(c(2)), equals(c(1)))
})

test_that("gen.weighted.edgelist works", {
  edges <- data.frame(fromId=c(1,1,2,2,3,4), toId=c(1,2,3,3,4,1))
  expect_that(gen.weighted.edgelist(edges),
              equals(data.frame(fromId=c(1,1,2,3,4),
                                toId=c(1,2,3,4,1),
                                weight=c(1,1,2,1,1)
                                )
              ))
})
