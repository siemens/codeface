# This file is part of prosoda.  prosoda is free software: you can
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

## Figures of merit should return a float value from 0 to 1

figure.of.merit.collaboration.warn <- 0.66
figure.of.merit.collaboration.bad <- 0.33
figure.of.merit.collaboration <- function(pid) {
  return(0.88)
}

figure.of.merit.communication.warn <- 0.66
figure.of.merit.communication.bad <- 0.33
figure.of.merit.communication <- function(pid) {
  return(0.88)
}

figure.of.merit.construction.warn <- 0.66
figure.of.merit.construction.bad <- 0.33
figure.of.merit.construction <- function(pid) {
  return(0.88)
}

figure.of.merit.complexity.warn <- 0.66
figure.of.merit.complexity.bad <- 0.33
figure.of.merit.complexity <- function(pid) {
  return(0.88)
}
