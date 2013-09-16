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

figure.of.merit.collaboration.warn <- 0.75
figure.of.merit.collaboration.bad <- 0.5
figure.of.merit.collaboration <- function(pid) {
  n.commits <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM commit WHERE projectId=", pid))[[1]]
  n.persons <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM person WHERE projectId=", pid))[[1]]
  #n.issues <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM issue WHERE projectId=", pid))[[1]]
  min(n.commits/1000., n.persons/20.)
}

figure.of.merit.communication.warn <- 500
figure.of.merit.communication.bad <- 100
figure.of.merit.communication <- function(pid) {
  n.mail.threads <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM mail_thread WHERE projectId=", pid))[[1]]
}

figure.of.merit.construction.warn <- 1
figure.of.merit.construction.bad <- 0
figure.of.merit.construction <- function(pid) {
  n.tsplots <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM plots WHERE name LIKE 'Progress%' AND projectId=", pid))[[1]]
  n.tsplots
}

figure.of.merit.complexity.warn <- 5
figure.of.merit.complexity.bad <- 0
figure.of.merit.complexity <- function(pid) {
  n.understand.plots <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM plots WHERE name LIKE 'Understand%' AND projectId=", pid))[[1]]
}
