## Worksheet for visualising performance measurement data.
## Meant to be used interactively.
## Copyright Siemens AG 2013, Wolfgang Mauerer
##
## Copying and distribution of this file, with or without modification,
## are permitted in any medium without royalty provided the copyright
## notice and this notice are preserved.  This file is offered as-is,
## without any warranty.

library(plyr)
library(lubridate)
library(ggplot2)
library(reshape)
library(stringr)
library(grid)

## Project specification. The measured data are expected in results/,
## with filenames collectl_<project>_<cores>.txt and scale_<project>.txt.
projects.collectl.list <- c("qemu_16", "bootstrap_8", "git_16")
projects.collectl.names <- c("QEMU", "Bootstrap", "git")
projects.scale.list <- c("qemu", "bootstrap")
projects.scale.names <- c("QEMU", "Bootstrap")

####################################################################
## Nothing project-customisable below here
## Visual parameters
col.1 <- "#8DA3AF" # Stone
col.2 <- "#B1B0A1" # Sand
col.3 <- "#D5DFE4" # Stone light
col.4 <- "#E5E4DD" # Sand light
col.5 <- "#004F71" # Natural blue
col.6 <- "#718737" # Natural green
col.7 <- "#F08100" # Natural yellow dark
col.8 <- "#5D1435" # Natural red dark
col.9 <- "#F8AC00" # Natural yellow light
col.10 <- "#B40056" # Natural red light

bar.colours <- c(col.1, col.3, col.2, col.4, col.5, col.6)
line.colours <- c(col.2, col.8, col.1, col.3, col.9, col.10,
                  col.6, col.5, col.7, col.4)
base.font.size <- 10
line.size <- 0.7

## Nothing customisable below here
prepare.scale.dat <- function(dat) {
  colnames(dat) <- c("Cores", "User", "Sys", "Elapsed", "CPU")

  ## We need to take two different formats into acount
  times <- strptime(as.character(dat$Elapsed), format="%H:%M:%S")
  times[is.na(times)] <- strptime(as.character(dat$Elapsed[is.na(times)]),
                                  format="%M:%OS")
  times <- ymd_hms(as.character(times), quiet=TRUE)

  origin <- ymd_hms(str_c(year(times[1]), "-", month(times[1]), "-", mday(times[1]),
                          " 00:00:00", sep=""), quiet=TRUE)
  dat$seconds <- as.numeric(times) - as.numeric(origin)
  dat$minutes <- dat$seconds/60
  dat$speedup <- dat[dat$Cores==1,]$seconds/dat$seconds

  return(dat)
}

## Analyse performance data gathered by collectl respectively perf_meas.sh
prepare.collectl.dat <- function(dat, project) {
  colnames(dat) <- c("Date", "Time", "PID", "User", "PR", "PPID", "THRD",
                     "S", "VmSize", "VmLck", "VmRSS", "VmData", "VmStk", "VmExe",
                     "VmLib", "CPU", "SysT", "UsrT", "PCT", "AccumT", "RKB",
                     "WKB", "RKBC", "WKBC", "RSYS", "WSYS", "CNCL", "MajF",
                     "MinF", "Command")
  dat <- dat[,1:29] # Process names and arguments don't carry important information

  dat.tot <- ddply(dat, .(Time), summarise, CPU=sum(CPU), SysT=sum(SysT),
                   UsrT=sum(UsrT),
                   Mem=sum(VmRSS+VmData)/1000, RKB=sum(RKBC), WKB=sum(WKBC),
                   IO=sum(RKBC+WKBC)/1000)
  dat.tot$Time <- strptime(as.character(dat.tot$Time), format="%H:%M:%S")
  dat.tot$seconds <- as.numeric(dat.tot$Time - dat.tot$Time[1])

  return(dat.tot)
}

#############################################################################
## Analyse collectl results
dat <- do.call(rbind, lapply(seq_along(projects.collectl.list), function(i) {
  dat.prep <- prepare.collectl.dat(read.csv(str_c("results/collectl_",
                                                  projects.collectl.list[[i]],
                                                  ".txt", sep=""),
                                         sep="\t", comment.char="#", header=FALSE))
  dat.prep$project <- projects.collectl.names[[i]]

  return(dat.prep)
}))

#ggplot(dat, aes(x=seconds, y=Mem)) + geom_line() + ylab("MiB") +
#  xlab("Zeit [s]")

#ggplot(dat, aes(x=seconds, y=IO)) + geom_line() + ylab("MiB/s") +
#  xlab("Zeit [s]") + geom_smooth()

dat.molten <- melt(dat[,c("seconds", "project", "Mem", "IO")],
                   id.vars=c("seconds", "project"))
g <- ggplot(dat.molten, aes(x=seconds, y=value, colour=project)) +
  facet_grid(variable~., scales="free_y") + geom_line(size=line.size) +
  xlab("Zeit [s]") + ylab("MiB/s bzw. MiB") +
  scale_colour_manual(values=line.colours, name="") +
  theme_bw(base.font.size) + theme(legend.position="top") +
  theme(plot.margin=unit(c(0,0,0,0), "cm"))
ggsave("resources.pdf", g, height=4.5, width=5)


#############################################################################
## Analyse scalability results
dat <- do.call(rbind, lapply(seq_along(projects.scale.list), function(i) {
  dat.prep <- prepare.scale.dat(read.csv(str_c("results/scale_",
                                               projects.scale.list[[i]],
                                               ".txt", sep=""),
                                         sep=" ", header=FALSE))
  dat.prep$project <- projects.scale.names[[i]]

  return(dat.prep)
}))

g <- ggplot(dat, aes(x=Cores, y=minutes, colour=project)) + geom_line() +
  geom_point() + xlab("Cores") + ylab("Minuten") + theme_bw(base.font.size) +
  scale_colour_manual(values=line.colours, name="") +
  theme_bw(base.font.size) + theme(legend.position="top") +
  theme(plot.margin=unit(c(0,0,0,0), "cm"))
ggsave("duration.pdf", g, height=3.5, width=5)

g <- ggplot(dat, aes(x=Cores, y=speedup, colour=project)) + geom_line() +
  geom_point() + xlab("Cores") + ylab("Speedup") + theme_bw(base.font.size) +
  scale_colour_manual(values=line.colours, name="") +
  theme_bw(base.font.size) + theme(legend.position="top") +
  theme(plot.margin=unit(c(0,0,0,0), "cm"))
ggsave("speedup.pdf", g, height=3.5, width=5)
