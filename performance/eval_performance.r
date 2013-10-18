## Worksheet for visualising performance measurement data.
## Meant to be used interactively.
## Copyright Siemens AG 2013, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
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

## Possible choices for language: de and en_GB
lang <- "de"

## Which analysis pass has been analysed (vcs or ml)
type <- "vcs"

## Project specification. The measured data are expected in results/<type>,
## with filenames collectl_<project>_<cores>.txt and scale_<project>.txt.
## We can compute various graphs of interest:

## Memory and I/O time series. This plots one curve per project and
## a fixed amount of cores per project. Typically, you should pick
## a number of CPUs that provides good speedup for the project size.
projects.collectl.list <- c("qemu_15", "bootstrap_8", "git_16")
projects.collectl.names <- c("QEMU", "Bootstrap", "git")

## Memory scaling behaviour. This compares different projects
## analysed with varying core numbers, where the range of cores need
## not be identical for every project.
projects.collectl.scale.list <- c("linux", "qemu", "bootstrap", "git")
projects.collectl.scale.cores <- list(1:16, 1:15, 1:8, 1:16)
projects.collectl.scale.names <- c("Linux kernel", "QEMU", "Bootstrap", "git")

## Time scaling behaviour. This reads the input data from scale_<project>.txt,
## the range of cores used is implicit in the file and does not need
## to be specified manually.
projects.scale.list <- c("linux", "qemu", "bootstrap", "git")
projects.scale.names <- c("Linux kernel", "QEMU", "Bootstrap", "git")

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

## Set up i10ned text strings for axis labels
if (lang == "de") {
  label.time.s <- "Zeit [s]"
  label.mibs <- "MiB/s bzw. MiB"
  label.cores <- "Cores"
  label.ram <- "RAM [MiB]"
  label.min.log <- "Minuten [log. Skala]"
  label.min <- "Minuten"
  label.speedup <- "Speedup"
} else {
  ## If an unknown language is given, default to en.
  label.time.s <- "Time [s]"
  label.mibs <- "MiB/s resp. MiB"
  label.cores <- "Cores"
  label.ram <- "RAM [MiB]"
  label.min.log <- "Minutes [log. scale]"
  label.min <- "Minutes"
  label.speedup <- "Speedup"
}

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
                   Mem=sum(VmRSS)/1000, RKB=sum(RKBC), WKB=sum(WKBC),
                   IO=sum(RKBC+WKBC)/1000)
  dat.tot$Time <- strptime(as.character(dat.tot$Time), format="%H:%M:%S")
  dat.tot$seconds <- as.numeric(dat.tot$Time - dat.tot$Time[1])

  return(dat.tot)
}

#############################################################################
## Analyse collectl results
dat.collectl <- do.call(rbind, lapply(seq_along(projects.collectl.list), function(i) {
  dat.prep <- prepare.collectl.dat(read.csv(str_c("results/", type,
                                                  "/collectl_",
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

dat.collectl.molten <- melt(dat.collectl[,c("seconds", "project", "Mem", "IO")],
                   id.vars=c("seconds", "project"))
g <- ggplot(dat.collectl.molten, aes(x=seconds, y=value, colour=project)) +
  facet_grid(variable~., scales="free_y") + geom_line(size=line.size) +
  xlab(label.time.s) + ylab(label.mibs) +
  scale_colour_manual(values=line.colours, name="") +
  theme_bw(base.font.size) + theme(legend.position="top") +
  theme(plot.margin=unit(c(0,0,0,0), "cm"))
ggsave(file.path("graphs", type, "resources.pdf"), g, height=4.5, width=5)

## Memory scaling behaviour
dat.scale.mem <- do.call(rbind,
                         lapply(seq_along(projects.collectl.scale.list), function(i) {
  dat.cores <- lapply(projects.collectl.scale.cores[[i]], function(cores) {
    dat.prep <- prepare.collectl.dat(read.csv(str_c("results/", type,
                                                    "/collectl_",
                                                    projects.collectl.scale.list[[i]],
                                                    "_", cores, ".txt", sep=""),
                                     sep="\t", comment.char="#", header=FALSE))

    return(data.frame(cores=cores, Mem=max(dat.prep$Mem)))
  })

  dat.cores <- do.call(rbind, dat.cores)
  dat.cores$project <- projects.collectl.scale.names[[i]]

  return(dat.cores)
}))
dat.scale.mem$cores <- as.factor(dat.scale.mem$cores)


g <- ggplot(dat.scale.mem, aes(x=cores, y=Mem, colour=project, group=project)) +
  geom_line(size=line.size) +
  geom_point() + xlab(label.cores) + ylab(label.ram) +
  scale_colour_manual(values=line.colours, name="") +
  theme_bw(base.font.size) + theme(legend.position="top") +
  theme(plot.margin=unit(c(0,0,0,0), "cm"))
ggsave(file.path("graphs", type, "scale_mem.pdf"), g, height=3.5, width=5)

#############################################################################
## Analyse scalability (execution time) results
dat.scale.time <- do.call(rbind, lapply(seq_along(projects.scale.list), function(i) {
  dat.prep <- prepare.scale.dat(read.csv(str_c("results/", type, "/scale_",
                                               projects.scale.list[[i]],
                                               ".txt", sep=""),
                                         sep=" ", header=FALSE))
  dat.prep$project <- projects.scale.names[[i]]

  return(dat.prep)
}))
dat.scale.time$Cores <- as.factor(dat.scale.time$Cores)

g <- ggplot(dat.scale.time, aes(x=Cores, y=minutes, colour=project, group=project)) +
  geom_line() + geom_point() + xlab(label.cores) + ylab(label.min) +
  theme_bw(base.font.size) +
  scale_colour_manual(values=line.colours, name="") +
  theme_bw(base.font.size) + theme(legend.position="top") +
  theme(plot.margin=unit(c(0,0,0,0), "cm"))
ggsave(file.path("graphs", type, "duration.pdf"), g, height=3.5, width=5)

g <- g + scale_y_log10() + ylab(label.min.log)
ggsave(file.path("graphs", type, "duration_log.pdf"), g, height=3.5, width=5)

g <- ggplot(dat.scale.time, aes(x=Cores, y=speedup, colour=project, group=project)) +
  geom_line() + geom_point() + xlab(label.cores) + ylab(label.speedup) +
  theme_bw(base.font.size) +
  scale_colour_manual(values=line.colours, name="") +
  theme_bw(base.font.size) + theme(legend.position="top") +
  theme(plot.margin=unit(c(0,0,0,0), "cm"))
ggsave(file.path("graphs", type, "speedup.pdf"), g, height=3.5, width=5)
