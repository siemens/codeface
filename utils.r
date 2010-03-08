# Some utility functions that can be loaded with source("utility.r"

tstamp_to_date <- function(z) as.POSIXct(as.integer(z), origin="1970-01-01")

shannon.entropy <- function(p)
{
	if (min(p) < 0 || sum(p) <= 0)
		return(NA)
	p.norm <- p[p>0]/sum(p)
	-sum(log2(p.norm)*p.norm)
}

to.regts <- function(rawts, smooth)
{
ts <- as.xts(rollmean(rawts, smooth))
ts_reduced <- as.xts(to.period(ts, "hours")[,1])
# Average difference in seconds between two data points
tstart <- unclass(index(ts_reduced[1]))
tend <- unclass(index(ts_reduced[length(ts_reduced)]))
tdiff <- floor((tend-tstart)/length(ts_reduced))
ts(data=coredata(ts_reduced), start=tstart, deltat=tdiff)
}