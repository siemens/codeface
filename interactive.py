# Commands that are useful after adist.yp has been
# run in ipython

initialiseR()
git = shelve.open("/home/wolfgang/linux-14-33")["git"]
res = createSeries(git, "__main__", ["v2.6.24", "v2.6.25"])
writeToFile(res, "/home/wolfgang/raw.dat")
runR('raw = as.xts(read.zoo(file="/home/wolfgang/raw.dat", FUN=tstamp_to_date))')
runR('reg = to.regts(raw[,1], 250)')
reg = RtoPython(runR('reg'))
raw = RtoPython(runR('raw'))

# ... and then commence with the analysis as desired
