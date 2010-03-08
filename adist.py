#! /usr/bin/env python

from VCS import gitVCS
from commit_analysis import createCumulativeSeries, createSeries, \
    writeToFile, getSeriesDuration
import rpy2.robjects as robjects
import matplotlib
matplotlib.use("pdf")
import matplotlib.pyplot as plt
import os.path
from pylab import *
import kerninfo
import shelve

def _abort(msg):
    print(msg + "\n")
    sys.exit(-1)

def status(msg):
    # Comment to disable status messages. Highly professional!
    print(msg)

def runR(cmd):
    return robjects.r(cmd)

def loadRpkg(pkg):
    robjects.r.library(pkg)

def RtoPython(array):
    """This is required since matplotlib can't handle R arrays natively."""
    return [array[i] for i in range(0,len(array))]

# TODO: We should use constants to denote the backend.
def _setupRGraphics(filename = None, backend = None):
    """Must be called before plotting commands are used in R.

    filename --- Where to store the plot.
    backend --- File format. Currently, only PDF is supported.
    """

    if backend == None:
        return;

    if filename:
        if backend.lower() == "pdf":
            runR('pdf(file="{0}.pdf")'.format(filename))
        elif backend.lower() == "png":
            runR('png(file="{0}.png")'.format(filename))
        else:
            _abort("Internal error: Backend {0} unsupported for R".
                   format(backend))

def _closeRGraphics(filename=None, backend=None):
    """Counterpart of _setupRGraphics() after plotting is finished."""

    if filename:
        runR('dev.off()')

def _setupPythonGraphics(filename=None, backend=None):
    """Must be called before plotting commands are used in Python.

    filename --- Where to store the plot.
    backend --- File format. Currently, only PDF is supported.
    """
    
    # NOTE: matplotlib does not seem to be able to dynamically switch
    # the output format (duh...), so this function remains empty -- we
    # have to do all the setup at startup

def _closePythonGraphics(filename=None, backend=None):
    """Counterpart of _setupPythonGraphics() after plotting is finished."""

    if filename:
        plt.savefig(filename)

def _backendToSuffix(backend=None):
    if backend == None:
        return

    if backend.lower() == "pdf":
        return ".pdf"
    elif backend.lower() == "png":
        return ".pdf"
    else:
        _abort("Unsupported backend {0}.".format(backend))
        

def _computeCorrelation(rdata, sequence, msg, filename=None, backend=None,
                        frameLabels = None):
    """
    Compute a correlation diagram (internal function, frontends available).

    rdata -- Name (string) of the R time series that contains the data.
    sequence -- Slice specification of the columns to compare, typically
                in the form "1:3" or "c(1,5,7)".
    msg -- Headline of the plot.
    filename -- Output file. If None, then plot to the screen.
    backend -- Which backend to use when plotting into a file.
    """
    
    _setupRGraphics(filename, backend)
    runR('corr = data.frame(coredata({0})[,{1}])'.format(rdata, sequence))
    if frameLabels:
        for i in range(0, len(frameLabels)):
            frameLabels[i] = '"' + frameLabels[i] + '"'
        runR('names(corr) <- {0}'.format("c(" + ", ".join(frameLabels) + ")"))

    runR('pairs(corr, panel=panel.smooth, ' \
             'main="{0}")'.format(msg))
    _closeRGraphics(filename, backend)

def computeDiffsizeCommitlengthCorrelation(rdata, difftype=1, filename=None,
                                           backend=None):
    """Compute correlation between commit size and commit description.

    rdata -- Name (string) of the R time series that contains the data.
    difftype -- Which diff type to use as basis (defaults to 1).
    filename -- Output file. If None, then plot to the screen.
    backend -- Which backend to use when plotting into a file."""

    # Column 5 contains the commit message length, 6 contains 
    # the number of signed-offs, 7 the number of all signatures 
    # (6 plus CC etc.)
    sequence = "c({0},5,6)".format(difftype)
    msg = ("Correlation: Diff method {0}, "
           "Msg length, # Signed-offs".format(difftype))
    _computeCorrelation(rdata, sequence, msg, filename, backend,
                        frameLabels = ["Diff Size", 
                                       "Commit description length", 
                                       "# Signed-offs"])

def computeDifftypeCorrelation(rdata, filename=None, backend=None):
    """Check if there is any corelation between commit sizes.

    We iterate over all tupled of different diff type methods.

    rdata -- Name (string) of the R time series that contains the data.
    filename -- Base string used to construct the output file. The tuple
                 for which the calculation is done is appended as _i_j,
                 followed by the suffix.
    backend -- Which backend to use when plotting into a file."""

    for i in range(1,5):
        for j in range(i,5):
            if i != j:
                sequence = "c({0},{1})".format(i,j)
                msg = ("Correlation between diff methods "
                       "{0} and {1}".format(i,j))

                if filename:
                    curr_file = filename
                    curr_file += "_{0}_{1}".format(i,j)
                    curr_file += _backendToSuffix(backend)

                _computeCorrelation(rdata, sequence, msg, curr_file, backend,
                                    frameLabels=["Diff method {0}".format(i), 
                                                 "Diff method {0}".format(j)])

def computeRecurrenceDiagram(rdata, m=3, d=1, filename=None, backend=None):
    """
    Compute the recurrence diagram for a time series.
    
    rdata -- Name (string) of the R time series that contains the data.
    m -- embedding dimension (default to 3)
    d -- time delay (defaults to 1)
    filename -- Output file. If None, then plot to the screen.
    backend -- Which backend to use when plotting into a file.
    """
    _setupRGraphics(filename, backend)
    runR('recurr({0}, m={1}, d={2})'.format(rdata, m, d))
    _closeRGraphics(filename, backend)

def computeDensity(rdata, bandwidth=10, 
                   filename=None, backend=None):
    """
    Compute the spectral density of a time series.
    
    rdata -- Name (string) of the R time series that contains the data.
    bandwidth -- Guess what.
    filename -- Output file. If None, then plot to the screen.
    backend -- Which backend to use when plotting into a file.
    """

    _setupRGraphics(filename, backend)
    runR('plot(density({0},bw={1}))'.format(rdata, bandwidth))
    _closeRGraphics(filename, backend)

def computeSpectrum(rdata, filename=None, backend=None):
    """
    Fit an AR model to the timeseries and compute the spectral density.
    
    rdata -- Name (string) of the R time series that contains the data.
    filename -- Output file. If None, then plot to the screen.
    backend -- Which backend to use when plotting into a file.
    """

    _setupRGraphics(filename, backend)
    runR('spec.ar({0})'.format(rdata))
    _closeRGraphics(filename, backend)

def computeLag(rdata, m=16, filename=None, backend=None):
    """Plot a grid of scatterplots of x(t-h) versus x(t) for h = 1,...,m."

    The autocorrelation value is given in the upper right border
    of the graph (in blue) and a lowess fit is added in red.
    
    rdata -- Name (string) of the R time series that contains the data.
    m -- Maximal lag.
    filename -- Output file. If None, then plot to the screen.
    backend -- Which backend to use when plotting into a file.
    """

    _setupRGraphics(filename, backend)
    runR('lag.plot1({0},{1},smooth=TRUE)'.format(rdata, m))
    _closeRGraphics(filename, backend)

def computeECDF(rdata, filename=None, backend=None):
    """Compute the empirical cumulative distribution function.

    rdata -- Name (string) of the R time series that contains the data.
    filename -- Output file. If None, then plot to the screen.
    backend -- Which backend to use when plotting into a file.
    """

    _setupRGraphics(filename, backend)
    runR('plot(ecdf({0}))'.format(rdata))
    _closeRGraphics(filename, backend)

def initialiseR():
    loadRpkg("zoo")
    loadRpkg("xts")
    loadRpkg("tseriesChaos")
    runR('source("utils.r")')
    runR('source("stoffer.r")')

def doAnalysis(vcs, basedir, revrange=None):
    # TODO: This needs to include the subrange analysis
    # TODO: Use a temporary dir for data storage (unless the R
    # data exchange problem is solved)
    print("Creating raw series")
    res = createSeries(vcs, "__main__", revrange)
    writeToFile(res, "/home/wolfgang/raw.dat")
    duration = getSeriesDuration(res)

    print("Creating cumulative series")
    res = createCumulativeSeries(vcs, "__main__", revrange)
    writeToFile(res, "/home/wolfgang/cum.dat")

    print("Obtained a list with {0} commits".format(len(res)))


    # TODO: How is it possible to exchange the data directly between python
    # and R? Writing to a file and then re-reading the stuff is a bit stupid
    # (if all else fails, we could at least use a named pipe)
    runR('raw = as.xts(read.zoo(file="/home/wolfgang/raw.dat", '\
             'FUN=tstamp_to_date))')
    raw = RtoPython(runR('raw'))

    # We use the average number of commits per quarter day as basis for the
    # moving average
    secs_per_hour = 60*60
    smooth_commits = len(raw)/(duration/(6*secs_per_hour))

    print("Length: {0}, duration: {1}".format(len(raw), duration))

    # ... but also ensure that we do not get excessively large or
    # small values
    
    if smooth_commits < 20:
        smooth_commits = 20
    elif smooth_commits > 350:
        smooth_commits = 350

    print("Using {0} as smoothing factor".format(smooth_commits))

    if (len(raw) < smooth_commits):
        print("Pathological case: Excessively short series with {} commits "
              "detected, giving up.".format(len(raw)))
        return

    runR('reg = to.regts(raw[,1], {0})'.format(smooth_commits))
    runR('cum = as.xts(read.zoo(file="/home/wolfgang/cum.dat", '\
             'FUN=tstamp_to_date))')

    reg = RtoPython(runR('reg'))
    cum = RtoPython(runR('cum'))

    # HARDCODED assumptions about the position of the data fields 
    # TODO: These should get symbolic R labels. How is this possible?
    diff_sizes = RtoPython(runR('coredata(raw)[,1]'))
    descr_sizes = RtoPython(runR('coredata(raw)[,5]'))

    deltat = int(runR('deltat(reg)')[0])
    tstart = int(runR('start(reg)')[0])
    tend = int(runR('end(reg)')[0])
    timelist_reg = RtoPython(runR('unclass(index(reg))'))

    # Create a simplified time range starting at zero
    timelist_reg_simplified = range(0, tend-tstart+1, deltat)

    timelist_cum = RtoPython(runR('unclass(index(cum))'))

    # Plot the cumulative and the averaged series
    # TODO: Use different y axes for the components because they
    # scale vastly different
    # TODO: We need to re-initialise the plot object somehow since
    # in the second run, the histogram of the previous run is 
    # plotted here.
    status("Computing Time Series Graphs")
    fig = plt.figure()
    ax = fig.add_subplot(111)
    _setupPythonGraphics(os.path.join(basedir, "timegraph"), "PDF")
    plot(timelist_reg, RtoPython(runR('reg')))
    xlabel("Time (TODO: Label with tags)")
    plt.show()
    _closePythonGraphics(os.path.join(basedir, "timegraph"), "PDF")

    _setupPythonGraphics(os.path.join(basedir, "timegraph_cum"), "PDF")
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(timelist_cum, RtoPython(runR('coredata(cum)[,1]')))
    xlabel("Time (TODO: Label with tags)")
    plt.show()
    _closePythonGraphics(os.path.join(basedir, "timegraph_cum"), "PDF")


    # Compare the histograms of commit size and description length
    # distributions 
    # TODO: The plots overlap so that information gets lost. This is
    # obviously bad.
    status("Computing Histograms")
    _setupPythonGraphics(os.path.join(basedir, "histograms"), "PDF")
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.hold(True)
    ax.hist(descr_sizes,100,range=(0,100),normed=True)
    ax.hist(diff_sizes,100,range=(0,100),normed=True,alpha=0.5)
    ax.set_xlabel("Commit/Diff size")
    ax.set_ylabel("Probability")
    ax.grid(True)
    ax.hold(False)
    plt.show()
    _closePythonGraphics(os.path.join(basedir, "histograms"), "PDF")

    # Let's look at some correlations: Between different diff approaches,
    # and the correlation between diff size and 
    status("Computing Correlations")
    computeDiffsizeCommitlengthCorrelation("raw", 
                                           filename=os.path.join(basedir, 
                                                 "diff_commit_corr"), 
                                           backend="PDF")
    computeDifftypeCorrelation("raw", 
                               filename=os.path.join(basedir, "difftype_corr"), 
                               backend="PDF")

    # Determine the density. TODO: Find the best bandwidth. 
    status("Computing Density")
    computeDensity("reg", bandwidth=10, 
                   filename=os.path.join(basedir, "density"),
                   backend="PDF")

    # We could also use reg, but coredata gives more regular labels
    status("Computing Spectrum")
    computeSpectrum("coredata(reg)", 
                    filename=os.path.join(basedir, "spectrum"),
                    backend="PDF")

    status("Computing ECDF")
    computeECDF("reg", filename=os.path.join(basedir, "ecdf"),
                backend="PDF")

    # Generate the recurrence diagram for a series 
    # NOTE: When the number of considered data points exceeds a
    # certain threshold, we don't do the plot because it's
    # computationally too expensive
    if len(reg) < 5000:
        # We use PNG for plotting here because the PDF gets huge.
        # (we could also just pass reg, but extracting the coredata gives
        # "nicer" labels")
        status("Computing Recurrence Diagram")
        computeRecurrenceDiagram("coredata(reg)[,1]", 
                                 filename=os.path.join(basedir, "recurrence"), 
                                 backend="PNG")
    else:
        status("Skipping recurrence diagram: Too many data points")

# NOTE: The histogram of the smoothed distribution looks different than
# the raw histogram - the smoothing shifts the distribution towards larger
# commit numbers
# To compare distA and distA_smoothed, use
# hist(distA,100,range=(0,100), cumulative=True, normed=True)
# hist(distA_smoothed,100,range=(0,100), cumulative=True, normed=True)
# This could be interpreted that the dominance of the small commits is
# not as drastic as many describe it to be.


# TODO: Check for which statistical distributions the power law
# results emerges. I have the feeling that it is true for all
# distributions.


def doSubrangeAnalysis(vcs, parts, basepath, revrange=None, subsys="__main__"):
    """Perform a subrange analysis of the given revision range.

    Split the given range into a number of parts covering
    (approximately) identical time intervals, and perform the
    standard analysis steps on them.

    vcs -- Revision control basis object.
    parts -- Number of sub-parts into which the revision range is 
             to be split.
    revrange -- Revision range.
    subsys -- Specific subsystem to consider (the whole project
              is analysed if subsys==None)."""
    if revrange==None:
        list = vcs.extractCommitData(subsys)
    else:
        list = vcs.extractCommitDataRange(revrange, subsys)
        
    sublist = []
        
    startdate = int(list[0].cdate)
    enddate = int(list[-1].cdate)
    interval = (enddate-startdate)/parts

    sublist.append(list[0])
    next_date = int(startdate + interval)

    # Yes, we could to this more clever. No, it would not pay off.
    curr_pos = 0
    for i in range(0, parts - 1):
        for j in range(curr_pos, len(list)):
            if int(list[j].cdate) >= next_date:
                sublist.append(list[j])
                next_date = next_date + interval
                curr_pos = j
                break # Contine with the next i iteration

    sublist.append(list[-1])
    
    if len(sublist) != parts + 1:
        _abort("Internal error: Subranges don't fulfill parts specification")

    for i in range(1, len(sublist)):
        path = os.path.join(basepath, "cycle{0}".format(i-1))
       
        if not(os.path.exists(path)):
            os.mkdir(path)
           
        print("Analysing development sub-cycle {0} ({1}..{2})".
              format(i-1, sublist[i-1].id, sublist[i].id))
        doAnalysis(vcs, path, revrange=[sublist[i-1].id, sublist[i].id])
   
#######################################################################
############################  Dispatcher  #############################
#######################################################################

def doRevisionAnalysis(vcs, revs, basepath, subrangeAnalysis=False, subparts=5):
    print("Unshelving the data")
    initialiseR()
    
    for i in range(1, len(revs)):
        path = os.path.join(basepath, revs[i])
        
        if not(os.path.exists(path)):
            os.mkdir(path)
            
        print("Analysing {0} development cycle".format(revs[i]))
        doAnalysis(vcs, path, revrange=[revs[i-1], revs[i]])

        if subrangeAnalysis:
            doSubrangeAnalysis(vcs, subparts, path, [revs[i-1], revs[i]])

# Let it rip!
#git = shelve.open("/home/wolfgang/linux-small")["git"]
git = shelve.open("/home/wolfgang/linux-14-33")["git"]
initialiseR()

revs = ["v2.6.{0}".format(i) for i in range(20,31) ]
doRevisionAnalysis(git, revs, "/tmp/graphs", subrangeAnalysis=True, subparts=5)

#revs = ["v2.6.{0}".format(i) for i in range(24,26) ]
#doRevisionAnalysis(git, revs, "/tmp/graphs", subrangeAnalysis=True, subparts=5)

#doRevisionAnalysis(git, ["v2.6.24", "v2.6.25"], "/tmp/graphs")
#doSubrangeAnalysis(git, 5, "/tmp/graphs/cycletest", 
#                   revrange=["v2.6.24", "v2.6.25"])

