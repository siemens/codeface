# Analysis methods to operate on lists of commits, and the associated 
# helper functions 
# NOTE: These could be included into the object, but this is not
# desirable: This way, we can change the definition of the analysis
# functions, but still re-use serialised objects.

tag_types = [ "Signed-off-by", "Acked-by", "CC", "Reviewed-by",
             "Reported-by", "Tested-by" ]
active_tag_types = [ "Signed-off-by", "Acked-by", "Reviewed-by",
                    "Tested-by" ]

def flatten(lst):
    for elem in lst:
        if type(elem) in (tuple, list):
            for i in flatten(elem):
                yield i
            else:
                yield elem

def _commit_size_ub(add, deleted):
    """
    Compute the upper bound on the commit size,

    It's as simple as adding the added and deleted lines.
    """
    return int(add + deleted)

# TODO: Add further measures for the commit size

def _mean(nums):
    if len(nums):
        return float(sum(nums)/len(nums))
    else:
        return 0.0


def _compute_next_timestamp(time, last_time):
    # Computing the time stamp for the NMA routine is done using
    # this seemingly bizarre way because we need to have strictly
    # monotonic timestamps on the one hand, but want to have
    # the distance between commits proportional to their real
    # temporal committance distance. So we cannot just use
    # the timestamp difference between this and the last event,
    # but add one.
    
    # Correct for identical dates
    if time == last_time:
        time += 1
	
    # Correct for negative time differences (can arise from multiple
    # consecutive identical time stamps)
    if time < last_time:
        time = last_time + 1

    return time


def createCumulativeSeries(vcs, subsys="__main__", revrange=None):
    """
    Create a cumulative diff history by summing up the diff sizes.

    Returns a list with one dictionary per commit. The dictionary
    contains two items: 
    
    commit -- A list with one entry per diff type of the cumulative value of 
           diff size up to (and including) the commit.
    cdate -- Unix timestamp of the committer date.
    """

    last_cum = [0] * vcs.getDiffVariations();

    # TODO: Check if subsys exists; if not, bark.

    res = []
    if revrange==None:
        list = vcs.extractCommitData(subsys)
    else:
        list = vcs.extractCommitDataRange(revrange, subsys)

    for cmt in list:
        entry = {"commit" : cmt,
                 "value" : [0] * vcs.getDiffVariations() }
        for difftype in range(0,vcs.getDiffVariations()):

            csize = _commit_size_ub(cmt.getAddedLines(difftype),
                                    cmt.getDeletedLines(difftype))

            entry["value"][difftype] = csize + last_cum[difftype]
            last_cum[difftype] = csize + last_cum[difftype]

        res.append(entry)

    return res


def createSeries(vcs, subsys="__main__", revrange=None):
    """
    Create the list of all diffs (time/value pairs) for subsystem subsys.

    Returns a list with one dictionary per commit. The dictionary
    contains two items: 
    
    commit -- A list with one entry per diff type of the diff size of 
              the commit.
    cdate -- Unix timestamp of the committer date.
    """

    # TODO: Check if subsys exists; if not, bark.

    res = []
    if revrange==None:
        list = vcs.extractCommitData(subsys)
    else:
        list = vcs.extractCommitDataRange(revrange, subsys)

    for cmt in list:
        entry = {"commit" : cmt,
                 "value" : [0] * vcs.getDiffVariations() }
        for difftype in range(0,vcs.getDiffVariations()):

            csize = _commit_size_ub(cmt.getAddedLines(difftype),
                                    cmt.getDeletedLines(difftype))

            entry["value"][difftype] = csize 

        res.append(entry)

    return res

def getSignoffCount(cmt):
    """Get the number of people who signed a commit off."""
    tag_names_list = cmt.getTagNames()
    if "Signed-off-by" in tag_names_list.keys():
        signoffs = len(tag_names_list["Signed-off-by"])
    else:
        signoffs = 0

    return signoffs

def getInvolvedPersons(cmt, categories):
    """Determine the names of persons involved with a commit. categories
    is a list with entries like Signed-off-by, Acked-by, etc. """
    signoffs = []

    if (not(type(categories) == list)):
        categories = [categories]
    
    tag_names_list = cmt.getTagNames()
    for key in categories:
        if key in tag_names_list.keys():
            signoffs.extend(tag_names_list[key])

    return signoffs


def getSignoffEtcCount(cmt):
    """Similar to getSignoffCount(), but also counts CCed, Acked-by, etc."""
    signoffs = 0

    tag_names_list = cmt.getTagNames()
    for key in tag_types:
        if key in tag_names_list.keys():
            signoffs += len(tag_names_list[key])

    return signoffs

def getSeriesDuration(res):
    """Compute the duration of a commit series in seconds."""

    return int(res[-1]["commit"].cdate)-int(res[0]["commit"].cdate)

def writeToFile(res, name, uniqueTS=True):
    """Write a result list to a file.

    res -- Time series obtained by createSeries etc.
    name -- Name of the output file.
    uniqueTS -- Transform the date indices into a strictly monotonic 
                series if true."""
    FILE=open(name, "w")
    last_timestamp = 0

    for i in range(0,len(res)):
        cmt = res[i]["commit"]
        if uniqueTS:
            timestamp = _compute_next_timestamp(int(cmt.cdate), last_timestamp)
            last_timestamp = int(timestamp)
        else:
            timestamp = cmt.cdate
        # TODO: Isn't there a better way of doing this? The format
        # string spec is a bit tedious (besides, we rely on four diff
        # variations being present, which might not be true
        FILE.write("{0}\t{1}\t{2}\t{3}\t{4}\t{5}\t{6}\t{7}\n".
                   format(timestamp, res[i]["value"][0],
                          res[i]["value"][1], res[i]["value"][2],
                          res[i]["value"][3], cmt.getCommitMessageLines(),
                          getSignoffCount(cmt),
                          getSignoffEtcCount(cmt)))
    FILE.close()

