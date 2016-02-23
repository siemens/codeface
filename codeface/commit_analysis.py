# Analysis methods to operate on lists of commits, and the associated
# helper functions

# NOTE: These could be included into the object, but this is not
# desirable: This way, we can change the definition of the analysis
# functions, but still re-use serialised objects.

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
# Copyright 2010, 2011, 2012 by Wolfgang Mauerer <wm@linux-kernel.net>
# All Rights Reserved.
"""Analyses the commits."""
# TODO: Add further measures for the commit size

from logging import getLogger
from codeface.commit import Commit
from codeface.linktype import LinkType
from codeface.TimeSeries import TimeSeries
from codeface.VCS import VCS

log = getLogger(__name__)


def _commit_size_ub(add, deleted):
    """Compute the upper bound on the commit size.

    It's as simple as adding the added and deleted lines.

    Args:
        add (int): Lines of codes added.
        deleted (int): Lines of code deleted.

    Returns:
        int: Upper bound for commit size.
    """
    return int(add + deleted)


def _mean(nums):
    """Compute mean over a sequence of numbers.

    Args:
        nums (list): A sequence of numbers.

    Returns:
        float: Mean value.
    """
    # TODO This is not mean but average
    if len(nums):
        return float(sum(nums) / len(nums))
    else:
        return 0.0


def _compute_next_timestamp(time, last_time):
    """Generate strictly monotonic increasing timestamp.

    Computing the time stamp for the NMA routine is done using
    this seemingly bizarre way because we need to have strictly
    monotonic timestamps on the one hand, but want to have
    the distance between commits proportional to their real
    temporal committance distance. So we cannot just use
    the timestamp difference between this and the last event,
    but add one.

    Args:
        time (int): Unix timestamp to be rebased.
        last_time (int): Base Unix timestamp.

    Returns:
        int: Rebased Unix timestamp.
    """
    # Correct for identical dates
    if time == last_time:
        time += 1

    # Correct for negative time differences (can arise from multiple
    # consecutive identical time stamps)
    if time < last_time:
        time = last_time + 1

    return time


def createCumulativeSeries(vcs, subsys="__main__", revrange=None):
    """Create a cumulative diff history by summing up the diff sizes.

    Args:
        vcs (VCS): Instance of VCS.
        subsys (str): Name of subsystem.
        revrange (tuple): Tuple of commit IDs or None.

    Returns:
        TimeSeries: Instance of TimeSeries.

        TimeSeries.series contains a list of dicts, using the following pattern:
        [{'commit': Commit ID, 'value': {Diff type: Lines affected, ...}, ...]
    """

    last_cum = [0] * vcs.getDiffVariations()

    # TODO Check if subsys exists; if not, bark.

    result = TimeSeries()
    if revrange is None:
        commits = vcs.extractCommitData(subsys)
    else:
        commits = vcs.extractCommitDataRange(revrange, subsys)

    for commit in commits:
        entry = {"commit": commit,
                 "value": [0] * vcs.getDiffVariations()}
        for difftype in range(0, vcs.getDiffVariations()):
            csize = _commit_size_ub(commit.getAddedLines(difftype),
                                    commit.getDeletedLines(difftype))

            entry["value"][difftype] = csize + last_cum[difftype]
            last_cum[difftype] = csize + last_cum[difftype]

        result.series.append(entry)

    return result


def createSeries(vcs, subsys="__main__", revrange=None, rc_start=None):
    """Create the list of all diffs (time/value pairs) for subsystem `subsys`.

    Args:
        vcs (VCS): Instance of VCS.
        subsys (str): Name of subsystem.
        revrange (tuple): Tuple of commit IDs or None.
        rc_start (str): Commit ID within revrange or None.

    Returns:
        TimeSeries: Instance of TimeSeries.

        TimeSeries.series contains a list of dicts, using the following pattern:
        [{'commit': Commit ID, 'value': {Diff type: Lines affected, ...}, ...]
    """

    # TODO: Check if subsys exists; if not, bark.

    result = TimeSeries()
    if revrange is None:
        commits = vcs.extractCommitData(subsys)
        result.set_start(vcs.getCommitDate(vcs.rev_start))
        result.set_end(vcs.getCommitDate(vcs.rev_end))
    else:
        commits = vcs.extractCommitDataRange(revrange, subsys)
        result.set_start(vcs.getCommitDate(revrange[0]))
        result.set_end(vcs.getCommitDate(revrange[1]))

    if rc_start:
        result.set_rc_start(vcs.getCommitDate(rc_start))

    for commit in commits:
        entry = {"commit": commit,
                 "value": [0] * vcs.getDiffVariations()}
        for difftype in range(0, vcs.getDiffVariations()):
            csize = _commit_size_ub(commit.getAddedLines(difftype),
                                    commit.getDeletedLines(difftype))

            entry["value"][difftype] = csize

        result.series.append(entry)

    return result


def getSignoffCount(cmt):
    """Get the number of people who signed a commit off.

    Args:
        cmt (Commit): Instance of Commit.

    Returns:
        int: Number of Signed-off-by tags.
    """
    tag_names_list = cmt.getTagNames()
    if "Signed-off-by" in tag_names_list.keys():
        signoffs = len(tag_names_list["Signed-off-by"])
    else:
        signoffs = 0

    return signoffs


def getInvolvedPersons(cmt, categories):
    """Determine the names of persons involved with a commit.

    Args:
        cmt (Commit): Instance of Commit.
        categories (list): List of tag names to be filtered by.

    Returns:
        list: List of person names matching the category filter.
    """
    signoffs = []

    if type(categories) is not list:
        categories = [categories]

    tag_names_list = cmt.getTagNames()
    for key in categories:
        if key in tag_names_list.keys():
            signoffs.extend(tag_names_list[key])

    return signoffs


def getSignoffEtcCount(cmt):
    """Similar to getSignoffCount(), but also counts CCed, Acked-by, etc.

    Args:
        cmt (Commit): Instance of Commit.

    Returns:
        int: Number of sign offs.
    """
    signoffs = 0

    tag_names_list = cmt.getTagNames()
    for key in LinkType.get_tag_types():
        if key in tag_names_list.keys():
            signoffs += len(tag_names_list[key])

    return signoffs


def getSeriesDuration(series):
    """Compute the duration of a commit series in seconds.

    The method computes the difference between the first and last commit in the
    series, which may not be the duration of the initially queried range,
    commits outside this range may be included in the series.

    Args:
        series (TimeSeries): Instance of TimeSeries.

    Returns:
        int: Length of the time series in seconds.
    """

    return int(series.series[-1]["commit"].cdate) - int(
        series.series[0]["commit"].cdate)


def writeToFile(res, name, uniqueTS=True):
    """Write a result list to a file.

    Args:
        res (TimeSeries): Instance of TimeSeries.
        name (str): Name of the output file.
        uniqueTS (bool): Transform the date indices into a strictly monotonic
        series if true.
    """
    # TODO deprecated, to be removed.
    FILE = open(name, "w")
    last_timestamp = 0

    FILE.write("#\t{0}\t{1}".format(res.get_start(), res.get_end()))
    if res.get_rc_start():
        FILE.write("\t{0}".format(res.get_rc_start()))
    FILE.write("\n")

    for i in range(0, len(res.series)):
        cmt = res.series[i]["commit"]
        if uniqueTS:
            try:
                timestamp = _compute_next_timestamp(int(cmt.cdate),
                                                    last_timestamp)
            except ValueError:
                log.warning("Could not determine timestamp for commit"
                            " '%s', skipping it.", cmt.id)
                continue
            last_timestamp = int(timestamp)
        else:
            timestamp = cmt.cdate
        # TODO: Isn't there a better way of doing this? The format
        # string spec is a bit tedious (besides, we rely on four diff
        # variations being present, which might not be true
        FILE.write("{0}\t{1}\t{2}\t{3}\t{4}\t{5}\t{6}\t{7}\n".
                   format(timestamp, res.series[i]["value"][0],
                          res.series[i]["value"][1], res.series[i]["value"][2],
                          res.series[i]["value"][3],
                          cmt.getCommitMessageLines(),
                          getSignoffCount(cmt),
                          getSignoffEtcCount(cmt)))
    FILE.close()
