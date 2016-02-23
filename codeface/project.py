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
# Copyright 2013 by Siemens AG
# All Rights Reserved.

"""Module containing analysis methods.

Attributes:
    log:

Methods:
    loginfo: Pickleable function for multiprocesssing
    project_setup: Setup analysis form configuration file
    project_analyse: Analyse git project
    mailinglist_analyse: Analyse mailing list
"""

from logging import getLogger
from pkg_resources import resource_filename
from os.path import join as pathjoin, split as pathsplit, abspath
from .dbmanager import DBManager
from .configuration import Configuration, ConfigurationError
from .cluster.cluster import doProjectAnalysis, LinkType
from .ts import dispatch_ts_analysis
from .util import (execute_command, generate_reports, check4ctags,
                   check4cppstats, BatchJobPool, generate_analysis_windows)

log = getLogger(__name__)


def loginfo(msg):
    """Pickleable function for multiprocessing

    Args:
        msg:
    """

    log.info(msg)


def project_setup(conf, recreate):
    """This method updates the project in the database with the release
    information given in the configuration.
    Returns the project ID, the database manager and the list of range ids
    for the ranges between the releases specified in the configuration.
    Set up project in database and retrieve ranges to analyse

    Args:
        conf:
        recreate:

    Returns:
        project_id:
        dbm:
        all_range_ids:
    """

    log.info("=> Setting up project '%s'", conf["project"])
    dbm = DBManager(conf)
    new_range_ids = dbm.update_release_timeline(conf["project"],
                                                conf["tagging"],
                                                conf["revisions"], conf["rcs"],
                                                recreate_project=recreate)
    project_id = dbm.getProjectID(conf["project"], conf["tagging"])
    revs = conf["revisions"]
    #TODO make readable in for each! Ultra-short is useless when it is confusing
    all_range_ids = [
        dbm.getReleaseRangeID(project_id, (
            dbm.getRevisionID(project_id, start),
            dbm.getRevisionID(project_id, end)))
        for (start, end) in zip(revs[0:], revs[1:])
        ]
    return project_id, dbm, all_range_ids

#TODO analyse and document functions then remove magic numbers and constants
def project_analyse(resdir, gitdir, codeface_conf, project_conf,
                    no_report, loglevel, logfile, recreate, profile_r,
                    n_jobs, tagging_type, reuse_db):
    """Analyses a git project.

    Args:
        resdir: Directory to store results in.
        mldir: Storage directory for source mailing list.
        codeface_conf: Codeface configuration file, contains database access,
            PersonID settings, Java BugExtractor settings and complexity
            analysis settings.
        project_conf: Project configuration file, contains project name, repo
            type, mailing list storage, mailing lists, descriptions, revisions,
            rcs and tagging.
        no_report: Enable/disable report generation.
        loglevel:
        logfile:
        recreate: Enable/disable recreation of
        profile_r: Specify R profile.
        n_jobs: Number of parallel processes.
        tagging_type: Specify tagging type, valid ones are:
            tag, proximity, committer2author, file, feature, feature_file
        reuse_db: Toggle reuse of existing database.

    """

    pool = BatchJobPool(int(n_jobs))
    conf = Configuration.load(codeface_conf, project_conf)
    tagging = conf["tagging"]
    if tagging_type is not "default":

        if not tagging_type in LinkType.get_all_link_types():
            log.critical('Unsupported tagging mechanism specified!')
            raise ConfigurationError('Unsupported tagging mechanism.')
        # we override the configuration value explicitly by cmd argument
        if tagging is not tagging_type:
            log.warn(
                "tagging value is overwritten to %s because of --tagging",
                tagging_type)
            tagging = tagging_type
            conf["tagging"] = tagging

    project = conf["project"]
    repo = pathjoin(gitdir, conf["repo"], ".git")
    project_resdir = pathjoin(resdir, project, tagging)
    range_by_date = False

    # When revisions are not provided by the configuration file
    # generate the analysis window automatically
    if len(conf["revisions"]) < 2:
        window_size_months = 3  # Window size in months
        num_window = -1  # Number of ranges to analyse, -1 captures all ranges
        revs, rcs = generate_analysis_windows(repo, window_size_months)
        conf["revisions"] = revs[-num_window - 1:]
        conf["rcs"] = rcs[-num_window - 1:]
        range_by_date = True

    # TODO: Sanity checks (ensure that git repo dir exists)
    if tagging == LinkType.proximity:
        check4ctags()
    elif tagging in (LinkType.feature, LinkType.feature_file):
        check4cppstats()

    project_id, dbm, all_range_ids = project_setup(conf, recreate)

    # Save configuration file
    conf.write()
    project_conf = conf.get_conf_file_loc()

    # Analyse new revision ranges
    for i, range_id in enumerate(all_range_ids):
        start_rev, end_rev, rc_rev = dbm.get_release_range(project_id, range_id)
        range_resdir = pathjoin(project_resdir, "{0}-{1}".
                                format(start_rev, end_rev))
        prefix = "  -> Revision range {0}..{1}: ".format(start_rev, end_rev)

        # STAGE 1: Commit analysis
        s1 = pool.add(
            doProjectAnalysis,
            (conf, start_rev, end_rev, rc_rev, range_resdir, repo,
             reuse_db, True, range_by_date),
            startmsg=prefix + "Analysing commits...",
            endmsg=prefix + "Commit analysis done."
        )

        # STAGE 2: Cluster analysis
        exe = abspath(resource_filename(__name__, "R/cluster/persons.r"))
        cwd, _ = pathsplit(exe)
        cmd = []
        cmd.append(exe)
        cmd.extend(("--loglevel", loglevel))
        if logfile:
            cmd.extend(("--logfile", "{}.R.r{}".format(logfile, i)))
        cmd.extend(("-c", codeface_conf))
        cmd.extend(("-p", project_conf))
        cmd.append(range_resdir)
        cmd.append(str(range_id))

        s2 = pool.add(
            execute_command,
            (cmd,),
            {"direct_io": True, "cwd": cwd},
            deps=[s1],
            startmsg=prefix + "Detecting clusters...",
            endmsg=prefix + "Detecting clusters done."
        )

        # STAGE 3: Generate cluster graphs
        if not no_report:
            pool.add(
                generate_reports,
                (start_rev, end_rev, range_resdir),
                deps=[s2],
                startmsg=prefix + "Generating reports...",
                endmsg=prefix + "Report generation done."
            )

    # Wait until all batch jobs are finished
    pool.join()

    # Global stage 1: Time series generation
    log.info("=> Preparing time series data")
    dispatch_ts_analysis(project_resdir, conf)

    # Global stage 2: Complexity analysis
    # NOTE: We rely on proper timestamps, so we can only run after time series
    # generation
    log.info("=> Performing complexity analysis")
    for i, range_id in enumerate(all_range_ids):
        log.info("  -> Analysing range '%s'", range_id)
        exe = abspath(resource_filename(__name__, "R/complexity.r"))
        cwd, _ = pathsplit(exe)
        cmd = [exe]
        if logfile:
            cmd.extend(("--logfile", "{}.R.complexity.{}".format(logfile, i)))
        cmd.extend(("--loglevel", loglevel))
        cmd.extend(("-c", codeface_conf))
        cmd.extend(("-p", project_conf))
        cmd.extend(("-j", str(n_jobs)))
        cmd.append(repo)
        cmd.append(str(range_id))
        execute_command(cmd, direct_io=True, cwd=cwd)

    # Global stage 3: Time series analysis
    log.info("=> Analysing time series")
    exe = abspath(resource_filename(__name__, "R/analyse_ts.r"))
    cwd, _ = pathsplit(exe)
    cmd = [exe]
    if profile_r:
        cmd.append("--profile")
    if logfile:
        cmd.extend(("--logfile", "{}.R.ts".format(logfile)))
    cmd.extend(("--loglevel", loglevel))
    cmd.extend(("-c", codeface_conf))
    cmd.extend(("-p", project_conf))
    cmd.extend(("-j", str(n_jobs)))
    cmd.append(project_resdir)
    execute_command(cmd, direct_io=True, cwd=cwd)
    log.info("=> Codeface run complete!")

#TODO sanity check mailing lists parameter
def mailinglist_analyse(resdir, mldir, codeface_conf, project_conf, loglevel,
                        logfile, jobs, mailinglists):
    """Analyse a mailing list.

    Args:
        resdir: Directory to store results in.
        mldir: Storage directory for source mailing list.
        codeface_conf: Codeface configuration file, contains database access,
            PersonID settings, Java BugExtractor settings and complexity
            analysis settings.
        project_conf: Project configuration file, contains project name, repo
            type, mailing list storage, mailing lists, descriptions, revisions,
            rcs and tagging.
        loglevel: Amount of logging done.
        logfile:
        jobs: Maximum parallel processes to work with.
        mailinglists: Mailing lists to check.

    """
    
    conf = Configuration.load(codeface_conf, project_conf)
    ml_resdir = pathjoin(resdir, conf["project"], "ml")

    exe = abspath(resource_filename(__name__, "R/ml/batch.r"))
    cwd, _ = pathsplit(exe)
    cmd = []
    cmd.extend(("--loglevel", loglevel))
    cmd.extend(("-c", codeface_conf))
    cmd.extend(("-p", project_conf))
    cmd.extend(("-j", str(jobs)))
    cmd.append(ml_resdir)
    cmd.append(mldir)
    if not mailinglists:
        mailinglist_conf = conf["mailinglists"]
    else:
        mailinglist_conf = []
        for mln in mailinglists:
            # TODO check ml/mln confusion and disambiguate! should be mln (prob)
            match = [ml for ml in conf["mailinglists"] if ml["name"] == mln]
            if not match:
                log.fatal(
                    "Mailinglist '%s' not listed in configuration file!",
                    ml)
                raise Exception("Unknown mailing list")
            if len(match) > 1:
                log.fatal(
                    "Mailinglist '%s' specified twice in configuration file!",
                    ml)
                raise Exception("Invalid config file")
            mailinglist_conf.append(match[0])

    for i, ml in enumerate(mailinglist_conf):
        log.info("=> Analysing mailing list '%s' of type '%s'",
                 ml["name"],
                 ml["type"])
        logargs = []
        if logfile:
            logargs = ["--logfile", "{}.R.ml.{}".format(logfile, i)]
        execute_command([exe] + logargs + cmd + [ml["name"]],
                        direct_io=True, cwd=cwd)
    log.info("=> Codeface mailing list analysis complete!")
