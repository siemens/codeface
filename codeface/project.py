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

from logging import getLogger; log = getLogger(__name__)
from pkg_resources import resource_filename
from os.path import join as pathjoin, split as pathsplit, abspath

from .dbmanager import DBManager
from .configuration import Configuration, ConfigurationError
from .cluster.cluster import doProjectAnalysis, LinkType
from .ts import dispatch_ts_analysis
from .conway import dispatch_jira_processing, parseCommitLoC
from .util import (execute_command, generate_reports, layout_graph, gen_range_path,
                   check4ctags, check4cppstats, BatchJobPool, generate_analysis_windows,
                   gen_prefix, get_analysis_windows)

def loginfo(msg):
    ''' Pickleable function for multiprocessing '''
    log.info(msg)

def project_setup(conf, recreate):
    '''
    This method updates the project in the database with the release
    information given in the configuration.
    Returns the project ID, the database manager and the list of range ids
    for the ranges between the releases specified in the configuration.
    '''
    # Set up project in database and retrieve ranges to analyse
    log.info("=> Setting up project '{c[project]}'".format(c=conf))
    dbm = DBManager(conf)
    new_range_ids = dbm.update_release_timeline(conf["project"],
            conf["tagging"], conf["revisions"], conf["rcs"],
            recreate_project=recreate)
    project_id = dbm.getProjectID(conf["project"], conf["tagging"])
    revs = conf["revisions"]
    all_range_ids = [dbm.getReleaseRangeID(project_id,
            ( dbm.getRevisionID(project_id, start),
              dbm.getRevisionID(project_id, end)))
            for (start, end) in zip(revs, revs[1:])]
    return project_id, dbm, all_range_ids

def project_analyse(resdir, gitdir, codeface_conf, project_conf,
                    no_report, loglevel, logfile, recreate, profile_r,
                    n_jobs, tagging_type, reuse_db):
    pool = BatchJobPool(int(n_jobs))
    conf = Configuration.load(codeface_conf, project_conf)
    tagging = conf["tagging"]
    if tagging_type is not "default":
        if not tagging_type in LinkType.get_all_link_types():
            log.critical('Unsupported tagging mechanism specified!')
            raise ConfigurationError('Unsupported tagging mechanism.')
        # we override the configuration value
        if tagging is not tagging_type:
            log.warn(
                "tagging value is overwritten to {0} because of --tagging"
                .format(tagging_type))
            tagging = tagging_type
            conf["tagging"] = tagging

    project = conf["project"]
    repo = pathjoin(gitdir, conf["repo"], ".git")
    project_resdir = pathjoin(resdir, project, tagging)
    range_by_date = False

    # When revisions are not provided by the configuration file
    # generate the analysis window automatically
    if len(conf["revisions"]) < 2:
        window_size_months, num_window = get_analysis_windows(conf)
        revs, rcs, dates = generate_analysis_windows(repo, window_size_months)
        conf["revisions"] = revs[-num_window-1:]
        conf["rcs"] = rcs[-num_window-1:]
        range_by_date = True

    # TODO: Sanity checks (ensure that git repo dir exists)
    if tagging == LinkType.proximity:
        check4ctags()
    elif tagging in (LinkType.feature, LinkType.feature_file):
        check4cppstats()

    project_id, dbm, all_range_ids = project_setup(conf, recreate)

    ## Save configuration file
    conf.write()
    project_conf = conf.get_conf_file_loc()

    # Analyse new revision ranges
    for i, range_id in enumerate(all_range_ids):
        start_rev, end_rev, rc_rev = dbm.get_release_range(project_id, range_id)
        range_resdir = gen_range_path(project_resdir, i+1, start_rev, end_rev)
        prefix = gen_prefix(i+1, len(all_range_ids), start_rev, end_rev)

        #######
        # STAGE 1: Commit analysis
        s1 = pool.add(
                doProjectAnalysis,
                (conf, start_rev, end_rev, rc_rev, range_resdir, repo,
                    reuse_db, True, range_by_date),
                startmsg=prefix + "Analysing commits...",
                endmsg=prefix + "Commit analysis done."
            )

        #########
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
                {"direct_io":True, "cwd":cwd},
                deps=[s1],
                startmsg=prefix + "Detecting clusters...",
                endmsg=prefix + "Detecting clusters done."
            )

        #########
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

    #########
    # Global stage 1: Time series generation
    log.info("=> Preparing time series data")
    dispatch_ts_analysis(project_resdir, conf)

    #########
    # Global stage 2: Complexity analysis
    ## NOTE: We rely on proper timestamps, so we can only run
    ## after time series generation
    log.info("=> Performing complexity analysis")
    for i, range_id in enumerate(all_range_ids):
        log.info("  -> Analysing range {}".format(range_id))
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

    #########
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

def mailinglist_analyse(resdir, mldir, codeface_conf, project_conf, loglevel,
                        logfile, n_jobs, mailinglists, use_corpus):
    conf = Configuration.load(codeface_conf, project_conf)
    ml_resdir = pathjoin(resdir, conf["project"], "ml")

    exe = abspath(resource_filename(__name__, "R/ml/batch.r"))
    cwd, _ = pathsplit(exe)
    cmd = []
    cmd.extend(("--loglevel", loglevel))
    cmd.extend(("-c", codeface_conf))
    cmd.extend(("-p", project_conf))
    cmd.extend(("-j", str(n_jobs)))
    if (use_corpus):
        cmd.append("--use-corpus")
    cmd.append(ml_resdir)
    cmd.append(mldir)
    if not mailinglists:
        mailinglist_conf = conf["mailinglists"]
    else:
        mailinglist_conf = []
        for mln in mailinglists:
            match = [ml for ml in conf["mailinglists"] if ml["name"] == mln]
            if not match:
                log.fatal("Mailinglist '{}' not listed in configuration file!".
                    format(ml))
                raise Exception("Unknown mailing list")
            if len(match) > 1:
                log.fatal("Mailinglist '{}' specified twice in configuration file!".
                    format(ml))
                raise Exception("Invalid config file")
            mailinglist_conf.append(match[0])

    for i, ml in enumerate(mailinglist_conf):
        log.info("=> Analysing mailing list '{name}' of type '{type}'".
                format(**ml))
        logargs = []
        if logfile:
            logargs = ["--logfile", "{}.R.ml.{}".format(logfile, i)]
        execute_command([exe] + logargs + cmd + [ml["name"]],
                direct_io=True, cwd=cwd)
    log.info("=> Codeface mailing list analysis complete!")


def conway_analyse(resdir, gitdir, titandir, codeface_conf, project_conf,
                   loglevel, logfile, n_jobs):
    log.info("Performing conway analysis")

    pool = BatchJobPool(int(n_jobs))
    conf = Configuration.load(codeface_conf, project_conf)
    project = conf["project"]
    repo = pathjoin(gitdir, conf["repo"], ".git")
    project_resdir = pathjoin(resdir, project, "conway")
    range_by_date = False

    if conf["tagging"] != "proximity":
        log.error("Conway analysis requires analysis in proximity mode!")
        return

    # Set defaults for the various analysis choices if they are not explicitly
    # given in the configuration file
    if "artifactType" not in conf.keys():
        conf["artifactType"] = "file"
        log.info("Conway analysis: No artefact type given, defaulting to 'file'")

    if "dependencyType" not in conf.keys():
        conf["dependencyType"] = "none"
        log.info("Conway analysis: No dependency type given, defaulting to 'none'")

    if "qualityType" not in conf.keys():
        conf["qualityType"] = "corrective"
        log.info("Conway analysis: No quality type given, defaulting to 'corrective'")

    if "communicationType" not in conf.keys():
        conf["communicationType"] = "mail"
        log.info("Conway analysis: No communication type given, defaulting to 'mail'")

    if (conf["communicationType"] == "jira" or conf["communicationType"] == "mail+jira") and \
        (("issueTrackerType" in conf.keys() and conf["issueTrackerType"] != "jira") or \
        not("issueTrackerType" in conf.keys())):
        log.info("Conway analysis only supports jira for bugtracking information, exiting")
        return

    # When revisions are not provided by the configuration file
    # generate the analysis window automatically
    if len(conf["revisions"]) < 2:
        window_size_months, num_window = get_analysis_windows(conf)
        revs, rcs, dates = generate_analysis_windows(repo, window_size_months)
        conf["revisions"] = revs[-num_window-1:]
        conf["rcs"] = rcs[-num_window-1:]
        range_by_date = True

    project_id, dbm, all_range_ids = project_setup(conf, False)

    ## Save modified configuration file to a temporary location
    conf.write()
    project_conf = conf.get_conf_file_loc()

    # Global stage: Download and process JIRA issues
    if conf["communicationType"] == "jira" or conf["communicationType"] == "mail+jira":
        log.info("=> Downloading and processing JIRA issues")
        dispatch_jira_processing(project_resdir, titandir, conf)

    # Revision range specific analysis
    for i, range_id in enumerate(all_range_ids):
        start_rev, end_rev, rc_rev = dbm.get_release_range(project_id, range_id)
        start_date = dbm.get_commit_cdate(project_id, start_rev)
        end_date = dbm.get_commit_cdate(project_id, end_rev)
        range_resdir = gen_range_path(project_resdir, i+1, start_rev, end_rev)
        prefix = gen_prefix(i+1, len(all_range_ids), start_rev, end_rev)

        if (dbm.get_num_commits_in_range(range_id) < 5):
            log.info("=> Release range contains less than 5 commits, skipping")
            continue

        #######
        # STAGE 1:
        s1 = pool.add(
                parseCommitLoC,
                (conf, dbm, project_id, range_id, start_rev, end_rev,
                 range_resdir, repo),
                startmsg=prefix + "Computing file/developer relations...",
                endmsg=prefix + "Computing file/developer relations done."
            )

        #########
        # STAGE 2: Connect commits and jira issues
        if "communicationType" in conf.keys() and \
            (conf["communicationType"] == "jira" or conf["communicationType"] == "mail+jira"):
            exe = abspath(resource_filename(__name__, "R/conway_metrics.r"))
            cwd, _ = pathsplit(exe)
            cmd = []
            cmd.append(exe)
            cmd.extend(("--loglevel", loglevel))
            if logfile:
                cmd.extend(("--logfile", "{}.R.r{}".format(logfile, i)))
            cmd.extend(("-c", codeface_conf))
            cmd.extend(("-p", project_conf))
            cmd.append(project_resdir)
            cmd.append(range_resdir)

            s2 = pool.add(
                execute_command,
                (cmd,),
                {"direct_io":True, "cwd":cwd},
                deps=[s1],
                startmsg=prefix + "Connecting commits and issues...",
                endmsg=prefix + "Connecting commits and issues done."
                )


        #######
        # STAGE 3: Obtain DSM using understand
        if "dependencyType" in conf.keys() and conf["dependencyType"] == "dsm":
            exe = abspath(resource_filename(__name__, "R/gen_dsm.r"))
            cwd, _ = pathsplit(exe)
            cmd = []
            cmd.append(exe)
            cmd.extend(("--loglevel", loglevel))
            if logfile:
                cmd.extend(("--logfile", "{}.R.r{}".format(logfile, i)))
            cmd.extend(("-c", codeface_conf))
            cmd.extend(("-p", project_conf))
            cmd.append(repo)
            cmd.append(range_resdir)
            cmd.append(end_rev)

            s3 = pool.add(
                execute_command,
                (cmd,),
                {"direct_io":True, "cwd":cwd},
                deps=[s2],
                startmsg=prefix + "Inferring architectural metrics with understand...",
                endmsg=prefix + "understand run done."
                )

        #########
        # STAGE 4: Perform socio-technical analysis
        exe = abspath(resource_filename(__name__, "R/socio_technical_analysis.r"))
        cwd, _ = pathsplit(exe)
        cmd = []
        cmd.append(exe)
        cmd.extend(("--loglevel", loglevel))
        if logfile:
            cmd.extend(("--logfile", "{}.R.r{}".format(logfile, i)))
        cmd.extend(("-c", codeface_conf))
        cmd.extend(("-p", project_conf))
        cmd.append(project_resdir)
        cmd.append(str(i+1))
        if "dependencyType" in conf.keys() and conf["dependencyType"] == "dsm":
            deps=[s3]
        else:
            deps=[s2]

        s4 = pool.add(
                execute_command,
                (cmd,),
                {"direct_io":True, "cwd":cwd},
                deps=deps,
                startmsg=prefix + "Performing socio-technical analysis...",
                endmsg=prefix + "Socio-technical analysis done."
            )

    # Wait until all batch jobs are finished
    pool.join()

    #########
    # Global conway analysis
    log.info("=> Performing global conway analysis")
    exe = abspath(resource_filename(__name__, "R/conway_global.r"))
    cwd, _ = pathsplit(exe)
    cmd = [exe]
    if logfile:
        cmd.extend(("--logfile", "{}.R.ts".format(logfile)))
    cmd.extend(("--loglevel", loglevel))
    cmd.extend(("-c", codeface_conf))
    cmd.extend(("-p", project_conf))
    cmd.extend(("-j", str(n_jobs)))
    cmd.append(project_resdir)
    cmd.append(titandir)
    execute_command(cmd, direct_io=True, cwd=cwd)

    log.info("=> Codeface conway analysis complete!")
