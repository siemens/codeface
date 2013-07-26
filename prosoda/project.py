from logging import getLogger; log = getLogger(__name__)
from pkg_resources import resource_filename
from os.path import join as pathjoin, split as pathsplit

from .dbmanager import DBManager
from .configuration import Configuration
from .cluster.cluster import doProjectAnalysis
from .ts import dispatch_ts_analysis
from .util import (execute_command, generate_report, layout_all_graphs,
        check4ctags)

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

def project_analyse(resdir, gitdir, prosoda_conf, project_conf,
                    no_report, loglevel, logfile, recreate):
    conf = Configuration.load(prosoda_conf, project_conf)
    revs = conf["revisions"]
    rcs = conf["rcs"] # release candidate tags
    project, tagging = conf["project"], conf["tagging"]
    repo = pathjoin(gitdir, conf["repo"], ".git")
    project_resdir = pathjoin(resdir, project, tagging)

    # TODO: Sanity checks (ensure that git repo dir exists)
    if 'proximity' == conf["tagging"]:
        check4ctags()

    project_id, dbm, all_range_ids = project_setup(conf, recreate)
    # Analyse new revision ranges
    for i, range_id in enumerate(all_range_ids):
        start_rev, end_rev, rc_rev = dbm.get_release_range(project_id, range_id)
        range_resdir = pathjoin(project_resdir, "{0}-{1}".
                format(start_rev, end_rev))
        log.info("  -> Analysing revision range {0}..{1}".
                format(start_rev, end_rev))

        #######
        # STAGE 1: Commit analysis
        limit_history = True
        log.info("    - Analysing commits")
        doProjectAnalysis(conf, dbm, start_rev, end_rev, rc_rev, range_resdir,
                repo, True, limit_history)

        #########
        # STAGE 2: Cluster analysis
        log.info("    - Detecting clusters")
        exe = resource_filename(__name__, "R/cluster/persons.r")
        cwd, _ = pathsplit(exe)
        cmd = []
        cmd.append(exe)
        cmd.extend(("--loglevel", loglevel))
        if logfile:
            cmd.extend(("--logfile", "{}.R.r{}".format(logfile, i)))
        cmd.extend(("-c", prosoda_conf))
        cmd.extend(("-p", project_conf))
        cmd.append(range_resdir)
        cmd.append(str(range_id))
        print(cmd, cwd)
        execute_command(cmd, direct_io=True, cwd=cwd)

        #########
        # STAGE 3: Generate cluster graphs
        if not no_report:
            log.info("    - Generating reports")
            layout_all_graphs(range_resdir)
            generate_report(start_rev, end_rev, range_resdir)

    #########
    # Global stage 1: Time series generation
    log.info("=> Preparing time series data")
    dispatch_ts_analysis(project_resdir, dbm, conf)

    #########
    # Global stage 2: Time series analysis
    log.info("=> Analysing time series")
    exe = resource_filename(__name__, "R/analyse_ts.r")
    cwd, _ = pathsplit(exe)
    cmd = [exe]
    if logfile:
        cmd.extend(("--logfile", "{}.R.ts".format(logfile)))
    cmd.extend(("--loglevel", loglevel))
    cmd.extend(("-c", prosoda_conf))
    cmd.extend(("-p", project_conf))
    cmd.append(project_resdir)
    execute_command(cmd, direct_io=True, cwd=cwd)
    log.info("=> Prosoda run complete!")
