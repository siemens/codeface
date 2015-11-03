#! /usr/bin/env python
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
# Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
# All Rights Reserved.

# Thin sql database wrapper

import MySQLdb as mdb
from datetime import datetime
from logging import getLogger; log = getLogger(__name__)
from contextlib import contextmanager

@contextmanager
def _log_db_error(action, args=None):
    try:
        yield
    except mdb.Error as e:
        if args:
            try:
                action = action % args
            except:
                pass
        log.critical('MySQL error {e[0]} during "{action}": {e[1]}'
                ''.format(e=e.args, action=action))
        raise

class DBManager:
    """This class provides an interface to the codeface sql database."""

    def __init__(self, conf):
        try:
            self.con = None
            self.con = mdb.Connection(host=conf["dbhost"],
                                      port=conf["dbport"],
                                      user=conf["dbuser"],
                                      passwd=conf["dbpwd"],
                                      db=conf["dbname"])
            log.debug(
                "Establishing MySQL connection to "
                "{c[dbuser]}@{c[dbhost]}:{c[dbport]}, DB '{c[dbname]}'"
                .format(c=conf))
        except mdb.Error as e:
            log.critical(
                "Failed to establish MySQL connection to "
                "{c[dbuser]}@{c[dbhost]}:{c[dbport]}, DB '{c[dbname]}'"
                ": {e[1]} ({e[0]})"
                "".format(c=conf, e=e.args))
            raise
        self.cur = self.con.cursor()

	max_packet_size = 1024*1024*32
	self.doExec("SET GLOBAL max_allowed_packet=%s", (max_packet_size,))

    def __del__(self):
        if self.con != None:
            self.con.close()

    def doExec(self, stmt, args=None):
        with _log_db_error(stmt, args):
            while True:
                try:
                    if isinstance(args, list):
                        res = self.cur.executemany(stmt, args)
                    else:
                        res = self.cur.execute(stmt, args)
                    return res
                except mdb.OperationalError as dbe:
                    log.info("DBE args: " + str(dbe.args))
                    if dbe.args[0] == 1213: # Deadlock! retry...
                        log.warning("Recoverable deadlock in MySQL - retrying.")
                    elif dbe.args[0] == 2006: # Server gone away...
                        log.warning("MySQL Server gone away, trying to reconnect.")
                        self.con.ping(True)
                    else:
                        raise

    def doFetchAll(self):
        with _log_db_error("fetchall"):
            return self.cur.fetchall()

    def doCommit(self):
        with _log_db_error("commit"):
            return self.con.commit()

    def doExecCommit(self, stmt, args=None):
        self.doExec(stmt, args)
        self.doCommit()

    # NOTE: We don't provide any synchronisation since by assumption,
    # a single project is never analysed from two threads.
    def getProjectID(self, name, analysisMethod):
        """
        Return the project ID of the given name/analysisMethod combination.
        If the project does not exist yet in the database, it is created.
        """
        self.doExec("SELECT id FROM project WHERE name=%s "
                    "AND analysisMethod=%s", (name, analysisMethod))
        if self.cur.rowcount == 0:
            # Project is not contained in the database
            log.devinfo("Creating new project {}/{}".
                    format(name, analysisMethod))
            self.doExecCommit("INSERT INTO project (name, analysisMethod) " +
                        "VALUES (%s, %s);", (name, analysisMethod))
            self.doExec("SELECT id FROM project WHERE name=%s;", (name,))
        elif self.cur.rowcount > 1:
            raise Exception("Duplicate projects {}/{} in database!".
                    format(name, analysisMethod))
        pid = self.doFetchAll()[0][0]
        log.devinfo("Using project {}/{} with ID {}".
                format(name, analysisMethod, pid))
        return pid

    def get_project(self, pid):
        self.doExec("SELECT name, analysisMethod FROM project"
                    " WHERE id=%s", pid)
        if self.cur.rowcount == 0:
            raise Exception("Project id {} not found!".format(pid))
        return self.doFetchAll()[0]

    def get_edgelist(self, cid):
        self.doExec("SELECT fromId, toId, weight FROM edgelist \
                    WHERE clusterId={}".format(cid))
        if self.cur.rowcount == 0:
            raise Exception("Cluster id {} not found!".format(cid))
        return self.doFetchAll()

    def get_release_ranges(self, project_id):
        self.doExec("SELECT id FROM release_range \
                    WHERE projectId={}".format(project_id))
        if self.cur.rowcount == 0:
            raise Exception("No release ranges found for project {}!"
                            .format(project_id))
        return [range_entry[0] for range_entry in self.doFetchAll()]

    def get_cluster_id(self, pid, release_range_id=None):
        if release_range_id:
            self.doExec("SELECT id FROM cluster WHERE clusterNumber=-1 \
                        AND projectId={} AND releaseRangeId={}"
                        .format(pid, release_range_id))
        else:
            self.doExec("SELECT id FROM cluster WHERE clusterNumber=-1 \
                        AND projectId={}".format(pid))
        if self.cur.rowcount == 0:
            raise Exception("Cluster from project {} not found!".format(pid))
        return self.doFetchAll()[0][0]

    def get_project_persons(self, pid):
        self.doExec("SELECT id, name FROM person \
                    WHERE projectId={}".format(pid))
        if self.cur.rowcount == 0:
            raise Exception("Persons from project {} not found!".format(pid))
        return(self.doFetchAll())

    def getTagID(self, projectID, tag, type):
        """Determine the ID of a tag, given its textual form and the type"""
        self.doExec("SELECT id FROM release_timeline WHERE projectId=%s " +
                    "AND tag=%s AND type=%s", (projectID, tag, type))
        if self.cur.rowcount != 1:
            raise Exception("Tag '{}' of type {} is {} times in the DB!".
                    format(tag, type, self.cur.rowcount))
        return self.doFetchAll()[0][0]

    def getCommitId(self, projectId, commitHash):
        self.doExec("SELECT id FROM commit" +
                   " WHERE commitHash=%s AND projectId=%s"
                   ,(commitHash, projectId))
        if self.cur.rowcount == 0:
            raise Exception("Commit from project {} not found!".
                    format(projectId))
        return self.doFetchAll()[0][0]

    def getRevisionID(self, projectID, tag):
        return self.getTagID(projectID, tag, "release")

    def getRCID(self, projectID, tag):
        return self.getTagID(projectID, tag, "rc")

    def getReleaseRangeID(self, projectID, revisionIDs):
        """Given a pair of release IDs, determine the release range ID"""
        self.doExec("SELECT id FROM release_range WHERE projectId=%s " +
                    "AND releaseStartId=%s AND releaseEndId=%s",
                    (projectID, revisionIDs[0], revisionIDs[1]))
        if self.cur.rowcount != 1:
            raise Exception("Release range from '{r[0]}' to '{r[1]}' is {c} "
                    "times in the DB!".
                    format(r=revisionIDs, c=self.cur.rowcount))
        return self.doFetchAll()[0][0]

    def get_release_range(self, project_id, range_id):
        self.doExec(
            "SELECT st.tag, nd.tag, rc.tag FROM release_range "
            "LEFT JOIN release_timeline AS st ON st.id=releaseStartId "
            "LEFT JOIN release_timeline AS nd ON nd.id=releaseEndId "
            "LEFT JOIN release_timeline AS rc ON rc.id=releaseRCStartId "
            "WHERE release_range.projectId=%s AND release_range.id=%s",
            (project_id, range_id))
        ranges = self.doFetchAll()
        if self.cur.rowcount == 0:
            raise Exception("Range id {} not found!".format(project_id))
        return ranges[0]

    def update_release_timeline(self, project, tagging, revs, rcs,
            recreate_project=False):
        '''
        For a project, update the release timeline table with the given
        revisions. If existing releases/rcs from the timeline are not in
        order, the conservative approach is taken and the whole project is
        recreated to avoid inconsistencies.

        Returns true if the project had to be recreated.
        '''
        assert len(revs) >= 2
        assert len(revs) == len(rcs)
        rcs = [rc if rc else rev for rc, rev in zip(rcs, revs)]
        pid = self.getProjectID(project, tagging)

        if not recreate_project:
            # First check if the release timeline is sane and in order
            self.doExec("SELECT tag FROM release_timeline WHERE projectId=%s "
                        "AND type='release' ORDER BY id", (pid,))
            tags = [tag for (tag,) in self.doFetchAll()]
            if len(set(tags)) != len(tags):
                log.error("Database corrupted: Duplicate release entries in "
                        "release_timeline! Recreating project.")
                recreate_project = True
            if len(tags) == 0:
                recreate_project = True

        # Check that the tags are in the same order
        if not recreate_project:
            for i, tag in enumerate(tags):
                if i >= len(revs):
                    log.warning("List of revisions to analyse was shortened.")
                    break
                if revs[i] != tag:
                    log.error("Release number {} changed tag from {} to "
                              "{}. Recreating project.".
                              format(i, tag, revs[i]))
                    recreate_project = True
                    break

        # Check that the RC tags are in order
        if not recreate_project:
            self.doExec("SELECT tag FROM release_timeline WHERE "
                        "projectId=%s AND type='rc' ORDER BY id", (pid,))
            rctags = [tag for (tag,) in self.doFetchAll()]
            if len(set(rctags)) != len(rctags):
                log.error("Database corrupted: Duplicate RC entries in "
                          "release_timeline! Recreating project.")
                recreate_project = True

        # Check for changes in release candidates
        # Note that the first RC is unused, since it refers to the end
        # of a previous period
        if not recreate_project:
            for i, tag in enumerate(rctags):
                if i+1 >= len(rcs):
                    log.warning("List of release candidates to analyse "
                                "was shortened.")
                    break
                if rcs[i+1] != tag:
                    log.error("Release candidate number {} changed tag "
                              "from {} to {}. Recreating project.".
                              format(i, tag, rcs[i+1]))
                    recreate_project = True
                    break

        # Go through the release ranges and check if they have changed
        if not recreate_project:
            self.doExec(
                "SELECT st.tag, nd.tag, rc.tag FROM release_range "
                "LEFT JOIN release_timeline AS st ON st.id=releaseStartId "
                "LEFT JOIN release_timeline AS nd ON nd.id=releaseEndId "
                "LEFT JOIN release_timeline AS rc ON rc.id=releaseRCStartId "
                "WHERE release_range.projectId=%s ORDER BY release_range.id",
                (pid,))
            ranges = self.doFetchAll()
            if len(set(ranges)) != len(tags)-1:
                log.error("Database corrupted: Number of release ranges"
                          " does not match number of release tags!")
                recreate_project = True

            for i, (start, end, rc) in enumerate(self.doFetchAll()):
                if i+1 >= len(revs) or recreate_project:
                    # List of revisions to analyse was shortened
                    break
                if (start, end) != (revs[i], revs[i+1]):
                    log.error("Release range {} changed from {} to {}."
                              " Recreating project.".
                              format(i, (start, end), (revs[i], revs[i+1])))
                    recreate_project = True
                    break
                if rc != rcs[i+1]:
                    log.error("Release candidate {} changed from {} to {}."
                              " Recreating project.".
                              format(i, rc, rcs[i+1]))
                    recreate_project = True
                    break

        # Recreate project if necessary
        if recreate_project:
            # This should ripple through the database and delete
            # all referencing entries for project
            log.warning("Deleting and re-creating project {}/{}.".
                    format(project, tagging))
            self.doExecCommit("DELETE FROM `project` WHERE id=%s", (pid,))
            pid = self.getProjectID(project, tagging)
            tags = []
            rctags = []

        # at this point we have verified that the first len(tags)
        # entries are identical
        new_ranges_to_process = []
        if len(revs) > len(tags):
            n_new = len(revs) - len(tags)
            log.info("Adding {} new releases...".format(n_new))
            previous_rev = None
            if len(tags) > 0:
                previous_rev = tags[-1]
            for rev, rc in zip(revs, rcs)[len(tags):]:
                self.doExecCommit("INSERT INTO release_timeline "
                                    "(type, tag, projectId) "
                                    "VALUES (%s, %s, %s)",
                                    ("release", rev, pid))

                if previous_rev is not None and rc:
                    self.doExecCommit("INSERT INTO release_timeline "
                                        "(type, tag, projectId) "
                                        "VALUES (%s, %s, %s)",
                                        ("rc", rc, pid))

                if previous_rev is not None:
                    startID = self.getRevisionID(pid, previous_rev)
                    endID = self.getRevisionID(pid, rev)
                    if rc:
                        rcID = self.getRCID(pid, rc)
                    else:
                        rcID = "NULL"
                    self.doExecCommit("INSERT INTO release_range "
                                        "(releaseStartId, releaseEndId, "
                                        "projectId, releaseRCStartId) "
                                        "VALUES (%s, %s, %s, %s)",
                                        (startID, endID, pid, rcID))
                    new_ranges_to_process.append(self.getReleaseRangeID(pid,
                            (startID, endID)))
                previous_rev = rev
        # now we are in a well-defined state.
        # Return the ids of the release ranges we have to process
        return new_ranges_to_process

def tstamp_to_sql(tstamp):
    """Convert a Unix timestamp into an SQL compatible DateTime string"""
    return(datetime.utcfromtimestamp(tstamp).strftime("%Y-%m-%d %H:%M:%S"))
