# This file is part of prosoda.  prosoda is free software: you can
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

from tempfile import mkdtemp, NamedTemporaryFile
from shutil import rmtree
from collections import namedtuple
from subprocess import check_call
from textwrap import dedent
from os import getcwd, chdir, listdir, unlink, getenv, makedirs, environ
from os.path import split as pathsplit, join as pathjoin, isdir, exists, basename
from datetime import datetime
from time import strptime

_Author = namedtuple("_Author", ["name", "email"])
class Author(_Author):
    def __str__(self):
        return "{a.name} <{a.email}>".format(a=self)
Commit = namedtuple("Commit", ["author", "committer", "datetime", "filetree", "signoff", "tags"])
Tag = namedtuple("Tag", ["author", "datetime", "type"])

class GitProject(object):
    '''
    Represents a Git repository with utility methods to create fake histories
    with fake authors, committers, rc and release tags.
    To get a physical git repository, use an instance of ExampleProject
    as a context manager using the with statement. This ensures that the git
    repository is properly deleted at the end of testing.
    '''
    def __init__(self, tagging="tag"):
        '''Creates a repository with no commits'''
        self._authors = []
        self._commits = []
        self._tagging = tagging
        self._mboxes = {}

    def __enter__(self):
        '''
        This function is called when entering a with statement.
        Here a real git repository is created and populated with the
        information in the project.
        '''
        self.directory = mkdtemp(prefix="prosoda_test_project")
        cwd = getcwd()
        try:
            chdir(self.directory)
            def git(cmds, committer=None, commitdate=None):
                if committer or commitdate:
                    env = dict(environ)
                    if committer:
                        env["GIT_COMMITTER_NAME"] = committer.name
                        env["GIT_COMMITTER_EMAIL"] = committer.email
                    if commitdate:
                        env["GIT_COMMITTER_DATE"] = commitdate
                    check_call(["git"] + cmds, env=env)
                else:
                    check_call(["git"] + cmds)

            git(["init"])
            next_release = 0
            next_rc = 0
            release_tags = []
            rc_tags = {}
            for i, c in enumerate(self._commits):
                # First, clear the directory
                for f in listdir("."):
                    if f != ".git":
                        if isdir(f):
                            rmtree(f)
                        else:
                            unlink(f)
                # Insert the files specified in the commits filetree
                for f, content in c.filetree.iteritems():
                    dn, fn = pathsplit(f)
                    if dn and not exists(dn):
                        makedirs(dn)
                    with file(f, "w") as fd:
                        fd.write(content)
                # Perform the commit
                git("add -A .".split())
                commitmsg = "Commit {}\n\nCommit message\n\n".format(i)
                for signer in c.signoff:
                    commitmsg += "Signed-off-by: {}\n".format(str(signer))
                git(["commit",
                     "--author", str(c.author),
                     "--date", c.datetime,
                     "-m", commitmsg],
                    committer=c.committer, commitdate=c.datetime)
                # Tag the commit
                for tag in c.tags:
                    name = "v{}_{}".format(next_release, tag.type)
                    if tag.type == "rc":
                        name += "_{}".format(next_rc)
                        rc_tags.setdefault(next_release, name) # do not overwrite first rc
                        next_rc += 1
                    elif tag.type == "release":
                        release_tags.append(name)
                        next_release += 1
                    git(["tag", name])
            # Create prosoda test configuration
            configuration = dedent("""
            ---
            project: {project}
            repo: {project} # Relative to git-dir as specified on the command line
            description: {project} Description
            mailinglists:
                -   name: {project}.dev1
                    type: dev
                    source: generated
                -   name: {project}.dev2
                    type: dev
                    source: generated
                -   name: {project}.user1
                    type: user
                    source: generated
                -   name: {project}.user2
                    type: user
                    source: generated
            revisions: {release_tags}
            rcs : {rctags}
            tagging: {tagging}
            """.format(release_tags=str(release_tags),
                       tagging=self._tagging,
                       rctags=str([rc_tags.get(i, release_tags[i]) for i in range(len(release_tags))]),
                       project=basename(self.directory)
                )
            )
            with file(self.prosoda_conf, "w") as fd:
                fd.write(configuration)
            for ml_name, ml_file in self.mboxes:
                with file(ml_file, "w") as fd:
                    fd.write(self.mbox_contents(ml_name))
        finally:
            chdir(cwd)

    def __exit__(self, exc_type, exc_value, traceback):
        if exc_type is None:
            rmtree(self.directory)
        else:
            print("Left directory '{}' for inspection.".format(self.directory))
        del self.directory

    def add_author(self, name, email):
        author = Author(name, email)
        self._authors.append(author)
        return author

    def commit(self, author, committer, datetime, filetree, signoff):
        self._commits.append(Commit(author, committer, datetime, filetree, signoff, []))

    def tag_rc(self, author, datetime):
        self._commits[-1].tags.append(Tag(author, datetime, "rc"))

    def tag_release(self, author, datetime):
        self._commits[-1].tags.append(Tag(author, datetime, "release"))

    @property
    def gitdir(self):
        return self.directory

    @property
    def prosoda_conf(self):
        return pathjoin(self.directory, ".git", "testproject.conf")

    @property
    def name(self):
        return basename(self.directory)

    @property
    def mboxes(self):
        project = basename(self.directory)
        return [(name, pathjoin(self.directory, ".git",
                 ".".join((project, name, "mbox"))))
                for name in ("dev1", "dev2", "user1", "user2")]

    @property
    def authors(self):
        return self._authors

    def email(self, mlist, sender, date, subject, content):
        cdate = datetime(*strptime(date, "%Y-%m-%dT%H:%M:%S")[:6]).strftime("%a, %d %b %Y %H:%M:%S")
        self._mboxes.setdefault(mlist, []).append(dedent(
        """
        From MAILER-DAEMON Thu Jul 18 13:48:48 2013
        Path: example.com!not-for-mail
        From: {sender}
        Newsgroups: gmane.prosoda.test.project
        Subject: {subject}
        Date: {date}
        Approved: auto
        Message-ID: <{messageid}@example.com>
        NNTP-Posting-Host: example.com
        Mime-Version: 1.0
        Content-Type: text/plain; charset=us-ascii; format=flowed
        Content-Transfer-Encoding: 7bit
        X-Complaints-To: complaints@example.com
        NNTP-Posting-Date: {date}
        User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:0.9.8) Gecko/20020205
        X-Accept-Language: en-us
        Original-To: prosoda.test.project@example.com
        Precedence: bulk
        X-Mailing-List: prosoda.test.project@example.com

        {content}""").
        format(date=cdate, sender=sender, subject=subject,
               content=content, messageid=len(self._mboxes.get(mlist, []))))

    def mbox_contents(self, mlist):
        return ("\n\n".join(self._mboxes.get(mlist, [])) + "\n\n").lstrip()

