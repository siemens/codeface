from tempfile import mkdtemp
from shutil import rmtree
from collections import namedtuple
from subprocess import check_call
from textwrap import dedent
from os import getcwd, chdir, listdir, unlink, getenv, makedirs, environ
from os.path import split as pathsplit, join as pathjoin, isdir, exists, basename

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
    def authors(self):
        return self._authors
