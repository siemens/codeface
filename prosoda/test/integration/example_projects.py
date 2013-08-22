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

from .gitproject import GitProject

example_project_func = {}

def get_example_project_1(tagging="tag", randomise_email_case=False):
    project = GitProject(tagging, randomise_email_case)
    Adam = project.add_author("Adam Awkward", "adam@awkward.net")
    Bill = project.add_author("Bill Bully", "bill@bullies.org")
    Clara = project.add_author("Clara Confident", "clara@foo.org")
    Max = project.add_author("Max Maintainer", "max@theboss.com")
    Peter = project.add_author("Peter Popular", "peter@gmail.com")
    project.email("dev1", Adam, "2013-01-07T15:00:00", "Project start", "Foo!")
    project.commit(Adam, Adam, "2013-01-07T16:00:00",
            {"README":"FOO\nBOO"}, signoff=[Adam])
    project.tag_rc(Adam, "2013-01-07T16:30:00")
    project.tag_release(Adam, "2013-01-07T16:59:00")
    project.email("dev1", Adam, "2013-01-08T14:00:00", "Mail 2a commit", "A commit!")
    project.email("dev1", Adam, "2013-01-08T14:00:01", "Mail 2b bug", "A bug!")
    project.email("dev1", Adam, "2013-01-08T14:00:02", "Mail 2c feature", "A feature!")
    project.email("dev1", Adam, "2013-01-08T14:00:03", "Mail 2d Message", "A message!")
    project.email("dev1", Adam, "2013-01-08T14:00:04", "Mail 2e bug", "A bug!")
    project.commit(Adam, Adam, "2013-01-08T15:00:00",
            {"README":"Foo\nBoo",
            "src/code.c":"int main() {return 0;};"},
            signoff=[Adam])
    project.tag_rc(Adam, "2013-01-08T15:30:00")
    project.email("dev1", Bill, "2013-01-09T14:00:00", "Re: Mail 2b bug", "A fix for the bug!")
    project.commit(Bill, Bill, "2013-01-09T15:30:00",
            {"README":"Foo\nBoo",
            "src/code.c":"int main() {return 0;};",
            "src/carp.c":"int main() {return -1;};"
            },
            signoff=[Bill, Clara])
    project.email("dev1", Adam, "2013-01-10T14:00:00", "Re: revert 2a commit", "I will revert the commit!")
    project.email("dev1", Adam, "2013-01-10T14:00:01", "Re: revert 2b bug", "I will revert the bug!")
    project.email("dev1", Adam, "2013-01-10T14:00:02", "Re: revert 2c feature", "I will revert the feature!")
    project.email("dev1", Adam, "2013-01-10T14:00:03", "Re: revert 2d Message", "I will revert the message!")
    project.email("dev1", Adam, "2013-01-10T14:00:04", "Re: revert 2e bug", "I will revert the bug!")
    project.email("dev1", Bill, "2013-01-10T14:05:04", "Re: Re: revert 2e bug", "Nooo!")
    project.email("dev1", "=2134garbled-email-23489", "2013-01-10T14:05:04", "Spam", "foo")
    project.commit(Adam, Adam, "2013-01-10T18:00:00",
            {"README":"Foo\nBoo",
            "src/code.c":"int main() {\n    return 0;\n};"
            },
            signoff=[Adam, Peter])

    project.tag_release(Adam, "2013-01-10T18:40:00")
    project.email("dev1", Adam, "2013-01-10T19:00:00", "Release 1", "Release.")
    project.commit(Max, Adam, "2013-01-14T12:30:42",
            {"README":"Foo\nBoo",
            "src/code.c":"int main() {\n    return 0;\n};",
            "src/newcode.c":"int answer() {\n    return 42;\n};"
            },
            signoff=[Adam, Max])
    project.email("dev2", Max, "2013-01-14T19:00:00", "commit", "A commit.")
    project.email("dev2", Max, "2013-01-14T19:00:01", "commit A feature", "A feature commit.")
    project.email("dev2", Max, "2013-01-14T19:00:02", "commit B Subsystem", "A subsystem commit.")
    project.email("dev2", Max, "2013-01-14T19:00:03", "commit C Foo", "A Foo commit.")
    project.email("dev2", Max, "2013-01-14T19:00:04", "commit D bumblebee", "A bumblebee commit.")
    project.email("dev2", Adam, "2013-01-14T19:20:00", "Re: commit D bumblebee", "Comment on bumblebee flying abilities.")
    project.email("dev2", Max, "2013-01-14T19:40:00", "Re: commit D bumblebee", "Explanation of bumblebee.")
    project.commit(Max, Max, "2013-01-14T12:50:42",
            {"README":"Foo\nBoo",
            "src/code.c":"int main() {\n    return 0;\n};",
            "src/answer.c":"int answer() {\n    return 42;\n};"
            },
            signoff=[Adam, Max])
    project.commit(Clara, Max, "2013-01-15T12:42:42",
            {"README":"Foo\nBoo",
            "src/code.c":"int main() {\n    return 0;\n};",
            "src/answer.c":"int answer() {\n    return 42;\n};",
            "Makefile": "all: src/code.o\n};"
            },
            signoff=[Adam, Max, Clara])
    project.tag_rc(Max, "2013-01-15T13:42:42")
    project.email("dev2", Max, "2013-01-15T13:43:00", "RC", "A RC.")
    project.email("dev2", Max, "2013-01-15T13:43:01", "RC A bug", "A RC bug.")
    project.email("dev2", Max, "2013-01-15T13:43:02", "RC B", "A RC.")
    project.email("dev2", Max, "2013-01-15T13:43:03", "RC C", "A RC.")
    project.email("dev2", Max, "2013-01-15T13:43:04", "RC D", "A RC.")
    project.email("dev2", Clara, "2013-01-15T14:00:00", "Re: RC A bug", "Nice bug :)")
    project.commit(Max, Max, "2013-01-21T12:50:42",
            {"README":"Foo\nBoo\nGoo.",
            "src/code.c":"int main() {\n    return 0;\n};",
            "src/answer.c":"int answer() {\n    return 42;\n};"
            },
            signoff=[Adam, Max])

    # These emails are simultaneous on purpose
    project.email("dev2", Max, "2013-01-23T13:40:00", "Release 2a", "A Release.")
    project.email("dev2", Max, "2013-01-23T13:40:00", "Release 2b", "A Release.")
    project.email("dev2", Max, "2013-01-23T13:40:00", "Release 2c", "A Release.")
    project.email("dev2", Max, "2013-01-23T13:40:00", "Release 2d", "A Release.")
    project.email("dev2", Max, "2013-01-23T13:40:00", "Release 2e", "A Release.")
    project.tag_release(Max, "2013-01-23T13:42:42")
    return project

example_project_func[1] = get_example_project_1

def get_example_project_2(tagging="tag"):
    '''
    This project is an example where the mailing list is too monotonous for
    prosoda and several calculations fail. Since this is unlikely to happen
    in any real project, we leave it at that for the moment
    '''
    project = GitProject(tagging)
    Adam = project.add_author("Adam Awkward", "adam@awkward.net")
    Bill = project.add_author("Bill Bully", "bill@bullies.org")
    Clara = project.add_author("Clara Confident", "clara@foo.org")
    Max = project.add_author("Max Maintainer", "max@theboss.com")
    Peter = project.add_author("Peter Popular", "peter@gmail.com")
    project.email("dev1", Adam, "2013-01-07T15:00:00", "Project start", "Foo!")
    project.commit(Adam, Adam, "2013-01-07T16:00:00",
            {"README":"FOO\nBOO"}, signoff=[Adam])
    project.tag_rc(Adam, "2013-01-07T16:30:00")
    project.tag_release(Adam, "2013-01-07T16:59:00")
    project.email("dev1", Adam, "2013-01-08T14:00:00", "Mail 2a", "A commit!")
    project.email("dev1", Adam, "2013-01-08T14:00:01", "Mail 2b", "A commit!")
    project.email("dev1", Adam, "2013-01-08T14:00:02", "Mail 2c", "A commit!")
    project.email("dev1", Adam, "2013-01-08T14:00:03", "Mail 2d", "A commit!")
    project.email("dev1", Adam, "2013-01-08T14:00:04", "Mail 2e", "A commit!")
    project.commit(Adam, Adam, "2013-01-08T15:00:00",
            {"README":"Foo\nBoo",
            "src/code.c":"int main() {return 0;};"},
            signoff=[Adam])
    project.tag_rc(Adam, "2013-01-08T15:30:00")
    project.email("dev1", Bill, "2013-01-09T14:00:00", "Re: Mail 2", "A fix!")
    project.commit(Bill, Bill, "2013-01-09T15:30:00",
            {"README":"Foo\nBoo",
            "src/code.c":"int main() {return 0;};",
            "src/carp.c":"int main() {return -1;};"
            },
            signoff=[Bill, Clara])
    project.email("dev1", Adam, "2013-01-10T14:00:00", "Re: Mail 2a", "I will revert!")
    project.email("dev1", Adam, "2013-01-10T14:00:01", "Re: Mail 2b", "I will revert!")
    project.email("dev1", Adam, "2013-01-10T14:00:02", "Re: Mail 2c", "I will revert!")
    project.email("dev1", Adam, "2013-01-10T14:00:03", "Re: Mail 2d", "I will revert!")
    project.email("dev1", Adam, "2013-01-10T14:00:04", "Re: Mail 2e", "I will revert!")
    project.commit(Adam, Adam, "2013-01-10T18:00:00",
            {"README":"Foo\nBoo",
            "src/code.c":"int main() {\n    return 0;\n};"
            },
            signoff=[Adam, Peter])

    project.tag_release(Adam, "2013-01-10T18:40:00")
    project.email("dev1", Adam, "2013-01-10T19:00:00", "Release 1a", "Released.")
    project.email("dev1", Adam, "2013-01-10T19:00:01", "Release 1b", "Released.")
    project.email("dev1", Adam, "2013-01-10T19:00:02", "Release 1c", "Released.")
    project.email("dev1", Adam, "2013-01-10T19:00:03", "Release 1d", "Released.")
    project.email("dev1", Adam, "2013-01-10T19:00:04", "Release 1e", "Released.")
    project.commit(Max, Adam, "2013-01-14T12:30:42",
            {"README":"Foo\nBoo",
            "src/code.c":"int main() {\n    return 0;\n};",
            "src/newcode.c":"int answer() {\n    return 42;\n};"
            },
            signoff=[Adam, Max])
    project.email("dev2", Max, "2013-01-14T19:00:00", "Commit", "A commit.")
    project.email("dev2", Max, "2013-01-14T19:00:01", "Commit A", "A commit.")
    project.email("dev2", Max, "2013-01-14T19:00:02", "Commit B", "A commit.")
    project.email("dev2", Max, "2013-01-14T19:00:03", "Commit C", "A commit.")
    project.email("dev2", Max, "2013-01-14T19:00:04", "Commit D", "A commit.")
    project.email("dev2", Adam, "2013-01-14T19:20:00", "Re: Commit", "Comment.")
    project.email("dev2", Max, "2013-01-14T19:40:00", "Re: Commit", "Comment too.")
    project.commit(Max, Max, "2013-01-14T12:50:42",
            {"README":"Foo\nBoo",
            "src/code.c":"int main() {\n    return 0;\n};",
            "src/answer.c":"int answer() {\n    return 42;\n};"
            },
            signoff=[Adam, Max])
    project.commit(Clara, Max, "2013-01-15T12:42:42",
            {"README":"Foo\nBoo",
            "src/code.c":"int main() {\n    return 0;\n};",
            "src/answer.c":"int answer() {\n    return 42;\n};",
            "Makefile": "all: src/code.o\n};"
            },
            signoff=[Adam, Max, Clara])
    project.tag_rc(Max, "2013-01-15T13:42:42")
    project.email("dev2", Max, "2013-01-15T13:43:00", "RC", "A RC.")
    project.email("dev2", Max, "2013-01-15T13:43:01", "RC A", "A RC.")
    project.email("dev2", Max, "2013-01-15T13:43:02", "RC B", "A RC.")
    project.email("dev2", Max, "2013-01-15T13:43:03", "RC C", "A RC.")
    project.email("dev2", Max, "2013-01-15T13:43:04", "RC D", "A RC.")
    project.email("dev2", Clara, "2013-01-15T14:00:00", "Re: RC", "Nice :)")
    project.commit(Max, Max, "2013-01-21T12:50:42",
            {"README":"Foo\nBoo\nGoo.",
            "src/code.c":"int main() {\n    return 0;\n};",
            "src/answer.c":"int answer() {\n    return 42;\n};"
            },
            signoff=[Adam, Max])

    # These emails are simultaneous on purpose
    project.email("dev2", Max, "2013-01-23T13:40:00", "Release 2a", "A Release.")
    project.email("dev2", Max, "2013-01-23T13:40:00", "Release 2b", "A Release.")
    project.email("dev2", Max, "2013-01-23T13:40:00", "Release 2c", "A Release.")
    project.email("dev2", Max, "2013-01-23T13:40:00", "Release 2d", "A Release.")
    project.email("dev2", Max, "2013-01-23T13:40:00", "Release 2e", "A Release.")
    project.tag_release(Max, "2013-01-23T13:42:42")
    return project

example_project_func[2] = get_example_project_2
