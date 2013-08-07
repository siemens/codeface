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

def get_example_project_1(tagging="tag"):
    project = GitProject(tagging)
    Adam = project.add_author("Adam Awkward", "adam@awkward.net")
    Bill = project.add_author("Bill Bully", "bill@bullies.org")
    Clara = project.add_author("Clara Confident", "clara@foo.org")
    Max = project.add_author("Max Maintainer", "max@theboss.com")
    Peter = project.add_author("Peter Popular", "peter@gmail.com")
    project.commit(Adam, Adam, "2013-01-07T16:00:00",
            {"README":"FOO\nBOO"}, signoff=[Adam])
    project.tag_rc(Adam, "2013-01-07T16:30:00")
    project.tag_release(Adam, "2013-01-07T16:60:00")
    project.commit(Adam, Adam, "2013-01-08T15:00:00",
            {"README":"Foo\nBoo",
            "src/code.c":"int main() {return 0;};"},
            signoff=[Adam])
    project.tag_rc(Adam, "2013-01-08T15:30:00")
    project.commit(Bill, Bill, "2013-01-09T15:30:00",
            {"README":"Foo\nBoo",
            "src/code.c":"int main() {return 0;};",
            "src/carp.c":"int main() {return -1;};"
            },
            signoff=[Bill, Clara])
    project.commit(Adam, Adam, "2013-01-10T18:00:00",
            {"README":"Foo\nBoo",
            "src/code.c":"int main() {\n    return 0;\n};"
            },
            signoff=[Adam, Peter])
    project.tag_release(Adam, "2013-01-10T18:40:00")
    project.commit(Max, Adam, "2013-01-14T12:30:42",
            {"README":"Foo\nBoo",
            "src/code.c":"int main() {\n    return 0;\n};",
            "src/newcode.c":"int answer() {\n    return 42;\n};"
            },
            signoff=[Adam, Max])
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
    project.commit(Max, Max, "2013-01-21T12:50:42",
            {"README":"Foo\nBoo\nGoo.",
            "src/code.c":"int main() {\n    return 0;\n};",
            "src/answer.c":"int answer() {\n    return 42;\n};"
            },
            signoff=[Adam, Max])
    project.tag_release(Max, "2013-01-23T13:42:42")
    return project
