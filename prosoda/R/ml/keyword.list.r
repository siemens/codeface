## This file is part of prosoda.  prosoda is free software: you can
## redistribute it and/or modify it under the terms of the GNU General Public
## License as published by the Free Software Foundation, version 2.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
##
## Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

## TODO: Integrate these lists into a generic analysis class, and
## create the ability to select custom subsets for each mailing list
## C/C++-specific terms
terms.c <- c("int", "void", "double", "long", "const", "return",
             "function", "char", "struct", "includ", "class", "templat",
             "type", "static", "array")
terms.programming <- c("code", "patch", "sourc", "compil", "build", "file",
                       "check", "call", "declar", "line", "argument", "version",
                       "foo")
## Colloquial/conversational terms
terms.coll <- c("pleas", "thank", "cheer", "instead", "look", "base", "add",
                "follow", "tri", "note", "use", "project")
