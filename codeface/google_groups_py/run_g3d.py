#! /usr/bin/env python
## This file is part of Codeface. Codeface is free software: you can
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
## Copyright 2016 by Georg Berner <georgberner@gmx.net>
## All Rights Reserved.

from g3d import Crawler
import sys

def main():
    argv = sys.argv
    crawler = Crawler()
    crawler.handle_arguments(argv)
    if crawler.execute is True:
        crawler.start()
        crawler.statistic()

main()
