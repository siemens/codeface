#!/usr/bin/env python

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
# Copyright 2013 by Siemens AG, Johannes Ebke <johannes.ebke.ext@siemens.com>
# All Rights Reserved.

from setuptools import setup

setup(name='codeface',
      version='0.2.0',
      description='Project for Social Data Analysis',
      author='Wolfgang Mauerer',
      author_email='wolfgang.mauerer@siemens.com',
      url='https://github.com/wolfgangmauerer/codeface',
      packages=['codeface', 'codeface.cluster'],
      package_data={'codeface': ['R/*.r', 'R/cluster/*.r', 'perl/*.pl']},
      entry_points={'console_scripts': ['codeface = codeface.cli:main']},
      install_requires=['progressbar', 'rpy2', 'matplotlib', 'VCS', 'cppstats',
                'python_ctags','PyYAML', 'MySQL_python']
      )
