#!/usr/bin/env python

from setuptools import setup

setup(name='prosoda',
      version='0.2.0',
      description='Project for Social Data Analysis',
      author='Wolfgang Mauerer',
      author_email='wolfgang.mauerer@siemens.com',
      url='https://github.com/wolfgangmauerer/prosoda',
      packages=['prosoda', 'prosoda.cluster'],
      package_data={'prosoda': ['R/*.r', 'R/cluster/*.r', 'perl/*.pl']},
      entry_points={'console_scripts': ['prosoda = prosoda.cli:main']}
     )
