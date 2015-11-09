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

"""
Command-line interface driver for the codeface package

Provides parser for run and ml options
If codeface is run as backend for the webservice dynamic parser is used
"""

import argparse
import unittest
import os
from pkg_resources import resource_filename
from codeface.logger import set_log_level, start_logfile, log
from codeface.util import execute_command
from codeface.project import project_analyse, mailinglist_analyse


def get_parser():
    """ Create parser with required command line arguments
    Returns: Parser for the command line interface
    """
    parser = argparse.ArgumentParser(
        prog='codeface',
        description='Program for Social Data Analysis')
    parser.add_argument(
        '-l', '--loglevel',
        help='Choose the logging level',
        choices=['debug', 'devinfo', 'info', 'warning', 'error'],
        default='info')
    parser.add_argument(
        '-f', '--logfile',
        help='Save all debug logging into the given log file')
    parser.add_argument(
        '-j', '--jobs',
        help='Number of cores to use in parallel',
        default=1)

    sub_parser = parser.add_subparsers(
        help='select action')
    test_parser = sub_parser.add_parser(
        'test',
        help='Run tests')
    test_parser.set_defaults(
        func=cmd_test)
    test_parser.add_argument(
        '-c', '--config',
        help="Codeface configuration file",
        required=True)
    test_parser.add_argument(
        '-p', '--pattern',
        help='run only tests matching the given pattern',
        default="*")
    test_parser.add_argument(
        '-u', '--unit',
        help='run only unit tests',
        action='store_true')

    run_parser = sub_parser.add_parser(
        'run',
        help='Run analysis')
    run_parser.set_defaults(
        func=cmd_run)
    run_parser.add_argument(
        '-c', '--config',
        help="Codeface configuration file",
        required=True)
    run_parser.add_argument(
        '--tagging',
        help=("Overrides the tagging configuration within the CLI. "
              "When used this parameter overrides the configured tagging, "
              "default is fallback to configuration value"),
        default='default')
    run_parser.add_argument(
        '-p', '--project',
        help="Project configuration file",
        required=True)
    run_parser.add_argument(
        'resdir',
        help="Directory to store analysis results in")
    run_parser.add_argument(
        'gitdir',
        help="Directory for git repositories")
    run_parser.add_argument(
        '--no-report',
        help="Skip LaTeX report generation (and dot compilation)",
        action="store_true")
    run_parser.add_argument(
        '--recreate',
        help="Force a delete of the project in the database",
        action="store_true")
    run_parser.add_argument(
        '--profile-r',
        help="Compute an execution time profile for R code",
        action="store_true")
    run_parser.add_argument(
        '--reuse-vcs-analysis',
        help=("Re-use an already existing vcs-analysis.db file. "
              "This flag is useful to continue a previously failed analysis"
              " or for debugging purposes."),
        action='store_true',
        dest="reuse_db")

    ml_parser = sub_parser.add_parser(
        'ml',
        help='Run mailing list analysis')
    ml_parser.set_defaults(
        func=cmd_ml)
    ml_parser.add_argument(
        '-c', '--config',
        help="Codeface configuration file",
        required=True)
    ml_parser.add_argument(
        '-p', '--project',
        help="Project configuration file",
        required=True)
    ml_parser.add_argument(
        '-m', '--mailinglist',
        help=("Only run on the specified mailing list "
              "(can be specified multiple times)"),
        action="append",
        default=[])
    ml_parser.add_argument(
        'resdir',
        help="Directory to store analysis results in")
    ml_parser.add_argument(
        'mldir',
        help="Directory for mailing lists")

    dyn_parser = sub_parser.add_parser(
        'dynamic',
        help='Start R server for a dynamic graph')
    dyn_parser.set_defaults(
        func=cmd_dynamic)
    dyn_parser.add_argument(
        '-c', '--config',
        help="Codeface configuration file",
        required=True)
    dyn_parser.add_argument(
        'graph',
        help="graph to show",
        nargs='?',
        default=None)
    dyn_parser.add_argument(
        '-l', '--list',
        help="list available graphs",
        action="store_true")
    dyn_parser.add_argument(
        '-p', '--port',
        help="Pass this to R as port to listen on",
        default="8100")
    return parser


def cmd_run(args):
    """Dispatch the ``run`` command.

    Args:
        args:
    """
    # First make all the args absolute, variant a
    resdir, gitdir = map(os.path.abspath, (args.resdir, args.gitdir))
    codeface_conf, project_conf = map(os.path.abspath,
                                      (args.config, args.project))
    logfile = args.logfile
    if logfile:
        logfile = os.path.abspath(logfile)
    project_analyse(resdir, gitdir, codeface_conf, project_conf,
                    args.no_report, args.loglevel, logfile, args.recreate,
                    args.profile_r, args.jobs, args.tagging, args.reuse_db)
    return 0


def cmd_ml(args):
    """Dispatch the ``ml`` command.

    Args:
        args:
    """
    # First make all the args absolute
    resdir, mldir = map(os.path.abspath, (args.resdir, args.mldir))
    codeface_conf, project_conf = map(os.path.abspath,
                                      (args.config, args.project))
    logfile = args.logfile
    if logfile:
        logfile = os.path.abspath(logfile)
    mailinglist_analyse(resdir, mldir, codeface_conf, project_conf,
                        args.loglevel, logfile, args.jobs, args.mailinglist)
    return 0


def cmd_dynamic(args):
    """ Executes if Codeface is run to provide the backend to the website
    Args:
        args:

    Returns:
    """
    dyn_directory = resource_filename(__name__, "R/shiny/apps")

    if args.graph is None and not args.list:
        log.critical("No dynamic graph given!")

    if args.list or args.graph is None:
        print 'List of possible dynamic graphs:'
        for sorted_member in sorted(os.listdir(dyn_directory)):
            if os.path.isdir(os.path.join(dyn_directory, sorted_member)):
                print " * " + sorted_member
        return 1

    cwd = os.path.join(dyn_directory, args.graph)
    cfg = os.path.abspath(args.config)
    if not os.path.exists(cwd):
        log.critical('Path "%s" not found!', cwd)
        return 1
    r_code = "library(shiny); runApp(host='0.0.0.0', port={})".format(args.port)
    cmd = ["Rscript", "-e", r_code, "-c", cfg]
    execute_command(cmd, direct_io=True, cwd=cwd)
    return 0


def cmd_test(args):
    """Sub-command handler for the ``test`` command.

    Args:
        args:
    """
    unit_only = args.unit
    pattern = args.pattern
    config_file = os.path.abspath(args.config)
    del args
    test_path = os.path.join(os.path.dirname(__file__), 'test')
    print '\n===== running unittests =====\n'
    tests = unittest.TestLoader().discover(os.path.join(test_path, 'unit'),
                                           pattern='test_{}.py'.format(pattern),
                                           top_level_dir=test_path)
    unit_result = unittest.TextTestRunner(verbosity=1).run(tests)
    unit_success = not (unit_result.failures or unit_result.errors)
    if unit_only:
        if unit_success:
            print '\n===== unit tests succeeded :) ====='
        else:
            print'\n===== unit tests failed :( ====='
        return 0 if unit_success else 1
    print '\n===== running integration tests =====\n'
    tests = unittest.TestLoader().discover(
        os.path.join(test_path, 'integration'),
        pattern='test_{}.py'.format(pattern), top_level_dir=test_path)

    def set_config(suite):
        """Set the testing configuration file as member variable
        for all integration tests, since we need the DB and REST configurations
        Args:
            suite: testsuite to run

        Returns:
        """
        if isinstance(suite, unittest.TestSuite):
            for test in suite:
                set_config(test)
        suite.config_file = config_file

    set_config(tests)
    int_result = unittest.TextTestRunner(verbosity=2).run(tests)
    int_success = not (int_result.failures or int_result.errors)
    if unit_success and int_success:
        print '\n===== all tests succeeded :) ====='
    else:
        print '\n===== some tests failed :( ====='
    return 0 if unit_success and int_success else 1


def run(argv):
    """Creates and applies parser for command line arguments
    Args:
        argv:

    Returns:

    """
    parser = get_parser()
    # Note: The first argument of argv is the name of the command
    args = parser.parse_args(argv[1:])
    set_log_level(args.loglevel)
    if args.logfile:
        start_logfile(args.logfile, 'debug')
    return args.func(args)


def main():
    """ main fuction for command line interface
    Returns:

    """
    import sys
    return run(sys.argv)
