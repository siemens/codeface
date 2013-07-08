import unittest
import argparse
import os

def get_parser():
    parser = argparse.ArgumentParser(prog='prosoda',
                description='Program for Social Data Analysis')
    parser.add_argument('--loglevel', help='Choose the logging level',
                choices=['debug', 'info', 'warning', 'error'])

    sub_parser = parser.add_subparsers(help='select action')
    test_parser = sub_parser.add_parser('test', help='Run tests')
    test_parser.set_defaults(func=cmd_test)
    test_parser.add_argument('-p', '--pattern', default="*",
                help='run only tests matching the given pattern')
    test_parser.add_argument('-u', '--unit', action='store_true',
                help='run only unit tests')

    run_parser = sub_parser.add_parser('run', help='Run analysis')
    run_parser.set_defaults(func=cmd_run)
    run_parser.add_argument('resdir',
                        help="Base directory to store analysis results in")
    run_parser.add_argument('gitdir',
                        help="Base directory for git repositories")
    run_parser.add_argument('conf', help="Project specific configuration file")
    run_parser.add_argument('--use_db', action='store_true',
                        help="Re-use existing database")
    run_parser.add_argument('--basedir',
                        help="Base directory where the prosoda infrastructure is found")
    run_parser.add_argument('--dry-run', action="store_true",
                        help="Just show the commands called, don't perform any work")
    run_parser.add_argument('--show-cmds', action="store_true",
                        help="Show commands before executing them")
    run_parser.add_argument('--no-report', action="store_true",
                        help="Skip LaTeX report generation (and dot compilation)")
    return parser


def cmd_run(args):
    print("TODO")

def cmd_test(args):
    unit_only=args.unit
    pattern=args.pattern
    del args
    '''Sub-command handler for the ``test`` command.'''
    test_path = os.path.join(os.path.dirname(__file__), 'test')
    print('\n===== running unittests =====\n')
    tests = unittest.TestLoader().discover(os.path.join(test_path, 'unit'),
        pattern='test_{}.py'.format(pattern), top_level_dir=test_path)
    unittest.TextTestRunner(verbosity=1).run(tests)
    if unit_only:
        return
    print('\n===== running integration tests =====\n')
    tests = unittest.TestLoader().discover(os.path.join(test_path, 'integration'),
        pattern='test_{}.py'.format(pattern), top_level_dir=test_path)
    unittest.TextTestRunner(verbosity=2).run(tests)

def run():
    parser = get_parser()
    args = parser.parse_args()
    args.func(args)
