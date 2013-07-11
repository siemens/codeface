from sys import argv, exit
from .cli import run

# This conditional triggers if the module is loaded on the command line as
# python -m prosoda
if __name__ == "__main__":
    exit(run(argv))

