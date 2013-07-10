from sys import argv
from .cli import run

# This conditional triggers if the module is loaded on the command line as
# python -m prosoda
if __name__ == "__main__":
    run(argv)

