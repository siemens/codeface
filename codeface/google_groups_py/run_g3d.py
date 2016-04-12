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