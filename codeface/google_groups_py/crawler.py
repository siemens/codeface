import re
import os.path
import urllib2
import time
import sys
import concurrent.futures


class Crawler(object):

    def __init__(self):
        self.group = ""
        self.output = ""
        self.link_topic = "https://groups.google.com/forum/?_escaped_fragment_=topic/"
        self.link_msg = "https://groups.google.com/forum/message/raw?msg="
        self.link_forum = "https://groups.google.com/forum/?_escaped_fragment_=forum/"
        self.opener = urllib2.build_opener()
        self.opener.addheaders = [('User-Agent', 'Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US; rv:1.9.1.5) Gecko/20091102 Firefox')]
        self.current_last_post_date = "00.00.00"
        self.max_worker_threads = 1
        self.group_is_set = False
        self.output_is_set = False
        self.start_date_is_set = False
        print "----------------------------------------------------"
        print "-------         Google Group Crawler         -------\n"

    # end execution if arguments aren't complete or help is displayed
    def handle_arguments(self, argv):
        if len(argv) is 1:
            self.print_help()
            sys.exit(0)
        for i in xrange(len(argv)):
            if argv[i] == "-h" or argv[i] == "--h" or argv[i] == "-help":
                self.print_help()
                sys.exit(0)
            if argv[i] == "-output":
                self.set_output(argv[i+1])
                self.output_is_set = True
            if argv[i] == "-group":
                self.set_group(argv[i+1])
                self.group_is_set = True
            if argv[i] == "-threads":
                self.set_max_worker_threads(argv[i+1])
            if argv[i] == "-start":
                self.set_current_last_post_date(argv[i+1])
                if self.check_date_format(self.current_last_post_date) is False:
                    print"Error: date wrong format"
                    self.print_help()
                    sys.exit(0)
                self.start_date_is_set = True
        if self.group_is_set is False:
            print "Error: missing group"
            self.print_help()
            sys.exit(0)
        if self.output_is_set is False:
            print "Error: missing output"
            self.print_help()
            sys.exit(0)

    @staticmethod
    def print_help():
        print "################################################################################"
        print "Help output:"
        print "################################################################################"
        print "crawler.py -output \"path\" -group \"groupname\" [-threads \"#threads\",\n -start \"DD.MM.YY\"]"
        print "################################################################################"
        print "-output: output path for ggc"
        print "-group: name of the google group"
        print "-threads: number of threads for simultaneous download (default = 1) (only for dl)"
        print "-start: date from which all messages of a group will be downloaded, \"DD.MM.YY\" (only for dl)"
        print ""
        print "If group has been downloaded before only new messages will be downloaded"
        print "To download the entire group again delete the threads.txt file or the complete directory"
        print "################################################################################"

    # checks if date is in format DD.MM.YY
    @staticmethod
    def check_date_format(date):
        result = re.findall("\\d\\d.\\d\\d.\\d\\d", date)
        if len(result) is 1:
            return True
        return False

    def set_group(self, group):
        self.group = group

    def set_current_last_post_date(self, start):
        self.current_last_post_date = start

    def set_output(self, output):
        if str(output).endswith("/") or str(output).endswith("\\"):
            self.output = output
        else:
            self.output = output + "/"

    def set_max_worker_threads(self, max_worker_threads):
        self.max_worker_threads = max_worker_threads

    def start(self):
        try:
            self.opener.open(self.link_forum + self.group)
        except urllib2.HTTPError:
            print "\nError: could not locate group"
            sys.exit(0)
        if self.start_date_is_set is True:
            self.get_group_from_start()
        if os.path.isfile(self.output + "threads.txt"):
            self.update()
        else:
            self.get_complete_group()

    def get_group_from_start(self):
        self.make_dir()
        with open(self.output + "threads.txt", 'w') as file_threads:
            file_threads.write("XXX," + self.current_last_post_date + ",\n")
        self.get_threads()

    def get_complete_group(self):
        print "Entered group: " + self.group
        print "complete group will be downloaded\n"
        self.make_dir()
        self.get_threads()

    def update(self):
        print "update"
        # load threads file, get first last_post_date
        first_line = open(self.output + "threads.txt", "r").readline()
        self.current_last_post_date = first_line.split(",")[1]
        # compare first last_post_date with last_post_date from file
        if self.threads_have_changes(self.output + "/threads.txt"):
            self.get_threads()
        else:
            print "no changes in " + self.group
        # if equal -> done(no new posts... possibly posts done on the same day as last download...)

    def get_threads(self):
        html = self.opener.open(self.link_forum + self.group)
        with concurrent.futures.ThreadPoolExecutor(max_workers=self.max_worker_threads) as executor:
            with open(self.output + "threads.txt", 'w') as file_threads:
                while True:
                    next_page = self.get_threads_and_date(html, file_threads, executor)
                    if next_page == "":
                        break
                    html = self.opener.open(next_page)

    def get_threads_and_date(self, html, file_threads, executor):
        next_page = ""
        current_id = ""
        for line in html:
            last_post_date = re.findall("(?<=\"lastPostDate\">)([0-9\.:]*)", line)
            if len(last_post_date) > 0:
                if ":" in last_post_date[0]:
                    last_post_date[0] = time.strftime("%d.%m.%y")
                if self.compare_date(last_post_date[0], self.current_last_post_date) == -1:
                    file_threads.write("XXXX," + self.current_last_post_date + ",\n")
                    next_page = ""
                    break
                if last_post_date[0] == self.current_last_post_date:
                    return ""
                file_threads.write(current_id + "," + last_post_date[0] + ",\n")
                executor.submit(self.get_messages, current_id)
                # self.get_messages(current_id)
            temp = re.findall("(href=\")(.*?)(\")", line)
            if len(temp) == 0:
                continue
            topic_id = re.findall("(?<=" + self.group + "/)(.*)", temp[0][1])
            if "topic" in temp[0][1]:
                current_id = topic_id[0]
            if "forum" in temp[0][1]:
                next_page = temp[0][1]
        return next_page

    # gets executed in own thread
    def get_messages(self, topic_id):
        counter = 0
        print "getting thread " + topic_id
        try:
            html = self.opener.open(self.link_topic + self.group + "/" + topic_id)
        except urllib2.HTTPError:
            print "skipping deleted thread " + topic_id
            return
        msg_id = ""
        for line in html:
            last_post_date = re.findall("(?<=\"lastPostDate\">)([0-9\.]*)", line)
            if len(last_post_date) > 0:
                if len(self.current_last_post_date) > 0:
                    if self.compare_date(last_post_date[0], self.current_last_post_date) < 1:
                        continue
                self.get_mbox(topic_id, msg_id)
                counter += 1
            temp = re.findall("(href=\")(.*?)(\")", line)
            if len(temp) == 0:
                continue
            if "/msg/" + self.group + "/" in temp[0][1]:
                msg_id = re.findall("(?<=" + topic_id + "/)(.*)", temp[0][1])[0]
            if len(last_post_date) > 0:
                if last_post_date == self.current_last_post_date:
                    print str(counter) + " messages for topic " + topic_id + " downloaded."
                    return ""
        print str(counter) + " messages for topic " + topic_id + " downloaded."

    def get_mbox(self, topic_id, msg_id):
        print "\tdownloading msg: " + msg_id
        try:
            mbox = self.opener.open(self.link_msg + self.group + "/" + topic_id + "/" + msg_id).read()
        except urllib2.HTTPError:
            print "skipping deleted message " + msg_id
            return
        with open(self.output + "mbox/" + msg_id + ".mbox", "w") as file_mbox:
            file_mbox.write(mbox)

    @staticmethod
    def compare_date(date1, date2):
        # date format: dd.mm.yy
        date1_split = date1.split(".")
        date2_split = date2.split(".")
        if int(date1_split[2]) == int(date2_split[2]):
            # year1 == year2
            if int(date1_split[1]) == int(date2_split[1]):
                # month1 == month
                if int(date1_split[0]) < int(date2_split[0]):
                    # day1 < day2
                    return -1
                if int(date1_split[0]) == int(date2_split[0]):
                    # day1 == day2
                    return 0
                else:
                    # day1 > day2
                    return 1
            if int(date1_split[1]) < int(date2_split[1]):
                return -1
            else:
                return 1
        if int(date1_split[2]) < int(date2_split[2]):
            return -1
        else:
            return 1

    def threads_have_changes(self, path_file):
        html = self.opener.open(self.link_forum + self.group)
        for line in html:
            if "lastPostDate" in line:
                last_post_date = re.findall("(?<=\"lastPostDate\">)([0-9.:]*)", line)[0]
                if ":" in last_post_date:
                    last_post_date = time.strftime("%d.%m.%y,\n")
                if last_post_date == self.current_last_post_date:
                    with open(path_file, "w") as file_threads:
                        # to save the last update date for next update try
                        file_threads.write("XXXX," + self.current_last_post_date + ",\n")
                    return False
                return True

    # creates subdirectories if they don't exist
    def make_dir(self):
        if not os.path.isdir(self.output + "mbox"):
            print "creating directories\n"
            os.makedirs(self.output + "mbox")


def main():
    argv = sys.argv
    # argv = ['crawlerThreads.py', '-output', 'output/', '-group', 'phaser3-dev', '-threads', '40']
    # argv = ['crawlerThreads.py', '-output', 'output/', '-group', 'artspace', '-threads', '10']
    # argv = ['crawlerThreads.py', '-output', 'output/', '-group', 'artspace', '-threads', '10', '-start', '01.01.10']
    # argv = ['crawlerThreads.py', '-output', 'output/pants-devel2', '-group', 'pants-devel', '-threads', '200']
    crawler = Crawler()
    crawler.handle_arguments(argv)
    crawler.start()


main()
