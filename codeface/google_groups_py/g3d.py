import re
import os.path
import urllib2
import time
import sys
import threading
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
        self.verbose = False
        self.execute = False
        self.lock = threading.Lock()
        self.counter = 0
        self.elapsed_time = 0
        print "------------------------g3d-------------------------"
        print "-------     Google Groups Group Downloader     -----\n"

    # end execution if arguments aren't complete or help is displayed
    def handle_arguments(self, argv):
        if len(argv) is 1:
            self.print_help()
            return
        for i in xrange(len(argv)):
            if argv[i] == "-h" or argv[i] == "--h" or argv[i] == "-help":
                self.print_help()
                return
            if argv[i] == "-v":
                self.verbose = True
                continue
            if argv[i] == "-output":
                if i+1 == len(argv):
                    break
                self.set_output(argv[i+1])
                self.output_is_set = True
                continue
            if argv[i] == "-group":
                if i+1 == len(argv):
                    break
                self.group = argv[i+1]
                self.group_is_set = True
                continue
            if argv[i] == "-threads":
                if i+1 == len(argv):
                    print "Error: amount of threads missing"
                    return
                try:
                    self.max_worker_threads = int(argv[i+1])
                except ValueError:
                    print "\nError: argument following -threads is not a number"
                    return
                continue
            if argv[i] == "-start":
                if i+1 == len(argv):
                    print "Error: start date missing"
                    return
                self.current_last_post_date = argv[i+1]
                if self.check_date_format(self.current_last_post_date) is False:
                    print"Error: date wrong format"
                    return
                self.start_date_is_set = True
                continue
        if self.group_is_set is False:
            print "Error: missing group"
            return
        if self.output_is_set is False:
            print "Warning: missing output"
            self.set_output("./")
            self.output_is_set = True
        self.execute = True

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
        print "-v: verbose output"
        print "################################################################################"

    # checks if date is in format DD.MM.YY
    @staticmethod
    def check_date_format(date):
        result = re.findall("^\\d\\d.\\d\\d.\\d\\d$", date)
        if len(result) is 1:
            return True
        return False

    def set_output(self, output):
        if str(output).endswith("/") or str(output).endswith("\\"):
            self.output = output
        else:
            self.output = output + "/"

    def start(self):
        start_time = time.time()
        try:
            self.opener.open(self.link_forum + self.group)
        except urllib2.HTTPError:
            print "\nError: could not locate group"
            sys.exit(0)
        if self.start_date_is_set is True:
            self.get_group_from_start()
        else:
            self.get_complete_group()
        self.elapsed_time = time.time()-start_time

    def get_group_from_start(self):
        self.make_dir()
        with open(self.output + self.group + ".mbox", "w"):
            print "creating output file"
        with open(self.output + "threads.txt", 'w') as file_threads:
            file_threads.write("XXX," + self.current_last_post_date + ",\n")
        self.get_threads()

    def get_complete_group(self):
        print "Entered group: " + self.group
        print "complete group will be downloaded\n"
        self.make_dir()
        with open(self.output + self.group + ".mbox", "w"):
            print "creating output file"
        if self.verbose is False:
            print "starting download, please wait..."
        self.get_threads()

    def get_threads(self):
        try:
            html = self.opener.open(self.link_forum + self.group)
        except urllib2.HTTPError:
            return
        with concurrent.futures.ThreadPoolExecutor(max_workers=self.max_worker_threads) as executor:
            while True:
                next_page = self.get_threads_and_date(html, executor)
                if next_page == "":
                    break
                html = self.opener.open(next_page)

    def get_threads_and_date(self, html, executor):
        next_page = ""
        current_id = ""
        for line in html:
            last_post_date = self.search_last_post_date(line)
            if len(last_post_date) > 0:
                if ":" in last_post_date:
                    last_post_date = time.strftime("%d.%m.%y")
                if self.compare_date(last_post_date, self.current_last_post_date) == -1:
                    next_page = ""
                    break
                if last_post_date == self.current_last_post_date:
                    return ""
                executor.submit(self.get_messages, current_id)
            href = self.search_href(line)
            if len(href) == 0:
                continue
            topic_id = self.search_topic_id(self.group, href)
            if "topic" in href:
                current_id = topic_id
            if "forum" in href:
                next_page = href
        return next_page

    @staticmethod
    def search_topic_id(group, line):
        temp = re.findall("(?<=" + group + "/)(.*)", line)
        if len(temp) == 0:
            return ""
        return temp[0]

    @staticmethod
    def search_href(line):
        temp = re.findall("(href=\")(.*?)(\")", line)
        if len(temp) == 0:
            return ""
        return temp[0][1]

    @staticmethod
    def search_last_post_date(line):
        temp = re.findall("(?<=\"lastPostDate\">)([0-9\.:]*)", line)
        if len(temp) == 0:
            return ""
        return temp[0]

    # gets executed in own thread
    def get_messages(self, topic_id):
        counter = 0
        if self.verbose is True:
            print "getting thread " + topic_id
        try:
            html = self.opener.open(self.link_topic + self.group + "/" + topic_id)
        except urllib2.HTTPError:
            if self.verbose is True:
                print "skipping deleted thread " + topic_id
            return
        msg_id = ""
        for line in html:
            last_post_date = self.search_last_post_date(line)
            if len(last_post_date) > 0:
                if len(self.current_last_post_date) > 0:
                    if self.compare_date(last_post_date, self.current_last_post_date) < 1:
                        continue
                self.get_mbox(topic_id, msg_id)
                counter += 1
            href = self.search_href(line)
            if len(href) == 0:
                continue
            if "/msg/" + self.group + "/" in href:
                msg_id = self.search_msg_id(topic_id, href)
            if len(last_post_date) > 0:
                if last_post_date == self.current_last_post_date:
                    if self.verbose is True:
                        print str(counter) + " messages for topic " + topic_id + " downloaded."
                    return ""
        if self.verbose is True:
            print str(counter) + " messages for topic " + topic_id + " downloaded."

    @staticmethod
    def search_msg_id(topic_id, href):
        return re.findall("(?<=" + topic_id + "/)(.*)", href)[0]

    def get_mbox(self, topic_id, msg_id):
        if self.verbose is True:
            print "\tdownloading -> topic: " + topic_id + " msg: " + msg_id
        try:
            mbox = self.opener.open(self.link_msg + self.group + "/" + topic_id + "/" + msg_id).read()
        except urllib2.HTTPError:
            if self.verbose is True:
                print "skipping deleted message " + msg_id
            return
        self.add_to_mbox(mbox)

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

    # creates subdirectories if they don't exist
    def make_dir(self):
        if not os.path.isdir(self.output):
            print "creating directory\n"
            os.makedirs(self.output)

    def add_to_mbox(self, raw):
        clean_mbox = "\n\n\nFrom Google Groups\n" + self.check_mbox(raw)
        with self.lock:
            with open(self.output + self.group + ".mbox", "a") as final_mbox:
                final_mbox.write(clean_mbox)
            self.counter += 1

    def check_mbox(self, raw):
        text = raw.split("\n")
        clean = ""
        for line in text:
            if line.startswith("From "):
                line = " " + line
            if line.startswith("From: "):
                 line = self.handle_from_line(line) + "\n"
            clean += line
        return clean

    @staticmethod
    def search_name(line):
        temp = re.findall("(?<=From: )\"?[ A-Za-z]*", line)
        if len(temp) > 0:
            name = temp[0].replace('"', '')
            if name.endswith(" "):
                name = name[:-1]
            return name
        return ""

    @staticmethod
    def search_email(line, name):
        email = re.findall("<.*>", line)
        if len(email) > 0:
            if "..." in email[0]:
                email_suffix = re.findall("@[a-zA-Z0-1\-.]*", line)
                if len(email_suffix) != 0:
                    email_suffix = email_suffix[0]
                else:
                    email_suffix = "@unknown.com"
                return "<" + name.replace(" ", "_") + email_suffix + ">"
        return ""

    def handle_from_line(self, line):
        name = self.search_name(line)
        if len(name) != 0:
            email = self.search_email(line, name)
            if len(email) > 0:
                return "From: " + name + " " + email
        return line

    def statistic(self):
        print "download complete"
        print "g3d downloaded " + str(self.counter) + " messages in " + str(int(self.elapsed_time)) + " seconds."
