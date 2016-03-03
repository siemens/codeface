import os.path
import sys
import re


class PostDownload(object):

    def __init__(self):
        self.group = ""
        self.output = ""
        self.group_is_set = False
        self.output_is_set = False

    @staticmethod
    def print_help():
        print "################################################################################"
        print "Help output:"
        print "################################################################################"
        print "post_download.py -output \"path\" -group \"groupname\""
        print "#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  # "
        print "-output: output path pointing to the directory containing the group's directory"
        print "-group: name of the google group"
        print ""
        print "Multiple mbox files will be combined into one file"
        print "Certain lines will be edited to be understandable by Codeface"
        print "example: \"post_download.py -output output\\ -group pants\""
        print "################################################################################"

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
                self.group = argv[i+1]
                self.group_is_set = True
        if self.group_is_set is False:
            print "Error: missing group"
            self.print_help()
            sys.exit(0)
        if self.output_is_set is False:
            print "Error: missing output"
            self.print_help()
            sys.exit(0)

    def set_output(self, output):
        if str(output).endswith("/") or str(output).endswith("\\"):
            self.output = output
        else:
            self.output = output + "/"

    def start(self):
        self.merge_to_single_file()

    def merge_to_single_file(self):
        with open(self.output + self.group + ".mbox", "w") as final_mbox:
            for root, dirs, files in os.walk(self.output + "/mbox"):
                for file in files:
                    if file.endswith(".mbox"):
                        with open(self.output + "mbox/" + file, "r") as single_mbox:
                            self.fix_content(single_mbox, final_mbox)
                            final_mbox.write("\n\n\n")

    def fix_content(self, single_mbox, final_mbox):
        text = single_mbox.read().split("\n")
        if "X-Received " in text[0]:
            text[0] = "From GoogleGroups"
        else:
            text = ["From GoogleGroups"] + text
        for line in text:
            if "From: " in line:
                line = self.handle_from_line(line)
            final_mbox.write(line)

    @staticmethod
    def handle_from_line(line):
        from_line = re.findall("(?<=From: )\"?[ A-Za-z]*", line)
        if len(from_line) == 1:
            name = from_line[0].replace('"', '')
            if name.endswith(" "):
                name = name[:-1]
            email = re.findall("<.*>", line)
            if len(email) > 0:
                if "..." in email[0]:
                    email_suffix = re.findall("@[a-zA-Z0-1\-.]*", line)
                    if len(email_suffix) != 0:
                        email_suffix = email_suffix[0]
                    else:
                        email_suffix = "@unknown.com"
                    return "From: " + name + " <" + name.replace(" ", "_") + email_suffix + ">\n"
                else:
                    return "From: " + name + " " + email[0] + "\n"
        return line


def main():
    argv = sys.argv
    argv = ["-output", "../test/pants", "-group", "pants-devel"]
    post_download = PostDownload()
    post_download.handle_arguments(argv)
    post_download.start()


main()