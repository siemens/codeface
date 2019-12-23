## This file is part of Codeface. Codeface is free software: you can
## redistribute it and/or modify it under the terms of the GNU General Public
## License as published by the Free Software Foundation, version 2.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
##
## Copyright 2016 by Georg Berner <georgberner@gmx.net>
## All Rights Reserved.

from g3d import Crawler
import unittest


class argumentHandlingTests(unittest.TestCase):
    # no errors
    def test1(self):
        argv = ["-group", "test_group", "-start", "11.11.11", "-output", "./", "-threads", "50"]
        crawler = Crawler()
        crawler.handle_arguments(argv)
        assert crawler.execute == True
        assert crawler.group_is_set == True
        assert crawler.group == "test_group"
        assert crawler.max_worker_threads == 50
        assert crawler.output_is_set == True
        assert crawler.output == "./"
        assert crawler.verbose == False

    # -output missing -> should work
    def test2(self):
        argv = ["-group", "test_group", "-threads", "50"]
        crawler = Crawler()
        crawler.handle_arguments(argv)
        assert crawler.execute == True
        assert crawler.output_is_set == True
        assert crawler.output == "./"

    # -group missing
    def test3(self):
        argv = ["-output", "./", "-threads", "50"]
        crawler = Crawler()
        crawler.handle_arguments(argv)
        assert crawler.execute == False
        assert crawler.group_is_set == False

    # -threads number missing
    def test4(self):
        argv = ["-group", "test_group", "-threads", "-output", "./"]
        crawler = Crawler()
        crawler.handle_arguments(argv)
        assert crawler.execute == False
        assert crawler.max_worker_threads == 1

    # -threads number missing at end
    def test5(self):
        argv = ["", "-threads"]
        crawler = Crawler()
        crawler.handle_arguments(argv)
        assert crawler.execute == False
        assert crawler.max_worker_threads == 1

    # -group follow up missing at end
    def test6(self):
        argv = ["", "-group"]
        crawler = Crawler()
        crawler.handle_arguments(argv)
        assert crawler.execute == False
        assert crawler.group_is_set == False


    # -output follow up missing at end
    def test7(self):
        argv = ["-group", "test_group", "-output"]
        crawler = Crawler()
        crawler.handle_arguments(argv)
        assert crawler.execute == True
        assert crawler.output_is_set == True
        assert crawler.output == "./"


    # -v is set
    def test8(self):
        argv = ["", "-v"]
        crawler = Crawler()
        crawler.handle_arguments(argv)
        assert crawler.verbose == True

    # -start follow up missing
    def test9(self):
        argv = ["-group", "test_group", "-start", "-output", "./"]
        crawler = Crawler()
        crawler.handle_arguments(argv)
        assert crawler.start_date_is_set == False


    # start format check
    def test10(self):
        crawler = Crawler()
        assert crawler.check_date_format("11.11.11") == True
        assert crawler.check_date_format("111.11.11") == False
        assert crawler.check_date_format("11.111.11") == False
        assert crawler.check_date_format("11.11.111") == False


class checkRegexFunction(unittest.TestCase):
    # href
    def test1(self):
        crawler = Crawler()
        line = '<td class="lastPostDate">27.04.10</td></tr> <tr><td class="subject"><a href="https://groups.google.com/d/topic/artspace/tBUZa7_8k0k" title="Drunk'
        expected_result = "https://groups.google.com/d/topic/artspace/tBUZa7_8k0k"
        result = crawler.search_href(line)
        assert result == expected_result

    # topic id
    def test2(self):
        crawler = Crawler()
        line = "https://groups.google.com/d/topic/artspace/tBUZa7_8k0k"
        expected_result = "tBUZa7_8k0k"
        result = crawler.search_topic_id("artspace", line)
        assert result == expected_result


    # last post date
    def test3(self):
        crawler = Crawler()
        line = '<td class="lastPostDate">27.04.10</td></tr> <tr><td class="subject"><a href="https://groups.google.com/d/topic/artspace/tBUZa7_8k0k" title="Drunk'
        expected_result = "27.04.10"
        result = crawler.search_last_post_date(line)
        assert result == expected_result

    # msg id
    def test4(self):
        crawler = Crawler()
        line = "https://groups.google.com/d/msg/artspace/Mo3E1JiCpcs/S145ITlPn8oJ"
        expected_result = "S145ITlPn8oJ"
        result = crawler.search_msg_id("Mo3E1JiCpcs", line)
        assert result == expected_result

class checkMsgCorrection(unittest.TestCase):
    # build email adress in From:...
    def test1(self):
        crawler = Crawler()
        line = "From: FunnyDave <Fu...@gmail.com>"
        expected_result = "From: FunnyDave <FunnyDave@gmail.com>"
        result = crawler.check_mbox(line)
        assert result == expected_result

    # build email adress in From:... with 2 names
    def test2(self):
        crawler = Crawler()
        line = "From: Funny Dave <Fu...@gmail.com>"
        expected_result = "From: Funny Dave <Funny_Dave@gmail.com>"
        result = crawler.check_mbox(line)
        assert result == expected_result

    # build email adress in From:... with 3 names
    def test3(self):
        crawler = Crawler()
        line = "From: Funny Dave Bob <Fu...@gmail.com>"
        expected_result = "From: Funny Dave Bob <Funny_Dave_Bob@gmail.com>"
        result = crawler.check_mbox(line)
        assert result == expected_result

    # check if whitespace gets added
    def test4(self):
        crawler = Crawler()
        line = "From the heart"
        expected_result = " From the heart"
        result = crawler.check_mbox(line)
        assert result == expected_result

    # check if name 1 name is generated
    def test5(self):
        crawler = Crawler()
        line = 'From: brian...@gmail.com'
        expected_result = "brian"
        result = crawler.search_name(line)
        assert result == expected_result

    # check if name(2) is generated
    def test6(self):
        crawler = Crawler()
        line = 'From: Nate Yocom <na...@pgina.org>'
        expected_result = "Nate Yocom"
        result = crawler.search_name(line)
        assert result == expected_result

class checkDateCompare(unittest.TestCase):
    # compare dates
    def test1(self):
        crawler = Crawler()
        date1 = "11.11.11"
        date2 = "22.11.11"
        result = crawler.compare_date(date1, date2)
        assert result == -1


    def test2(self):
        crawler = Crawler()
        date1 = "11.11.11"
        date2 = "11.22.11"
        result = crawler.compare_date(date1, date2)
        assert result == -1

    def test3(self):
        crawler = Crawler()
        date1 = "11.11.11"
        date2 = "11.11.22"
        result = crawler.compare_date(date1, date2)
        assert result == -1

    def test4(self):
        crawler = Crawler()
        date1 = "22.11.11"
        date2 = "11.11.11"
        result = crawler.compare_date(date1, date2)
        assert result == 1

    def test5(self):
        crawler = Crawler()
        date1 = "11.22.11"
        date2 = "11.11.11"
        result = crawler.compare_date(date1, date2)
        assert result == 1

    def test6(self):
        crawler = Crawler()
        date1 = "11.11.22"
        date2 = "11.11.11"
        result = crawler.compare_date(date1, date2)
        assert result == 1

    def test7(self):
        crawler = Crawler()
        date1 = "11.11.11"
        date2 = "11.11.11"
        result = crawler.compare_date(date1, date2)
        assert result == 0
