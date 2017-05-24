#! /usr/bin/env python

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
# Copyright 2014, Siemens AG, Mitchell Joblin <mitchell.joblin.ext@siemens.com>
# Copyright 2017, Wolfgang Mauerer <wm@linux-kernel.net>
# All Rights Reserved.

import xml.etree.ElementTree as ET
import os
import sys
if os.name == 'posix' and sys.version_info[0] < 3:
    from subprocess32 import Popen, PIPE, TimeoutExpired
else:
    from subprocess import Popen, PIPE, TimeoutExpired
from logging import getLogger
log = getLogger(__name__)

class FileAnalysis:

    ## List of source code elements we want to capture
    SRC_ELEMS = ['function']

    def __init__(self, filename, doxygen_conf, outdir):
        self.filename = filename
        self.conf = doxygen_conf
        self.outdir = outdir
        self.src_elem_list = []

    def gen_XML_files(self):
        # Run source code analysis and generate xml files
        input_file = 'INPUT=' + self.filename
        output_dir = 'OUTPUT_DIRECTORY=' + self.outdir
        cmd_1 = ['cat', self.conf]
        p1 = Popen(cmd_1 ,stdout=PIPE)
        doxy_conf = p1.communicate()[0]
        doxy_conf = doxy_conf + input_file + '\n'
        doxy_conf = doxy_conf + output_dir

        cmd = 'doxygen -'
        cmd_2 = cmd.split()
        p2 = Popen(cmd_2, stdin=PIPE, stdout=PIPE, stderr=PIPE)
        p2.stdin.write(doxy_conf)
        # On rare occasions, doxygen may 'hang' on input files, not delivering
        # any result. Use a timeout to work around such situations
        try:
            p2.communicate(timeout=5*60) # Stop trying after 5 minutes
        except TimeoutExpired:
            log.warning("Doxygen got stuck, cancelling analysis run")
            p2.kill()

    def _parse_XML_index(self):
        # Parse index file generate by deoxygen that contains the compound
        # elements
        comp_list = []
        index_file = os.path.join(self.outdir, 'xml', 'index.xml')
        tree = ET.parse(index_file)
        root = tree.getroot()
        comp_elements = root.findall('compound')

        for elem in comp_elements:
            # Check if the element contains a child function
            member_kind = [child.get('kind') for child in elem.iter('member')]
            if any([src_elem in member_kind for src_elem in \
                    FileAnalysis.SRC_ELEMS]):
                refid = elem.get('refid')
                kind = elem.get('kind')
                name = elem.find('name').text
                comp_list.append({'refid':refid, 'kind':kind, 'name':name})

        return(comp_list)

    def _parse_XML_compound(self, comp_list):
        for comp_elem in comp_list:
            comp_file = os.path.join(self.outdir, 'xml', comp_elem['refid'] + '.xml')
            xml_string = self._prepare_clean_xml(comp_file)
            root = ET.fromstring(xml_string)

            for child in root.iter('memberdef'):
                kind = child.get('kind')
                if kind in FileAnalysis.SRC_ELEMS:
                    loc = child.find('location')
                    start = loc.get('bodystart')
                    end = loc.get('bodyend')
                    name = child.find('name').text
                    # Some elements of function type are not assigned
                    # start and end (e.g., definitions)
                    if None not in [start, end, name]:
                        self.src_elem_list.append({'bodystart':start,
                                                   'bodyend':end,
                                                   'name':name,
                                                   'mem_kind': kind,
                                                   'comp_kind': comp_elem['kind'],
                                                   'comp_name': comp_elem['name']})

    def _prepare_clean_xml(self, filename):
        # Read file
        with open (filename, "r") as file:
            xml_string = file.read()

        # Clean file of control characters
        xml_clean = ''.join([i for i in xml_string if 31 < ord(i) < 127])

        return xml_clean

    def run_analysis(self):
        self.gen_XML_files()
        comp_list = self._parse_XML_index()
        self._parse_XML_compound(comp_list)
