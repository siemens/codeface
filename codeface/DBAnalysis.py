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
# Copyright 2019, Stefanie Scherzinger <stefanie.scherzinger@oth-regensburg.de>
# All Rights Reserved.

'''This class obtains SQL artefacts from data definition language statements'''

import sys
import pytest
from fileCommit import FileDict
from logging import getLogger

from parse_tablename import parse_create_tbl_stmt, parse_drop_tbl_stmt, parse_alter_tbl_stmt
from parse_databasename import parse_create_db_stmt, parse_alter_db_stmt
from parse_indexname import parse_create_idx_stmt

log = getLogger(__name__)

class DBAnalysis:
    """
    TODO: Document
    """
    def __init__(self, src_file):
      self.src_file = src_file
      return(parse())

    def parse(self):
        lcounter = 0

        action = None
        kind = None
        name = None
        start = -1
        end = -1
  
        in_artefact = False
        res = []
        
        # TODO - what to do about encoding errors?
        for i in range(1, length(src_file)):
            line = src_file[i]
            log.debug("LINE " + str(i) + ": " + line)

            # TODO - ignore multi-line comments
            # TODO: Why do we do use startswith here -- whitespace before drop?
            if line.upper().startswith("DROP"):
                if in_artefact:
                    # Probably, the previous artefact is missing its ";"
                    # We just ignore the previous artefact and this one...

                    # TODO - count these problems and report them in threats to validity section.
                    log.warning("LINE " + str(lcounter) + ": " + line + " previous artefact not closed? Recovering...")        
                    in_artefact = False
                    kind = action = None
                    start = end = 0      
                    continue
      
                #assert(not in_artefact)
                in_artefact = True
                action = "DROP"
                start = lcounter
   
                if "TABLE" in line.upper():
                    kind = "TABLE"
                    tokens = line.upper().split()      
                    name = parse_drop_tbl_stmt(line)
                else:
                    raise Exception("Unknown DROP <kind> at line " + str(lcounter) + ".")

            elif line.upper().startswith("CREATE"):
                if in_artefact:
                    # Probably, the previous artefact is missing its ";"
                    # We just ignore the previous artefact and this one...

                    # TODO - count these problems and report them in threats to validity section.
                    log.error("LINE " + str(lcounter) + ": " + line.rstrip("\n") + " previous artefact not closed? Recovering...")
                    in_artefact = False
                    kind = action = None
                    start = end = 0      
                    continue      
      
                in_artefact = True
                start = lcounter

                if "TABLE" in line.upper():
                    kind = "TABLE"
                    action = "CREATE"
                    name = parse_create_tbl_stmt(line)

                elif "DATABASE" in line.upper():
                    kind = "DATABASE"
                    action = "CREATE"
                    name = parse_create_db_stmt(line)

                elif "INDEX" in line.upper():
                    kind = "INDEX"
                    action = "CREATE"
                    name = parse_create_idx_stmt(line)

                # Handling special case of incomplete CREATE statement
                # in tikiwiki_schemas/Rev_24_tiki.sql 
                elif line == "CREATE\n":
                    in_artefact = False
                    log.error("LINE " + str(lcounter) + ": " + line.rstrip("\n") + " -- incomplete statement? Recovering...")
      
                else:
                    raise Exception("Unknown CREATE <kind> at line " + str(lcounter) + ", line " + line)

            elif line.upper().startswith("ALTER"):
                assert(not in_artefact)
                in_artefact = True
                action = "ALTER"
      
                if "TABLE" in line.upper():
                    kind = "TABLE"
                    start = lcounter
                    name = parse_alter_tbl_stmt(line)

                elif "DATABASE" in line.upper():
                    kind = "DATABASE"
                    start = lcounter
                    name = parse_alter_db_stmt(line)
                else:
                    raise Exception("Unknown ALTER <kind> at line " + str(lcounter) + ", line " + line)    

            if (';' in line) and in_artefact:
                # Parsing out the artefact name may have been unsuccessful,
                # e.g. because of syntax errors. In this case, the name is None and we continue.
                if name == None:
                    log.debug("Reached ';', but there was some error parsing the statement in line " + str(lcounter))
                else:      
                    end = lcounter
                    assert(kind != None)
                    assert(action != None)
                    res.append({'kind': kind, 'name': name, 'start': start,
                                'end': end, 'action': action})
                    #                    writer.writerow([kind, name, start, end, action])

                action = kind = name = None
                start = end = -1
                in_artefact = False

        return(res)
