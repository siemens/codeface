# Subsystem characterisations for various Unix kernels
# (following kerninfo.pm from the Perl scripts)

# This file is part of prosoda.  prosoda is free software: you can
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
# Copyright 2010, 2011, 2012 by Wolfgang Mauerer <wm@linux-kernel.net>
# All Rights Reserved.

# Subsystems are characterised by a dictionary with the subsys
# categories as keys. Each value is a list of all directories that comprise 
# the subsystem. None must be used if the subsystem does
# not exist, but this fact is of importance (for instance in a comparison
# of similarly structured systems)

# TODO: For all kernels except Linux, the block layer is missing!

subsysDescrLinux = {
    "core" : ["kernel", "ipc", "virt", "security" ], 
    "drivers" : ["drivers", "sound"], 
    "net" : ["net"],
    "mm" : ["mm"], 
    "block": ["block"],
    "library" : ["lib"],
    "headers" : ["include"],
    "arch" : ["arch"], 
    "security" : ["crypto", "security"], 
#    "contrib" : ["DOESNOTEXIST"], # External contributions to Linux remain external 
    "scripts" : ["scripts"], 
    "fs" : ["fs"], 
    "virt" : ["virt"], 
    "docs" : ["Documentation"] }

