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
# Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
# Copyright 2014 by Matthias Dittrich <matthi.d@gmail.com>
# All Rights Reserved.
"""Module for containing the LinkType class"""
__author__ = "drag0on"


class LinkType(object):
    """enum-like class to distinguish between the various
    methods used to link individuals

    Attributes:

    """
    tag = "tag"
    proximity = "proximity"
    committer2author = "committer2author"
    file = "file"
    feature = "feature"
    feature_file = "feature_file"

    _all_link_types = \
        (tag, proximity, committer2author, file, feature, feature_file)

    @staticmethod
    def get_all_link_types():
        """

        Returns: all link types

        """
        return LinkType._all_link_types

    @staticmethod
    def get_tag_types():
        """

        Returns: List containing the tag types for committer2author relationship
        """
        return ["Signed-off-by", "Acked-by", "CC", "Reviewed-by",
                "Reported-by", "Tested-by", "Patch"]
