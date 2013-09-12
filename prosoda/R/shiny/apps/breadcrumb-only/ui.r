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
# Copyright 2013 by Siemens AG, Albert Eckert <albert.eckert@siemens.com>

shinyUI(bootstrapPage(

	div(class = "container",
		div(class = "row",
			breadcrumbOutput("quantarchBreadcrumb" )),
		div(class = "row",
			div(class = "span12",
			    selectInput("currentapp", "Select current app:",
			                c("Projects" = "projects",
			                  "Dashboard" = "dashboard",
			                  "Commit Information" = "commit.info",
			                  "Commit Structure" = "commit.structure",
			                  "Contributions overview" = "contributions",
			                  "Contributors" = "contributors",
			                  "Activity punch cards" = "punchcard",
			                  "ML activity punch cards" = "punchcard_ml",
			                  "Inter-Release Distance" = "release_distance",
			                  "Mailing list activity" = "timeseries",
			                  "Collaboration clusters" = "vis.clusters")
                        )
          )
			)
		)
	))
