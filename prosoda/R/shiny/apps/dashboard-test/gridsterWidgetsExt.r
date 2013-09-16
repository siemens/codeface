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

gridsterButton <- function(inputId, widgetdialogId) {
  tagList(
    #singleton(tags$head(tags$script(src = "gridsterWidgetsExt.js"))),
    tags$div( class="dropdown",
      tags$button( id = inputId, class = "btn gridsterButton dropdown-toggle", "data-toggle"="dropdown",
        type = "button",
        tags$i(class="icon-th")
				), 
      tags$ul( class="dropdown-menu", #role="menu", "aria-labelledby"="dLabel",
        tags$li( a( tabindex="-1", class="gridsterAction", "gridster-action"="saveconfig", 
                    href="#", "Save configuration")),
        tags$li( a( tabindex="-1", class="gridsterAction", "gridster-action"="deletemode", 
                   href="#", "Delete ON")),
        tags$li( a( tabindex="-1", class="gridsterAction", "gridster-action"="addwidget", 
                   href="#modalAddWidget", "data-toggle"="modal", "Add widget")),
        tags$li( a( tabindex="-1", href="#", "Action")),
        tags$li( class="divider" ),
        tags$li( a( tabindex="-1", href="#", "Action"))
        )
	    ),
      
    # Modal dialog for adding widget content
    tags$div( id="modalAddWidget", class="modal hide fade", tabindex="-1", role="dialog",
              "aria-labelledby"="myModalLabel", "aria-hidden"="true",
              tags$div( class="modal-header",
                tags$button( type="button", class="close", "data-dismiss"="modal", "aria-hidden"="true","A"),
                h3( id="myModalLabel", "Modal header")),
              tags$div( class="modal-body",
                tags$p( uiOutput( widgetdialogId ) )
                ),
              tags$div( class="modal-footer",
                tags$button( class="btn", "data-dismiss"="modal", "aria-hidden"="true","Close"),
                #tags$button( type="submit", class="btn, btn-primary","Save changes" )))
                # must replace submit button by actioButton to avoid global reactivity switch-off
                tags$button( id="addWidgetButton", type="button", class="btn action-button btn-primary","Save changes" )))
    )
}
