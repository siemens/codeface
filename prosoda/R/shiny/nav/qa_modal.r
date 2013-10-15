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
## Copyright 2013 by Siemens AG, Albert Eckert <albert.eckert@siemens.com>
## All Rights Reserved.

##
## ui.r component for defining modal dialog 
##

##
## General modal Dialog with Button
##
## Parameters
##    modalDialogId: also used for an outputUI(modalDialogId) element to send a dialog ui via server.r
##    modalDialogTitle: title of this dialog, 
##    actionButtonId: the id for the submit button as used by server r to return results 
##    actionButtonLabel: label for the submit button 
##    dismissButtonLabel: label for the cancel button
##

addResourcePath(
  prefix = 'modal_js',
  #directoryPath = system.file('gridster', package='shinyGridster'))
  directoryPath = ffile.path(getwd(),"js"))

modalDialog <- function( modalDialogId, modalDialogTitle="Modal Title", 
                         actionButtonId, actionButtonLabel="OK", dismissButtonLabel="Close" ) {
  tagList(
    singleton(tags$head(tags$script(src = "nav_js/qa_modal.js"))),
  # Modal dialog for selecting projects to compare with
    tags$div( id=paste(modalDialogId,"Modal",sep=""), class="modal hide fade", tabindex="-1", role="dialog",
            "aria-labelledby"=paste(modalDialogId,"ModalLabel",sep=""), "aria-hidden"="true",
  
            tags$div( class="modal-header",
                      tags$button( type="button", class="close", "data-dismiss"="modal", "aria-hidden"="true","X"),
                      h3( id=paste(modalDialogId,"ModalLabel",sep=""), modalDialogTitle)),
            
            tags$div( class="modal-body",
                      tags$p( uiOutput( modalDialogId ))),
            
            tags$div( class="modal-footer",
                      tags$button( class="btn", "data-dismiss"="modal", "aria-hidden"="true",dismissButtonLabel),
                      tags$button( id=actionButtonId, type="button", 
                                   class="btn action-button btn-primary",actionButtonLabel ))
            )
    )
  }

##
## Special standalone button for lauching the modal dialog (just adpat similar elements as needed)
##
## All parameters see the modalDialog function
##
modalDialogButton <- function( modalDialogId, modalDialogTitle="Modal Title", 
                               actionButtonId, actionButtonLabel="OK", dismissButtonLabel="Close" ) {
  tagList(
    
    tags$div( class="btn-group pull-right",  
              a( class="btn", 
                 
                 # refer to the dialog via "#<modalDialogId>Mode" internal reference               
                 href=paste("#",modalDialogId,"Modal",sep=""), 
                 
                 "data-toggle"="modal", "Compare with")),
    
    # modal dialog added here, but could be elsewhere
    modalDialog(modalDialogId, modalDialogTitle, actionButtonId, actionButtonLabel, dismissButtonLabel))
  }



##
## server.r component for handling modal dialog 
##

##
## (1) Creating the modal dialog (hints only)
##
## To create the modal dialog ui for a modal dialog with id "mymodaldialog", 
## add the following into an observe context:
##
##    observe({
##
##        output$mymodaldialog <- renderUI({
##
##            # place any html and input/output elements for the ui
##            # also render any outputs that will not depend on the input elements
##
##        })
##    })
##
## (2) handling the modal dialog submit/ActioButton
##
## After pressing the submit button in the modal dialog, all inputs will be handled
## Inputs that will influence the modal dialog ui must be handled as full reactives
##
##    observe({
##
##        # modal dialog Submit button with id "mybuttonid" will trigger this context
##        if (input$mybuttonid == 0) return()
##
##        # make shure that inputs that hold final dialog returns do not trigger this observe
##        var1 <- isolate({ input$someinput1 })
##        
##    }) #end observe
##
## (Alternatively create a reactive variable if you expect some return values instead of observe)
##
##    # if needed, update outputs depending on inputs as needed (must not depend on submit button input)
##    output$someoutput <- renderThisOuput({ depends on some input$someinput2 })
##
