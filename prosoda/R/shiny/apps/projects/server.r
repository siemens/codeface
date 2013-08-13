suppressPackageStartupMessages(library(shiny))

## BEGIN QUANTARCH FUNCTIONALITY 

## initialize globals in local scope, so they are visible for all user sessions
source("../../../config.r", chdir=TRUE, local=TRUE)  # this activates logging
source("../../../query.r", chdir=TRUE, local=TRUE)
conf <- config.from.args(require_project=FALSE)
projects.list <- query.projects(conf$con)
dbDisconnect(conf$con) # close database session, because we got the data needed for this app
## source the breadcrumb functionality, i.e. nav.list configuration
##(functions in a list) and utility functions
source(file.path("..","..","nav","breadcrumb.r"), chdir = TRUE, local=TRUE)

## END QUANTARCH FUNCTIONALITY 


## the server function
##
shinyServer(function(input, output, session) {
  
	loginfo(isolate(session$clientData$url_search)) # log query string

	## values synchonizes the following reactive blocks  
	values <- reactiveValues()

	## observer
	observe({
    ## (1) Handle parameters
	
	#loginfo("Entering observe")
    paramstr <- session$clientData$url_search
    args.list <- parseQueryString(paramstr)
    pid <- args.list[["projectid"]]
    #loginfo(paste("projectid=<",as.character(pid),">"))
    
	## (2) Send breadcrumb navigation as HTML to app client 
    
	navData <- breadcrumbPanelData("quantarch", as.character(paramstr))
    #print(navData)
	breadcrumbhtml <- as.character(breadcrumbBrandville( navData ))
	#logdebug(breadcrumbhtml)
    session$sendCustomMessage(
      type = "sendToIframeParent", 
      message = list(
        # Name of message to send
        msgname = "setNav",
        navhtml = breadcrumbhtml
		))
        
	## (3) Handle project selected by URL parameter "projectid" (if any)
    
	selectedId <- 0 # assume that nothing was selected (no URL parameter)
    projects.n <- nrow(projects.list) # get number of projects
    if (projects.n == 0) stop("no projects") # must be non-zero
    ## if URL parameter supplied a project id, check valid range
	if (!is.null(pid) && !(str_trim(as.character(pid)) == "")) {
		selectedId <- as.integer(pid)
		if (selectedId < 1) selectedId <- 1
		if (selectedId > projects.n) selectedId <- projects.n  
		}
    ## finally ends with a valid selection
	## store in values to trigger output
    values$sid <- selectedId
	}) # end observe
    
	## (4) Ouput navigation sidebar
	
	output$selectionlistelements <- renderUI({      
		#loginfo("Entering renderUI")  
		selectedItem <- values$sid
		childrenIds <- nav.list$quantarch$childrenIds(paramstr)
		#print(childrenIds)
		childtags <- tagList()
		
		if (nrow(childrenIds) > 0) {
		  for (i in 1:nrow(childrenIds)) {
			
			ptr <- nav.list[[as.character(childrenIds$id[i])]]
			
			cparamstr <- as.character(childrenIds$params[i])
			
			#print(ptr$label(cparamstr))
			#print(ptr$url(cparamstr))
			
			if ( i == selectedItem) {
			  childtags <- tagAppendChild(childtags, 
				  tags$li( class = "active", a(href = ptr$url(cparamstr), ptr$label(cparamstr))))
			} else {
			  childtags <- tagAppendChild(childtags, 
				  tags$li(a(href = ptr$url(cparamstr), ptr$label(cparamstr))))
			}
			print(childtags)
		  }
		}
    
    print(childtags)
    
    tagsul <- tags$ul( class = "nav nav-list", 
             tags$li( class="nav-header",  "Open Source Projects"))
    tagAppendChild( tagsul, childtags )
    })


})