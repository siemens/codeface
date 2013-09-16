##
## Creates a hidden div which defines a cookie with name "inputId"
##
cookieInput <- function(inputId) {
  tagList(
    singleton(tags$head(tags$script(src = "qa_cookie.js"))),
    tags$div( id=inputId, class='quantarch-cookie-input hidden'))
}


##
## Updates a Cookie defined in ui.r
##
## session: Shiny session variable (reactive)
## pathLevel: defines the path for which this cookie is valid
##    pathLevel=0 means "Cookie valid for shiny app path"
##    pathLevel=1 means "Cookie valid for shiny app path parent" etc.
## expiresInDays: translates to cookie's expires tag
##    expiresInDays=NULL creates a session Cookie
##    expiresInDays = n stores Cookie for n days
##    expiresInDays = 0 deletes the Cookie  
##
## (this function uses  the jQuery Cookie Plugin available at
##  https://github.com/carhartl/jquery-cookie, 
##  Released under the MIT license)
##
updateCookieInput <- function(session, inputId, value, pathLevel=0, expiresInDays=NULL ) {
  
  hostname <- session$clientData$url_hostname
  pathname <- session$clientData$url_pathname
  
  message = list()
  message$name <- inputId
  message$value <- value
  
  pathelements <- strsplit(pathname,"/")[[1]]
  previous <- length(pathelements)-abs(pathLevel)
  mypath <- "/"
  if (previous > 0) mypath <- paste(paste(pathelements[1:previous],sep="",collapse="/"),"/",sep="")
  
  message$options <- list(
    domain = hostname,
    path = mypath,
    expires = expiresInDays
  )
  
  #   if (expireSeconds > 0) {
  #     message$options$expires <- format((Sys.time() + expireSeconds), tz="UTC", usetz=TRUE)}
  
  #print(message)
  session$sendInputMessage( inputId, message )
}