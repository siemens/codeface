shinyUI(bootstrapPage(
    
    div(class = "container",
        div(class = "row",
            div( class="span4", style = "padding: 10px 0px;",  
                 textOutput("monitor"))),
        div(class = "row",
            div( class="span8", style = "padding: 10px 0px;",  
                 modalDialogButton( "selectpidsui", modalDialogTitle="Select Projects to compare with", 
                                    "selected", actionButtonLabel="OK", dismissButtonLabel="Close" ) ))
    )
    ,
    cookieInput("qacompareids")
))
