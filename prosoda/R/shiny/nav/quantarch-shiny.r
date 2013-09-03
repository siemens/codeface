pageWithBreadcrumbAndHeader <- function (breadcrumbPanel, headerPanel, mainPanel) 
{
    bootstrapPage(div(class = "container", div(class = "row", 
        breadcrumbPanel), div(class = "row", headerPanel, 
        mainPanel)))
}
