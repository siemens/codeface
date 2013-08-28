gridsterGetConfigButton <- function(inputId) {
  tagList(
    singleton(tags$head(tags$script(src = "initwidgets.js"))),
    tags$button(id = inputId,
                class = "gridsterSaveConfig btn",
                type = "button",
                tags$i(class="icon-th")
				)
	)
}