
## TODO: rewrite this
## StatusOutput os a simple output that does nothing special other than
## outputting some html (so it could easily be replaced by some shiny
## renderUI or whatever function
statusOutput <- function(outputId) {
  tags$div(id=outputId, class="status_output",
           tags$div(class = 'grid_bigtext'),
           tags$p()
  )
}

## JustGage Output
## TODO: rewrite this to allow better initialization (max, title)
justgageOutput <- function(outputId, width, height) {
  tags$div(id = outputId, class = "justgage_output", style = sprintf("width:%dpx; height:%dpx", width, height))
}
