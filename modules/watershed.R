watershedUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             tags$h1('SIT Model Explorer'),
             selectInput(ns('shed'), 'Watershed', choices = misc_inputs$Watershed, selected = 'Merced River'))
    )
  )
}

watershed <- function(input, output, session) {
  watershed <- reactive(input$shed)
  return(watershed)
}