watershedUI <- function(id) {
  ns <- NS(id)
  
  column(width = 2,
         selectInput(ns('shed'), 'Watershed', choices = misc_inputs$Watershed, selected = 'Merced River'))
}

watershed <- function(input, output, session) {
  watershed <- reactive(input$shed)
  return(watershed)
  
  # observe({
  #   if ('Spawning') {
  #     shinyjs::hide()
  #   }
  # })
}