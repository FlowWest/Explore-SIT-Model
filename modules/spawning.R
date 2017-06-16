spawningUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             tags$div(
               tags$h4('Initial Natural Adults'),
               textOutput(ns('init_ad'))
             ),
             tags$div(
               tags$h4('Hatchery Allocation'),
               textOutput(ns('hatch'))
             ),
             tags$div(
               tags$h4('Adult Harvest'),
               textOutput(ns('ad_harv'))
             ),
             tags$div(
               tags$h4('Proportion Scour'),
               textOutput(ns('scour'))
             ),
             tags$div(
               tags$h4('Fecundity'),
               textOutput(ns('fecund'))
             ),
             tags$div(
               tags$h4('Redd Size'),
               textOutput(ns('redd_sz'))
             ))
    )
  )
  
}

spawning <- function(input, output, session, shed) {
  
  this_shed <- reactive({
    dplyr::filter(misc_inputs, Watershed == shed())
  })
  
  output$init_ad <- renderText(this_shed()$init.adult)
  output$hatch <- renderText(this_shed()$hatch.alloc * qunif(0.5, 80000, 150000))
  output$ad_harv <- renderText(this_shed()$A.HARV)
  output$scour <- renderText(this_shed()$P.scour.nst)
  output$fecund <- renderText(5522)
  output$redd_sz <- renderText(12.4)
  
}