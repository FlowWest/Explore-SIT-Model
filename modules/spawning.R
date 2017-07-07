spawningUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 7,
             tags$div(id = 'spawn_fry_text',
                      tags$h2('Spawners to Fry Model Inputs'),
                      tags$p('Each watershed estimates reproductive success as a function of the number of females, 
                             fecundity, and egg-to-fry survival. Potential spawners are the sum of natural adults 
                             and hatchery strays, less a proportional harvest. Each female is assumed to carry 5522 eggs,
                             and each redd is assumed to require 12.4 square meters of suitable spawning habitat. 
                             Each watershed also has an estimated loss of eggs due to scour that results from fluctuating river flows.'),
                      tags$h5('To explore this relation further, visit the', 
                             tags$a(href = 'https://flowwest.shinyapps.io/carrying-capacity-app/', 
                                    target = '_blank', 'Carrying Capacity App.')))),
      column(width = 5,
             textOutput(ns('spawn_shed')),
             tags$div(
               tags$h4('Initial Natural Adults', class = 'spawn'),
               textOutput(ns('init_ad'))
             ),
             tags$div(
               tags$h4('Hatchery Allocation', class = 'spawn'),
               textOutput(ns('hatch'))
             ),
             tags$div(
               tags$h4('Adult Harvest', class = 'spawn'),
               textOutput(ns('ad_harv'))
             ),
             tags$div(
               tags$h4('Proportion Scour', class = 'spawn'),
               textOutput(ns('scour'))
             )
      )
    ))
  
}

spawning <- function(input, output, session, shed) {
  
  this_shed <- reactive({
    dplyr::filter(misc_inputs, Watershed == shed())
  })
  
  output$spawn_shed <- renderText(shed())
  output$init_ad <- renderText(pretty_num(this_shed()$init.adult, 0))
  output$hatch <- renderText(pretty_num(this_shed()$hatch.alloc * qunif(0.5, 80000, 150000), 0))
  output$ad_harv <- renderText(this_shed()$A.HARV)
  output$scour <- renderText(this_shed()$P.scour.nst)
  
}