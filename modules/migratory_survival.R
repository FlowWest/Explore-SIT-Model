migratory_survivalUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             tags$h3('Sub-Model Inputs'),
             numericInput(ns('Qcms'), min = 0, value = 600),
             numericInput(ns('prop_div'), 'Proportion Diverted', min = 0, max = 1, value = .6, step = .05),
             numericInput(ns('tot_div'), 'Total Diverted', min = 0, value = 300),
             numericInput(ns('temp'), 'Average Temperature (Celcius)', min = 0, value = 16))
    )
  )
  
}

migratory_survival <- function(input, output, session) {
  
Juv.OUTM.S(Q.cms = input$Qcms, 
           aveT, tot.div, prop.div)
  
}

