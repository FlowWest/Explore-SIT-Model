delta_rearingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 2),
      column(width = 8),
      column(width = 2,
             tags$h3('Percent Survival'),
             tags$div(
               tags$h4('Small', style = 'width:100px;'),
               textOutput(ns('surv_sm'))
             ),
             tags$div(
               tags$h4('Medium', style = 'width:100px;'),
               textOutput(ns('surv_md'))
             ),
             tags$div(
               tags$h4('Large', style = 'width:100px;'),
               textOutput(ns('surv_lg'))))
    )
  )
}

delta_rearing <- function(input, output, session) {
  
}

