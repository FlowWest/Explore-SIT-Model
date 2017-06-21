aboutUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             tags$h1('Fall Chinook Science Interation Team Model'),
             tags$br(),
             tags$h4('Primary Author', tags$a(href = 'mailto:jt.peterson@oregonstate.edu', 'James T. Peterson')),
             tags$h5('U.S. Geological Survey, Oregon Cooperative Cooperative'),
             tags$h5('Fish and Wildlife Research Unit, Oregon State University'),
             tags$h5('Corvallis, Oregon 97331-3803'),
             tags$br(),
             tags$img(src = 'conceptual_model.png', width = 1000),
             tags$br(),
             tags$br())
    ),
    fluidRow(
      column(width = 12,
             tags$a(tags$img(src = 'TransLogoTreb.png', width = 200), href = 'http://www.flowwest.com/', target = '_blank', 
                    style = 'display: inline-block;'),
             tags$h5('App created and maintained by', tags$a(href = 'mailto:sgill@flowwest.com', 'Sadie Gill', target = '_blank'),
                     style = 'display: inline-block; font-weight: 300;'))
    )
  )
  
}

about <- function(input, output, session) {
  
}