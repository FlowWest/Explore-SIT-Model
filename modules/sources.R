sourcesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12, style = 'padding-left:30px;',
             tags$h3('Spawning'),
             tags$a(href = 'http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.456.4017&rep=rep1&type=pdf', 
                    target = '_blank', 'Redd Size - Healey (1991)'),
             tags$h3('Juvenile Survival'),
             tags$a(href = 'https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/Brett+temperature+tolerance.pdf',
                    target = '_blank', 'Maximum Water Temperature'),
             tags$br(),
             tags$a(href = 'https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/NAJFM_24_545-560_Postrelease+Survival+juvenile+Chin.pdf',
                    target = '_blank', 'Body Size'),
             tags$br(),
             tags$a(href = 'https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/231_Marine_and_Cech_2004.pdf',
                    target = '_blank', 'Predation'),
             tags$br(),
             tags$a(href = 'https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/Environ_Bio_Fish_Cavallo_et_al_2012.pdf',
                    target = '_blank', 'Predator Prevalence'),
             tags$br(),
             tags$a(href = 'https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/Habitat+Alterations+and+a+Nonnative+Predator+the+Striped+Bass+Increase+Native+Chinook+Salmon+Mortality.pdf',
                    target = '_blank', 'Contact Points'),
             tags$br(),
             tags$a(href = ' 	https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/Sommer+et+al+Floodplain+rearing+growth+survival.pdf',
                    target = '_blank', 'Bypass'),
             tags$h3('Outmigrating Survival'),
             tags$a(href = 'https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/2012+Chinook+south+Delta+survival+study+9+4+15+Final.pdf',
                    target = '_blank', 'Delta')
             # tags$br(),
             # tags$h3('Model Parameters'),
             # tags$img(src = 'input_sources.png')
             ))
  )
}

sources <- function(input, output, session) {
  
  
}