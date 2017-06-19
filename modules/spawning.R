spawningUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 7,
             tags$div(id = 'spawn_fry_text',
                      tags$h2('Spawners to Fry Model Inputs'),
                      tags$p('Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Duis sollicitudin nisl nec sagittis tempus. Vestibulum pretium 
                    libero ut quam tincidunt malesuada. Fusce egestas fermentum justo, 
                    eu porta lectus efficitur in. Proin id nisi nec tortor fringilla 
                    consectetur ac et nulla. Sed vel quam neque. Donec nibh magna, luctus 
                    ac suscipit ut, sollicitudin at ligula. Phasellus porttitor vel dui 
                    vitae ultricies. Nam placerat commodo fermentum. Sed ut nisi erat. 
                    Phasellus faucibus laoreet quam non posuere. Fusce viverra sed tortor 
                    sit amet congue.'),
                      tags$p(' Integer non posuere velit. Vestibulum tincidunt, quam efficitur scelerisque
                    cursus, eros elit congue elit, placerat commodo mi urna ut neque. Aliquam erat
                    volutpat. Sed risus sapien, posuere et lorem in, faucibus ullamcorper orci.
                    Sed at feugiat lacus, in dignissim purus. Donec mollis ultricies commodo. 
                    Aenean finibus augue eu mauris ultricies dictum. Ut pellentesque tellus et ligula 
                    dapibus imperdiet. Cras mattis urna sed turpis suscipit dictum. Donec posuere mi 
                    metus, id facilisis felis euismod et. Proin malesuada, odio ornare condimentum 
                    volutpat, metus mauris mattis quam, eget efficitur nisi quam elementum justo.'),
                      tags$p('To explore this relation further, visit the', 
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
             ),
             tags$div(
               tags$h4('Fecundity', class = 'spawn'),
               textOutput(ns('fecund'))
             ),
             tags$div(
               tags$h4('Redd Size', class = 'spawn'),
               textOutput(ns('redd_sz'))
             )
      )
    ))
  
}

spawning <- function(input, output, session, shed) {
  
  this_shed <- reactive({
    dplyr::filter(misc_inputs, Watershed == shed())
  })
  
  output$spawn_shed <- renderText(shed())
  output$init_ad <- renderText(this_shed()$init.adult)
  output$hatch <- renderText(pretty_num(this_shed()$hatch.alloc * qunif(0.5, 80000, 150000), 0))
  output$ad_harv <- renderText(this_shed()$A.HARV)
  output$scour <- renderText(this_shed()$P.scour.nst)
  output$fecund <- renderText(5522)
  output$redd_sz <- renderText(12.4)
  
}