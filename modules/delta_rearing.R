delta_rearingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 2,
             radioButtons(ns('delta'), 'Select', choices = c('North Delta', 'South Delta'), 
                          selected = 'North Delta', inline = TRUE),
             uiOutput(ns('cont')),
             uiOutput(ns('pred')),
             numericInput(ns('prop_div'), 'Proportion Diverted*', min = 0, max = 1, value = 0, step = .05),
             numericInput(ns('tot_div'), 'Total Diverted*', min = 0, value = 0),
             radioButtons(ns('temp'), 'Temperature Exceedance*', 
                          choiceNames = c('Monthly Mean < 20°C', 'Monthly Mean > 20°C', 'Monthly Mean > 25°C'),
                          choiceValues = c(0 , 1, 2),
                          selected = 0),
             tags$h5('* Value varies by month', class = 'note')),
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
 
  ns <- session$ns
  
  this_delta_input <- reactive({
    filter(delta_inputs, watershed == input$delta)
  })
  
  output$cont <- renderUI({
    numericInput(ns('contact'), 'Number of Contact Points', min = 0, value = this_delta_input()$contact_points)
  })
  
  output$pred <- renderUI({
    numericInput(ns('high_pred'), 'Probability High Predation', min = 0, max = 1, value = this_delta_input()$high_pred, 
                 step = .05)
  })
  

  
  temps <- reactive({
    if (input$temp == 0) {
      maxT25 <- 0
      aveT20 <- 0
    } else if (input$temp == 1) {
      maxT25 <- 0
      aveT20 <- 1
    } else {
      maxT25 <- 1
      aveT20 <- 1
    }
    return(list(T25 = maxT25, T20 = aveT20))
    
  })
  
 surv <- reactive({
    surv <- Juv.DLT.S(maxT25 = temps()$T25, 
              aveT20 = temps()$T20, 
              high.pred = input$high_pred, 
              no.con.pts = input$contact, 
              prop.div = input$prop_div, 
              tot.div = input$tot_div) * 100
    
    paste(round(surv, 2), '%')
    
  })
  
  output$surv_sm <- renderText({
    surv()[1]
    
  })
  
  output$surv_md <- renderText({
    surv()[2]
    
  })
  
  output$surv_lg <- renderText({
    surv()[3]
    
  }) 
}

