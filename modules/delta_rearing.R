delta_rearingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 2,
             radioButtons(ns('delta'), 'Select', choices = c('North Delta', 'South Delta'), 
                          selected = 'North Delta', inline = TRUE)),
      column(width = 8,
             "This calculator returns juvenile rearing survival rates in the delta by size class 
             given a set of hypothetical monthly conditions. The center charts give context for 
             potential monthly input values. Inputs that do not vary by month are set to the current 
             value used by the model. Users can explore the ranges of survival by size class for any 
             set of expected conditions in any month.")
    ),
    fluidRow(
      column(width = 2,
             uiOutput(ns('cont')),
             uiOutput(ns('pred')),
             numericInput(ns('prop_div'), 'Proportion Diverted*', min = 0, max = 1, value = 0, step = .05),
             numericInput(ns('tot_div'), 'Total Diverted*', min = 0, value = 0),
             radioButtons(ns('temp'), 'Temperature Exceedance*', 
                          choiceNames = c('Monthly Mean < 20°C', 'Monthly Mean > 20°C', 'Monthly Mean > 25°C'),
                          choiceValues = c(0 , 1, 2),
                          selected = 0),
             tags$h5('* Value varies by month', class = 'note')),
      column(width = 8,
             div(id = 'context',
                 tags$h3('Context'),
                 fluidRow(
                   tabsetPanel(
                     tabPanel('Total Diversions',
                              plotlyOutput(ns('div'), height = 500)),
                     tabPanel('Proportion Diversion',
                              plotlyOutput(ns('p_div'), height = 500)),
                     tabPanel('Average Temperature',
                              plotlyOutput(ns('temp_graph'), height = 500)),
                     tags$p('1970-1989 Dayflow and Calite')
                   ))
             )),
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
               textOutput(ns('surv_lg'))),
             tags$br(),
             tags$h5('Size Class Ranges:', style = 'font-weight: initial;'),
             tags$h6('small (37.5 - 42 mm)', style = 'font-weight: initial;'), 
             tags$h6('medium (42 - 74 mm)', style = 'font-weight: initial;'), 
             tags$h6('large (74 - 110 mm)', style = 'font-weight: initial;')
             )
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
  
  output$div <- renderPlotly({
    delta_total_div %>% 
      dplyr::filter(watershed == input$delta, month < 9) %>% 
      plot_ly(x = ~fct_inorder(month.name[month]), y = ~tot_div, type = 'scatter', mode = 'markers',
              text = ~paste0('<b>Year </b>', year, "<br>", pretty_num(tot_div), ' cfs'), hoverinfo = 'text',
              marker = list(color = 'rgba(54,144,192,.7)')) %>% 
      add_lines(x = ~fct_inorder(month.name[month]), y = ~fitted(loess(tot_div ~ month)),
                line = list(color = 'rgba(5,112,176, 1)'), inherit = FALSE) %>% 
      layout(xaxis = list(title = 'month'), yaxis = list(title = 'diversions (cfs)'),
             showlegend = FALSE) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$p_div <- renderPlotly({
    delta_prop_div %>% 
      dplyr::filter(watershed == input$delta, month < 9) %>% 
      plot_ly(x = ~fct_inorder(month.name[month]), y = ~prop_div, type = 'scatter', mode = 'markers',
              text = ~paste0('<b>Year </b>', year, "<br>", pretty_num(prop_div), ' cfs'), hoverinfo = 'text',
              marker = list(color = 'rgba(54,144,192,.7)')) %>% 
      add_lines(x = ~fct_inorder(month.name[month]), y = ~fitted(loess(prop_div ~ month)),
                line = list(color = 'rgba(5,112,176, 1)'), inherit = FALSE) %>% 
      layout(xaxis = list(title = 'month'), yaxis = list(title = 'proportion diverted'),
             showlegend = FALSE) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$temp_graph <- renderPlotly({
    delta_temperature %>% 
      dplyr::filter(watershed == input$delta, month < 9) %>% 
      plot_ly(x = ~fct_inorder(month.name[month]), y = ~temperature, type = 'bar',
              hoverinfo = 'text', text = ~temperature) %>% 
      layout(xaxis = list(title = 'month'), 
             yaxis = list(title = 'temperature (°C)'),
             showlegend = FALSE) %>% 
      config(displayModeBar = FALSE)
  })
  
}

