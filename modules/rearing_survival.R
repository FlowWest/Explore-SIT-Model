rearing_survivalUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 2,
             tags$h3('Sub-Model Inputs'),
             radioButtons(ns('hab'), label = 'Habitat',
                          choices = c('In-Channel', 'Floodplain'), selected = 'In-Channel', inline = TRUE),
             uiOutput(ns('cont')),
             uiOutput(ns('pred')),
             radioButtons(ns('timing'), label = 'Proportion Stranded*', choices = c('Early', 'Late'), selected = 'Early', inline = TRUE),
             uiOutput(ns('strand')),
             numericInput(ns('prop_div'), 'Proportion Diverted**', min = 0, max = 1, value = 0, step = .05),
             numericInput(ns('tot_div'), 'Total Diverted**', min = 0, value = 0),
             radioButtons(ns('temp'), 'Temperature Exceedance**', 
                          choiceNames = c('Monthly Mean < 20°C', 'Montly Mean > 20°C', 'Montly Mean > 25°C'),
                          choiceValues = c(0 , 1, 2),
                          selected = 0),
             tags$h5('* Use early stranding rate for months January through April', class = 'note'),
             tags$h5('** Value varies by month', class = 'note')),
      column(width = 8, 
             div(id = 'context',
                 tags$h3('Context'),
                 tabsetPanel(
                   tabPanel('Total Diversions',
                            plotlyOutput(ns('div')),
                            tags$p('1970-1989 Cal Lite Simulated flows')),
                   tabPanel('Proportion Diversion',
                            plotlyOutput(ns('p_div')),
                            tags$p('1970-1989 Cal Lite Simulated flows')),
                   tabPanel('Average Temperature',
                            plotlyOutput(ns('temp_graph')),
                            tags$p('1970-1989 Cal Lite Simulated flows')),
                   tabPanel('Flood Plain Activation',
                            plotlyOutput(ns('fp')),
                            tags$p('1970-1989 Cal Lite Simulated flows'))
                 ))),
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
               textOutput(ns('surv_lg'))
             ))
    )
  )
  
}

rearing_survival <- function(input, output, session, shed) {
  
  ns <- session$ns
  
  this_misc <- reactive({
    dplyr::filter(misc_inputs, Watershed == shed())
  })
  
  month <- reactive({
    monthly %>% 
      dplyr::filter(watershed == shed(), year >= 1970, year < 1990, month < 9)
  })
  
  observeEvent(shed(), {
    shinyjs::reset('prop_div')
    shinyjs::reset('tot_div')
  })
  
  observe({
    if (shed() %in% c('Yolo Bypass', 'Sutter Bypass')) {
      shinyjs::hide('hab')
      shinyjs::hide('prop_div')
      shinyjs::hide('tot_div')
      shinyjs::hide('timing')
    } else {
      shinyjs::show('hab')
      shinyjs::show('prop_div')
      shinyjs::show('tot_div')
      shinyjs::show('timing')
    }
  })
  
  output$div <- renderPlotly({
    validate(
      need(shed() %in% shed_with_div, 'No diversions on this watershed.')
    )
    
    month() %>% 
      dplyr::select(year, month, diversion) %>% 
      plot_ly(x = ~forcats::fct_inorder(month.abb[month]), y = ~diversion, type = 'scatter', mode = 'markers',
              text = ~paste0('<b>Year </b>', year, "<br>", pretty_num(diversion), ' cfs'), hoverinfo = 'text',
              marker = list(color = 'rgba(54,144,192,.7)')) %>% 
      add_lines(y = ~fitted(loess(diversion ~ month)),
                line = list(color = 'rgba(5,112,176, 1)')) %>% 
      layout(xaxis = list(title = 'month'), yaxis = list(title = 'diversions (cfs)'),
             showlegend = FALSE) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$p_div <- renderPlotly({
    validate(
      need(shed() %in% shed_with_div, 'No diversions on this watershed.')
    )
    
    month() %>% 
      dplyr::select(year, month, diversion, flow) %>% 
      dplyr::mutate(p_div = diversion / (diversion + flow)) %>% 
      plot_ly(x = ~forcats::fct_inorder(month.abb[month]), y = ~p_div, type = 'scatter', mode = 'markers',
              text = ~paste0('<b>Year </b>', year, "<br>", pretty_num(p_div)), hoverinfo = 'text', 
              marker = list(color = 'rgba(54,144,192,.7)')) %>% 
      add_lines(y = ~fitted(loess(p_div ~ month)),
                line = list(color = 'rgba(5,112,176, 1)')) %>% 
      layout(xaxis = list(title = 'month'), yaxis = list(title = 'proportion diverted'),
             showlegend = FALSE) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$temp_graph <- renderPlotly({
    month() %>% 
      dplyr::select(year, month, avg_temp) %>% 
      plot_ly(x = ~forcats::fct_inorder(month.abb[month]), y = ~avg_temp, type = 'scatter', mode = 'markers',
              text = ~paste0('<b>Year </b>', year, "<br>", pretty_num(avg_temp), ' °C'), hoverinfo = 'text', 
              marker = list(color = 'rgba(54,144,192,.7)'), jitter = .3) %>% 
      add_lines(y = ~fitted(loess(avg_temp ~ month)),
                line = list(color = 'rgba(5,112,176, 1)')) %>% 
      layout(xaxis = list(title = 'month'), yaxis = list(title = 'average temperature (°C)'),
             showlegend = FALSE) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$fp <- renderPlotly({
    month() %>% 
      dplyr::left_join(threshold) %>% 
      dplyr::select(watershed, year, month, flow, threshold, acres) %>% 
      dplyr::mutate(acres = replace(acres, flow < threshold, 0),
                    sq_meters = acres * 4046.86,
                    fp_active = ifelse(flow < threshold, 0, 1)) %>% 
      dplyr::group_by(watershed, month) %>% 
      dplyr::summarise(prop_active = sum(fp_active, na.rm = TRUE)/n()) %>% 
      plot_ly(x = ~forcats::fct_inorder(month.abb[month]), y = ~prop_active, type = 'bar',
              hoverinfo = 'text', text = ~prop_active) %>% 
      layout(xaxis = list(title = 'month'), 
             yaxis = list(title = 'proportion of years with activated floodplain', range = c(0, 1)),
             showlegend = FALSE) %>% 
      config(displayModeBar = FALSE)
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
  
  output$cont <- renderUI({
    if (shed() %in% c('Yolo Bypass', 'Sutter Bypass')) {
      NULL
    } else {
      numericInput(ns('contact'), 'Number of Contact Points', min = 0, value = this_misc()$contact)
    }
  })
  
  output$pred <- renderUI({
    numericInput(ns('high_pred'), 'Proportion High Predation', min = 0, max = 1, value = this_misc()$High.pred, step = .05)
  })
  
  timin <- reactive({
    if (input$timing == 'Early') {
      this_misc()$P.strand.early
    } else {
      this_misc()$P.strand.late
    }
  })
  
  output$strand <- renderUI({
    if (shed() %in% c('Yolo Bypass', 'Sutter Bypass')) {
      NULL
    } else {
      numericInput(ns('prop_strand'), label = NULL, min = 0, max = 1, value = timin(), step = .05)
    }    
  })
  
  surv <- reactive({
    
    if (shed() %in% c('Yolo Bypass', 'Sutter Bypass')) {
      surv <- Juv.BYP.Ss(maxT25 = temps()$T25,
                        aveT20 = temps()$T20,
                        high.pred = input$high_pred) * 100
    } else if (input$hab == 'In-Channel') {
      surv <- Juv.IC.S(maxT25 = temps()$T25,
                       aveT20 = temps()$T20,
                       high.pred = input$high_pred,
                       no.con.pts = input$contact,
                       prop.div = input$prop_div,
                       tot.div = input$tot_div,
                       strand = input$prop_strand) * 100
    } else {
      surv <- Juv.FP.S(maxT25 = temps()$T25,
                       aveT20 = temps()$T20,
                       high.pred = input$high_pred,
                       no.con.pts = input$contact,
                       prop.div = input$prop_div,
                       tot.div = input$tot_div,
                       strand = input$prop_strand,
                       wks.fld = 2) * 100
    }
    
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


# Juv.FP.S(maxT25, aveT20, high.pred, no.con.pts, prop.div, tot.div, strand, wks.fld = 2)
# # for each watershed have range of values for each variable show change vs current inputs
# # which hydrology to use...
