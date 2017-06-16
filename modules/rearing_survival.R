rearing_survivalUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 3,
             tags$h3('Sub-Model Inputs'),
             radioButtons(ns('hab'), label = 'Habitat',
                          choices = c('In-Channel', 'Floodplain'), selected = 'In-Channel'),
             uiOutput(ns('cont')),
             uiOutput(ns('pred')),
             uiOutput(ns('strand')),
             numericInput(ns('prop_div'), 'Proportion Diverted', min = 0, max = 1, value = .6, step = .05),
             numericInput(ns('tot_div'), 'Total Diverted', min = 0, value = 300),
             radioButtons(ns('temp'), 'Temperature Exceedance', 
                          choiceNames = c('Monthly Mean < 20°C', 'Montly Mean > 20°C', 'Montly Mean > 25°C'),
                          choiceValues = c(0 , 1, 2),
                          selected = 0)),
      column(width = 6,
             tags$h3('Context'),
             tabsetPanel(
               tabPanel('Total Diversions',
                        plotlyOutput(ns('div')),
                        tags$p('1970-1989 Cal Lite Simulated flows')),
               tabPanel('Proportion Diversion',
                        plotlyOutput(ns('p_div')),
                        tags$p('1970-1989 Cal Lite Simulated flows')),
               tabPanel('Contact Points',
                        withSpinner(leafletOutput(ns('contact_map'), height = 600), color = '#666666', type = 8))
             )),
      column(width = 3,
             tags$h3('Percent Survival'),
             tags$div(
               tags$h4('Small'),
               textOutput(ns('surv_sm'))
             ),
             tags$div(
               tags$h4('Medium'),
               textOutput(ns('surv_md'))
             ),
             tags$div(
               tags$h4('Large'),
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
  
  output$div <- renderPlotly({
    validate(
      need(shed() %in% shed_with_div, 'No diversions on this watershed.')
    )
    
    month() %>% 
      dplyr::select(year, month, diversion) %>% 
      plot_ly(x = ~forcats::fct_inorder(month.abb[month]), y = ~diversion, type = 'scatter', mode = 'markers') %>% 
      add_lines(y = ~fitted(loess(diversion ~ month))) %>% 
      layout(xaxis = list(title = 'month')) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$p_div <- renderPlotly({
    validate(
      need(shed() %in% shed_with_div, 'No diversions on this watershed.')
    )
    
    month() %>% 
      dplyr::select(year, month, diversion, flow) %>% 
      dplyr::mutate(p_div = diversion / (diversion + flow)) %>% 
      plot_ly(x = ~forcats::fct_inorder(month.abb[month]), y = ~p_div, type = 'scatter', mode = 'markers') %>% 
      add_lines(y = ~fitted(loess(p_div ~ month))) %>% 
      layout(xaxis = list(title = 'month'), yaxis = list(title = 'proportion diverted')) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$contact_map <- renderLeaflet({
    
    pal <- colorFactor(palette = 'Paired', domain = contact_pts$WebLegend)
    
    leaflet() %>% 
      addPolygons(data = CVPIAwatersheds, weight = 1, group = 'Watersheds', label = ~Moonshed) %>% 
      addCircleMarkers(data = contact_pts, weight = 1, radius = 7, opacity = 1, fillOpacity = 0.75, 
                       popup = ~paste(paste0("<b>", SiteType, "</b>"), SiteName, sep = "<br/>"), 
                       label = ~WebLegend, color = ~pal(WebLegend)) %>%
      addProviderTiles(providers$CartoDB.Positron, group = 'Grey Basemap') %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satelite Basemap') %>%
      addLayersControl(baseGroups = c('Grey Basemap', 'Satelite Basemap'), overlayGroups = c('Watersheds')) 
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
    numericInput(ns('contact'), 'Number of Contact Points', min = 0, value = this_misc()$contact)
  })
  
  output$pred <- renderUI({
    numericInput(ns('high_pred'), 'Proportion High Predation', min = 0, max = 1, value = this_misc()$High.pred, step = .05)
  })
  
  output$strand <- renderUI({
    numericInput(ns('prop_strand'), 'Proportion Stranded', min = 0, max = 1, value = this_misc()$P.strand.early, step = .05)
  })
  
  surv <- reactive({
    
    if (input$hab == 'In-Channel') {
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
