contactUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 2,
             tags$h2('Contact Points'),
             tags$a('Source', href = 'http://www.calfish.org/tabid/420/Default.aspx', target = '_blank')),
      column(width = 10,
             withSpinner(leafletOutput(ns('contact_map'), height = 800), color = '#666666', type = 8))
    )
  )
  
}

contact <- function(input, output, session) {
  output$contact_map <- renderLeaflet({

    leaflet() %>%
      addPolygons(data = CVPIAwatersheds, weight = 2, group = 'Watersheds', label = ~Moonshed, fillOpacity = 0.1) %>%
      addProviderTiles(providers$CartoDB.Positron, group = 'Grey Basemap') %>%
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satelite Basemap') %>%
      addLayersControl(baseGroups = c('Grey Basemap', 'Satelite Basemap'), overlayGroups = c('Watersheds', 'Contact Points')) %>%  
      addCircleMarkers(data = contact_pts, weight = 1, radius = 6, opacity = 1, fillOpacity = 0.75,
                 popup = ~paste(paste0("<b>", SiteType, "</b>"), SiteName, sep = "<br/>"),
                 label = ~WebLegend, color = '#C51B8A', group = 'Contact Points', 
                 clusterOptions = markerClusterOptions())
  })
}
