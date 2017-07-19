shinyServer(function(input, output, session) {
  
  one_watershed <- callModule(module = watershed, id = 'one')
  two_watershed <- callModule(module = watershed, id = 'two')
  
  callModule(module = rearing_survival, id = 'one', shed = one_watershed)
  callModule(module = spawning, id = 'one', shed =  two_watershed)
  callModule(module = contact, id = 'one')
  callModule(module = delta_rearing, id = 'two')
  
  observeEvent(input$`one-shed`, {
    updateSelectInput(session, 'two-shed', selected = input$`one-shed`)
  })
  
  observeEvent(input$`two-shed`, {
    updateSelectInput(session, 'one-shed', selected = input$`two-shed`)
  })
  
})
