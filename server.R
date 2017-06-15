shinyServer(function(input, output) {
  
  this_watershed <- callModule(module = watershed, id = 'one')
  callModule(module = rearing_survival, id = 'one', shed = this_watershed)
  callModule(module = spawning, id = 'one', shed =  this_watershed)
})
