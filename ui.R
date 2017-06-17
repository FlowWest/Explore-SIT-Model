shinyUI(fluidPage(
  title = 'SIT Model Explorer',
  theme = shinytheme('readable'),
  tags$header(includeCSS('styles.css')),
  watershedUI('one'),
  tabsetPanel(
    tabPanel('Rearing Survival', rearing_survivalUI('one')),
    tabPanel('Spawning', spawningUI('one')),
    tabPanel('Sources', sourcesUI('one'))
  )

))
