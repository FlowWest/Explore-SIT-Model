shinyUI(fluidPage(
  title = 'Exploring SIT Model',
  theme = shinytheme('readable'),
  watershedUI('one'),
  tabsetPanel(
    tabPanel('Rearing Survival', rearing_survivalUI('one')),
    tabPanel('Spawning', spawningUI('one')),
    tabPanel('Sources', sourcesUI('one'))
  )

))
