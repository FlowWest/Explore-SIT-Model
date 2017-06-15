shinyUI(fluidPage(
  watershedUI('one'),
  tabsetPanel(
    tabPanel('Rearing Survival', rearing_survivalUI('one'))
  )

))
