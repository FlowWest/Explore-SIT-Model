shinyUI(navbarPage(
  title = 'SIT Model Explorer',
  theme = shinytheme('readable'),
  header= includeCSS('styles.css'),
  tabPanel('Rearing Survival', 
           watershedUI('one'),
           rearing_survivalUI('one')),
  tabPanel('Spawning', 
           watershedUI('two'),
           spawningUI('one')),
  tabPanel('Sources', sourcesUI('one'))
  
  
))
