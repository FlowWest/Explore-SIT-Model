shinyUI(navbarPage(
  title = 'SIT Model Explorer',
  theme = shinytheme('readable'),
  header= includeCSS('styles.css'),
  navbarMenu('Rearing Survival',
             tabPanel('Calculator', icon = icon('calculator'),
                      watershedUI('one'),
                      rearing_survivalUI('one')),
             tabPanel('Contact Points Map', icon = icon('globe'),
                      contactUI('one'))),
  tabPanel('Spawning', 
           watershedUI('two'),
           spawningUI('one')),
  tabPanel('Sources', sourcesUI('one')),
  tabPanel('About', aboutUI('one'))
  
))
