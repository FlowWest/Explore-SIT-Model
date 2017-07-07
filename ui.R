tagList(
  shinyjs::useShinyjs(),
  navbarPage(
    title = 'SIT Model Explorer (beta)',
    theme = shinytheme('readable'),
    header= includeCSS('styles.css'),
    tabPanel('About', aboutUI('one')),
    tabPanel('Spawning', 
             fluidRow(
               watershedUI('two')
             ),
             spawningUI('one')),
    navbarMenu('Rearing Survival',
               tabPanel('Calculator', icon = icon('calculator'),
                        fluidRow(
                          watershedUI('one'),
                          column(width = 10,
                                 tags$p("This calculator returns juvenile rearing survival rates for each 
                                      watershed by size class given a set of hypothetical monthly conditions. 
                                      The center charts give context for potential monthly input values. 
                                      Inputs that do not vary by month are set to the current value used by the model. 
                                      Users can explore the ranges of survival by size class for any set of expected 
                                      conditions in any month.", 
                                        style = 'padding-left:10px;')  
                          )),
                        rearing_survivalUI('one')),
               tabPanel('Contact Points Map', icon = icon('globe'),
                        contactUI('one'))),
    tabPanel('Sources', sourcesUI('one'))

    
))