source('tstat.R')
source('pval.R')
source('combined.R')
library(shiny); library(graphics)
ui <- tagList(
  includeCSS('styles.css'),
  withMathJax(),
  navbarPage(title='Test for population mean', inverse=T,
             tabPanel('Introduction', 
                  wellPanel(   
                  h3(strong('Introduction to hypothesis test for a population mean \\(\\mu\\)')), 
                  'Click on the tabs on the black bar at the top of the page to navigate to
                  the different apps. The tabs are:',
                  tags$ul(
                    tags$li('Introduction (currently selected)'),
                    tags$li('Test statistic'),
                    tags$li('P-value'),
                    tags$li('Test statistic / p-value')
                  )
              )),
                 tabPanel('Test statistic', tstat.ui()),
                 tabPanel('P-value', pval.ui()), 
                 tabPanel('Test statistic / p-value', combined.ui())
))

server <- function(input, output){
  tstat.serv(input, output)
  pval.serv(input, output)  
  combined.serv(input, output)
}

shinyApp(ui = ui, server = server)