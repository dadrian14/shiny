library(shiny)

shinyUI(tagList(includeCSS("styles.css"),
                navbarPage(
  title='Hypothesis test for proportion', 
  
    tabPanel('Introduction', 
             uiOutput('intro_panel')
    ),
    tabPanel('Test statistic', 
             uiOutput('tstat_panel')
    ),
    tabPanel('P-value', 
             uiOutput('pval_panel')
    ), 
    tabPanel('Test statistic / p-value', 
             uiOutput('combined_panel')        
    ),
  inverse=T
)))
