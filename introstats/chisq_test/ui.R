library(shiny)
library(rhandsontable)

shinyUI(fluidPage(
  includeCSS('styles.css'),
  withMathJax(),
navbarPage(title='Chi-squared test', inverse=T,
           tabPanel('Introduction',
            wellPanel(
              h3(strong('Introduction to Chi-squared \\((\\chi^2)\\) test')), 
              'Click on the tabs on the black bar at the top of the page to navigate to the 
              different apps. The tabs are:',
              tags$ul(
                tags$li(strong('Introduction'), '(currently selected)'), 
                tags$li(strong('Analysis:'), 'Enter in the counts into a two-way table and calculate the 
                        test statistic and p-value.'), 
                tags$li(strong('Picture of p-value:'), 'View the p-value as the area under 
                               the \\(\\chi^2\\) distribution (for a specified df) for values 
                               greater than the observed test statistic.')
              )  
            )
          ),
           tabPanel('Analysis',
  wellPanel(
    h3(strong('Chi-squared \\((\\chi^2)\\) test')),
    tags$div(
      checkboxInput('show_info', label='Click to show more info!'),
      style='margin-bottom: -10px;'
    ),
    conditionalPanel('input.show_info == true', 
      uiOutput('info')
    , style='margin-bottom: -15px;')
  ),
  fluidRow(
    column(4, wellPanel(
      tags$div(id="myScrollBox",
      radioButtons('nrow', label='How many rows?', inline=T, choices=list(2,3,4,5)),
      radioButtons('ncol', label='How many columns?', inline=T, choices=list(2,3,4,5)), 
      strong('Enter Counts Here'),
      rHandsontableOutput('table')
      )),
      wellPanel( tags$div(id="myScrollBox",
        strong('Observed and Expected Counts'), 
        uiOutput('twoway')
    ))),
  column(8, wellPanel(
    tabsetPanel(
      tabPanel('Conditions', uiOutput('conditions')), 
      tabPanel('Test statistic', uiOutput('test_stat')), 
      tabPanel('P-value', uiOutput('pval_tab')), 
      tabPanel('Graphical Interpretation', uiOutput('graph_tab')),
      type='pills', id='tab'
    )
    
  ))
  )
           ), 
  tabPanel('Picture of p-value', 
           uiOutput('pval_pic_tab')
  )
)
))
