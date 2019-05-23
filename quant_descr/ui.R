library(shiny)
library(rhandsontable)


shinyUI(fluidPage(
  includeCSS('styles.css'),
  withMathJax(),
  wellPanel(h3(strong('Descriptive statistics for a quantitative variable')), 
            checkboxInput('description', label='Click to show more info!'),
            conditionalPanel("input.description == true", 
                             uiOutput('info')
            ), 
    style='padding-bottom: 0px;'),
  fluidRow(
    column(2, wellPanel(
      h4(strong('Enter Data')),
      rHandsontableOutput('table')
    )), 
    column(5, wellPanel(
      h4(strong('Histogram')),
      radioButtons('hist_type', label='Y-axis represents:', 
                   choices=list('counts', 'proportions'), inline=T),
      radioButtons('auto_bins', label='How number of bins is determined', 
                   choices=list('Automatically', 'You choose'), inline=T),
      conditionalPanel('input.auto_bins == "You choose"', 
                       sliderInput('n_bins', label=NULL, min=1, max=30, value=10, step=1)               
      )
      ,
      plotOutput('hist', height='250px')
    )),
    column(5, wellPanel(
      h4(strong('Boxplot')),
      radioButtons('box_type', label='Boxplot type', 
                   choices=list('simple', 'modified'), inline=T),
      conditionalPanel('input.box_type == "modified"', 
                       checkboxInput('fences', label='Show fences?'),
                       conditionalPanel('input.fences == true', 
                                        uiOutput('fences_table')   
                       )
      ),
      plotOutput('box', height='200px')
    )),
    column(6, wellPanel(
      h4(strong('Numerical summaries')),
      uiOutput('stats_table')
    )), 
    column(4, wellPanel(
      h4(strong('Sorted from smallest to largest')), 
      textOutput('sorted')
    ))
  )
))
