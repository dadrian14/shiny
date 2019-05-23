library(shiny)

shinyUI(fluidPage(
  includeCSS('styles.css'),
  withMathJax(),
  wellPanel(
    h3(strong('Leverage')),
    'In a dataset with 2 explanatory variables \\((x_1, x_2)\\)'
  ),
  fluidRow(
  column(4, wellPanel(
    actionButton('new_samp', label=h4('Draw new sample!')), br(), br(),
    h4('Characteristics of sample:'),
    numericInput('mean_x1', label='Mean of \\(x_1\\)', value=10),
    numericInput('sd_x1', label='Standard deviation of \\(x_1\\)', value=2),
    numericInput('mean_x2', label='Mean of \\(x_2\\)', value=10),
    numericInput('sd_x2', label='Standard deviation of \\(x_2\\)', value=3),
    numericInput('cor', label='Correlation', value=0, min=-.99, max=.99, step=.01)
  )),
  column(8, wellPanel(
    wellPanel(
      'Calculate leverage for which point \\((x_1^\\ast, x_2^\\ast)\\)?',
    fluidRow(
      column(6, 
        numericInput('x1star', label='\\(x_1^\\ast\\)', value=15)     
      , style='padding-right:10px'),
      column(6, 
        numericInput('x2star', label='\\(x_2^\\ast\\)', value=15)     
      )
    ),
    strong('Leverage = ', textOutput('lev', inline=T))
    , style='margin:0px'),
    br(),
    plotOutput('plot')
    
    
  ))
  )
))
