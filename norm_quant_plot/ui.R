library(shiny)

shinyUI(fluidPage(
  includeCSS('styles.css'),
  withMathJax(),
  wellPanel(h3(strong('Normal Quantile Plot'))),
  fluidRow(
    column(5, wellPanel(
      radioButtons('dist', label='Distribution to simulate from', 
                   choiceNames=list('Normal \\((\\mu = 7, \\sigma = 2)\\)', 
                                    'Right-skewed', 
                                    'Heavy-tailed', 
                                    'Left-skewed'), 
                   choiceValues = list('normal', 'exp', 'cauchy', 'left')),
      radioButtons('n', label='Sample size', 
                   choices=list(10, 20, 50, 100, 500), inline=T),
      actionButton('draw', label='Draw New Sample!')
    ), 
    wellPanel(
      h4(strong('General Explanation')), 
      'For \\(i=1,2,...,n,\\)',
      tags$ul(
        tags$li('Let \\(y_{(i)}\\) be the \\(i^{th}\\) smallest sample value.'),
        tags$li('The normal quantile \\(z_{(i-0.5)/n}\\) is the value of the standard normal distribution so 
                there is an area of \\((i-0.5)/n\\) to the left'),
        tags$li('The points plotted are \\((z_{(i-0.5)/n}, y_{(i)})\\).')
      )
    ),
    wellPanel(
     checkboxInput('show_hist', label="Show histogram?"),
     conditionalPanel('input.show_hist == true',
                      plotOutput('hist', height='250px')
     )
    )),
    column(7, wellPanel(
      strong('Normal Quantile Plot'),
      plotOutput('qqplot', height='auto')
    ))
  ),
  wellPanel(
    h4(strong('Specific Explanation')),
    'The following describes the', tags$span('red', style='color:red;'), 
    'point on the normal quantile plot',
    fluidRow(
      column(4, wellPanel(
        sliderInput('which_i', label='Select \\(i\\) from 1 to \\(n\\)', min=1, max=500, value=1),
        'The coordinates of the point are', br(),
        h4(textOutput('coord', inline=T))
      , style='margin:0px')), 
      column(4, wellPanel(
        strong('X-coordinate: normal quantile'), br(),
        tags$ul(
          tags$li('\\(z_{(i-0.5)/n} =\\)', textOutput('quant', inline=T)),
          tags$li('\\(\\frac{i-0.5}{n} =\\)', textOutput('perc', inline=T))
        ),
        'Picture:', br(),
        plotOutput('norm_exp', height='210px')
      , style='margin:0px'
      )),
      column(4, wellPanel(
        strong('Y-coordinate: Ordered sample value'), br(),
        tags$ul(
          tags$li('The \\(i^{th}\\) smallest sample value is ', 
            strong(textOutput('ordered_sample_val', inline=T))),
          tags$li('It is bolded in the list of ordered sample values below.')
        ),
        tags$div(uiOutput('listed'), style='background-color:white')
      , style='margin:0px'
      ))
    )
  )
))
