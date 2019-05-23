library(shiny)

shinyUI(fluidPage(
  includeCSS('styles.css'),
  withMathJax(),
  wellPanel(
    h3(strong('Consequences of underfitting and overfitting')),
    h4('The bias / variance tradeoff'),
    checkboxInput('description', label='Click to show more info!'),
    conditionalPanel("input.description == true", 
                     fluidRow(
                       column(4, wellPanel(
                         tags$div('', 
                                  style='font-size: x-small')
                       )),
                       column(8, wellPanel(
                         tags$div('', 
                                  style='font-size: x-small')
                       ))
                       , style="margin: 0px -20px;")               
    )
    , style='padding-bottom: -0px;'),
  fluidRow(
    column(4, wellPanel(
      h4('The Model'),
      '\\(Y = \\beta_0 + \\beta_1 x_1 + \\beta_2 x_2 + E\\),', 
      '\\(E_i \\sim\\)  iid \\(N(0, \\sigma^2)\\)', br(), 
      '\\(\\beta_0 = 5, \\beta_1=2, n = 100, \\sigma = 5\\),',
      
      hr(),
      sliderInput('beta2', label='\\(\\beta_2\\)', min=-1, max=1, value=0.1, step=.1),
      'Note:', uiOutput('underover', inline=T), hr(),
      sliderInput('cor_xs', label='Correlation between \\(x_1\\) and \\(x_2\\)', 
                  min=-0.9, max=0.9, value=0.5, step=0.1)
      #sliderInput('n', label='Sample size \\(n\\)', min=10, max=1000, value=100, step=10),
      #sliderInput('sigma_y', label='\\(\\sigma\\)', min=0.1, max=10, value=1, step=.1)
    )), 
    column(8, wellPanel(
      h4('Sampling distributions of \\(\\hat{\\beta}_1\\)'),
      HTML('<span style=color:red> ------- </span>'),
      'Model with \\(x_1\\) only', br(),
      HTML('<span style=color:blue> ------- </span>'),
      'Model with \\(x_1\\) and \\(x_2\\)', br(),
      '- - - -', 'True value of \\(\\beta_1\\)',
          plotOutput('samp_betahat1', height='250px'), br(), 
      tags$div(tags$div(
        tableOutput('beta1_table'), 
        style='display: inline-block;'),
        style='text-align: center;')
    ))
  )
))
