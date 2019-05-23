library(shiny)

shinyUI(fluidPage(
  includeCSS('styles.css'),
  withMathJax(),
wellPanel(
  h3(strong('Three-factor effects model')), 
  '\\(\\mu_{ijk} = \\mu + \\alpha_i + \\beta_j + \\gamma_k + 
  (\\alpha\\beta)_{ij} + (\\alpha\\gamma)_{ik} + (\\beta\\gamma)_{jk} + 
  (\\alpha\\beta\\gamma)_{ijk}\\)'
), 
fluidRow(
  column(4, wellPanel(
    fluidRow(
    column(6, 
    sliderInput('mu', label='\\(\\mu\\)', min=9, max=11, value=10, step=1),
    sliderInput('a', label='\\(\\alpha_1\\)', min=-1, max=1, value=0, step=1),
    sliderInput('b', label='\\(\\beta_1\\)', min=-1, max=1, value=0, step=1),
    sliderInput('c', label='\\(\\gamma_1\\)', min=-1, max=1, value=0, step=1)
    , style='padding-right: 5px'),
    column(6,
           sliderInput('ab', label='\\((\\alpha\\beta)_{11}\\)', min=-1, max=1, value=0, step=1),
           sliderInput('ac', label='\\((\\alpha\\gamma)_{11}\\)', min=-1, max=1, value=0, step=1),
           sliderInput('bc', label='\\((\\beta\\gamma)_{11}\\)', min=-1, max=1, value=0, step=1),
           sliderInput('abc', label='\\((\\alpha\\beta\\gamma)_{111}\\)', min=-1, max=1, value=0, step=1)
    , style='padding-right: 5px'))
  )),
  
  column(8, wellPanel(
    fluidRow(
      column(3,
        plotOutput('mu111', height='200px')    
      ),
      column(3, 
        plotOutput('mu112', height='200px')
      ),
      column(3,
             plotOutput('mu121', height='200px')    
      ),
      column(3, 
             plotOutput('mu122', height='200px')
      )
    ),
    hr(),
    fluidRow(
      column(3,
             plotOutput('mu211', height='200px')   
      ),
      column(3, 
             plotOutput('mu212', height='200px')
      ),
      column(3,
             plotOutput('mu221', height='200px')     
      ),
      column(3, 
             plotOutput('mu222', height='200px')
      )
    )
  ))
)
))
