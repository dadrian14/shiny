library(shiny)

shinyUI(fluidPage(
  includeCSS('styles.css'), 
  withMathJax(),
  wellPanel(
    h3(strong('Confidence interval for the population mean \\(\\mu\\)')),
    h4('Formula: ', 
      '\\(\\bar{x} \\pm t^\\ast \\frac{s}{\\sqrt{n}}\\)' 
    ),
    tags$div(checkboxInput('show_info', label='Click for more info!'), 
             style='margin-bottom: -10px;'), 
    conditionalPanel('input.show_info', 
                     uiOutput('info')
    )
  ), 
  fluidRow(
    column(4, wellPanel(
      sliderInput('xbar', label='Sample mean \\(\\bar{x}\\)', 
                  min=10, max=90, value=47, step=.1), 
      sliderInput('s', label='Sample standard deviation \\(s\\)', min=1, max=50, value=9, step=.1), 
      sliderInput('n', label='Sample size \\(n\\)', 
                  min=2, max=100, value=10, step=1),
      sliderInput('C', label='Confidence level', min=80, max=99, value=95, step=1, post='%')
    )),
    column(4, wellPanel(
      h4(strong('Parts of the result: \\(t^\\ast, se, me\\)')), 
      '\\(t^\\ast = \\)', textOutput('tstar', inline=T),
      plotOutput('tstar_plot', height='175px'), 
      hr(),
      strong('Standard error'), br(),
      '\\(se = \\frac{s}{\\sqrt{n}} = \\)', textOutput('se', inline=T), 
      hr(), 
      strong('Margin of error'), br(), 
      '\\(me = t^\\ast(se) =\\)', textOutput('me', inline=T)
    )),
    column(4, wellPanel( 
      h4(strong(textOutput('conf', inline=T), 'Confidence interval for \\(\\mu\\)')), 
         textOutput('ci', inline=T),
      hr(),
      strong('Picture'),
      plotOutput('plot', height="120px"),
      sliderInput('range', label='Range shown', min=0, max=100, value=c(0,100), step=1)
    ))
    
  )
  
))
