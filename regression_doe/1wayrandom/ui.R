library(shiny)

shinyUI(fluidPage(
  includeCSS('styles.css'),
  withMathJax(),
    h4(strong('One-factor fixed / random effect models')), 
  tabsetPanel(
    tabPanel(title='Fixed', 
    fluidRow(
      column(4, wellPanel(
        '\\(y_{ij} = \\mu + \\tau_i + e_{ij},\\)', br(),'\\(t=2, r=10,\\)',  
        '\\(e_{ij} \\sim N(0, \\sigma^2)\\)',
        hr(),
        sliderInput('mu_fix', label='\\(\\mu\\)', min=5, max=15, value=10, step=1), 
        sliderInput('tau1', label='\\(\\tau_1\\)', min=-5, max=5, value=2, step=1),
        sliderInput('sig_fix', label='\\(\\sigma\\)', min=0, max=5, value=.5, step=.1), 
        actionButton('gen_fix', 'Generate new sample')
      )), 
      column(4, wellPanel(
        'Plot of current experiment',
        plotOutput('curr_fix')
      )), 
      column(4, wellPanel(
        'Plot of \\(y_{11}\\) (red) vs. \\(y_{12}\\) (blue) over experiments',
        plotOutput('cor_fix'), 
        'Theoretical correlation = 0', br(),
        actionButton('clear_fix', 'Clear Plot')
      ))
    )
    ),
    tabPanel(title='Random', 
             fluidRow(
               column(4, wellPanel(
                 '\\(y_{ij} = \\mu + a_i + e_{ij},\\)', br(),'\\(t=2, r=10,\\)',  
                 '\\(a_i \\sim N(0, \\sigma_a^2),\\)',
                 '\\(e_{ij} \\sim N(0, \\sigma^2)\\)',
                 hr(),
                 sliderInput('mu_ran', label='\\(\\mu\\)', min=5, max=15, value=10, step=1), 
                 sliderInput('sigT', label='\\(\\sigma_a\\)', min=0, max=5, value=2, step=.1),
                 sliderInput('sig_ran', label='\\(\\sigma\\)', min=0, max=5, value=.5, step=.1), 
                 actionButton('gen_ran', 'Generate new sample')
               )), 
               column(4, wellPanel(
                 'Plot of current experiment',
                 plotOutput('curr_ran')
               )), 
               column(4, wellPanel(
                 'Plot of \\(y_{11}\\) (red) vs. \\(y_{12}\\) (blue) over experiments',
                 plotOutput('cor_ran'), 
                 'Theoretical correlation = ', '\\(\\frac{\\sigma^2_a}{\\sigma^2_a + \\sigma^2} = \\)',
                 textOutput('theo_corr_rand', inline=T), br(),
                 actionButton('clear_ran', 'Clear Plot')
               ))
             )
    )
  )
))
