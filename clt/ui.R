library(shiny)
library(shinyWidgets)
pops <- c('Extremely skewed left', 'Very skewed left', 
          'Moderately skewed left', 'Slightly skewed left', 
          'Symmetric (normal)', 'Slightly skewed right', 
          'Moderately skewed right', 'Very skewed right', 
          'Extremely skewed right')

shinyUI(fluidPage(
  includeCSS('styles.css'),
  withMathJax(),
  wellPanel(
    h3(strong('The Sampling Distribution of the Sample Mean and The Central Limit Theorem'))#, 
    # tags$div(checkboxInput('info_check', label='Click for more info!'),
    #          style='margin-bottom:-10px'),
    # conditionalPanel('input.info_check == true', 
    #   uiOutput('info')               
    # , style='margin-bottom: -10px;')
  ),
  fluidRow(
      column(6, wellPanel(
        uiOutput('pop_slider_ui')
      )),
      conditionalPanel(condition="input.tab != 'pop'",
      column(6, wellPanel(
        sliderInput('n', label='Sample size \\((n)\\) -- Hit \"Play\" for animation!', 
                    min=1, max=40, value=1, step=1, 
                    animate=animationOptions(interval=300, loop=T)), 
                 style='padding-bottom:0px'
      )) 
      )
  ),
  tabsetPanel(
    tabPanel(title='Population',
             fluidRow(
        column(3, wellPanel(
        'All populations (regardless of shape) have',
        tags$ul(
          tags$li('mean \\(\\mu = 15\\)'),
          tags$li('standard deviation \\(\\sigma = 5\\)'),
        style='padding-left:25px')
        )),
        column(3, wellPanel(
          strong('Boxplot'),
          plotOutput('pop.boxplot', height='140px')
        )),
        column(3, wellPanel(
          strong('Histogram'),
          plotOutput('pop.histogram', height='140px')
        )),
        column(3, wellPanel(
          strong('Density Plot'),
          plotOutput('pop.dens.plot', height='140px')
        ))
      ),
    value='pop'),
    tabPanel(title='Population vs. Sampling Dist. of \\(\\bar{x}\\)',
             fluidRow(
               column(3, wellPanel(
                 tags$table(
                   tags$tr(tags$td(''), tags$td('Mean'), tags$td('SD')),
                   tags$tr(tags$td('Population'),
                           tags$td('\\(\\mu=15\\)'),
                           tags$td('\\(\\sigma=5\\)')),
                   tags$tr(tags$td('Samp. dist. of \\(\\bar{x}\\)'),
                           tags$td('\\(\\mu=15\\)'),
                           tags$td('\\(\\frac{\\sigma}{\\sqrt{n}} =\\)', 
                                   textOutput('samp.sd', inline=T)))
                 )
               )),
             column(3, wellPanel(
               strong('Boxplot of Population'),
               plotOutput('pop.boxplot2', height='140px'), br(),
               strong('Boxplot of sample means'), 
               plotOutput('xbar.boxplot', height='140px')
             )),
             column(3, wellPanel(
               strong('Histogram of Population'),
               plotOutput('pop.histogram2', height='140px'), br(),
               strong('Histogram of sample means'),
               plotOutput('xbar.histogram', height='140px')
             )),
             column(3, wellPanel(
               strong('Density Plots of Population / Sample Means'),
               plotOutput('pop.dens.plot2', height='210px'),
               p('Note: The density curves are scaled so they have 
                         the same height.', style='font-size: x-small')
             ))
             ),
    value='pop_samp'), 
    tabPanel(title='Sampling Dist. of \\(\\bar{x}\\) vs. Normal Dist.', 
             fluidRow(
               column(4, wellPanel(
                 'The plots at right compare the sampling distribution of \\(\\bar{x}\\) to a normal 
             distribution with the same mean and standard deviation, letting us see how \"far 
             from normal\" the distribution of the sample means is.'
               )),
               column(4, wellPanel(
                 strong('Constant scale'),
                 plotOutput('xbar.plot', height='210px')
               )),
               column(4, wellPanel(
                 strong('Scale shrinks as n increases'),
                 plotOutput('compare.normal2', height='210px')
               ))
             ),
    value='comp_normal'),
    id='tab', type='tabs'
  )
))
