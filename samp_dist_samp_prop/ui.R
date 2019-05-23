library(shiny)

shinyUI(fluidPage(
  withMathJax(),
  includeCSS('styles.css'),
  wellPanel(h3(strong('Sampling distribution of the sample proportion (\\(\\hat{p}\\))')), 
            checkboxInput('instructions', label='Click for more info!'),
            conditionalPanel("input.instructions == true", 
                             uiOutput('info') 
                             
  ),style='padding-bottom: 0px;'
  ), 
  fluidRow(
    column(3, 
     wellPanel(
      sliderInput('p', label= "Population proportion (\\(p\\))", 
                  min=.01, max=.99, value=.5, step=.01),
      sliderInput("n", label = "Sample size (\\(n\\))", 
                  min=2, max=1000, value=20, step=1)
    ), 
     wellPanel(
      h4(strong('Normal conditions')),
      'Both:',
      tags$ul(
        tags$li('\"Successes\" \\(np \\geq 10 \\)'),
        tags$li('\"Failures\" \\(n(1-p) \\geq 10 \\)')
      ),
      hr(),
      'Check:',
      tags$ul(
        tags$li('\\(np\\) = ', uiOutput('np', inline=T)), 
        tags$li('\\(n (1 - p)\\) = ', uiOutput('n_fail', inline=T)), 
        tags$li('Both?', uiOutput('cond', inline=T))
      )
    )),  
    column(9, wellPanel(
      tabsetPanel(
        tabPanel('Sampling Distribution of \\(\\hat{p}\\)', 
          uiOutput('dist_panel'), 
          value='samp_dist'
        ), 
        tabPanel('Comparison with Normal Approximation', 
          uiOutput('norm_panel'), 
          value='compare_norm'
        ), id='tab', type='pills'
      ), 
      sliderInput('xlim', label='X-axis Range Shown',
                           min=0, max=1, value=c(0,1), step=.01)
      )
    )
  )
))
