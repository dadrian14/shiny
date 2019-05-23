library(shiny)

shinyUI(fluidPage(
  includeCSS('styles.css'),
  withMathJax(),
  fluidRow(wellPanel(
    h3(strong('The \\(t\\) Distribution')), 
    tags$div(checkboxInput('show_info', label='Show more info!'), 
             style='margin-bottom: -10px;'),
    conditionalPanel('input.show_info == true', 
                     uiOutput('info')
    )
    )),
  fluidRow(
    column(4, wellPanel(
      h4(strong('Degrees of Freedom (df)')),
      radioButtons('df_input', label='Input type for df', 
                   choiceNames = list('Slider', 'Text Input'), 
                   choiceValues = list(1, 2)),
      hr(),
      conditionalPanel('input.df_input == 1',
                       tags$div(sliderInput(inputId='slider.df', label='df', 
                                   min=1, max=30, value=1, step=1, 
                                   animate=list(interval=250, loop=T)),
                       style='margin-bottom: -15px;')),
      conditionalPanel('input.df_input == 2', 
                       numericInput('text.df', label='df', 
                                    value=1, min=1, step=1), 
                       'Required: df > 0')
    ),
    wellPanel(
      checkboxInput('show_mult', 
                    label='Show values of \\(t^\\ast\\) and \\(z^\\ast\\) on the plot'), 
      conditionalPanel('input.show_mult', 
                       sliderInput(inputId='C', label='For which confidence level?', post='%',
                                   min=80, max=99, value=95, animate=list(interval=300, loop=T))
      )
    )),
    column(8, wellPanel(
      h4(strong('Comparing \\(t\\) and Standard Normal Distributions')),
      conditionalPanel('input.show_mult == true', 
        '(And \\(t^\\ast\\) to \\(z^\\ast\\))'               
      ),
      plotOutput(outputId='plot')
    ))
    )
))
