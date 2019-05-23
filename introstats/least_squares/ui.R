library(shiny)
library(RColorBrewer); library(DT)

shinyUI(fluidPage(
  includeCSS('styles.css'),
  withMathJax(),
  wellPanel(h3('The', strong('least squares'), 'regression line'),
            tags$div(
              checkboxInput('details', label='Click for more info!'),
              style='margin-bottom:-10px;'
            ),
            conditionalPanel('input.details == true', 
              uiOutput('info')
              , style='margin-bottom:-10px'
            )
  ),
  fluidRow(
    column(2, wellPanel(
      tags$div(
        radioButtons('dataset', label='Which dataset?', 
                   choices=list('Houses', 'Data 1', 'Data 2', 'Data 3', 'Data 4', 
                                'Data 5', 'Random')), 
        style='margin-bottom: -8px;'
      ),
      conditionalPanel("input.dataset == 'Random'", 
                       actionButton('draw', label='Draw New'))
    ),
    wellPanel(
      strong('Guess the LS line!'), br(),
      br(),
      tags$div(
        numericInput('yint_guess', label='Y-intercept', value=0), 
        numericInput('slope_guess', label='Slope', value=0), 
        style='margin: -10px 0px;'
      )
      
    ),
    wellPanel(
      strong('Show which lines?'),
      checkboxInput('guess_line', label="Your Guess", value=T),
      conditionalPanel(
        'input.guess_line == true', 
        checkboxInput('show_res', label='Residuals', value=T),
        checkboxInput('true_line', label='The true LS regression line')
      )
    )),
    column(10, 
      fluidRow(
        column(6, wellPanel(
             strong('Table'), DTOutput('table')
        , id='myScrollBox')),
             column(6, wellPanel(
               strong('Scatterplot'), 
               plotOutput('scatter', height="380px"), 
               strong('Equations of LS lines'), 
               tags$ul(
                 tags$li('Guess: ', uiOutput('guess_eq', inline=T))
               ), 
               conditionalPanel('input.true_line == true', 
                tags$ul(
                  tags$li('Truth: ', uiOutput('true_eq', inline=T))
                )                
                , style='margin: -5px 0px;')
             , style='padding-bottom: 5px')
            )),
           wellPanel(
             strong('Sum of squared residuals'),
           conditionalPanel('input.guess_line == true & input.show_res == true', 
              tags$ul(
                tags$li('For guessed line = ', textOutput('SS.guess', inline=T))
              ),
              conditionalPanel('input.guess_line == true & input.show_res == true & input.true_line == true', 
                tags$ul(tags$li('For true LS regression line = ', textOutput('SS.true', inline=T), 
                                '(Minimum)'))
              , style='margin-top:-6px'),
              plotOutput('SS', height='90px')
           )
           )
  )

)))
