library(shiny)

shinyUI(fluidPage(
  withMathJax(), 
  includeCSS('styles.css'),
  wellPanel(
    h3(strong('"Forward" and "Backward" Problems using the Standard Normal distribution')), 
    tags$div(
      h4('Range of \\(z\\) \\( \\matrix{\\xrightarrow{\\textrm{Forward Problem}} \\cr \\xleftarrow[\\textrm{Backward Problem}]{}}\\) Probability'),
      style='padding: 15px;'),
    tags$div(
      checkboxInput('show_info', label='Click for more info!'), 
      style='margin-bottom: -10px;'),
    conditionalPanel('input.show_info == true', 
      uiOutput('info')               
    )
  ),
  fluidRow(
    column(5, wellPanel(
      radioButtons('problem_type', label='1. Forward or Backward Problem?', 
                   choices=list('Forward' = 1, 
                                'Backward' =2), 
                   selected=1), 
      hr(),
      conditionalPanel(condition = "input.problem_type=='1'", 
                       radioButtons('range_type', label='2. What type of range of \\(z\\)?',
                                    choices=list('Between two \\(z\\)\'s'=1, 
                                                 'Greater than \\(z\\)'=2, 
                                                 'Less than \\(z\\)'=3), 
                                    selected=1)), 
      conditionalPanel(condition = "input.problem_type=='2'", 
                       radioButtons('prob_type', label='2. What type of probability?',
                                    choices=list('Between \\(-z\\) and \\(+z\\)'=1, 
                                                 'Greater than \\(z\\)'=2, 
                                                 'Less than \\(z\\)'=3), 
                                    selected=3)), 
      hr(),
      conditionalPanel(condition = "input.problem_type=='1' && input.range_type=='1'",
                       sliderInput('zrange', label='3. Between which two \\(z\\)\'s?', min=-4, 
                                   max=4, value=c(-2, 2), step=.01)), 
      conditionalPanel(condition = "input.problem_type=='1' && input.range_type!='1'",
                       sliderInput('z', label='3. Greater than which \\(z\\)?', min=-4, 
                                   max=4, value=0, step=.01)), 
      conditionalPanel(condition = "input.problem_type=='2'",
                       sliderInput('prob', label='3. Which Probability?', min=.001, max=.999, 
                                   value=.5, step=.001)), 
      hr(),
      strong('Summary of Problem'), br(),
      uiOutput('summary_prob')
    )),
    column(7, wellPanel(
      strong('Picture of problem'),
      plotOutput('plot', height='240px'),
      hr(),
      strong('Answer'), br(),
      conditionalPanel(condition = "input.problem_type=='1'", 
                       'Probability = ', textOutput('prob.text', inline=T)),
      conditionalPanel(condition = "input.problem_type=='2'",
                       'z = ', textOutput('z.txt', inline=T))
    ))
  )
))
