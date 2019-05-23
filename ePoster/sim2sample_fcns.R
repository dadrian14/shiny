#sim2sample_fcns.R
#inputs and outputs use .t8

sim2sample_ui <- function()
{  
  ui <- fluidPage(
  fluidRow(wellPanel(h2(strong('Simulate two independent samples')),
                     'Each sample will be drawn from a',
                     strong('normal population'),  'with  
                     population mean, population standard deviation, and 
                     sample size that you control with the sliders on 
                     the left panel.')),
  fluidRow(
    column(3, wellPanel(
      h3(strong('Population means')),
      sliderInput(inputId='mu1.t8', label=HTML(paste0('Pop. mean 1 (&mu;', tags$sub('1'), ')')), 
                  min=0, max=10, value=5, step=.1), 
      sliderInput(inputId='mu2.t8', label=HTML(paste0('Pop. mean 2 (&mu;', tags$sub('2'), ')')), 
                  min=0, max=10, value=3, step=.1),
      h3(strong('Population standard deviations')),
      sliderInput(inputId='sigma1.t8', 
                  label=HTML('Pop. s.d. 1 (&sigma;<sub>1</sub>)'), 
                  min=1, max=5, value=3, step=.1),
      sliderInput(inputId='sigma2.t8', 
                  label=HTML('Pop. s.d. 2 (&sigma;<sub>2</sub>)'), 
                  min=1, max=5, value=3, step=.1),
      h3(strong('Sample sizes')),
      sliderInput(inputId='n1.t8', 
                  label=HTML('Sample 1 size (n<sub>1</sub>)'), 
                  min=5, max=100, value=30, step=5),
      sliderInput(inputId='n2.t8', 
                  label=HTML('Sample 2 size (n<sub>2</sub>)'), 
                  min=5, max=100, value=30, step=5)
    )), 
    column(5, wellPanel(
      h3(strong('Plot of samples')), 
      p('The points are horizontally \"jittered\" for better viewing'),
      plotOutput(outputId='plot.t8')
    )), 
    column(4, wellPanel(
      h3(strong('Summary statistics')), 
      h5(strong('Sample 1')),
      tags$ul(
        tags$li(HTML('mean x&#772;<sub>1</sub> = '), 
                textOutput('xbar1.t8', inline=T)), 
        tags$li(HTML('standard deviation s<sub>1</sub> = '),
                textOutput('s1.t8', inline=T))
      ), 
      h5(strong('Sample 2')),
      tags$ul(
        tags$li(HTML('mean x&#772;<sub>2</sub> = '), 
                textOutput('xbar2.t8', inline=T)), 
        tags$li(HTML('standard deviation s<sub>2</sub> = '),
                textOutput('s2.t8', inline=T))
      ), 
      hr(),
      h5(strong('Note:')),
      tags$ul(
        tags$li('The horizontal lines in the plot represent 
        the sample means'),
        tags$li('The summary statistics are estimates of (but not 
                exactly equal to) the parameters specified in the 
                left panel.')
      ) 
    ))
  )
)}

sim2sample_server <- function(input, output) {
  jit <- .1 #jitter
  data <- reactive({
    list(y1=rnorm(n=input$n1.t8, mean=input$mu1.t8, sd=input$sigma1.t8), 
         y2=rnorm(n=input$n2.t8, mean=input$mu2.t8, sd=input$sigma2.t8), 
         x1=runif(n=input$n1.t8, min=1-jit, max=1+jit), 
         x2=runif(n=input$n2.t8, min=2-jit, max=2+jit))
  })
  output$plot.t8 <- renderPlot({
    par(mar=c(4.5, 2.5, .5, .5), cex.lab=1.5, cex.axis=1.3)
    plot(c(data()$x1, data()$x2), c(data()$y1, data()$y2), 
         xlim=c(.5, 2.5), ylim=c(-10, 20), 
         xaxt='n', xlab='Samples', ylab='')
    axis(side=1, at=c(1,2))
    abline(h=seq(-10, 20, by=5), col=grey(.7), lty=3)
    segments(.75, mean(data()$y1), 1.25, mean(data()$y1))
    segments(1.75, mean(data()$y2), 2.25, mean(data()$y2))
  })
  output$xbar1.t8 <- renderText(round(mean(data()$y1), 2))
  output$s1.t8 <- renderText(round(sd(data()$y1), 2))
  output$xbar2.t8 <- renderText(round(mean(data()$y2), 2))
  output$s2.t8 <- renderText(round(sd(data()$y2), 2))
}