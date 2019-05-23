#ePoster
tstat <- function(xbar1, xbar2, s, n)
{
  (xbar1 - xbar2) / sqrt(2*s^2/n)
}

library(shiny)
ui <- fluidPage(
#  fluidRow(wellPanel(
#    h2('Test statistic'), h4('Two sample t test')
#  )), 
  fluidRow(
    column(4, wellPanel(style = "background-color: #ffffff;",
      h3(strong('Summary statistics')),
      p('For simplicity, we will assume the standard deviations 
          and sample sizes of both groups are equal, i.e.', 
        HTML('s<sub>1</sub> = s<sub>2</sub> and 
             n<sub>1</sub> = n<sub>2</sub>.')),
      sliderInput('xbar1', label=HTML('Sample 1 mean (x&#772;<sub>1</sub>)'), min=0, 
                  max=10, value=5, step=.1),
      sliderInput('xbar2', label=HTML('Sample 2 mean (x&#772;<sub>2</sub>)'), min=0, 
                  max=10, value=3, step=.1),
      hr(), 
      sliderInput(inputId='s', label=
        HTML('Sample std devs (s<sub>1</sub> = s<sub>2</sub>)'), 
                  min=1, max=10, value=3, step=.1),
      hr(),
      sliderInput(inputId='n', label=
                    HTML('Sample sizes (n<sub>1</sub> = n<sub>2</sub>)'), 
                  min=5, max=200, value=30, step=5)
    )),
    column(4, wellPanel(style = "background-color: #ffffff;",
      h3(strong('Test statistic')), 
      p(style="text-align:center", tags$img(width='125px', src='t_formula.PNG')),
      hr(),
      h4(strong('Two parts of formula:')), 
      tags$ul(
        tags$li(strong('Numerator:'), 'difference in means'), 
        verbatimTextOutput('diff'), 
        tags$li(strong('Denominator:'), 'standard error'),
        verbatimTextOutput('se')
      ),
      hr(),
      h4(strong('Value of test statistic')),
      plotOutput(outputId='tplot', height="140px")
    )), 
    column(4, wellPanel(style = "background-color: #ffffff;",
      h3(strong('Plot of data')), 
      p('Based on simulating samples from normal populations with means and 
        standard deviations equal to those specified'),
      plotOutput(outputId='dataplot')
    ))
  )
)

server <- function(input, output) {
  output$tplot <- renderPlot({
    par(mar=c(2.5, .5, 2, .5), cex.main=2, cex.lab=1.7, cex.axis=1.5)
    t.obs <- tstat(input$xbar1, input$xbar2, input$s, input$n)
    plot(x=NULL, y=NULL, xlim=c(-10, 10), ylim=c(-1, 1), yaxt='n', xlab='',
         main=substitute('t = '*t.obs, list(t.obs=round(t.obs, 2))))
    abline(v=seq(-10, 10, by=1), col=grey(.8))
    abline(v=0)
    col <- ifelse(t.obs > 0, 4, 2)
    if(t.obs!=0) arrows(0, 0, t.obs, 0, length=.1, lwd=3, col=col)
    if(t.obs==0) points(0, 0 , pch=16)
    if(t.obs < -11) text(-11, .5, 'Off-screen', pos=4)
    if(t.obs > 11) text(11, .5, 'Off-screen', pos=2)
  })
  output$diff <- renderPrint(input$xbar1 - input$xbar2)
  output$se <- renderPrint(
    round(sqrt(2*input$s^2/input$n), 2)
  )
  output$dataplot <- renderPlot({
    jit <- .1 #jitter
    par(mar=c(4.5, 2.5, .5, .5), cex.lab=1.5, cex.axis=1.1)
    y1 <- rnorm(n=input$n, mean=input$xbar1, sd=input$s)
    y2 <- rnorm(n=input$n, mean=input$xbar2, sd=input$s) 
    x1 <- runif(n=input$n, min=1-jit, max=1+jit) 
    x2 <- runif(n=input$n, min=2-jit, max=2+jit)
    plot(c(x1, x2), c(y1, y2), 
         xlim=c(.5, 2.5), ylim=c(-20, 30), 
         xaxt='n', xlab='Group', ylab='')
    axis(side=1, at=c(1,2))
  })
}

shinyApp(ui = ui, server = server)