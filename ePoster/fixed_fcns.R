#.t10

fixed_ui <- function()
{
  fluidPage(
  fluidRow(wellPanel(
    h3('Simulating from 2x2', strong('fixed-effects'), 'model with crossed factors'),
    h4(HTML('Y<sub>ijk</sub> = &mu; + &alpha;<sub>i</sub> + &beta;<sub>j</sub> + 
         (&alpha;&beta;)<sub>ij</sub> + E<sub>ijk</sub>,')),
    tags$ul(
      tags$li(p(strong('Fixed effects: '), HTML('&mu;, &alpha;<sub>i</sub>, &beta;<sub>j</sub>, 
      (&alpha;&beta;)<sub>ij</sub>'))), 
      tags$li(p(strong('Random effects: '), HTML('E<sub>ijk</sub> ~ 
                                                 N(0, &sigma;<sup>2</sup>)')))
    )
  )),
  fluidRow(wellPanel(
    actionButton('simnew.t10', label='Simulate new sample')
  )),
  fluidRow(
    column(4, wellPanel(
      sliderInput('mu.t10', label=HTML('Overall mean &mu;'), min=-5, max=5, value=0, step=.1), 
      sliderInput('a1.t10', label=HTML(paste0('Fixed effect &alpha;', tags$sub('1'))), 
                  min=-5, max=5, value=0, step=.1),
      sliderInput('b1.t10', label=HTML(paste0('Fixed effect &beta;', tags$sub('1'))), 
                  min=-5, max=5, value=0, step=.1),
      sliderInput('ab11.t10', label=HTML(paste0('Fixed effect (&alpha;&beta;)', tags$sub('11'))), 
                  min=-5, max=5, value=0, step=.1), 
      sliderInput('sigma.t10', label=HTML('Std. dev. &sigma; of errors E<sub>ijk</sub>'), 
                  min=0, max=5, value=1, step=.1),
      sliderInput('n.t10', label='Sample size n of each treatment', min=2, max=100, value=20)
    )), 
    column(7, wellPanel(
      plotOutput('plot.t10'), 
      hr(),
      p(strong('Note:'), 'Parameter restrictions are applied such that'), 
      p(HTML(paste0('&alpha;', tags$sub('1'), ' + &alpha;', tags$sub('2'), ' = 0'))), 
      p(HTML(paste0('&beta;', tags$sub('1'), ' + &beta;', tags$sub('2'), ' = 0'))), 
      p(HTML(paste0('(&alpha;&beta;)', tags$sub('11'), 
                    ' + (&alpha;&beta;)', tags$sub('12'), ' = 0'))),
      p(HTML(paste0('(&alpha;&beta;)', tags$sub('11'), 
                    ' + (&alpha;&beta;)', tags$sub('21'), ' = 0'))),
      p(HTML(paste0('(&alpha;&beta;)', tags$sub('21'), 
                    ' + (&alpha;&beta;)', tags$sub('22'), ' = 0'))), 
      p(HTML(paste0('(&alpha;&beta;)', tags$sub('12'), 
                    ' + (&alpha;&beta;)', tags$sub('22'), ' = 0')))
    ))
  )
)}


fixed_server <- function(input, output) {
  output$plot.t10 <- renderPlot({
    par(cex.main=2, cex.lab=1.5, cex.axis=1.3)
    button <- input$simnew.t10
    jit <- .15
    y <- array(dim=c(2, 2, input$n.t10))
    a2 <- -input$a1.t10
    b2 <- -input$b1.t10
    ab12 <- -input$ab11.t10
    ab21 <- -input$ab11.t10
    ab22 <- input$ab11.t10
    y[1,1,] <- input$mu.t10 + input$a1.t10 + input$b1.t10 + input$ab11.t10 + 
      rnorm(input$n.t10, mean=0, sd=input$sigma.t10)
    y[1,2,] <- input$mu.t10 + input$a1.t10 + b2 + ab12 + 
      rnorm(input$n.t10, mean=0, sd=input$sigma.t10)
    y[2,1,] <- input$mu.t10 + a2 + input$b1.t10 + ab21 + 
      rnorm(input$n.t10, mean=0, sd=input$sigma.t10)
    y[2,2,] <- input$mu.t10 + a2 + b2 + ab22 + 
      rnorm(input$n.t10, mean=0, sd=input$sigma.t10)
    ymat <- xmat <- array(dim=c(input$n.t10, 4))
    ymat[,1] <- y[1,1,]
    ymat[,2] <- y[1,2,]
    ymat[,3] <- y[2,1,]
    ymat[,4] <- y[2,2,]
    for(i in 1:4) xmat[,i] <- i + runif(input$n.t10, min=-jit, max=jit)
    par(mar=c(4, 2, 3, .5))
    matplot(xmat, ymat, xaxt='n', xlab='Levels of Factors (A,B)', 
            ylab='', ylim=c(-20, 20), pch=1, xlim=c(.5, 4.5), 
            main='Simulated data')
    legend('topleft', 'sample means', lty=1)
    axis(side=1, at=c(1:4), label=c('(1,1)', '(1,2)', '(2,1)', '(2,2)'))
    abline(h=seq(-20, 20, by=10), col=grey(.7), lty=3)
    segments(x0=.8, x1=1.2, y0=mean(y[1,1,]))
    segments(x0=1.8, x1=2.2, y0=mean(y[1,2,]), col=2)
    segments(x0=2.8, x1=3.2, y0=mean(y[2,1,]), col=3)
    segments(x0=3.8, x1=4.2, y0=mean(y[2,2,]), col=4)
  })
}