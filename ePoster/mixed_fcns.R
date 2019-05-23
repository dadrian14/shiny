#.t11

mixed_ui <- function()
{
  fluidPage(
  fluidRow(wellPanel(
    h3('Simulating from 2x2', strong('mixed-effects'), 'model with crossed factors'), 
    h4(HTML('Y<sub>ijk</sub> = &mu; + &alpha;<sub>i</sub> + B<sub>j</sub> + 
         (&alpha;B)<sub>ij</sub> + E<sub>ijk</sub>,')),
    tags$ul(
      tags$li(p(strong('Fixed effects: '), HTML('&mu;, &alpha;<sub>i</sub>'))), 
      tags$li(p(strong('Random effects: '), 
        HTML('B<sub>j</sub> ~ N(0, &sigma;<sup>2</sup><sub>B</sub>),
              (&alpha;B)<sub>ij</sub> ~ N(0, &sigma;<sup>2</sup><sub>&alpha;B</sub>),
              E<sub>ijk</sub> ~ N(0, &sigma;<sup>2</sup>)')))
      )
  )),
  fluidRow(wellPanel(
    actionButton('simnew.t11', label='Simulate new sample')
  )),
  fluidRow(
    column(4, wellPanel(
      sliderInput('mu.t11', label=HTML('Overall mean &mu;'), min=-5, max=5, value=0, step=.1), 
      sliderInput('a1.t11', label=HTML(paste0('Fixed effect &alpha;', tags$sub('1'))), 
                  min=-5, max=5, value=0, step=.1),
      sliderInput('sigb.t11', label=HTML('Std. dev. &sigma;<sub>B</sub> 
                  of random effect B<sub>j</sub>'), 
                  min=0, max=8, value=0, step=.1),
      sliderInput('sigab.t11', 
                  label=HTML('Std. dev. &sigma;<sub>&alpha;B</sub>
        of random effect (&alpha;B)<sub>ij</sub>'), 
                  min=0, max=8, value=0, step=.1), 
      sliderInput('sigma.t11', 
                  label=HTML('Std. dev. &sigma; of errors E<sub>ijk</sub>'), 
                  min=0, max=5, value=1, step=.1),
      sliderInput('n.t11', label='Sample size n of each treatment', min=2, max=100, value=20)
    )), 
    column(7, wellPanel(
      plotOutput('plot.t11'), 
      hr(),
      p(strong('Note:'), 'Parameter restrictions are applied such that'), 
      p(HTML(paste0('&alpha;', tags$sub('1'), ' + &alpha;', 
                    tags$sub('2'), ' = 0')))
      
    ))
  )
)}

mixed_server <- function(input, output) {
  output$plot.t11 <- renderPlot({
    button <- input$simnew.t11
    jit <- .15
    y <- array(dim=c(2, 2, input$n.t11))
    a2 <- -input$a1.t11
    b <- rnorm(2, mean=0, sd=input$sigb.t11)
    ab <- array(rnorm(4, mean=0, sd=input$sigab.t11), dim=c(2,2))
    y[1,1,] <- input$mu.t11 + input$a1.t11 + b[1] + ab[1,1] + 
      rnorm(input$n.t11, mean=0, sd=input$sigma.t11)
    y[1,2,] <- input$mu.t11 + input$a1.t11 + b[2] + ab[1,2] + 
      rnorm(input$n.t11, mean=0, sd=input$sigma.t11)
    y[2,1,] <- input$mu.t11 + a2 + b[1] + ab[2,1] + 
      rnorm(input$n.t11, mean=0, sd=input$sigma.t11)
    y[2,2,] <- input$mu.t11 + a2 + b[2] + ab[2,2] + 
      rnorm(input$n.t11, mean=0, sd=input$sigma.t11)
    ymat <- xmat <- array(dim=c(input$n.t11, 4))
    ymat[,1] <- y[1,1,]
    ymat[,2] <- y[1,2,]
    ymat[,3] <- y[2,1,]
    ymat[,4] <- y[2,2,]
    for(i in 1:4) xmat[,i] <- i + runif(input$n.t11, min=-jit, max=jit)
    par(mar=c(4, 2, 3, .5), cex.main=2, cex.lab=1.5, cex.axis=1.3)
    matplot(xmat, ymat, xaxt='n', xlab='Levels of Factors (A,B)', 
            ylab='', ylim=c(-20, 20), pch=1, xlim=c(.5, 4.5), 
            main='Simulated data')
    legend('topleft', 'sample means', lty=1)
    axis(side=1, at=1:4, label=c('(1,1)', '(1,2)', '(2,1)', '(2,2)'))
    abline(h=seq(-20, 20, by=10), col=grey(.7), lty=3)
    segments(x0=.8, x1=1.2, y0=mean(y[1,1,]))
    segments(x0=1.8, x1=2.2, y0=mean(y[1,2,]), col=2)
    segments(x0=2.8, x1=3.2, y0=mean(y[2,1,]), col=3)
    segments(x0=3.8, x1=4.2, y0=mean(y[2,2,]), col=4)
  })
}