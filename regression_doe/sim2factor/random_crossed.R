#library(shiny)
random_crossed_ui <- function(){
  fluidPage(
  fluidRow(
    wellPanel(h3(strong('Simulating from random-effects model with crossed factors')), 
              h4(HTML('Y<sub>ijk</sub> = '), 
                 HTML('&mu; + A<sub>i</sub> + B<sub>j</sub> + '), 
                 HTML('(AB)<sub>ij</sub> + E<sub>ijk</sub>,')), 
              'i=1,2, j=1,2, k=1,...,n'
  )),
  fluidRow(
    column(3, wellPanel(
      sliderInput('mu.rc', label=HTML('Overall mean &mu;'), min=-5, max=5, value=0, step=.1), 
      sliderInput('siga.rc', label=HTML(paste0('&sigma;', tags$sub('A'),
                                      ' (Standard deviation of A<sub>i</sub>)')), 
                  min=0, max=5, value=0, step=.1),
      sliderInput('sigb.rc', label=HTML(paste0('&sigma;', tags$sub('B'), 
                                      ' (Standard deviation of B<sub>j</sub>)')), 
                  min=0, max=5, value=0, step=.1),
      sliderInput('sigab.rc', label=HTML('&sigma;<sub>AB</sub>', 
                                      '(Standard deviation of (AB)<sub>ij</sub>)'), 
                  min=0, max=5, value=0, step=.1), 
      sliderInput('sigma.rc', label=HTML('&sigma; (Standard deviation of E<sub>ijk</sub>)'), 
                  min=0, max=5, value=0, step=.1)
    )), 
    column(3, wellPanel(
      sliderInput('n.rc', label='n (Sample size of each treatment)', min=2, max=100, value=20),
      actionButton("draw.rc", 'Draw new sample!')
    )),
    column(6, wellPanel(
      plotOutput('plot.rc')
    ))
  )
)
}

random_crossed_server <- function(input, output) {
  jit <- .2
  plot.obj.rc <- reactive({
    input$draw.rc
    y <- array(dim=c(2, 2, input$n.rc))
    a <- rnorm(2, mean=0, sd=input$siga.rc)
    b <- rnorm(2, mean=0, sd=input$sigb.rc)
    ab <- array(rnorm(4, mean=0, sd=input$sigab.rc), dim=c(2,2))
    y[1,1,] <- input$mu.rc + a[1] + b[1] + ab[1,1] + 
      rnorm(input$n.rc, mean=0, sd=input$sigma.rc)
    y[1,2,] <- input$mu.rc + a[1] + b[2] + ab[1,2] + 
      rnorm(input$n.rc, mean=0, sd=input$sigma.rc)
    y[2,1,] <- input$mu.rc + a[2] + b[1] + ab[2,1] + 
      rnorm(input$n.rc, mean=0, sd=input$sigma.rc)
    y[2,2,] <- input$mu.rc + a[2] + b[2] + ab[2,2] + 
      rnorm(input$n.rc, mean=0, sd=input$sigma.rc)
    ymat <- xmat <- array(dim=c(input$n.rc, 4))
    ymat[,1] <- y[1,1,]
    ymat[,2] <- y[1,2,]
    ymat[,3] <- y[2,1,]
    ymat[,4] <- y[2,2,]
    for(i in 1:4) xmat[,i] <- i + runif(input$n.rc, min=-jit, max=jit)
    return(list(xmat=xmat, ymat=ymat))
  })
  output$plot.rc <- renderPlot({
    par(mar=c(4, 2, 2, .5), cex.lab=1.5, cex.axis=1.3, cex.main=1.7)
    matplot(plot.obj.rc()$xmat, plot.obj.rc()$ymat, xaxt='n', xlab='Levels of Factors (A, B)', 
            ylab='', ylim=c(-20, 20), pch=1, xlim=c(1-jit, 4+jit), 
            main='Simulated data')
    axis(side=1, at=c(1:4), label=c('(1, 1)', '(1, 2)', '(2, 1)', '(2, 2)'))
  })
}

#shinyApp(ui = ui, server = server)