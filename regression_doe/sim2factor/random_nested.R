#library(shiny)
random_nested_ui <- function(){
  fluidPage(
  fluidRow(
    wellPanel(h3(strong('Simulating from Random-effects model with nested factors')), 
              h4(HTML('Y<sub>ijk</sub> = '), 
                 HTML('&mu; + A<sub>i</sub> + B<sub>(i)j</sub> + E<sub>ijk</sub>,')), 
              'i=1,2, j=1,2, k=1,...,n'
    )),
  fluidRow(
    column(3, wellPanel(
      sliderInput('mu.rn', label=HTML('Overall mean &mu;'), min=-5, max=5, value=0, step=.1), 
      sliderInput('siga.rn', label=HTML('&sigma;<sub>A</sub> (Standard deviation of A<sub>i</sub>)'), 
                  min=0, max=5, value=0, step=.1), 
      sliderInput('sigb.rn', label=HTML('&sigma;<sub>B(A)</sub> (Standard deviation of B<sub>(i)j</sub>)'), 
                  min=0, max=5, value=0, step=.1),
      sliderInput('sigma.rn', label=HTML('&sigma; (Standard deviation of E<sub>ijk</sub>)'), 
                  min=0, max=5, value=0, step=.1)
    )),
    column(3, wellPanel(
      sliderInput('n.rn', label='n (Sample size of each treatment)', min=2, max=100, value=20),
      actionButton("draw.rn", 'Draw new sample!')
    )),
    column(6, wellPanel(
      plotOutput('plot.rn')
    ))
  )
)
}

random_nested_server <- function(input, output) {
  jit <- .2
  plot.obj.rn <- reactive({
    input$draw.rn
    y <- array(dim=c(2, 2, input$n.rn))
    a <- rnorm(2, sd=input$siga.rn)
    b <- rnorm(4, sd=input$sigb.rn)
    y[1,1,] <- input$mu.rn + a[1] + b[1] +
      rnorm(input$n.rn, mean=0, sd=input$sigma.rn) 
    y[1,2,] <- input$mu.rn + a[1] + b[2] +
      rnorm(input$n.rn, mean=0, sd=input$sigma.rn)
    y[2,1,] <- input$mu.rn + a[2] + b[3] +
      rnorm(input$n.rn, mean=0, sd=input$sigma.rn)
    y[2,2,] <- input$mu.rn + a[2] + b[4] +
      rnorm(input$n.rn, mean=0, sd=input$sigma.rn)
    ymat <- xmat <- array(dim=c(input$n.rn, 4))
    ymat[,1] <- y[1,1,]
    ymat[,2] <- y[1,2,]
    ymat[,3] <- y[2,1,]
    ymat[,4] <- y[2,2,]
    for(i in 1:4) xmat[,i] <- i + runif(input$n.rn, min=-jit, max=jit)
    return(list(xmat=xmat, ymat=ymat))
  })
  output$plot.rn <- renderPlot({
    par(mar=c(4, 2, 2, .5), cex.lab=1.5, cex.axis=1.3, cex.main=1.7)
    matplot(plot.obj.rn()$xmat, plot.obj.rn()$ymat, xaxt='n', xlab='Levels of Factors (A,B)', 
            ylab='', ylim=c(-20, 20), pch=1, xlim=c(1-jit, 4+jit), 
            main='Simulated data')
    axis(side=1, at=c(1:4), label=c('(1, 1)', '(1, 2)', '(2, 3)', '(2, 4)'))
  })
}

#shinyApp(ui = ui, server = server)