#library(shiny)
mixed_nested_ui <- function()
{
  fluidPage(
  fluidRow(
    wellPanel(h3(strong('Simulating from Mixed-effects model with nested factors')), 
              h4(HTML('Y<sub>ijk</sub> = '), 
                 HTML('&mu; + &alpha;<sub>i</sub> + B<sub>(i)j</sub> + E<sub>ijk</sub>,')), 
              'i=1,2, j=1,2, k=1,...,n'
    )),
  fluidRow(
    column(3, wellPanel(
      sliderInput('mu.mn', label=HTML('Overall mean &mu;'), min=-5, max=5, value=0, step=.1), 
      sliderInput('a1.mn', label=HTML('&alpha;<sub>1</sub>'), 
                  min=-5, max=5, value=0, step=.1), 
      sliderInput('sigb.mn', label=HTML('&sigma;<sub>B(A)</sub> (Standard deviation of B<sub>(i)j</sub>)'), 
                  min=0, max=5, value=0, step=.1),
      sliderInput('sigma.mn', label=HTML('&sigma; (Standard deviation of E<sub>ijk</sub>)'), 
                  min=0, max=5, value=0, step=.1)
    )),
    column(3, wellPanel(
      sliderInput('n.mn', label='n (Sample size of each treatment)', min=2, max=100, value=20),
      actionButton("draw.mn", 'Draw new sample!'),
      hr(),
      p(HTML('Note: Due to parameter restrictions, &alpha;<sub>2</sub> = -&alpha;<sub>1</sub>'))
    )),
    column(6, wellPanel(
      plotOutput('plot.mn')
    ))
  )
)
}

mixed_nested_server <- function(input, output) {
  jit <- .2
  plot.obj.mn <- reactive({
    input$draw.mn
    y <- array(dim=c(2, 2, input$n.mn))
    a2 <- -input$a1.mn
    b <- rnorm(4, sd=input$sigb.mn)
    y[1,1,] <- input$mu.mn + input$a1.mn + b[1] +
      rnorm(input$n.mn, mean=0, sd=input$sigma.mn) 
    y[1,2,] <- input$mu.mn + input$a1.mn + b[2] +
      rnorm(input$n.mn, mean=0, sd=input$sigma.mn)
    y[2,1,] <- input$mu.mn + a2 + b[3] +
      rnorm(input$n.mn, mean=0, sd=input$sigma.mn)
    y[2,2,] <- input$mu.mn + a2 + b[4] +
      rnorm(input$n.mn, mean=0, sd=input$sigma.mn)
    ymat <- xmat <- array(dim=c(input$n.mn, 4))
    ymat[,1] <- y[1,1,]
    ymat[,2] <- y[1,2,]
    ymat[,3] <- y[2,1,]
    ymat[,4] <- y[2,2,]
    for(i in 1:4) xmat[,i] <- i + runif(input$n.mn, min=-jit, max=jit)
    return(list(xmat=xmat, ymat=ymat))
  })
  output$plot.mn <- renderPlot({
    par(mar=c(4, 2, 2, .5), cex.lab=1.5, cex.axis=1.3, cex.main=1.7)
    matplot(plot.obj.mn()$xmat, plot.obj.mn()$ymat, xaxt='n', xlab='Levels of Factors (A,B)', 
            ylab='', ylim=c(-20, 20), pch=1, xlim=c(1-jit, 4+jit), 
            main='Simulated data')
    axis(side=1, at=c(1:4), label=c('(1, 1)', '(1, 2)', '(2, 3)', '(2, 4)'))
  })
}

#shinyApp(ui = ui, server = server)