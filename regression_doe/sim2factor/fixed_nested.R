#library(shiny)
fixed_nested_ui <- function(){
  fluidPage(
  fluidRow(
    wellPanel(h3(strong('Simulating from fixed-effects model with nested factors')), 
              h4(HTML('Y<sub>ijk</sub> = '), 
                 HTML('&mu; + &alpha;<sub>i</sub> + &beta;<sub>(i)j</sub> + E<sub>ijk</sub>,')), 
                      'i=1,2, j=1,2, k=1,...,n'
    )),
  fluidRow(
    column(3, wellPanel(
      sliderInput('mu.fn', label=HTML('Overall mean &mu;'), min=-5, max=5, value=0, step=.1), 
      sliderInput('a1.fn', label=HTML('&alpha;<sub>1</sub>'), 
                  min=-5, max=5, value=0, step=.1), 
      sliderInput('b11', label=HTML('&beta;<sub>(1)1</sub>'), 
                  min=-5, max=5, value=0, step=.1),
      sliderInput('b21', label=HTML('&beta;<sub>(2)1</sub>'), 
                  min=-5, max=5, value=0, step=.1), 
      sliderInput('sigma.fn', label=HTML('&sigma; (Standard deviation of E<sub>ijk</sub>)'), 
                  min=0, max=5, value=0, step=.1)
    )),
    column(3, wellPanel(
      sliderInput('n.fn', label='n (Sample size of each treatment)', min=2, max=100, value=20),
      actionButton("draw.fn", 'Draw new sample!'),
      hr(),
      'Note: Due to parameter restrictions',
      tags$ul(
        tags$li(HTML('&alpha;<sub>2</sub> = -&alpha;<sub>1</sub>')), 
        tags$li(HTML('&beta;<sub>(1)2</sub> = -&beta;<sub>(1)1</sub>')), 
        tags$li(HTML('&beta;<sub>(2)2</sub> = -&beta;<sub>(2)1</sub>'))
      )
    )),
    column(6, wellPanel(
      plotOutput('plot.fn')
    ))
  )
)
}

fixed_nested_server <- function(input, output) {
  jit <- .2
  plot.obj.fn <- reactive({
    input$draw.fn
    y <- array(dim=c(2, 2, input$n.fn))
    a2 <- -input$a1.fn
    b12 <- -input$b11
    b22 <- -input$b21
    y[1,1,] <- input$mu.fn + input$a1.fn + input$b11 +
      rnorm(input$n.fn, mean=0, sd=input$sigma.fn) 
    y[1,2,] <- input$mu.fn + input$a1.fn + b12 +
      rnorm(input$n.fn, mean=0, sd=input$sigma.fn)
    y[2,1,] <- input$mu.fn + a2 + input$b21 +
      rnorm(input$n.fn, mean=0, sd=input$sigma.fn)
    y[2,2,] <- input$mu.fn + a2 + b22 +
      rnorm(input$n.fn, mean=0, sd=input$sigma.fn)
    ymat <- xmat <- array(dim=c(input$n.fn, 4))
    ymat[,1] <- y[1,1,]
    ymat[,2] <- y[1,2,]
    ymat[,3] <- y[2,1,]
    ymat[,4] <- y[2,2,]
    for(i in 1:4) xmat[,i] <- i + runif(input$n.fn, min=-jit, max=jit)
    return(list(xmat=xmat, ymat=ymat))
  })
  output$plot.fn <- renderPlot({
    par(mar=c(4, 2, 2, .5), cex.lab=1.5, cex.axis=1.3, cex.main=1.7)
    matplot(plot.obj.fn()$xmat, plot.obj.fn()$ymat, xaxt='n', xlab='Levels of Factors (A,B)', 
            ylab='', ylim=c(-20, 20), pch=1, xlim=c(1-jit, 4+jit), 
            main='Simulated data')
    axis(side=1, at=c(1:4), label=c('(1, 1)', '(1, 2)', '(2, 3)', '(2, 4)'))
  })
}

#shinyApp(ui = ui, server = server)