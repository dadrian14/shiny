library(shiny)
mixed_crossed_ui <- function(){
  fluidPage(
  fluidRow(
    wellPanel(h3(strong('Simulating from mixed-effects model with crossed factors')), 
              h4(HTML('Y<sub>ijk</sub> = '), 
                 HTML('&mu; + &alpha;<sub>i</sub> + B<sub>j</sub> + '), 
                 HTML('(&alpha;B)<sub>ij</sub> + E<sub>ijk</sub>,')),
              'i=1,2, j=1,2, k=1,...n'
  )),
  fluidRow(
    column(3, wellPanel(
      sliderInput('mu.mc', label=HTML('Overall mean &mu;'), min=-5, max=5, value=0, step=.1), 
      sliderInput('a1.mc', label=HTML(paste0('&alpha;', tags$sub('1'))), 
                  min=-5, max=5, value=0, step=.1),
      sliderInput('sigb.mc', label=HTML(paste0('&sigma;', tags$sub('B'), 
                                      ' (Standard deviation of B<sub>j</sub>)')), 
                  min=0, max=5, value=0, step=.1),
      sliderInput('sigab.mc', label=HTML('&sigma;<sub>&alpha;B</sub> (Standard deviation of (&alpha;B)<sub>ij</sub>)'), 
                  min=0, max=5, value=0, step=.1), 
      sliderInput('sigma.mc', label=HTML('&sigma; (Standard deviation of E<sub>ijk</sub>)'), 
                  min=0, max=5, value=0, step=.1)
    )), 
    column(3, wellPanel(
      sliderInput('n.mc', label='n (Sample size of each treatment)', min=2, max=100, value=20),
      actionButton("draw.mc", 'Draw new sample!'),
      hr(),
      p(HTML('Note: Due to parameter restrictions, &alpha;<sub>2</sub> = -&alpha;<sub>1</sub>'))
    )),
    column(6, wellPanel(
      plotOutput('plot.mc')
    ))
  )
)
}

mixed_crossed_server <- function(input, output) {
  jit <- .2
  plot.obj.mc <- reactive({
    input$draw.mc
    y <- array(dim=c(2, 2, input$n.mc))
    a2 <- -input$a1.mc
    b <- rnorm(2, mean=0, sd=input$sigb.mc)
    ab <- array(rnorm(4, mean=0, sd=input$sigab.mc), dim=c(2,2))
    y[1,1,] <- input$mu.mc + input$a1.mc + b[1] + ab[1,1] + 
      rnorm(input$n.mc, mean=0, sd=input$sigma.mc)
    y[1,2,] <- input$mu.mc + input$a1.mc + b[2] + ab[1,2] + 
      rnorm(input$n.mc, mean=0, sd=input$sigma.mc)
    y[2,1,] <- input$mu.mc + a2 + b[1] + ab[2,1] + 
      rnorm(input$n.mc, mean=0, sd=input$sigma.mc)
    y[2,2,] <- input$mu.mc + a2 + b[2] + ab[2,2] + 
      rnorm(input$n.mc, mean=0, sd=input$sigma.mc)
    ymat <- xmat <- array(dim=c(input$n.mc, 4))
    ymat[,1] <- y[1,1,]
    ymat[,2] <- y[1,2,]
    ymat[,3] <- y[2,1,]
    ymat[,4] <- y[2,2,]
    for(i in 1:4) xmat[,i] <- i + runif(input$n.mc, min=-jit, max=jit)
    return(list(xmat=xmat, ymat=ymat))
  })
  output$plot.mc <- renderPlot({
    par(mar=c(4, 2, 2, .5), cex.lab=1.5, cex.axis=1.3, cex.main=1.7)
    matplot(plot.obj.mc()$xmat, plot.obj.mc()$ymat, xaxt='n', xlab='Levels of Factors (A, B)', 
            ylab='', ylim=c(-20, 20), pch=1, xlim=c(1-jit, 4+jit), 
            main='Simulated data')
    axis(side=1, at=c(1:4), label=c('(1, 1)', '(1, 2)', '(2, 1)', '(2, 2)'))
  })
}

#shinyApp(ui = ui, server = server)