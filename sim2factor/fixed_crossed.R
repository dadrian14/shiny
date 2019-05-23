#library(shiny)
fixed_crossed_ui <- function()
{
  fluidPage(
  fluidRow(
    wellPanel(h3(strong('Simulating from fixed-effects model with crossed factors')), 
              h4(HTML('Y<sub>ijk</sub> = '), 
              HTML('&mu; + &alpha;<sub>i</sub> + &beta;<sub>j</sub> + '), 
              HTML('(&alpha;&beta;)<sub>ij</sub> + E<sub>ijk</sub>,')), 
              'i=1,2, j=1,2, k=1,...,n'
  )),
  fluidRow(
    column(3, wellPanel(
      sliderInput('mu.fc', label=HTML('Overall mean &mu;'), min=-5, max=5, value=0, step=.1), 
      sliderInput('a1.fc', label=HTML('&alpha;<sub>1</sub>'), 
                  min=-5, max=5, value=0, step=.1), 
      sliderInput('b1.fc', label=HTML(paste0('&beta;', tags$sub('1'))), 
                  min=-5, max=5, value=0, step=.1),
      sliderInput('ab11', label=HTML(paste0('(&alpha;&beta;)', tags$sub('11'))), 
                  min=-5, max=5, value=0, step=.1), 
      sliderInput('sigma.fc', label=HTML('&sigma; (Standard deviation of E<sub>ijk</sub>)'), 
                  min=0, max=5, value=0, step=.1)
    )),
    column(3, wellPanel(
      sliderInput('n.fc', label='n (Sample size of each treatment)', min=2, max=100, value=20),
      actionButton("draw.fc", 'Draw new sample!'),
      hr(),
      'Note: Due to parameter restrictions',
      tags$ul(
        tags$li(HTML('&alpha;<sub>2</sub> = -&alpha;<sub>1</sub>')), 
        tags$li(HTML('&beta;<sub>2</sub> = -&beta;<sub>1</sub>')), 
        tags$li(HTML('(&alpha;&beta;)<sub>12</sub> = -(&alpha;&beta;)<sub>11</sub>')),
        tags$li(HTML('(&alpha;&beta;)<sub>21</sub> = -(&alpha;&beta;)<sub>11</sub>')),
        tags$li(HTML('(&alpha;&beta;)<sub>22</sub> = (&alpha;&beta;)<sub>11</sub>'))
      )
    )),
    column(6, wellPanel(
      plotOutput('plot.fc')
    ))
  )
)
}

fixed_crossed_server <- function(input, output) {
  jit <- .2
  plot.obj.fc <- reactive({
    input$draw.fc
    y <- array(dim=c(2, 2, input$n.fc))
    a2 <- -input$a1.fc
    b2 <- -input$b1.fc
    ab12 <- -input$ab11
    ab21 <- -input$ab11
    ab22 <- input$ab11
    y[1,1,] <- input$mu.fc + input$a1.fc + input$b1.fc + input$ab11 + 
      rnorm(input$n.fc, mean=0, sd=input$sigma.fc)
    y[1,2,] <- input$mu.fc + input$a1.fc + b2 + ab12 + 
      rnorm(input$n.fc, mean=0, sd=input$sigma.fc)
    y[2,1,] <- input$mu.fc + a2 + input$b1.fc + ab21 + 
      rnorm(input$n.fc, mean=0, sd=input$sigma.fc)
    y[2,2,] <- input$mu.fc + a2 + b2 + ab22 + 
      rnorm(input$n.fc, mean=0, sd=input$sigma.fc)
    ymat <- xmat <- array(dim=c(input$n.fc, 4))
    ymat[,1] <- y[1,1,]
    ymat[,2] <- y[1,2,]
    ymat[,3] <- y[2,1,]
    ymat[,4] <- y[2,2,]
    for(i in 1:4) xmat[,i] <- i + runif(input$n.fc, min=-jit, max=jit)
    return(list(xmat=xmat, ymat=ymat))
  })
  output$plot.fc <- renderPlot({
    par(mar=c(4, 2, 2, .5), cex.lab=1.5, cex.axis=1.3, cex.main=1.7)
    matplot(plot.obj.fc()$xmat, plot.obj.fc()$ymat, xaxt='n', xlab='Levels of Factors (A,B)', 
            ylab='', ylim=c(-20, 20), pch=1, xlim=c(1-jit, 4+jit), 
            main='Simulated data')
    axis(side=1, at=c(1:4), label=c('(1, 1)', '(1, 2)', '(2, 1)', '(2, 2)'))
  })
}

#shinyApp(ui = ui, server = server)