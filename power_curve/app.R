power <- function(del, sigma, n, alpha){
  ncp <- del / (sigma * sqrt(2/n))
  tquant <- qt(1-alpha/2, df=2*n-2)
  1 - pt(tquant, df=2*n-2, ncp=ncp) + pt(-tquant, df=2*n-2, ncp=ncp)
}

library(shiny)
ui <- fluidPage(
  fluidRow(wellPanel(
    h2('Power Curve')
  )),
  fluidRow(
    column(4, wellPanel(
      sliderInput('del', label=HTML('&mu;<sub>1</sub> - &mu;<sub>2</sub>'), 
                  min=-5, max=5, step=.1, value=1),
      hr(),
      sliderInput('sigma', label=HTML('&sigma;'), 
                  min=1, max=5, step=.1, value=3),
      sliderInput('n', label=HTML('Sample sizes'), 
                  min=5, max=100, step=5, value=30),
      sliderInput('alpha', label=HTML('&alpha;'),
                  min=.01, max=.20, step=.01, value=.05)
    )),
    column(8, wellPanel(
      plotOutput('curve')
    ))
  )
)

server <- function(input, output) {
  output$curve <- renderPlot({
    par(mar=c(4, 4.5, .5, .5), cex.lab=1.5, cex.axis=1.3)
    del <- seq(-5, 5, by=.1)
    all.power <- power(del, input$sigma, input$n, input$alpha)
    plot(del, all.power, type='l', lwd=2, ylim=c(0,1), ylab='Power', 
         xlab=expression(mu[1]-mu[2]))
    points(input$del, power(input$del, input$sigma, input$n, input$alpha), pch=16, cex=1.5)
    abline(v=input$del)
  })
}

shinyApp(ui = ui, server = server)