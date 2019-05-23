library(shiny)
ui <- fluidPage(
  wellPanel(strong(h2('Confidence interval for the population proportion (p)'))),
  fluidRow(column(4, wellPanel(
    sliderInput('C', label=h4('Confidence level (%)'), min=80, max=99, value=95, step=1),
    sliderInput('phat', label=HTML('<h4>Sample proportion (p&#770;)</h4>'), 
                min=.01, max=.99, value=.5, step=.01),
    sliderInput('n', label=h4('Sample size (n)'), min=10, max=1000, value=100, step=10)
  )),
  column(8, wellPanel(
    tags$ul(tags$li('The \"+\" represents the center of the interval.'), 
            tags$li('The \" [ \" and \" ] \" represent the endpoints of the interval.')),
    plotOutput('plot', height="190px"),
    hr(),
    h4('Normal conditions'),
    tags$ul(tags$li(HTML('np&#770; &#8805; 10')), 
            tags$li(HTML('n(1-p&#770;) &#8805; 10'))),
    hr(),
    h4('Formula'),
    p(style="text-align:center",
      tags$img(height='100px',src='ci_formula.PNG'))
  ))
)
)

server <- function(input, output) {
  output$plot <- renderPlot({
    zstar <- qnorm((1-input$C/100)/2, lower.tail=F)
    se <- sqrt(input$phat*(1-input$phat)/input$n)
    ci <- c(input$phat-zstar*se, input$phat+zstar*se)
    succ <- input$n * input$phat
    fail <- input$n * (1-input$phat)
    par(mar=c(5.1, .5, .5, .5), cex.lab=1.5)
    if(any(c(succ,fail)<10)){
      plot(ci, c(0,0), xlim=c(0, 1), ylim=c(-.05, .05), 
           pch=c(' ', ' '), cex=3, yaxt='n', xlab='Values of the population proportion', xaxt='n', ylab='')
      axis(side=1, at=seq(0, 1, by=.1), cex.axis=1.5)
      text(0.5, 0, 'Normal conditions not met', col=2, cex=3)
    }
    else{
      plot(ci, c(0,0), xlim=c(0, 1), ylim=c(-.05, .05), 
           pch=c('[', ']'), cex=3, yaxt='n', xlab='Values of the population proportion', xaxt='n', ylab='')
      axis(side=1, at=seq(0, 1, by=.1), cex.axis=1.5)
      abline(v=(0:20)/20, lty=3, col=grey(.6))
      points(input$phat, 0, pch="+", cex=3)
    }
  })
}

shinyApp(ui = ui, server = server)

