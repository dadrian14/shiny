library(shiny)
ui <- fluidPage(
  fluidRow(wellPanel(strong(h2('Confidence interval for the population mean', HTML('&mu;'))))), 
  fluidRow(
    column(4, wellPanel(
      #h3('Numbers effecting the confidence interval'),
      sliderInput('C', label=h4('Confidence level (%)'), min=80, max=99, value=95, step=1),
      sliderInput('xbar', label=HTML('<h4>Sample mean (x&#772;)</h4>'), 
                  min=10, max=90, value=47, step=.1), 
      sliderInput('s', label=h4('Sample standard deviation (s)'), min=1, max=50, value=9, step=.1), 
      sliderInput('n', label=h4('Sample size (n)'), 
                  min=2, max=100, value=10, step=1)
    )),
    column(8, wellPanel( 
      'The \" + \" gives the center of the interval', br(),
      'The \" [ \" and the \" ] \" give the endpoints of the interval',
      plotOutput('plot', height="150px"),
      hr(),
      h4('Confidence interval formula'),
      p(style="text-align:center",
        tags$img(height='100px',src='ci_formula.PNG')),
      hr(),
      tags$p(strong('Note:'), 
             'We assume the population is normally distributed here so we can use the t distribution for any sample size.')
    ))
  
  )
)
server <- function(input, output) {
  output$plot <- renderPlot({
    tstar <- qt((1-input$C/100)/2, df=input$n-1, lower.tail=F)
    se <- input$s/sqrt(input$n)
    ci <- c(input$xbar-tstar*se, input$xbar+tstar*se)
    par(mar=c(3.1, .5, .5, .5))
    plot(ci, c(0,0), xlim=c(0, 100), ylim=c(-.05, .05), 
         pch=c('[', ']'), cex=3, yaxt='n', xlab='', xaxt='n', ylab='')
    axis(side=1, at=seq(0, 100, by=10), cex.axis=1.5)
    abline(v=(0:20)*5, lty=3, col=grey(.6))
    points(input$xbar, 0, pch="+", cex=3)
    if(ci[1] < -4) text(5, .04, '<- Lower limit', col=2)
    if(ci[2] > 104) text(95, .04, 'Upper limit ->', col=2)
  })
}

shinyApp(ui = ui, server = server)
