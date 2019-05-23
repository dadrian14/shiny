tstat.ui <- function(){
  fluidRow(column(4, wellPanel(
  h3(strong('Numbers in formula for test statistic z')),
  br(),
  p(style="text-align:center",
    tags$img(height='100px',src='z_formula.PNG')
  ),
  sliderInput('phat', label=HTML('<h4>Sample proportion (p&#770;)</h4>'), 
              min=.01, max=.99, value=.51, step=.01), 
  sliderInput('p0', label=HTML('<h4>Null value of population proportion (p<sub>0</sub>)</h4>'), 
              min=.01, max=.99, value=.5, step=.01),
  sliderInput('n', label=h4('Sample size (n)'), min=10, max=1000, 
              value=100, step=10)
)), 
column(8, wellPanel(
  h3(strong('Graph of Test statistic')),
  plotOutput('tstat', height="150px"), 
  hr(),
  h3(strong('Breaking down the Formula')),
    br(),
    p(style="text-align:center",
      tags$img(height='100px',src='z_formula.PNG')),
    fluidRow(column(6,wellPanel(
      h4('Numerator'),
      textOutput('znum')
    )),
    column(6,wellPanel(
      h4('Denominator'),
      textOutput('zden')
    ))
    )
  ))
  )
}

tstat.serv <- function(input, output){
  output$tstat <- renderPlot({
    z <- (input$phat - input$p0) / sqrt(input$p0 * (1-input$p0) / input$n)
    assump.ok <- all((input$n * c(input$p0, 1-input$p0)) >= 10)
    par(cex.main=2, cex.axis=1.5, mar=c(2.1, .5, 3, .5))
    plot(NA, NA, xlim=c(-5, 5), ylim=c(-.05, .05), xaxt='n', yaxt='n', xlab='', 
         ylab='')
    if(assump.ok==F) text(0,0, 'Normal conditions not met', col=2, cex=3)
    else{
      abline(v=-10:10/2, lty=3, col=grey(.5))
      axis(side=1, at=-5:5)
      title(paste('z =', round(z,2)))
      if(z > 0) arrows(0,0,x1=z, col=4, lwd=3, length=.12)
      if(z < 0) arrows(0,0,x1=z, col=3, lwd=3, length=.12)
      if(z>5.4) text(4.8, .02, 'Off-screen', col=2)
      if(z< -5.4) text(-4.8, .02, 'Off-screen', col=2)
    }
  })
  output$znum <- renderText({input$phat - input$p0})
  output$zden <- renderText({
    round(sqrt(input$p0 * (1-input$p0) / input$n), 4)
          })
}