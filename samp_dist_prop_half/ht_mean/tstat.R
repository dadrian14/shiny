tstat.ui <- function(){
  fluidRow(column(4, wellPanel(
    h3(strong('Numbers in test statistic formula')),
    sliderInput('xbar', label=HTML('<h4>Sample mean (x&#772;)</h4>'), 
                min=1, max=99, value=47, step=.1), 
    sliderInput('mu0', label=HTML('<h4>Null value (&mu;<sub>0</sub>)</h4>'), 
                min=1, max=99, value=46, step=.1),
    sliderInput('s', label=h4('Sample standard deviation (s)'), 
                min=1, max=50, value=9, step=.1),
    sliderInput('n', label=h4('Sample size (n)'), min=2, max=200, 
                value=20, step=10)
  )), 
  column(8, wellPanel(
    h3(strong('Test statistic')),
    plotOutput('tstat', height="150px"), 
    hr(),
    fluidRow(column(6, wellPanel(
      h3(strong('Formula')),
      br(),
      p(style="text-align:center",
        tags$img(height='100px',src='t_formula.PNG')
      )
    )),
    column(6, wellPanel(
      h3(strong('Parts of formula')), 
      h4('Numerator'),
      verbatimTextOutput('znum'),
      h4('Denominator'),
      verbatimTextOutput('zden')
    ))
    )))
  )
}

tstat.serv <- function(input, output){
  output$tstat <- renderPlot({
    t <- (input$xbar - input$mu0) / (input$s / sqrt(input$n))
    par(cex.main=2, cex.axis=1.5, mar=c(2.1, .5, 3, .5))
    plot(NA, NA, xlim=c(-5, 5), ylim=c(-.05, .05), xaxt='n', yaxt='n', xlab='', 
         ylab='')
    abline(v=-10:10/2, lty=3, col=grey(.5))
    axis(side=1, at=-5:5)
      title(paste('t =', round(t,2)))
      if(t > 0) arrows(0,0,x1=t, col=4, lwd=3, length=.12)
      if(t < 0) arrows(0,0,x1=t, col=3, lwd=3, length=.12)
      if(t>5.4) text(4.8, .02, 'Off-screen', col=2)
      if(t< -5.4) text(-4.8, .02, 'Off-screen', col=2)
    
  })
  output$znum <- renderPrint({input$xbar - input$mu0})
  output$zden <- renderPrint({input$s / sqrt(input$n)})
}