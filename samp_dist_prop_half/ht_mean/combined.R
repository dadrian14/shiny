combined.ui <- function(){
  fluidRow(
    column(4, wellPanel(
      h3(strong('Numbers in test statistic formula')), 
      sliderInput('xbarc', label=HTML('<h4>Sample mean (x&#772;)</h4>'), 
                  min=1, max=99, value=47, step=.1), 
      sliderInput('mu0c', label=HTML('<h4>Null value (&mu;<sub>0</sub>)</h4>'), 
                  min=1, max=99, value=46, step=.1),
      sliderInput('sc', label=h4('Sample standard deviation'), 
                  min=1, max=50, value=9, step=.1),
      sliderInput('nc', label=h4('Sample size (n)'), min=2, max=200, 
                  value=20, step=1), 
      hr(),
      h3(strong('Test statistic')), 
      plotOutput('tstatc', height="125px")
    )), 
    column(8, wellPanel(
      h3(strong('P-value')), 
      fluidRow(
        column(4, wellPanel(
          radioButtons('Hac', 
                       label=HTML('<h4>Alternative Hypothesis (H<sub>a</a>)</h4>'), 
                       choiceValues=list('gr', 'le', 'ne'),
                       choiceNames=list(HTML('H<sub>a</sub>: &mu; > &mu;<sub>0</sub>'), 
                                        HTML('H<sub>a</sub>: &mu; < &mu;<sub>0</sub>'), 
                                        HTML('H<sub>a</sub>: &mu; &#8800; &mu;<sub>0</sub>'))
          )
        )), 
        column(8, wellPanel(
          plotOutput('pval.picc')
        ))
      )
    ))
  )
}

combined.serv <- function(input, output){
  output$tstatc <- renderPlot({
    t <- (input$xbarc - input$mu0c) / (input$s / sqrt(input$nc))
    par(cex.main=2, cex.axis=1.5, mar=c(2.1, .1, 3, .1))
    plot(NA, NA, xlim=c(-5, 5), ylim=c(-.05, .05), xaxt='n', yaxt='n', xlab='', 
         ylab='')
    abline(v=-10:10/2, lty=3, col=grey(.5))
    axis(side=1, at=-5:5)
    title(paste('t =', round(t,2)))
    if(t > 0) arrows(0,0,x1=t, col=4, lwd=3, length=.12)
    if(t < 0) arrows(0,0,x1=t, col=3, lwd=3, length=.12)
    if(t>5.4) text(4.2, .03, 'Off-screen', col=2)
    if(t< -5.4) text(-4.2, .03, 'Off-screen', col=2)
  })
  output$tnumc <- renderPrint({input$xbarc - input$mu0c})
  output$tdenc <- renderPrint({input$sc / sqrt(input$nc)})
  output$pval.picc <- renderPlot({
    t <- (input$xbarc - input$mu0c) / (input$sc / sqrt(input$nc))
    if(input$Hac == 'le') pval <- pt(t, df=input$nc -1)
    if(input$Hac == 'gr') pval <- 1-pt(t, df=input$nc-1)
    if(input$Hac == 'ne') pval <- 2*pt(-abs(t), df=input$nc-1)
    t <- round(t, 2); z <- t
    pval <- ifelse(pval < .001, '<.001', round(pval, 3))
    x <- seq(-3.6, 3.6, by=.01)
    par(cex.main=2, cex.lab=1.6, cex.axis=1.4, mar=c(4.1, .5, 3.1, .5))
    plot(x, dt(x, input$nc-1), type='l', main=paste0('p-value = ',pval), 
         yaxt='n', ylab='', xaxt='n', xlab='Test statistic (t)')
    if(input$Hac == 'gr'){
      if(z < -3.6){
          axis(side=1, at=-3, label=paste('<-', z), tick=F)
          z.seq <- seq(-3.6, 3.6, by=.01)
          polygon(c(z.seq, 3.6, -3.6), c(dt(z.seq, input$nc-1), 0,0), col=grey(.6))
        }
      else if(z < 3.6){
          axis(side=1, at=z)
          z.seq <- seq(z, 3.6, by=.01)
          polygon(c(z.seq, 3.6, z), c(dt(z.seq, input$nc-1), 0,0), col=grey(.6))
        }
      else{axis(side=1, at=3, label=paste(z, ' ->'), tick=F)}
    }
    if(input$Hac == 'le'){
        if(z < -3.6){
          axis(side=1, at=-3, label=paste('<-', z), tick=F)
        }
        else if(z < 3.6){
          axis(side=1, at=z)
          z.seq <- seq(z, -3.6, by=-.01)
          polygon(c(z.seq, -3.6, z), c(dt(z.seq, input$nc-1), 0, 0), col=grey(.6))
        }
        else{
          axis(side=1, at=3, label=paste(z, ' ->'), tick=F)
          z.seq <- seq(3.6, -3.6, by=-.01)
          polygon(c(z.seq, -3.6, z), c(dt(z.seq, input$nc-1), 0, 0), col=grey(.6))
        }
    }
    if(input$Hac == 'ne'){
        if(abs(z) < 3.6){
          axis(side=1, at=c(-z,z))
          z.seq <- seq(-abs(z), -3.6, by=-.01)
          polygon(c(z.seq, -3.6, -abs(z)), c(dt(z.seq, input$nc-1), 0, 0), col=grey(.6))
          z.seq <- seq(abs(z), 3.6, by=.01)
          polygon(c(z.seq, 3.6, abs(z)), c(dt(z.seq, input$nc-1), 0,0), col=grey(.6))
        }
        else{
          axis(side=1, at=c(-3, 3), labels=c(paste('<-',-abs(z)), paste(abs(z),'->')))
        }
    }
  })
}