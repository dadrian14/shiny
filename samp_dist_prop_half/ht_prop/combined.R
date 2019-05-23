combined.ui <- function(){
  fluidRow(
  column(4, wellPanel(
    h3(strong('Numbers in test statistic formula')), 
    sliderInput('phatc', label=HTML('<h4>Sample proportion (p&#770;)</h4>'), 
                min=.01, max=.99, value=.51, step=.01), 
    sliderInput('p0c', label=HTML('<h4>Null value (p<sub>0</sub>)</h4>'), 
                min=.01, max=.99, value=.5, step=.01),
    sliderInput('nc', label=h4('Sample size (n)'), min=10, max=1000, 
                value=100, step=10), 
    hr(),
    h3(strong('Test statistic')), 
    plotOutput('tstatc', height="125px") 
  )), 
  column(8, wellPanel(
    h3(strong('P-value')), 
    fluidRow(
      column(4, wellPanel(
        radioButtons('Hac', 
                     label=HTML('<h4>Alternative Hypothesis (H<sub>a</sub>)</h4>'), 
                     choices=list('p > p0' = 'gr', 'p < p0' = 'le', 'p =/= p0' = 'ne'))
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
    z <- (input$phatc - input$p0c) / sqrt(input$p0c * (1-input$p0c) / input$nc)
    assump.ok <- all((input$nc * c(input$p0c, 1-input$p0c)) >= 10)
    par(cex.main=2, cex.axis=1.5, mar=c(2.1, .1, 3, .1))
    plot(NA, NA, xlim=c(-5, 5), ylim=c(-.05, .05), xaxt='n', yaxt='n', xlab='', 
         ylab='')
    if(assump.ok==F) text(0,0, 'Normal conditions not met', col=2, cex=1.6)
    else{
      abline(v=-10:10/2, lty=3, col=grey(.5))
      axis(side=1, at=-5:5)
      title(paste('z =', round(z,2)))
      if(z > 0) arrows(0,0,x1=z, col=4, lwd=3, length=.12)
      if(z < 0) arrows(0,0,x1=z, col=3, lwd=3, length=.12)
      if(z>5.4) text(4.2, .03, 'Off-screen', col=2)
      if(z< -5.4) text(-4.2, .03, 'Off-screen', col=2)
    }
  })
  output$znumc <- renderPrint({input$phatc - input$p0c})
  output$zdenc <- renderPrint({sqrt(input$p0c * (1-input$p0c) / input$nc)})
  output$pval.picc <- renderPlot({
    assump.ok <- all((input$nc * c(input$p0c, 1-input$p0c)) >= 10)
    if(assump.ok==F){
      par(cex.main=2, cex.lab=1.6, cex.axis=1.4, mar=c(4.1, .5, 5.1, .5))
      plot(NA, NA, xlim=c(-5, 5), ylim=c(-.05, .05), xaxt='n', yaxt='n', xlab='', ylab='')
      text(0,0, 'Normal conditions\nnot met', col=2, cex=2)
    }
    else{
      z <- (input$phatc - input$p0c) / sqrt(input$p0c * (1-input$p0c) / input$nc)
      if(input$Hac == 'le') pval <- pnorm(z)
      if(input$Hac == 'gr') pval <- 1-pnorm(z)
      if(input$Hac == 'ne') pval <- 2*pnorm(-abs(z))
      z <- round(z, 2)
      pval <- ifelse(pval < .001, '<.001', round(pval, 3))
      x <- seq(-3.6, 3.6, by=.01)
      par(cex.main=2, cex.lab=1.6, cex.axis=1.4, mar=c(4.1, .5, 5.1, .5))
      plot(x, dnorm(x), type='l', main=paste0('p-value = ',pval), 
           yaxt='n', ylab='', xaxt='n', xlab='Test statistic (z)')
      if(input$Hac == 'gr'){
        if(z < -3.6){
          axis(side=1, at=-3, label=paste('<-', z), tick=F)
          z.seq <- seq(-3.6, 3.6, by=.01)
          polygon(c(z.seq, 3.6, -3.6), c(dnorm(z.seq), 0,0), col=grey(.6))
        }
        else if(z < 3.6){
          axis(side=1, at=z)
          z.seq <- seq(z, 3.6, by=.01)
          polygon(c(z.seq, 3.6, z), c(dnorm(z.seq), 0,0), col=grey(.6))
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
          polygon(c(z.seq, -3.6, z), c(dnorm(z.seq), 0, 0), col=grey(.6))
        }
        else{
          axis(side=1, at=3, label=paste(z, ' ->'), tick=F)
          z.seq <- seq(3.6, -3.6, by=-.01)
          polygon(c(z.seq, -3.6, z), c(dnorm(z.seq), 0, 0), col=grey(.6))
        }
      }
      if(input$Hac == 'ne'){
        if(abs(z) < 3.6){
          axis(side=1, at=c(-z,z))
          z.seq <- seq(-abs(z), -3.6, by=-.01)
          polygon(c(z.seq, -3.6, -abs(z)), c(dnorm(z.seq), 0, 0), col=grey(.6))
          z.seq <- seq(abs(z), 3.6, by=.01)
          polygon(c(z.seq, 3.6, abs(z)), c(dnorm(z.seq), 0,0), col=grey(.6))
        }
        else{
          axis(side=1, at=c(-3, 3), labels=c(paste('<-',-abs(z)), paste(abs(z),'->')))
        }
      }
    }
  })
}
