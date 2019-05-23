pval.ui <- function(){
  fluidRow(
    column(4, wellPanel(
      sliderInput('t', label=h4('Test statistic (t)'), min=-5, max=5, 
                  value=.1, step=.05), 
      sliderInput('df', label=h4('Degrees of freedom (df)'), min=1, max=100, value=1),
      radioButtons('Ha', 
                   label=HTML('<h4>Alternative Hypothesis (H<sub>a</sub>)</h4>'), 
                   choiceValues=list('gr', 'le', 'ne'),
                   choiceNames=list(HTML('H<sub>a</sub>: &mu; > &mu;<sub>0</sub>'), 
                                    HTML('H<sub>a</sub>: &mu; < &mu;<sub>0</sub>'), 
                                    HTML('H<sub>a</sub>: &mu; &#8800; &mu;<sub>0</sub>'))
                   )
    )),
    column(8, wellPanel(
      plotOutput('pval.pic'), br(),
      'Recall that the shaded area under the curve represents the p-value.'
    ))
  )
}

pval.serv <- function(input, output){
  output$pval.pic <- renderPlot({
    if(input$Ha == 'le') pval <- pt(input$t, df=input$df)
    if(input$Ha == 'gr') pval <- 1-pt(input$t, df=input$df)
    if(input$Ha == 'ne') pval <- 2*pt(-abs(input$t), df=input$df)
    pval <- ifelse(pval < .001, '<.001', round(pval, 3))
    x <- seq(-5.1, 5.1, by=.01)
    par(cex.main=2, cex.lab=1.6, cex.axis=1.4, mar=c(4.1, .5, 4.1, .5))
    plot(x, dt(x, input$df), type='l', main=paste0('p-value = ',pval), 
         yaxt='n', ylab='', xaxt='n', xlab='Test statistic (t)')
    axis(side=1, at=input$t)
    if(input$Ha == 'ne') 
      if(abs(input$t) >=.3) axis(side=1, at=-input$t)
    #segments(x0=input$z, y0=0, y1=dnorm(input$z))
    if(input$Ha == 'gr'){
      z.seq <- seq(input$t, 5.1, by=.01)
      polygon(c(z.seq, 5.1, input$t), c(dt(z.seq, input$df), 0,0), col=grey(.6))
    }
    if(input$Ha == 'le'){
      z.seq <- seq(input$t, -5.1, by=-.01)
      polygon(c(z.seq, -5.1, input$t), c(dt(z.seq, input$df), 0, 0), col=grey(.6))
    }
    if(input$Ha == 'ne'){
      z.seq <- seq(-abs(input$t), -5.1, by=-.01)
      polygon(c(z.seq, -5.1, -abs(input$t)), c(dt(z.seq, input$df), 0, 0), col=grey(.6))
      z.seq <- seq(abs(input$t), 5.1, by=.01)
      polygon(c(z.seq, 5.1, abs(input$t)), c(dt(z.seq, input$df), 0,0), col=grey(.6))
    }
  })
}