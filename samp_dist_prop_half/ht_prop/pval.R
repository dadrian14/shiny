pval.ui <- function(){
  fluidRow(
    column(4, wellPanel(
      sliderInput('z', label=h4('Test statistic (z)'), min=-3.5, max=3.5, 
                  value=.05, step=.05), 
      hr(),
      radioButtons('Ha', 
                   label=HTML('<h4>Alternative Hypothesis (H<sub>a</sub>)</h4>'), 
                   choices=list('p > p0' = 'gr', 'p < p0' = 'le', 'p =/= p0' = 'ne'))
    )),
    column(8, wellPanel(
      plotOutput('pval.pic'), 
      br(),
      tags$ul(
        tags$li('This bell-shaped curve is the standard normal distribution.'), 
        tags$li('The p-value is the area under the curve.'),
        tags$li('The area under the entire curve is 1.')
      )
      
    ))
  )
}

pval.serv <- function(input, output){
  output$pval.pic <- renderPlot({
    if(input$Ha == 'le') pval <- pnorm(input$z)
    if(input$Ha == 'gr') pval <- 1-pnorm(input$z)
    if(input$Ha == 'ne') pval <- 2*pnorm(-abs(input$z))
    pval <- ifelse(pval < .001, '<.001', round(pval, 3))
    x <- seq(-3.6, 3.6, by=.01)
    par(cex.main=2, cex.lab=1.6, cex.axis=1.4, mar=c(4.1, .5, 4.1, .5))
    plot(x, dnorm(x), type='l', main=paste0('p-value = ',pval), 
         yaxt='n', ylab='', xaxt='n', xlab='Test statistic (z)')
    axis(side=1, at=input$z)
    if(input$Ha == 'ne') axis(side=1, at=-input$z)
    #segments(x0=input$z, y0=0, y1=dnorm(input$z))
    if(input$Ha == 'gr'){
      z.seq <- seq(input$z, 3.6, by=.01)
      polygon(c(z.seq, 3.6, input$z), c(dnorm(z.seq), 0,0), col=grey(.6))
    }
    if(input$Ha == 'le'){
      z.seq <- seq(input$z, -3.6, by=-.01)
      polygon(c(z.seq, -3.6, input$z), c(dnorm(z.seq), 0, 0), col=grey(.6))
    }
    if(input$Ha == 'ne'){
      z.seq <- seq(-abs(input$z), -3.6, by=-.01)
      polygon(c(z.seq, -3.6, -abs(input$z)), c(dnorm(z.seq), 0, 0), col=grey(.6))
      z.seq <- seq(abs(input$z), 3.6, by=.01)
      polygon(c(z.seq, 3.6, abs(input$z)), c(dnorm(z.seq), 0,0), col=grey(.6))
    }
  })
}