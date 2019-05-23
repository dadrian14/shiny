#server functions for p-value tab
server.pval <- function(input, output){
  output$pval_panel <- renderUI({
    fluidPage(
    wellPanel(
      h3(strong('P-value')), 
      checkboxInput('pval_info', label='Click for more info!'),
    conditionalPanel('input.pval_info == true',
      fluidRow(
        column(4, wellPanel(
          tags$div(withMathJax(tags$span('Use the slider to change the value of the test 
              statistic \\(z\\) and use the buttons to change the alternative hypothesis 
              \\(H_a\\) that is tested.')), 
                   style="font-size: x-small;")
        )),
        column(8, wellPanel(
          tags$div(withMathJax('Note how the p-value changes (both its value and 
                the corresponding area under the curve) when you change the test statistic
                and the alternative hypothesis.', br(), 
                'More background:',
                tags$ul(
                  tags$li('The definition of the p-value is the probability of obtaining the observed 
                test statistic or one more extreme in the direction of \\(H_a\\) 
                assuming that \\(H_0\\) is true.'), 
                tags$li('Because we are assuming \\(H_0\\) is true, the test statistic \\(z\\)
                  follows the standard normal distribution. This is the normal curve
                shown and the p-value is the shaded area under this curve.'),
                tags$li('The \"observed test statistic or more extreme in the direction of \\(H_a\\)\"
                part of the definition is the reason the shaded area starts at the test 
                statistic value \\(z\\) and is the region'),
                tags$ul(
                  tags$li('greater than \\(z\\) for \\(H_a:p>p_0\\)'), 
                  tags$li('less than \\(z\\) for \\(H_a:p<p_0\\)'),
                  tags$li('farther from 0 than \\(z\\) for \\(H_a:p\\ne p_0\\)')
                )
            )), style="font-size: x-small;")
        )), style="margin: 0px -20px;"
      )
    ), style="padding-bottom: 0px;"),
    fluidRow(
      column(4, wellPanel(
        sliderInput('z', label=withMathJax('Test statistic \\((z)\\)'), 
                    min=-3.5, max=3.5, value=.5, step=.01), 
        hr(),
        radioButtons('Ha', 
                     label=withMathJax('Alternative Hypothesis \\((H_a)\\)'), 
                     choiceValues=list('gr', 'le', 'ne'),
                     choiceNames=list(withMathJax('\\(H_a: p > p_0\\)'),
                                      withMathJax('\\(H_a: p < p_0\\)'),
                                      withMathJax('\\(H_a: p \\ne p_0\\)')))
      )),
      column(8, wellPanel(
        plotOutput('pval.pic', height='350px')
      ))
    ))
  })
  
  output$pval.pic <- renderPlot({
    if(input$Ha == 'le') pval <- pnorm(input$z)
    if(input$Ha == 'gr') pval <- 1-pnorm(input$z)
    if(input$Ha == 'ne') pval <- 2*pnorm(-abs(input$z))
    pval <- sprintf("%5.4f", pval)
    x <- seq(-3.6, 3.6, by=.01)
    par(cex.main=1.6, cex.lab=1.4, cex.axis=1.2, mar=c(4.1, .1, 2.5, .1))
    plot(x, dnorm(x), type='l', main=paste0('p-value = ',pval), 
         yaxt='n', ylab='', xaxt='n', xlab='Test statistic (z)')
    axis(side=1, at=input$z)
    if(input$Ha == 'ne') axis(side=1, at=-input$z, 
                              label=ifelse(abs(input$z)>=.2, -input$z, NA))
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