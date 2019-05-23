pval.ui <- function(){
  fluidPage(
  wellPanel(
    h3(strong('P-value')),
    tags$div(checkboxInput('info_pval', label='Click for more info!'), 
             style='margin-bottom: -10px'), 
    conditionalPanel('input.info_pval == true', 
                     fluidRow(
                       column(4, wellPanel(
                         tags$div('Use the slider to change the value of the test statistic \\(t\\), 
                                  and use the buttons to change the alternative hypothesis \\(H_a\\) 
                                  that is tested.', 
                                  style='font-size: x-small;')
                       )),
                       column(8, wellPanel(
                         tags$div('Note how the p-value changes (both its value and the corresponding 
                                area under the curve) when you change the test statistic
                                  and the alternative hypothesis.', br(), 
                                  'More background:',
                                  tags$ul(
                                  tags$li('The definition of the p-value is the probability of obtaining the observed 
                                  test statistic or one more extreme in the direction of \\(H_a\\) 
                                  assuming that \\(H_0\\) is true.'), 
                                  tags$li('Because we are assuming \\(H_0\\) is true, the test statistic \\(t\\)
                                  follows the \\(t\\) distribution with \\(n-1\\) degrees of freedom.  This is the bell-shaped 
                                  curve shown and the p-value is the shaded area under this curve.'),
                                  tags$li('The \"observed test statistic or more extreme in the direction of \\(H_a\\)\"
                                  part of the definition is the reason the shaded area starts at the test 
                                  statistic value \\(t\\) and is the region'),
                                  tags$ul(
                                    tags$li('greater than \\(t\\) for \\(H_a:\\mu>\\mu_0\\)'), 
                                    tags$li('less than \\(t\\) for \\(H_a:\\mu<\\mu_0\\)'),
                                    tags$li('farther from 0 than \\(t\\) for \\(H_a:\\mu\\ne \\mu_0\\)')
                                  )),
                                  style='font-size: x-small; margin-bottom:-10px;')
                       ))
                       , style="margin: 0px -20px;") 
                     , style='margin-bottom: -10px;')
  ),
  fluidRow(
    column(4, wellPanel(
      sliderInput('t', label='Test statistic \\((t)\\)', min=-5, max=5, 
                  value=.9, step=.01), 
      sliderInput('df', label='Degrees of freedom (df)', min=1, max=100, value=1),
      hr(),
      radioButtons('Ha', 
                   label='Alternative Hypothesis \\((H_a)\\)', 
                   choiceValues=list('gr', 'le', 'ne'),
                   choiceNames=list(HTML('\\(H_a: \\mu > \\mu_0\\)'), 
                                    HTML('\\(H_a: \\mu < \\mu_0\\)'), 
                                    HTML('\\(H_a: \\mu \\ne \\mu_0\\)'))
                   )
    )),
    column(8, wellPanel(
      plotOutput('pval.pic', height='350px')
    ))
  )
)}

pval.serv <- function(input, output){
  output$pval.pic <- renderPlot({
    if(input$Ha == 'le') pval <- pt(input$t, df=input$df)
    if(input$Ha == 'gr') pval <- 1-pt(input$t, df=input$df)
    if(input$Ha == 'ne') pval <- 2*pt(-abs(input$t), df=input$df)
    pval <- ifelse(pval < .0001, '<.0001', 
		  ifelse(pval>.9999, '>.9999', sprintf("%.4f", pval)))
    x <- seq(-5.1, 5.1, by=.01)
    par(cex.main=1.5, cex.lab=1.2, cex.axis=1, mar=c(4.1, .1, 2.1, .1))
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