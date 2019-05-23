#inputs and output have suffix .t3

sattherthwaite <- function(s1, s2, n1, n2)
{
  (s1^2/n1 + s2^2/n2)^2 / 
    ((s1^2/n1)^2 / (n1-1) + (s2^2/n2)^2 / (n2-1))
}

combined_ui <- function()
{
	fluidPage(
	fluidRow(wellPanel(
	                   h2(strong('Two-sample t test: test statistic and p-value'))
	)),
  fluidRow(
    column(3, wellPanel(h2(style="text-align:center", strong('Summary statistics')),
                hr(),
                p('For simplicity, we will assume',
                  HTML('s<sub>1</sub> = s<sub>2</sub> and 
                  n<sub>1</sub> = n<sub>2</sub>.')),
                        sliderInput('xbar1.t3', label=HTML('Sample 1 mean (x&#772;<sub>1</sub>)'), min=0, 
                                    max=10, value=5, step=.01),
                        sliderInput('xbar2.t3', label=HTML('Sample 2 mean (x&#772;<sub>2</sub>)'), min=0, 
                                    max=10, value=3, step=.01),
                        hr(), 
                        sliderInput(inputId='s.t3', label=
                                      HTML('Sample std devs (s<sub>1</sub> = s<sub>2</sub>)'), 
                                    min=1, max=10, value=3, step=.01),
                        hr(),
                radioButtons('n_input', label=h4('Input type for sample sizes'), 
                             choiceNames = list('Slider', 'Text Input'), 
                             choiceValues = list(1, 2)),
                hr(),
                conditionalPanel('input.n_input == 1',
                                 sliderInput(inputId='slider.n', label=
                                               HTML('Sample sizes (n<sub>1</sub> = n<sub>2</sub>)'), 
                                             min=2, max=200, value=30, step=1)),
                conditionalPanel('input.n_input == 2', 
                                 numericInput('text.n', label=
                                                HTML('Sample sizes (n<sub>1</sub> = n<sub>2</sub>)'), 
                                              value=30, min=2, step=1), 
                                 'Required: n > 1')
    )), 
    column(3, wellPanel(h2(style="text-align:center", strong('Test statistic')), 
                        hr(),
                        p(style="text-align:center", tags$img(width='125px', src='t_formula.PNG')),
                        hr(),
                        h4(strong('Two parts of formula:')), 
                        tags$ul(
                          tags$li(strong('Numerator:'), 'difference in means'), 
                          verbatimTextOutput('diff.t3'), 
                          tags$li(strong('Denominator:'), 'standard error'),
                          verbatimTextOutput('se.t3')
                        ),
                        hr(),
                        h4(strong('Value of test statistic')),
                        plotOutput(outputId='tplot.t3', height="140px")
    )), 
    column(6, wellPanel(h2(style="text-align:center", strong('P-value')), 
                        hr(),
            fluidRow(column(4, wellPanel(
                        radioButtons(inputId='Ha.t3', label=
                                       HTML('Alternative hypothesis (H<sub>a</sub>)'), 
                                     inline=F,
                                     choiceNames=list(HTML('H<sub>a</sub>: &mu;<sub>1</sub> > &mu;<sub>2</sub>'), 
                                                      HTML('H<sub>a</sub>: &mu;<sub>1</sub> < &mu;<sub>2</sub>'), 
                                                      HTML('H<sub>a</sub>: &mu;<sub>1</sub> &ne; &mu;<sub>2</sub>')),
                                     choiceValues=c('mu1 > mu2', 'mu1 < mu2', 'mu1 =/= mu2')),
                        hr(),
                        h5(strong('DF (Easy Approx*)')),
                        verbatimTextOutput('df.t3'), 
                        hr(), 
                        h5(strong('What to enter in TI calculator')),
                        verbatimTextOutput('TI.t3') 
            )),
            column(8, wellPanel(
                        h4(strong('Picture:')),
                        plotOutput('picture.t3', height='300px')
            ))),
            
    '*DF approximated as minimum of the two sample sizes minus one.'))
  )
)}

combined_serv <- function(input, output)
{
  n.t3 <- reactive({
    ifelse(input$n_input==1, input$slider.n, input$text.n)
  })
  output$diff.t3 <- renderText(input$xbar1.t3 - input$xbar2.t3)
  output$se.t3 <- renderText({
    if(is.na(n.t3())) return('ENTER N')
    else if(n.t3()<=1) return('Required: n > 1')
    else if(n.t3()!=round(n.t3())) return('n must be an integer')
    round(sqrt(2*input$s.t3^2/n.t3()), 2)
  })
  output$df.t3 <- renderText({
    if(is.na(n.t3())) return('ENTER N')
    else if(n.t3()<=1) return('Required: n > 1')
    else if(n.t3()!=round(n.t3())) return('n must be an integer')
    n.t3()-1
  })
  output$TI.t3 <- renderText({
    if(is.na(n.t3())) return('ENTER N')
    else if(n.t3()<=1) return('Required: n > 1')
    else if(n.t3()!=round(n.t3())) return('n must be an integer')
    t.obs <- round(tstat(input$xbar1.t3, input$xbar2.t3, input$s.t3, n.t3()), 2)
    df <- n.t3() - 1
    if(input$Ha.t3 == 'mu1 > mu2') text <- paste0('tcdf(', t.obs,', 9999, ',
                                               df, ')')
    if(input$Ha.t3 == 'mu1 < mu2') text <- paste0('tcdf(-9999, ', t.obs, ', ',
                                               df, ')')
    if(input$Ha.t3 == 'mu1 =/= mu2') text <- paste0('2*tcdf(', 
                                                 abs(t.obs),', 9999, ', df, ')')
    text
  })
  output$tplot.t3 <- renderPlot({
    par(mar=c(2.5, .5, 2, .5), cex.main=2, cex.lab=1.7, cex.axis=1.5)
    if(is.na(n.t3())){
      plot(x=0,y=0,xaxt='n', xlab='', yaxt='n', type='n')
      text(0,0,'Enter n', col=2, cex=3)
    }
    else if(n.t3()<=1){
      plot(x=0,y=0,xaxt='n', xlab='', yaxt='n', type='n')
      text(0,0,'Required: n > 1', col=2, cex=3)
    }
    else if(n.t3()!=round(n.t3())){
      plot(x=0,y=0,xaxt='n', xlab='', yaxt='n', type='n')
      text(0,0,'n must be an integer', col=2, cex=3)
    }
    else {t.obs <- tstat(input$xbar1.t3, input$xbar2.t3, input$s.t3, n.t3())
    plot(x=NULL, y=NULL, xlim=c(-10, 10), ylim=c(-1, 1), yaxt='n', xlab='',
         main=substitute('t = '*t.obs, list(t.obs=round(t.obs, 2))))
    abline(v=seq(-10, 10, by=1), col=grey(.8))
    abline(v=0)
    col <- ifelse(t.obs > 0, 4, 2)
    if(t.obs!=0) arrows(0, 0, t.obs, 0, length=.1, lwd=3, col=col)
    if(t.obs==0) points(0, 0 , pch=16)
    if(t.obs < -11) text(-11, .5, 'Off-screen', pos=4)
    if(t.obs > 11) text(11, .5, 'Off-screen', pos=2)
    }
  })
  output$picture.t3 <- renderPlot({
    par(mar=c(4, .5, 2, .5), cex.main=2, cex.lab=1.7, cex.axis=1.5)
    if(is.na(n.t3())){
      plot(x=0,y=0,xaxt='n', xlab='', yaxt='n', type='n')
      text(0,0,'Enter n', col=2, cex=3)
    }
    else if(n.t3()<=1){
      plot(x=0,y=0,xaxt='n', xlab='', yaxt='n', type='n')
      text(0,0,'Required: n > 1', col=2, cex=3)
    }
    else if(n.t3()!=round(n.t3())){
      plot(x=0,y=0,xaxt='n', xlab='', yaxt='n', type='n')
      text(0,0,'n must be an integer', col=2, cex=3)
    }
    else{
    x <- seq(-4, 4, by=.1)
    df <- n.t3()-1
    t <- tstat(input$xbar1.t3, input$xbar2.t3, input$s.t3, n.t3())
    if(input$Ha.t3 == 'mu1 > mu2') pval <- pt(t, df=df, lower.tail=F)
    if(input$Ha.t3 == 'mu1 < mu2') pval <- pt(t, df=df, lower.tail=T)
    if(input$Ha.t3 == 'mu1 =/= mu2') pval <- 2*pt(-abs(t), df=df)
    par(mar=c(4, .5, 2, .5), cex.main=2, cex.lab=1.7, cex.axis=1.5)
    plot(x, dt(x, df), type='l', lwd=1, xlab='test statistic t', xaxt='n', yaxt='n', ylab='')
    pval <- ifelse(pval < .0001, '< .0001', 
                   ifelse(pval > .9999, '> .9999', round(pval, 4)))
    title(substitute('P-value = '*pval, 
                     list(pval=pval)))
    if(t < -4.3) axis(side=1, at=-4.1, substitute('<-  '*t, list(t=round(t, 2))), 
                    hadj=0, tick=F)
    else if(t > 4.3) axis(side=1, at=4.1, substitute(t*'  ->', list(t=round(t, 2))), 
                   hadj=1, tick=F)
    else axis(side=1, at=round(t, 2))
    if(input$Ha.t3 == 'mu1 > mu2'){ if(t < 4){ 
      t.start <- round(max(t, -4), 2)
      t.seq <- seq(t.start, 4, by=.01)
      polygon(c(t.seq, 4, t.start), c(dt(t.seq, df=df), 0, 0), col=grey(.6))
    }}
    if(input$Ha.t3 == 'mu1 < mu2'){ if(t > -4){ 
      t.start <- round(min(t, 4), 2)
      t.seq <- seq(-4, t.start, by=.01)
      polygon(c(t.seq, t.start, -4), c(dt(t.seq, df=df), 0, 0), col=grey(.6))
    }}
    if(input$Ha.t3 == 'mu1 =/= mu2'){ if(abs(t) < 4){
      t.temp <- round(abs(t), 2)
      t.neg <- seq(-4, -t.temp, by=.01)
      polygon(c(t.neg, -t.temp, -4), c(dt(t.neg, df=df),0,0), col=grey(.6))
      t.pos <- seq(t.temp, 4, by=.01)
      polygon(c(t.pos, 4, t.temp), c(dt(t.pos, df=df), 0,0), col=grey(.6))
    }}
    }
  })
}