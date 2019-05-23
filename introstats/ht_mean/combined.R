combined.ui <- function(){fluidPage(
  wellPanel(
    h3(strong('Combined test statistic and p-value')), 
    tags$div(checkboxInput('show_info_comb', label='Click for more info!'), 
             style='margin-bottom: -10px;'), 
    conditionalPanel('input.show_info_comb == true', 
                     uiOutput('info_comb')
    , style='margin-bottom: -10px;')
  ),
  fluidRow(
    column(3, wellPanel(
      h4(strong('Inputs')),
      sliderInput('xbarc', label='Sample mean \\(\\bar{x}\\)', 
                  min=1, max=99, value=47, step=.1), 
      sliderInput('mu0c', label='Population mean under \\(H_0\\) \\((\\mu_0)\\)', 
                  min=1, max=99, value=46, step=.1),
      sliderInput('sc', label='Sample standard deviation \\(s\\)', 
                  min=1, max=50, value=9, step=.1),
      sliderInput('nc', label='Sample size \\(n\\)', min=2, max=200, 
                  value=20, step=1)
    )),
    column(3, wellPanel(
      h4(strong('Test statistic')), 
      '$$t = \\frac{\\bar{x} - \\mu_0}{s/\\sqrt{n}}$$',
      hr(),
      strong('Result'),
      uiOutput('num_den'), 
      hr(),
      strong('Plot'),
      plotOutput('tstatc', height="110px")
    )), 
    column(6, wellPanel(
      h4(strong('P-value')), 
      radioButtons('Hac', 
                       label='Alternative Hypothesis \\(H_a\\)', 
                       choiceValues=list('gr', 'le', 'ne'),
                       choiceNames=list('\\(H_a: \\mu > \\mu_0\\)', 
                                        '\\(H_a: \\mu < \\mu_0\\)', 
                                        '\\(H_a: \\mu \\ne \\mu_0\\)')
      ), 
      hr(),
      plotOutput('pval.picc', height='350px')
      
    ))
  )
)}

combined.serv <- function(input, output){
  output$info_comb <- renderUI({
    fluidRow(
      column(3, wellPanel(
        tags$div('Use the sliders to change the inputs to the test statistic formula', 
                 style='font-size: x-small')
      )),
      column(3, wellPanel(
        tags$div('Observe how the test statistic is affected by the changing inputs.', 
                 style='font-size: x-small')
      )),
      column(6, wellPanel(
        tags$div('Displays the p-value corresponding to the current value of the test statistic
                 and the alternative hypothesis chosen.', 
                 style='font-size: x-small')
      ))
      , style="margin: 0px -20px;") 
  })
  output$tstatc <- renderPlot({
    t <- (input$xbarc - input$mu0c) / (input$sc / sqrt(input$nc))
    par(cex.main=1.5, cex.axis=1, mar=c(2.1, .1, 2.1, .1))
    plot(NA, NA, xlim=c(-5, 5), ylim=c(-.05, .05), xaxt='n', yaxt='n', xlab='', 
         ylab='')
    abline(v=-10:10/2, lty=3, col=grey(.5))
    axis(side=1, at=-5:5)
    title(paste('t =', round(t,2)))
    if(t > 0) arrows(0,0,x1=t, col=4, lwd=3, length=.07)
    if(t < 0) arrows(0,0,x1=t, col=3, lwd=3, length=.07)
    if(t>5.4) text(4.2, .03, 'Off-screen', col=2)
    if(t< -5.4) text(-4.2, .03, 'Off-screen', col=2)
  })
  output$num_den <- renderUI({
    num <- input$xbarc - input$mu0c
    den <- input$sc / sqrt(input$nc)
    t <- round(num / den, 2)
    den <- round(den, 3)
    fluidPage(
      withMathJax(paste0('$$t = \\frac{', round(num, 1), '}{', den, '} = ', t, '$$'))
    )
  })
  output$pval.picc <- renderPlot({
    t <- (input$xbarc - input$mu0c) / (input$sc / sqrt(input$nc))
    if(input$Hac == 'le') pval <- pt(t, df=input$nc -1)
    if(input$Hac == 'gr') pval <- 1-pt(t, df=input$nc-1)
    if(input$Hac == 'ne') pval <- 2*pt(-abs(t), df=input$nc-1)
    t <- round(t, 2); z <- t
    pval <- ifelse(pval < .0001, '<.0001', 
                   ifelse(pval>.9999, '>.9999', sprintf("%.4f", pval)))
    x <- seq(-3.6, 3.6, by=.01)
    par(cex.main=1.5, cex.lab=1.4, cex.axis=1.2, mar=c(4.1, .1, 2.1, .1))
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
          axis(side=1, at=c(-z,z), labels=c(ifelse(abs(z)<.3, NA, -z), z))
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