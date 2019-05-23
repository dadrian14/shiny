#server functions for combined panel

server.combined <- function(input, output){
  #Makes UI
  output$combined_panel <- renderUI({
    fluidRow(
      wellPanel(
        h3(strong('Combined test statistic / p-value')), 
        checkboxInput('combined_info', label='Click for more info!'), 
        conditionalPanel('input.combined_info == true', 
          fluidRow(
            column(3, wellPanel(
              tags$div('Use the sliders to change the variables in the formula 
                       for the test statistic.', 
                       style='font-size: x-small;')
            )),
            column(3, wellPanel(
              tags$div('The normal conditions are checked, and the test statistic 
                       is displayed.', 
                       style='font-size: x-small;')
            )),
            column(6, wellPanel(
              tags$div('Displays the p-value corresponding to the current value of the 
                       test statistic and the alternative hypothesis chosen.', 
                       style='font-size: x-small;')
            )),style="margin: 0px -20px;")),
          style="padding-bottom: 0px;"),
      column(3, wellPanel(
        h4(strong('Test statistic')), 
        withMathJax('$$z = \\frac{\\hat{p} - p_0}{\\sqrt{\\frac{p_0(1-p_0)}{n}}}$$'),
        hr(),
        sliderInput('phatc', label=withMathJax('Sample proportion \\(\\hat{p}\\)'), 
                    min=.01, max=.99, value=.51, step=.001), 
        sliderInput('p0c', label=withMathJax('Null value \\(p_0\\)'), 
                    min=.01, max=.99, value=.5, step=.001),
        sliderInput('nc', label=withMathJax('Sample size \\(n\\)'), min=2, max=1000, 
                    value=100, step=1)
      )),
      column(3, wellPanel(
        h4(strong('Normal conditions')),
        br(),
        withMathJax('\\(np_0\\geq 10\\)'), 'and', withMathJax('\\(n(1-p_0)\\geq 10\\)'),
        br(), br(),
        tags$ul(
          tags$li(
            withMathJax('\\(np_0 = \\)'), uiOutput('n_succ_c', inline=T)
          ), 
          tags$li(
            withMathJax('\\(n(1-p_0) = \\)'), uiOutput('n_fail_c', inline=T)
          ), 
          tags$li(
            'Both?', uiOutput('norm_cond_c', inline=T)
          )
        ),
        hr(),
        h4(strong('Test statistic')), 
        plotOutput('tstatc', height="125px") 
      )),
      column(6, wellPanel(
        h4(strong('P-value')), 
            radioButtons('Hac', 
                         label=withMathJax('Alternative Hypothesis \\((H_a)\\)'), 
                         choiceValues=list('gr', 'le', 'ne'),
                         choiceNames=list(withMathJax('\\(H_a: p > p_0\\)'),
                                          withMathJax('\\(H_a: p < p_0\\)'),
                                          withMathJax('\\(H_a: p \\ne p_0\\)'))),
        hr(),
            plotOutput('pval.picc', height='300px')
      ))
    )
  })
  
  #Other server functions
  output$n_succ_c <- renderUI({
    succ <- input$nc * input$p0c
    HTML(paste(round(succ, 2), 
               ifelse(succ >= 10,
                      '&ge; 10 <span style="color:green">&#10004;</span>', 
                      '< 10 <span style="color:red">&#10008;</span>')))
  })
  output$n_fail_c <- renderUI({
    fail <- input$nc * (1-input$p0c)
    HTML(paste(round(fail, 2), 
               ifelse(fail >= 10,
                      '&ge; 10 <span style="color:green">&#10004;</span>', 
                      '<10 <span style="color:red">&#10008;</span>')))
  })
  output$norm_cond_c <- renderUI({
    succ <- input$nc * input$p0c
    fail <- input$nc * (1-input$p0c)
    HTML(ifelse(succ >=10 & fail >= 10,
                '<span style="color:green">&#10004;</span>', 
                '<span style="color:red">&#10008;</span>'))
  })
  output$tstatc <- renderPlot({
    z <- (input$phatc - input$p0c) / sqrt(input$p0c * (1-input$p0c) / input$nc)
    assump.ok <- all((input$nc * c(input$p0c, 1-input$p0c)) >= 10)
    par(cex.main=1.7, cex.axis=1.2, mar=c(2.1, .1, 3, .1))
    plot(NA, NA, xlim=c(-5, 5), ylim=c(-.05, .05), xaxt='n', yaxt='n', xlab='', 
         ylab='')
    if(assump.ok==F) text(0,0, 'Normal conditions not met', col=2, cex=1.6)
    else{
      abline(v=-10:10/2, lty=3, col=grey(.5))
      axis(side=1, at=-5:5)
      title(paste('z =', round(z,2)))
      if(z > 0) arrows(0,0,x1=z, col=4, lwd=3, length=.07)
      if(z < 0) arrows(0,0,x1=z, col=3, lwd=3, length=.07)
      if(z>5.4) text(4.2, .03, 'Off-screen', col=2)
      if(z< -5.4) text(-4.2, .03, 'Off-screen', col=2)
    }
  })
  output$znumc <- renderPrint({input$phatc - input$p0c})
  output$zdenc <- renderPrint({sqrt(input$p0c * (1-input$p0c) / input$nc)})
  output$pval.picc <- renderPlot({
    assump.ok <- all((input$nc * c(input$p0c, 1-input$p0c)) >= 10)
    if(assump.ok==F){
      par(cex.main=1.7, cex.lab=1.5, cex.axis=1.4, mar=c(4.1, .5, 3.1, .5))
      plot(NA, NA, xlim=c(-5, 5), ylim=c(-.05, .05), xaxt='n', yaxt='n', xlab='', 
           ylab='')
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
      par(cex.main=1.7, cex.lab=1.5, cex.axis=1.4, mar=c(4.1, .5, 3.1, .5))
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
          axis(side=1, at=c(z, -z))
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