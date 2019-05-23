#server functions for test statistic tab

server.tstat <- function(input, output){
  
  #Makes UI
  output$tstat_panel <- renderUI({
    fluidPage(
    wellPanel(
      h3(strong('Test statistic:'), 
         withMathJax('\\(z = \\frac{\\hat{p} - p_0} {\\sqrt{\\frac{p_0(1-p_0)}{n}}}\\)'
      ), style="margin:0px;"), 
      checkboxInput('tstat_info', label='Click for more info!'), 
      conditionalPanel('input.tstat_info == true', 
        'Each box below provides explanation regarding the panel in the app directly below it.',
        fluidRow(
        column(3, wellPanel(
          tags$div('Use the', strong('sliders'), 'to change the variables 
                  in the formula for the test statistic.  (Helpful hint: Using the', 
          strong('arrow keys'), 'after clicking on a slider allows for finer control.)', 
          style='font-size: x-small;')
        )),
        column(3, wellPanel(
          tags$div('Using the z-test for the hypothesis test for a single proportion
                   relies on the', strong('assumption'), 'that the sampling distribution of the 
                   sample proportion is', strong('normally distributed.'), 
                   'The conditions making this assumption (that is, that the expected number 
                   of \"successes\" and \"failures\" under the null hypothesis are at least 10)
                   are checked for the current values given by the sliders.',
            style='font-size: x-small;')
        )),
        column(6, wellPanel(
          tags$div('Observe what happens to the', strong('test statistic'), 'z (including its numerator and denominator) when 
                   you change', withMathJax('\\(\\hat{p}, p_0,\\) and \\(n\\) using the sliders.'), br(), 
                   'More background: The definition of a test statistic is a number that summarizes the difference between 
                   the observed sample and what we would expect to observe if Ho is true.', 
                   'Certainly this difference is measured by the numerator', withMathJax('\\((\\hat{p} - p_0)\\),'), 
                   'and dividing by the denominator tells us whether this is a large or small difference.',
                   withMathJax(tags$span('This is because the test statistic \\(z\\) is the', strong('z-score'), 
                   'reporting how many standard deviations \\(\\hat{p}\\) is above or below \\(p_0\\).',
                   'This follows that the sampling distribution of the sample proportion \\(\\hat{p}\\) is normal 
                    with mean \\(p_0\\) and standard deviation \\(\\sqrt{\\frac{p_0(1-p_0)}{n}}\\)
                   (provided the normal assumption is satisfied and we assume Ho is true).')),
            style='font-size: x-small;')
        ), style="margin-right: 0px;"), 
        style="margin: 0px -20px;"),
        style="margin-top: -10px;"),
    style="padding-bottom: 0px;"),
    fluidRow(column(3, wellPanel(
      sliderInput('phat', label=withMathJax('Sample proportion \\(\\hat{p}\\)'), 
                  min=.01, max=.99, value=.51, step=.001), 
      br(),
      sliderInput('p0', label=withMathJax('Population proportion under \\(H_0\\) \\((p_0)\\)'), 
                  min=.01, max=.99, value=.5, step=.001),
      br(),
      sliderInput('n', label=withMathJax('Sample size \\(n\\)'), 
        min=2, max=1000, value=100, step=1)
    )), 
    column(3, wellPanel(
      h4(strong('Normal conditions')),
      "Both must be true:", br(),
      tags$ul(
        tags$li(withMathJax('\\(np_0\\geq 10\\)')),
        tags$li(withMathJax('\\(n(1-p_0)\\geq 10\\)'))
      ),
      hr(),
      "Checking conditions:",
      tags$ul(
        tags$li(
          withMathJax('\\(np_0 = \\)'), uiOutput('n_succ', inline=T)
        ), 
        tags$li(
          withMathJax('\\(n(1-p_0) = \\)'), uiOutput('n_fail', inline=T)
        ), 
        tags$li(
          'Both?', uiOutput('norm_cond', inline=T)
        )
      )
    )),
    column(6, wellPanel(
    conditionalPanel('output.norm_tf == true', 
      h4(strong('Parts of formula')),
      tags$ul(
        tags$li(withMathJax('\\(\\hat{p} - p_0 = \\)'), textOutput('znum', inline=T)), 
        tags$li(withMathJax('Denominator ='), textOutput('zden', inline=T))
      ),
      hr(),
      h4(strong('Graph of Test statistic')),
      plotOutput('tstat', height="100px")
    ),
    conditionalPanel('output.norm_tf == false',
      h3(strong('Normal conditions not met!')),
      'It\'s not appropriate to use this formula to calculate the test statistic.'         
    )
    ))
    ))
  })
  
  output$tstat <- renderPlot({
    z <- (input$phat - input$p0) / sqrt(input$p0 * (1-input$p0) / input$n)
    par(cex.main=1.5, cex.axis=1.2, mar=c(2.1, .1, 2, .1))
    plot(NA, NA, xlim=c(-5, 5), ylim=c(-.05, .05), xaxt='n', yaxt='n', xlab='', 
         ylab='')
      abline(v=-10:10/2, lty=3, col=grey(.5))
      axis(side=1, at=-5:5)
      title(paste('z =', round(z,2)))
      if(z > 0) arrows(0,0,x1=z, col=4, lwd=3, length=.08)
      if(z < 0) arrows(0,0,x1=z, col=3, lwd=3, length=.08)
      if(z>5.4) text(4.5, .02, 'Off-screen', col=2)
      if(z< -5.4) text(-4.5, .02, 'Off-screen', col=2)
  })
  output$znum <- renderText({input$phat - input$p0})
  output$zden <- renderText({
    round(sqrt(input$p0 * (1-input$p0) / input$n), 4)
  })
  output$n_succ <- renderUI({
    succ <- input$n * input$p0
    HTML(paste(round(succ, 2), 
               ifelse(succ >= 10,
                      '&ge; 10 <span style="color:green">&#10004;</span>', 
                      '< 10 <span style="color:red">&#10008;</span>')))
  })
  output$n_fail <- renderUI({
    fail <- input$n * (1-input$p0)
    HTML(paste(round(fail, 2), 
               ifelse(fail >= 10,
                      '&ge; 10 <span style="color:green">&#10004;</span>', 
                      '<10 <span style="color:red">&#10008;</span>')))
  })
  output$norm_cond <- renderUI({
    succ <- input$n * input$p0
    fail <- input$n * (1-input$p0)
    HTML(ifelse(succ >=10 & fail >= 10,
                '<span style="color:green">&#10004;</span>', 
                '<span style="color:red">&#10008;</span>'))
  })
  output$norm_tf <- reactive({
      succ <- input$n * input$p0
      fail <- input$n * (1-input$p0)
      succ >=10 & fail >= 10
    })
  outputOptions(output, "norm_tf", suspendWhenHidden = FALSE)
}