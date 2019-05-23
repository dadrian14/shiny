tstat.ui <- function(){
  fluidPage(
  wellPanel(
    h3(strong('Test statistic:'), 
       '\\(t = \\frac{\\bar{x}-\\mu_0}{s/\\sqrt{n}}\\)'), 
    tags$div(checkboxInput('info_tstat', label='Click for more info!'), 
             style='margin-bottom: -10px'), 
    conditionalPanel('input.info_tstat == true', 
                     fluidRow(
                       column(4, wellPanel(
                         tags$div('Use the sliders to change the variables in the formula for 
                                  the test statistic. (Helpful hint: Using the arrow keys after 
                                  clicking on a slider allows for finer control.)', 
                                  style='font-size: x-small;')
                       )),
                       column(3, wellPanel(
                         tags$div('Observe what happens to the numerator and denominator of the test 
                                  statistic formula when you change \\(\\bar{x}, \\mu_0, s\\), 
                                  and \\(n\\) using the sliders.',
                                  style='font-size: x-small;')
                       )),
                       column(5, wellPanel(
                         tags$div('The definition of a test statistic is a number that summarizes the 
                                  difference between the observed sample and what we would expect to 
                                  observe if \\(Ho\\) is true. Certainly this difference is measured 
                                  by the numerator \\(\\bar{x}-\\mu_0\\), and dividing by the 
                                  denominator tells us whether this is a large or small difference. 
                                  This is because the test statistic \\(t\\) reports how many 
                                  standard errors \\(\\bar{x}\\) is above or below \\(\\mu_0\\) 
                                  (where the standard error is the estimated standard deviation of the 
                                  sampling distribution of \\(\\bar{x}\\)).', 
                                  style='font-size: x-small;')
                       ))
                       , style="margin: 0px -20px;") 
    , style='margin-bottom: -10px;')
  ),
  fluidRow(column(4, wellPanel(
    sliderInput('xbar', label='Sample mean \\(\\bar{x}\\)', 
                min=1, max=99, value=47, step=.1), 
    sliderInput('mu0', label='Population mean under \\(H_0\\) \\((\\mu_0)\\)', 
                min=1, max=99, value=46, step=.1),
    sliderInput('s', label='Sample standard deviation \\(s\\)', 
                min=1, max=50, value=10, step=.1),
    sliderInput('n', label='Sample size \\(n\\)', min=2, max=200, 
                value=20, step=1)
  )), 
  column(3, wellPanel(
    h4(strong('Result')),
    uiOutput('tres')
  )),
  column(5, wellPanel(
    h4(strong('Graph of test statistic')),
    plotOutput('tstat', height="100px")
  ))
    
  )
)}

tstat.serv <- function(input, output){
  output$tres <- renderUI({
    num <- input$xbar - input$mu0
    den <- input$s / sqrt(input$n)
    t <- num / den
    fluidPage(
      withMathJax(
        paste0('$$ t = \\frac{', round(num, 1), '}{', round(den, 3), '} = ', round(t, 2), '$$')
      )
    )
  })
  output$tstat <- renderPlot({
    t <- (input$xbar - input$mu0) / (input$s / sqrt(input$n))
    par(cex.main=1.4, cex.axis=1, mar=c(2.1, .1, 2.1, .1))
    plot(NA, NA, xlim=c(-5, 5), ylim=c(-.05, .05), xaxt='n', yaxt='n', xlab='', 
         ylab='')
    abline(v=-10:10/2, lty=3, col=grey(.5))
    axis(side=1, at=-5:5)
      title(paste('t =', round(t,2)))
      if(t > 0) arrows(0,0,x1=t, col=4, lwd=3, length=.09)
      if(t < 0) arrows(0,0,x1=t, col=3, lwd=3, length=.09)
      if(t>5.4) text(4.8, .02, 'Off-screen', col=2)
      if(t< -5.4) text(-4.8, .02, 'Off-screen', col=2)
    
  })
  output$znum <- renderText({input$xbar - input$mu0})
  output$zden <- renderText({
    sprintf('%.3f', input$s / sqrt(input$n))
  })
}