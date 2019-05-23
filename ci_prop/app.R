library(shiny)
ui <- fluidPage(
  includeCSS('styles.css'),
  withMathJax(),
  wellPanel(
    h3(strong('Confidence interval for the population proportion (\\(p\\))')),
    h4('Formula: \\(\\hat{p} \\pm z^\\ast\\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}\\)'),
    checkboxInput('description', label='Click to show more info!'),
    conditionalPanel("input.description == true", 
                     'The boxes below correspond to the panels directly below them.',
      fluidRow(
        column(4, wellPanel(
          tags$div('Use the sliders to change the different variables in the formula:
                   \\(\\hat{p}, n\\), and the confidence level (which affects \\(z^\\ast\\).
                   (Helpful hint: Use the keyboard arrows after clicking on a slider for 
                   finer control.)',
            style='font-size: x-small;')
          ), 
          wellPanel(
            tags$div('Using this formula to calculate the confidence interval depends on 
                     the assumption that the sampling distribution of the sample proportion 
                     is normally distributed.  The conditions for making this assumption are 
                     that the number of observed \"successes\" \\(n\\hat{p}\\) and the number 
                     of observed \"failures\" \\(n(1-\\hat{p})\\) are both greater than or 
                     equal to 10.  The app checks these conditions using the current values 
                     of \\(n\\) and \\(\\hat{p}\\) on the sliders and will not display the 
                     results of the formula is they are not met.', 
                     style='font-size: x-small;')
          )
        ),
        column(4, wellPanel(
          tags$div('This panel shows the different quantities that go into the confidence interval 
                   calculation:',
                   tags$ul(
                     tags$li('The confidence level multiplier \\(z^\\ast\\). This is determined 
                             from the confidence level C so there is C% of the area under 
                             the standard normal distribution between \\(-z^\\ast\\) and 
                             \\(+z^\\ast\\), as shown in the plot.'),
                     tags$li('The standard error \\(se = \\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}\\).
                             '),
                     tags$li('The margin of error \\(me = z^\\ast (se) \\).')
                   ),
                   'Note that the confidence interval formula simplifies to \\(\\hat{p} \\pm me\\)
                   so',
                   tags$ul(
                     tags$li('The center of the CI is determined by \\(\\hat{p}\\)'),
                     tags$li('The width of the CI is determined by the margin of error.  
                             (Specifically, the width = upper endpoint - lower endpoint = 
                             twice the margin of error.)')
                   ),
                   style='font-size:x-small;')
        )),
        column(4, wellPanel(
          tags$div('This panel reports the lower and upper endpoints of the interval and provides 
                   a picture of the interval as well. (The dot shows the center of the interval.)
                   You can change the range shown in this picture using the slider underneath.  
                   However, if the confidence interval is ever outside the range that you provide,
                   the range will revert to 0 to 1.',
                   style='font-size:x-small;')
        ))
      , style="margin: 0px -20px;")               
    )
  , style='padding-bottom: -0px;'),
  fluidRow(
  column(4, 
   wellPanel(
    sliderInput('phat', label='Sample proportion \\(\\hat{p}\\)', 
                min=.01, max=.99, value=.5, step=.001),
    sliderInput('n', label='Sample size \\(n\\)', min=2, max=1000, value=100, step=1),
    sliderInput('C', label='Confidence level (%)', 
                min=80, max=99.8, value=95, step=.1, post='%')
   ), 
   wellPanel(
    h4(strong('Normal conditions')),
    tags$ul(
      tags$li('\"Successes\" \\(n\\hat{p}\\) = ', uiOutput('n_succ', inline=T)),
      tags$li('\"Failures\" \\(n(1-\\hat{p})\\) = ', uiOutput('n_fail', inline=T)),
      tags$li('Both?', uiOutput('norm_cond', inline=T))
    )
  )),
  conditionalPanel("output.norm_tf == true",
  column(4, 
   wellPanel(
    h4(strong('Parts of the result: z*, se, me')),
    '\\(z^\\ast = \\)', textOutput('zstar', inline=T),   
    plotOutput('zstar_plot', height='140px'),
    hr(),
    'Standard Error = \\(\\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}\\) = ',
    textOutput('se', inline=T),
    hr(),
    'Margin of Error = \\(z^\\ast (se)\\)  = ', textOutput('me', inline=T)
  )),
  column(4, wellPanel(
    h4('Confidence interval = ', textOutput('ci', inline=T)),
    plotOutput('plot', height="110px"),
    sliderInput('range', label='Range Shown', min=0, max=1, value=c(0,1), step=.01)
    
  ))
  ),
  conditionalPanel("output.norm_tf == false",
    column(8, wellPanel(
      h3(strong('Normal conditions not met!')),
      'It\'s not appropriate to use this formula to calculate the confidence interval.'
    ))                 
  )
))

server <- function(input, output, session) {
  res <- reactive({
    succ <- input$n * input$phat
    fail <- input$n * (1 - input$phat)
    zstar <- qnorm((1-input$C/100)/2, lower.tail=F)
    se <- sqrt(input$phat*(1-input$phat)/input$n)
    me <- zstar * se
    ci <- input$phat + c(-1, 1) * me
    list(succ=succ, fail=fail, zstar=zstar, se=se, me=me, ci=ci)
  })
  # displays successes or failures and makes green check marks or x's
    output$n_succ <- renderUI({
      HTML(paste(round(res()$succ, 2), 
               ifelse(res()$succ >= 10,
                      '&ge; 10 <span style="color:green">&#10004;</span>', 
                      '< 10 <span style="color:red">&#10008;</span>')))
    })
    output$n_fail <- renderUI({
      HTML(paste(round(res()$fail, 2), 
               ifelse(res()$fail >= 10,
                      '&ge; 10 <span style="color:green">&#10004;</span>', 
                      '<10 <span style="color:red">&#10008;</span>')))
    })
    output$norm_cond <- renderUI({
      HTML(ifelse(res()$succ >=10 & res()$fail >= 10,
                '<span style="color:green">&#10004;</span>', 
                '<span style="color:red">&#10008;</span>'))
    })
  #outputs T/F variable for whether normal conditions are met for use in the conditionalPanel
    output$norm_tf <- reactive({
      res()$succ >=10 & res()$fail >= 10
    })
    outputOptions(output, "norm_tf", suspendWhenHidden = FALSE)
  # Parts of the result
    output$zstar <- renderText({
      round(res()$zstar, 3)
    })
    output$zstar_plot <- renderPlot({
      par(mar=c(2,.1,.1,.1))
      z <- seq(from=-3.1, to=3.1, by=.01)
      plot(z, dnorm(z), type='l', xaxt='n', yaxt='n', lwd=2)
      zstar3 <- round(res()$zstar, 3)
      zstar2 <- floor(100*zstar3)/100
      segments(x0=zstar3*c(-1,1), y0=0, y1=dnorm(zstar3))
      polygon(x=c(-zstar3, -zstar3, seq(from=-zstar2, to=zstar2, by=.01), zstar3, zstar3), 
              y=c(0, dnorm(-zstar3), dnorm(seq(from=-zstar2, to=zstar2, by=.01)), dnorm(zstar3), 0), 
              col='skyblue')
      axis(side=1, at=c(-zstar3, 0, zstar3), labels=c(-zstar3, '0', zstar3))
      text(0, .5*dnorm(0), paste0(input$C,'%'))
    })
    output$se <- renderText({
      round(res()$se, 4)
    })
    output$me <- renderText({
      round(res()$me, 4)
    })
  #Text and plot of CI  
    output$ci <- renderText({
      paste0("(", round(res()$ci[1],3), ", ", round(res()$ci[2],3), ")")
    })
  output$plot <- renderPlot({
    par(mar=c(2.1, .1, .1, .1))
      plot(res()$ci, c(0,0), xlim=input$range, ylim=c(-.05, .05), 
           pch=c('[', ']'), cex=3, yaxt='n', xlab='Values of the population proportion', ylab='')
      segments(x0=res()$ci[1],x1=res()$ci[2],y0=0, col=4, lwd=3)
      points(x=input$phat, y=0, col=4, cex=2, pch=19)
      grid(NULL, NA)
  })
  observe({
    if(res()$ci[1] < input$range[1] | res()$ci[2] > input$range[2])
      updateSliderInput(session, 'range', value=c(0,1))
  })
}

shinyApp(ui = ui, server = server)

