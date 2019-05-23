library(shiny)

shinyServer(function(input, output, session) {
  output$info <- renderUI({fluidPage(
   'The boxes below correspond to the panels directly below them in the app.',
   fluidRow(
     column(4, wellPanel(
       tags$div('Use the sliders to change the different variables in the formula:
                   \\(\\bar{x}, s, n\\), and the confidence level (which affects \\(t^\\ast\\).
                (Helpful hint: Use the keyboard arrows after clicking on a slider for 
                finer control.)', 
                style='font-size: x-small')
      ), 
      wellPanel(
        tags$div('Note: In order to calculate the confidence interval for \\(\\mu\\) for any
                 \\(n\\) using this formula, we must assume that the population is normally
                 distributed.', 
                 style='font-size: x-small')
      )
     ),
     column(4, wellPanel(
       tags$div('This panel shows the different quantities that go into the confidence interval 
                calculation:',
                   tags$ul(
                     tags$li('The confidence level multiplier \\(t^\\ast\\). This is determined 
                from the confidence level C so there is C% of the area under 
                the \\(t\\) distribution with \\(n-1\\) degrees of freedom between
                \\(-t^\\ast\\) and \\(+t^\\ast\\), as shown in the plot.'),
                     tags$li('The standard error \\(se = \\frac{s}{\\sqrt{n}}\\).'),
                     tags$li('The margin of error \\(me = t^\\ast (se) \\).')
                   ),
                   'Note that the confidence interval formula simplifies to \\(\\bar{x} \\pm me\\)
                so',
                   tags$ul(
                     tags$li('The center of the CI is determined by \\(\\bar{x}\\)'),
                     tags$li('The width of the CI is determined by the margin of error.  
                (Specifically, the width = upper endpoint - lower endpoint = 
                    twice the margin of error.)')
                   ),
                style='font-size: x-small')
     )),
     column(4, wellPanel(
       tags$div('This panel reports the lower and upper endpoints of the interval and provides 
                   a picture of the interval as well. (The dot shows the center of the interval.)
                   You can change the range shown in this picture using the slider underneath.  
                   However, if the center of the confidence interval is ever outside the range
                that you provide, the range will revert to 0 to 100.', 
                style='font-size: x-small')
     ))
     , style="margin: 0px -20px;")
  )})
  results <- reactive({
    tstar <- qt((1-input$C/100)/2, df=input$n-1, lower.tail=F)
    se <- input$s/sqrt(input$n)
    me <- tstar * se
    ci <- input$xbar + c(-1, 1)*me
    list(tstar=tstar, se=se, me=me, ci=ci)
  })
  output$tstar <- renderText({
    sprintf('%.3f', results()$tstar)
  })
  output$tstar_plot <- renderPlot({
    df <- input$n - 1
    par(mar=c(2,.1,.1,.1))
    t <- seq(from=-4, to=4, by=.01)
    plot(t, dt(t, df=df), type='l', xaxt='n', yaxt='n', lwd=2, ylim=c(0, dnorm(0)))
    abline(h=0)
    tstar3 <- round(results()$tstar, 3)
    tstar2 <- floor(100*tstar3)/100
    segments(x0=tstar3*c(-1,1), y0=0, y1=dt(tstar3, df=df))
    polygon(x=c(-tstar3, -tstar3, seq(from=-tstar2, to=tstar2, by=.01), tstar3, tstar3), 
            y=c(0, dt(-tstar3, df=df), dt(seq(from=-tstar2, to=tstar2, by=.01), df=df), 
                dt(tstar3, df=df), 0), 
            col='skyblue')
    if(results()$tstar < 4.2)
      axis(side=1, at=c(-tstar3, 0, tstar3), labels=c(-tstar3, '0', tstar3))
    else{
      axis(side=1, at=0)
      axis(side=1, at=c(-3.4, 3.4), labels=c(paste0('<-- -', tstar3), paste0(tstar3, ' -->')), 
           tick=F, col=2)
    }
    text(0, .5*dnorm(0), paste0(input$C,'%'))
  })
  output$se <- renderText({
    sprintf('%.3f', results()$se)
  })
  output$me <- renderText({
    sprintf('%.2f', results()$me)
  })
  output$conf <- renderText({
    paste0(input$C, '%')
  })
  output$ci <- renderText({
    paste0('(', toString(sprintf('%.2f', results()$ci)), ')')
  })
  output$plot <- renderPlot({
    par(mar=c(2.1, .1, .1, .1))
    plot(results()$ci, c(0,0), xlim=input$range, ylim=c(-.05, .05), 
         pch=c('[', ']'), cex=3, yaxt='n', xlab='', ylab='')
    grid(ny=NA)
    points(input$xbar, 0, pch=16, cex=2, col=4)
    segments(x0=results()$ci[1], x1=results()$ci[2], y0=0, lwd=3, col=4)
    if(results()$ci[1] < input$range[1] - .03*diff(input$range)) 
      text(input$range[1] + .1*diff(input$range), .04, '<- Lower limit', col=2)
    if(results()$ci[2] > input$range[2] + .03*diff(input$range)) 
      text(input$range[2] - .1*diff(input$range), .04, 'Upper limit ->', col=2)
  })
  observe({
    if(input$xbar < input$range[1] | input$xbar > input$range[2])
      updateSliderInput(session, 'range', value=c(0, 100))
  })
  
})
