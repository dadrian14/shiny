library(shiny)
ui <- fluidPage(
  fluidRow(wellPanel(
    h3(strong('t Distribution vs. Standard Normal Distribution for 
              Different df')), 
    h4(strong('(and t* vs. z*)'))
  )),
  fluidRow(
    column(4, wellPanel(
      p('Move the slider to change the number of degrees of freedom (df) of the 
        t distribution.'),
      sliderInput(inputId='df', label=h4('Degrees of Freedom (df)'), 
              min=1, max=30, value=3, step=1),
      hr(),
    checkboxInput('show_mult', label='Show the values of t* and z* on the plot to the right'), 
    conditionalPanel('input.show_mult', 
      sliderInput(inputId='C', label=h4('For which confidence level?'), min=80, max=99, 
                  value=95)
    )
    )),
    column(8, wellPanel(
      plotOutput(outputId='plot'), br(),
      'Note: As the df increases, the t distribution gets closer 
      to the standard normal distribution.',
      conditionalPanel('input.show_mult', br(),'(And the value of t* gets closer to the 
                       value of z*.)')
    ))
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    par(mar=c(6, .5, .5, .5), cex.lab=1.5, cex.axis=1.3)
    x <- seq(-4, 4, by=.1)
    plot(x, dnorm(x), type='l', ylab='', lwd=2, xlab='', yaxt='n', xaxt='n')
    axis(side=1, at=-4:4)
    lines(x, dt(x, df=input$df), col=2, lwd=2)
    legend('topleft', c("Standard Normal Dist", paste("t Dist with df =", input$df)),
           col=c(1,2), lty=1, lwd=2)
    abline(h=0)
    if(input$show_mult==TRUE){
      text(0, .18, paste0(input$C, '%'), cex=1.5)
      zstar <- qnorm((1-input$C/100)/2, lower.tail=F)
      axis(side=1, at=c(-1, 1)*zstar, label=c('-z*', paste0('z* = ', round(zstar, 3))), line=2)
      segments(x0=c(-zstar, zstar), y0=0, y1=dnorm(zstar))
      tstar <- qt((1-input$C/100)/2, input$df, lower.tail=F)
      if(tstar < 3.9) axis(side=1, at=c(-1, 1)*tstar, 
                           label=c('-t*', paste0('t* = ', round(tstar, 3))), 
                           col=2, line=4, col.axis=2)
      else axis(side=1, at=c(-1, 1)*3.5, tick=F,
                label=c('<- -t*', paste0('t* = ', round(tstar, 3), ' ->')), 
                col=2, line=4, col.axis=2)
      segments(x0=c(-tstar, tstar), y0=0, y1=dt(tstar, df=input$df), col=2)
    }
  })
}

shinyApp(ui = ui, server = server)