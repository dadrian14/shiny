library(shiny)

shinyServer(function(input, output) {
  output$info <- renderUI({fluidPage(
    'The boxes below describe the panels directly below them in the app.',
    fluidRow(
      column(4, wellPanel(
        tags$div('The degrees of freedom (df) of the \\(t\\) distribution can either be 
                 controlled with a slider or typing it in.  You can set the df to increase 
                 automatically by clicking on the \"play\" button in the lower right corner 
                 of the slider.  (Clicking the pause button will stop the animation.)', 
                 style='font-size: x-small;')
        ), wellPanel(
          tags$div('You can show a graphical interpretation of the values of \\(t^\\ast\\) and 
                   \\(z^\\ast\\) on the plot for a confidence level that you provide.  You can 
                   increase the confidence level automatically as well.',
                   style='font-size: x-small;')
        )
      ),
      column(8, wellPanel(
        tags$div('The \\(t\\) distribution is similar to the standard normal distribution because 
                 both are symmetric, unimodal, and centered around zero.  In fact, as the df 
                 increases, the \\(t\\) distribution approaches the standard normal distribution. 
                 For the confidence level \\(C\\), the value \\(t^\\ast\\) is such that the middle 
                 \\(C\\)% of the area under the \\(t\\) distribution is between \\(-t^\\ast\\) and 
                 \\(t^\\ast\\).  The value \\(z^\\ast\\) has a similar definition in regards to the 
                 standard normal distribution.  Note that:',
                 tags$ul(
                   tags$li('As the df increases, \\(t^\\ast\\) decreases and gets closer to 
                           \\(z^\\ast\\).'),
                   tags$li('As the confidence leve increases, both \\(z^\\ast\\) and \\(t^\\ast\\) 
                           increase.')
                 ),
                 style='font-size: x-small;')
      ))
      , style="margin: 0px -20px;") 
  )})
  df <- reactive({
    ifelse(input$df_input==1, input$slider.df, input$text.df)
  })
  output$plot <- renderPlot({
    par(mar=c(6, .1, .1, .1), cex.lab=1.5, cex.axis=1.3)
    if(is.na(df())){
      plot(x=0,y=0,xaxt='n', xlab='', yaxt='n', type='n')
      text(0,0,'Enter df', col=2, cex=3)
    }
    else if(df()<0){
      plot(x=0,y=0,xaxt='n', xlab='', yaxt='n', type='n')
      text(0,0,'Required: df > 0', col=2, cex=3)
    }
    else{
      x <- seq(-4, 4, by=.1)
      plot(x, dnorm(x), type='l', ylab='', lwd=1, xlab='', yaxt='n', xaxt='n')
      axis(side=1, at=-4:4)
      lines(x, dt(x, df=df()), col=2, lwd=1)
      legend('topleft', c("Standard Normal Dist", paste("t Dist with df =", df())),
             col=c(1,2), lty=1, lwd=1, cex=1.2)
      abline(h=0)
      if(input$show_mult==TRUE){
        zstar <- qnorm((1-input$C/100)/2, lower.tail=F)
        axis(side=1, at=c(-1, 1)*zstar, label=c('-z*', paste0('z* = ', round(zstar, 3))), line=2)
        segments(x0=zstar, y0=0, y1=dnorm(zstar), lwd=3)
        x.zstar <- seq(from=-zstar, to=zstar, length=100)
        polygon(x=c(-zstar, x.zstar, zstar, -zstar), 
                y=c(0, dnorm(x.zstar), 0, 0), density=10, angle=45)
        tstar <- qt((1-input$C/100)/2, df(), lower.tail=F)
        if(tstar < 3.9){
          axis(side=1, at=c(-1, 1)*tstar, 
                             label=c('-t*', paste0('t* = ', round(tstar, 3))), 
                             col=2, line=4, col.axis=2)
          x.tstar <- seq(from=-tstar, to=tstar, length=100)
          polygon(x=c(-tstar, x.tstar, tstar, -tstar), 
                  y=c(0, dt(x.tstar, df=df()), 0, 0), 
                  density=5, angle=-45, col=2)
          segments(x0=tstar, y0=0, y1=dt(tstar, df=df()), lwd=3, col=2)
        }
        else{
          axis(side=1, at=c(-1, 1)*3.5, tick=F,
                  label=c('<- -t*', paste0('t* = ', round(tstar, 3), ' ->')), 
                  col=2, line=4, col.axis=2)
          x.tstar <- seq(from=-4, to=4, length=100)
          polygon(x=c(-4, x.tstar, 4, -4), 
                  y=c(0, dt(x.tstar, df=df()), 0, 0), 
                  density=5, angle=-45, col=2)
        }
        polygon(x=c(-.4, .35, .35, -.4, -.4), y=.16+.04*c(0,0,1,1,0), col='white')
        text(0, .18, paste0(input$C, '%'), cex=2)
      }
    }
  })
})
