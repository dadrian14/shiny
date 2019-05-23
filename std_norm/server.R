library(shiny)

shinyServer(function(input, output, session) {
  output$info <- renderUI({
    no1.txt <- 'Is it a \"Forward\" or \"Backward\" problem?  As suggested in the diagram 
                           at the top, a forward problem takes a range of \\(z\\) -- where \\(z\\) 
                           denotes values of the standard normal distribution -- and finds a 
                           probability.  Conversely, a backward problem takes a probability and 
                           finds a range of \\(z\\). (The descriptions of 2 and 3 below change depending 
                           on whether forward or backward is selected in the app.)'
    fluidPage(
      'This app concerns the standard normal distribution, the normal distribution with 
                 mean 0 and standard deviation 1.  In the app, \\(z\\) denotes values of the standard 
      normal distribution, which also stand for the number of standard deviations above 
      or below the mean.  The panels below correspond to the panels in the app directly below them.',
    fluidRow(
      
      column(5, wellPanel(
        tags$div('The options in this panel specify the type of problem that is answered',
                 tags$ol(
                   conditionalPanel('input.problem_type=="1"', 
                          tags$li(no1.txt),
                          tags$li('What type of range of z are we taking: a range between z values, 
                                  the range greater than a z value, or the range less than a z value?'), 
                          tags$li('What is/are the value(s) of z that defines the range?')
                   ), 
                   conditionalPanel('input.problem_type=="2"', 
                          tags$li(no1.txt),
                          tags$li('For what type of range of z are we trying to find the probability: the 
                                  range between -z and +z (a certain number of z units away from 0), 
                                  the range greater than z, or the range less than z?'), 
                          tags$li('For which probability should we find this range of z?')
                                    ), 
                   'Finally, a summary of the problem is given at the bottom.'
                 ),
                 style='font-size: x-small;')
      )),
      column(7, wellPanel(
        tags$div('This panel gives a picture of the problem.  Note that the probability 
                 is the shaded area under the normal curve (out of the total area of 1) 
                 and z values correspond to values on the x-axis.  Last, the answer to the 
                 problem (either z value or probability) is given at the bottom.', 
                 style='font-size: x-small;')
      ))
      , style="margin: 0px -20px;") 
  )})
  observeEvent(input$range_type, {
    if(input$range_type==2)
      updateSliderInput(session, 'z', label='3. Greater than which z?')
    if(input$range_type==3)
      updateSliderInput(session, 'z', label='3. Less than which z?')
  })
  output$summary_prob <- renderUI({
    if(input$problem_type==1){
      if(input$range_type==1)
        return(paste0('What is the probability that z is between ', input$zrange[1], ' and ', 
                     input$zrange[2], '?'))
      if(input$range_type==2)
        return(paste0('What is the probability that z is greater than ',
                      input$z, '?'))
      if(input$range_type==3)
        return(paste0('What is the probability that z is less than ', 
                      input$z, '?'))
    }
    if(input$problem_type==2){
      if(input$prob_type==1)
        return(paste0('What is the value of z so the probability of being between -z and z is ',
                      input$prob, '?'))
      if(input$prob_type==2)
        return(paste0('What is the value of z so the probability of being greater than z is ', 
                      input$prob, '?'))
      if(input$prob_type==3)
        return(paste0('What is the value of z so the probability of being less than z is ', 
                      input$prob, '?'))
    }
  })
  output$prob.text <- renderText({
    if(input$problem_type==1){
      if(input$range_type==1) #between two z's
        prob <- pnorm(input$zrange[2]) - pnorm(input$zrange[1])
      if(input$range_type==2) #greater than z
        prob <- pnorm(input$z, lower.tail=F)
      if(input$range_type==3) #less than z
        prob <- pnorm(input$z)
    return(ifelse(prob >= .00005, sprintf("%.4f", prob), '< 0.0001'))
    }
  })
  output$z.txt <- renderText({
    if(input$problem_type==2 & input$prob_type==1) #between two z's
      return(round(qnorm(input$prob+(1-input$prob)/2), 2))
    if(input$problem_type==2 & input$prob_type==2) #greater than z
      return(round(qnorm(1-input$prob), 2))
    if(input$problem_type==2 & input$prob_type==3) #less than z
      return(round(qnorm(input$prob), 2))
  })
  output$plot <- renderPlot({
    x <- seq(-4, 4, by=.01)
    par(mar=c(2.1, .1, .1, .1), cex.main=1.7, cex.lab=1.5, cex.axis=1.3)
    plot(x, dnorm(x), type='l', ylab='', yaxt='n', xlab='', 
         main="", xaxt='n')
    axis(side=1, at=-4:4, labels=F)
    abline(h=0)
    if(input$problem_type==1)
      text(x=-3, y=.7*dnorm(0), 'What is the shaded\narea (probability)?', cex=1.3, col=2)
    if(input$problem_type==2){
      text(x=-3, y=.7*dnorm(0), 'What is the\n value of z?', cex=1.3, col=2)
      text(x=2.8, y=.7*dnorm(0), paste('Shaded area =', input$prob), cex=1.3)
    }
    if(input$problem_type==1 & input$range_type==1){ #between two z's
      axis(side=1, at=input$zrange)
      segments(x0=input$zrange, y0=0, y1=dnorm(input$zrange), col=4)
      zseq <- seq(from=input$zrange[1], to=input$zrange[2], by=.01)
      polygon(c(zseq, rev(zseq)), y=c(dnorm(zseq), rep(0, length(zseq))),
              col='lightsteelblue')
    }
    if(input$problem_type==1 & input$range_type==2){ #greater than z
      axis(side=1, at=input$z)
      segments(x0=input$z, y0=0, y1=dnorm(input$z), col=4)
      zseq <- seq(from=input$z, to=4, by=.01)
      polygon(c(zseq, rev(zseq)), y=c(dnorm(zseq), rep(0, length(zseq))),
              col='lightsteelblue')
    }
    if(input$problem_type==1 & input$range_type==3){ #less than z
      axis(side=1, at=input$z)
      segments(x0=input$z, y0=0, y1=dnorm(input$z), col=4)
      zseq <- seq(from=-4, to=input$z, by=.01)
      polygon(c(zseq, rev(zseq)), y=c(dnorm(zseq), rep(0, length(zseq))),
              col='lightsteelblue')
    }
    if(input$problem_type==2 & input$prob_type==1){ #between -z and +z
      plus.z <- round(qnorm(input$prob+(1-input$prob)/2), 2)
      zs <- c(-plus.z, plus.z)
      axis(side=1, at=zs, label=c('-z', '+z'))
      segments(x0=zs, y0=0, y1=dnorm(zs), col=4)
      zseq <- seq(from=zs[1], to=zs[2], by=.01)
      polygon(c(zseq, rev(zseq)), y=c(dnorm(zseq), rep(0, length(zseq))),
              col='lightsteelblue')
    }
    if(input$problem_type==2 & input$prob_type==2){ #greater than z
      z <- round(qnorm(1-input$prob), 2)
      axis(side=1, at=z, label='z')
      segments(x0=z, y0=0, y1=dnorm(z), col=4)
      zseq <- seq(from=z, to=4, by=.01)
      polygon(c(zseq, rev(zseq)), y=c(dnorm(zseq), rep(0, length(zseq))),
              col='lightsteelblue')
    }
    if(input$problem_type==2 & input$prob_type==3){ #less than z
      z <- round(qnorm(input$prob), 2)
      axis(side=1, at=z, label='z')
      segments(x0=z, y0=0, y1=dnorm(z), col=4)
      zseq <- seq(from=-4, to=z, by=.01)
      polygon(c(zseq, rev(zseq)), y=c(dnorm(zseq), rep(0, length(zseq))),
              col='lightsteelblue')
    }
  })

})
