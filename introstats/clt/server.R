library(shiny)

shinyServer(function(input, output) {
  # output$info <- renderUI({
  #   fluidPage(
  #     withMathJax(),
  #     'The instructions in the boxes below correspond to the panels directly below them.',
  #   fluidRow(
  #     column(6, wellPanel(
  #       tags$div(tags$ul(
  #                 tags$li('Use the slider to change the shape of the population distribution: specifically, 
  #                the direction and amount of skewness.  Recall that the direction of skewness 
  #                denotes the side of the distribution (right or left, according to the number line)
  #                that is more spread out from the median to the minimum/maximum.'),
  #                 tags$li('A boxplot, histogram,
  #                and density plot of the current population distribution is shown.  
  #                Note that the density plot essentially traces the tops of the histogram bars.  (All 
  #                population distributions have mean \\(\\mu = 15\\) and standard deviation 
  #                \\(\\sigma = 5\\).  The symmetric distribution is a normal distribution.)')
  #                 ),
  #                style='font-size: x-small;')
  #     )),
  #     column(6, wellPanel(
  #       tags$div(tags$ul(
  #                 tags$li('The sampling distribution of the sample mean \\(\\bar{x}\\) is the 
  #                distribution of sample means that results from taking repeated samples of size \\(n\\) from the 
  #                population (and taking the mean of each sample).  The slider changes the value of 
  #                \\(n\\).  You can animate the increasing of \\(n\\) by clicking on the \"play\" button 
  #                at the bottom right of the slider.  (Clicking the \"pause\" button stops this animation.)'
  #                 ), 
  #                 tags$li('The plot below compares the sampling distribution of \\(\\bar{x}\\) for the current 
  #                population and value of \\(n\\) to a normal distribution that has the same mean 
  #                and standard deviation.  This illustrates ', strong('the Central Limit Theorem:'), 
  #                'as \\(n\\) increases, the sampling distribution of \\(\\bar{x}\\) approaches a 
  #                normal distribution.  Note that the more skewed the population, the larger the \\(n\\)
  #                necessary to make the sampling distribution of \\(\\bar{x}\\) close to normal (and 
  #                that if the population is normal, the sampling distribution of \\(\\bar{x}\\) is 
  #                normal for all values of \\(n\\)).'),
  #                tags$li('Because the standard deviations of the distributions shown decrease as \\(n\\) 
  #                increases (i.e. their standard deviations \\(= \\sigma/\\sqrt{n} = 5/\\sqrt{n}\\)), 
  #                the checkbox below the plot sets the x-scale of the plot equal to the mean 
  #                plus or minus 3 standard deviations (i.e. \\(15 \\pm 3(\\frac{5}{\\sqrt{n}})\\)).  
  #                This makes it easier to compare the distributions across \\(n\\) values.')
  #                 ),
  #                style='font-size: x-small;')
  #     ))
  #     , style="margin: 0px -20px;")
  # )})
  pops <- c('Extremely skewed left', 'Very skewed left', 
            'Moderately skewed left', 'Slightly skewed left', 
            'Symmetric', 'Slightly skewed right', 
            'Moderately skewed right', 'Very skewed right', 
            'Extremely skewed right')
  output$pop_slider_ui <- renderUI({
    fluidPage(
      strong("Shape of Population"),
    tags$div(sliderTextInput('pop', label=NULL, 
                    choices=pops, grid=T, hide_min_max = T), 
             style='padding: 0px 50px;')
  )})
  var <- 25; mean <- 15
  lim <- mean + c(-3, 3)*sqrt(var)
  alpha <- c(.7, 1.2, 2, 5) #Alpha parameters for gamma dist
  pop <- reactive({
    req(input$pop)
    for(i in 1:9) if(input$pop == pops[i]) return(10-i)
  })
  x <- seq(from=lim[1], to=lim[2], by=.5)
  box.quant <- c(.025, .25, .5, .75, .975)
  help.pop.plots <- reactive({
    if(pop() == 5){ #normal pop
      num5 <- qnorm(box.quant, mean=mean, sd=sqrt(var))
      y <- dnorm(x, mean=mean, sd=sqrt(var))
    }
    else{ #skewed pop
      alpha1 <- ifelse(pop() <=4, alpha[pop()], alpha[10-pop()])
      beta <- sqrt(var/alpha1)
      add <- mean - sqrt(alpha1*var)
      if(pop() <=4) arg1 <- x-add
      else arg1 <- lim[2] - add - x
      if(pop() <=4) num5 <- qgamma(box.quant, shape=alpha1, scale=beta) + add
      else num5 <- 30 - add - qgamma(box.quant, shape=alpha1, scale=beta)
      y <- dgamma(arg1, shape=alpha1, scale=beta)
    }
    list(num5=num5, y=y)
  })
  output$pop.boxplot <- renderPlot({
    par(mar=c(2.1, .1, .1, .1))
    boxplot(help.pop.plots()$num5, range=0, horizontal=T, ylim=lim, col=grey(.75), 
            whisklty=1)
  })
  output$pop.histogram <- renderPlot({
    par(mar=c(2.1, .1, .1, .1))
    barplot(help.pop.plots()$y, space=0, yaxt='n', col=grey(.75))
    axis(side=1, at=0:6*10, label=0:6*5)
    box()
  })
  output$pop.dens.plot <- renderPlot({
    par(mar=c(2.1, .1, .1, .1))
    plot(x, help.pop.plots()$y, type='l', yaxt='n')
  })
  output$pop.boxplot2 <- renderPlot({
    par(mar=c(2.1, .1, .1, .1))
    boxplot(help.pop.plots()$num5, range=0, horizontal=T, ylim=lim, col=grey(.75), 
            whisklty=1)
  })
  output$pop.histogram2 <- renderPlot({
    par(mar=c(2.1, .1, .1, .1))
    barplot(help.pop.plots()$y, space=0, yaxt='n', col=grey(.75))
    axis(side=1, at=0:6*10, label=0:6*5)
    box()
  })
  output$pop.dens.plot2 <- renderPlot({
    par(mar=c(2.1, .1, .1, .1))
    popy <- help.pop.plots()$y; xbary <- help.xbar.plots()$y
    ymax <- max(c(popy, xbary))
    max.popy <- max(popy); max.xbary <- max(xbary)
    if(max.popy > max.xbary) xbary <- max.popy / max.xbary * xbary
    else
      popy <- max.xbary / max.popy * popy
    ylim <- c(0, ymax*1.3)
    matplot(x, cbind(popy, xbary), type='l', ylim=ylim,
            lty=1, col=c(2,4), yaxt='n')
    leg.txt <- c('Population', 'Sample means')
    legend('topright', leg.txt, lty=1, col=c(2,4))
  })
  help.xbar.plots <- reactive({
    if(pop() == 5){ #normal pop
      num5 <- qnorm(box.quant, mean=mean, sd=sqrt(var/input$n))
      y <- dnorm(x, mean=mean, sd=sqrt(var/input$n))
    }
    else{
      alpha1 <- ifelse(pop() <=4, alpha[pop()], alpha[10-pop()])
      beta <- sqrt(var/alpha1)
      add <- mean - sqrt(alpha1*var)
      if(pop() <=4){ 
        arg1 <- x-add
        num5 <- qgamma(box.quant, shape=alpha1*input$n, scale=beta/input$n) + add
      }
      else{
        arg1 <- lim[2] - add - x
        num5 <- 30 - add - qgamma(box.quant, shape=alpha1*input$n, scale=beta/input$n)
      } 
      y <- dgamma(arg1, shape=alpha1*input$n, scale=beta/input$n)
    }
    list(num5=num5, y=y)
  })
  output$xbar.boxplot <- renderPlot({
    par(mar=c(2.1, .1, .1, .1))
    boxplot(help.xbar.plots()$num5, range=0, horizontal=T, ylim=lim, col=grey(.75), 
            whisklty=1)
  })
  output$xbar.histogram <- renderPlot({
    par(mar=c(2.1, .1, .1, .1))
    barplot(help.xbar.plots()$y, space=0, yaxt='n', col=grey(.75))
    axis(side=1, at=0:6*10, label=0:6*5)
    box()
  })
  output$xbar.plot <- renderPlot({
    x <- seq(from=lim[1], to=lim[2], length=200)
    par(mar=c(2, .1, .1, .1))
    if(pop() == 5){ #normal pop
      y <- dnorm(x, mean=mean, sd=sqrt(var/input$n))
      y.norm <- dnorm(x, mean=mean, sd=sqrt(var / input$n))
      plot(x, y, type='l', yaxt='n')
      lines(x, y.norm, type='l', col=2)
    }
    else{
      alpha1 <- ifelse(pop() <=4, alpha[pop()], alpha[10-pop()])
      beta <- sqrt(var/alpha1)
      add <- mean - sqrt(alpha1*var)
      if(pop() <=4){ 
        arg1 <- x-add
      }
      else{
        arg1 <- lim[2] - add - x
      } 
      y <- dgamma(arg1, shape=alpha1*input$n, scale=beta/input$n)
      plot(x, y, type='l', yaxt='n')
      y.norm <- dnorm(x, mean=mean, sd=sqrt(var / input$n))
      lines(x, y.norm, type='l', col=2)
    }
    leg.txt <- c(expression(paste('Samp. dist. of  ', bar(x))), 'Normal dist.')
    legend('topright', leg.txt, col=1:2, lwd=1)
  })
  output$compare.normal2 <- renderPlot({
    xlim <- round(mean + c(-3, 3)*sqrt(var/input$n), 2)
    x <- seq(from=xlim[1], to=xlim[2], length=100)
    if(pop() == 5){ #normal pop
      y <- dnorm(x, mean=mean, sd=sqrt(var/input$n))
    }
    else{
      alpha1 <- ifelse(pop() <= 4, alpha[pop()], alpha[10-pop()])
      beta <- sqrt(var/alpha1)
      add <- mean - sqrt(alpha1*var)
      if(pop() <=4){ 
        arg1 <- x-add
      }
      else{
        arg1 <- lim[2] - add - x
      } 
      y <- dgamma(arg1, shape=alpha1*input$n, scale=beta/input$n)
    }
    norm.y <- dnorm(x, mean=mean, sd=sqrt(var/input$n))
    par(mar=c(2, .1, .1, .1))
    matplot(x, cbind(y, norm.y), type='l', lty=1, yaxt='n')
    leg.txt <- c(expression(paste('Samp. dist. of  ', bar(x))), 'Normal dist.')
    legend('topright', leg.txt, col=1:2, lwd=1)
  })
  output$samp.sd <- renderText({
    format(5/sqrt(input$n), digits=3)
  })
})
