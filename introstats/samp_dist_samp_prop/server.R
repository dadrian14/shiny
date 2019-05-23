library(shiny)
library(RColorBrewer)

shinyServer(function(input, output, session) {
  #Instructions at top
  output$info <- renderUI({
    fluidPage(
    'The following boxes correspond to their layout in the app below.',
    fluidRow(
      column(3, wellPanel(
        tags$div('Change the values of the population proportion \\(p\\) and the 
                 sample size \\(n\\) using the sliders.  (Helpful hint: Use the arrow 
                 keys after clicking on a sider for finer control.)', 
                 style='font-size: x-small;')
        ), 
        wellPanel(
          tags$div('This panel checks whether the conditions for assuming that the sampling 
                   distribution of the sample proportion is normally distributed.  It evaluates 
                   whether the number of expected \"successes\" \\(np\\) and the number of expected
                   \"failures\" \\(n(1-p)\\) are both greater than or equal to 10 for the current 
                   values of \\(n\\) and \\(p\\) given by the sliders.', 
                   style='font-size: x-small;')
        )
      ),
      column(9, wellPanel(
        tags$div('Click on the 2 tabs at the top to control which output is shown.  (The 
                 information below will also depend on which tab is open.)', br(), br(),
                 conditionalPanel('input.tab == "samp_dist"', 
                   strong('Sampling distribution of \\(\\hat{p}:\\)'), 'This app simulates 
                   the drawing of repeated simple random samples (SRSs) of size \\(n\\) from a
                   population with proportion \\(p\\) of individuals in some category.  The 
                   repeated samples will result in different sample proportions \\(\\hat{p}\\)
                   which make up the sampling distribution of the sample proportion.  The 
                   mean and standard deviation of this distribution are shown at the top.  The graph 
                   represents the sampling distribution of \\(\\hat{p}\\), with the vertical lines representing the different
                   possibilities for \\(\\hat{p}\\).  For example, for \\(n=20\\), the possibilities
                   for \\(\\hat{p}\\) are \\(0, \\frac{1}{20}, \\frac{2}{20}, \\frac{3}{20}, 
                   \\dots, \\frac{19}{20}\\), and 1.  The heights of the lines represent the 
                   percent of the samples that will give each value of \\(\\hat{p}\\) (or the 
                   probability that a single sample will give that value of \\(\\hat{p}\\)).
                   When you increase \\(n\\), the lines will get closer together (and you should 
                   not make anything of artifacts that appear when the lines converge together).
                   You can zoom in on the x-axis range shown in the plot by changing the slider 
                   at the bottom.  Note, however, that the range will revert to 0 to 1 if the 
                   current value of \\(p\\) is outside the range of the plot.'              
                  ),
                 conditionalPanel('input.tab == "compare_norm"', 
                    strong('Comparison with normal approximation:'), 'This tab adds a normal 
                    curve with the same mean and standard deviation as the sampling 
                    distribution of \\(\\hat{p}\\).  (Note that the tops of the lines align 
                    better with the normal curve when the normal conditions are satisfied.) 
                    In addition, if you check the box under the graph, you can compare 
                    the probabilities that \\(\\hat{p}\\) falls in a certain range based on 
                    the sampling distribution and its normal approximation.  This range is 
                    specified with the slider.  These two probabilities are shown in the graph 
                    in different ways:', 
                    tags$ul(
                      tags$li('For the sampling distribution, it is 
                              the sum of the heights of the white lines (in the green region).'), 
                      tags$li('For the normal approximation, it is the proportion of all the 
                              area under the normal curve in the green region.')
                    )
                  ),
                 style='font-size: x-small;')
      )), style="margin: 0px -20px;")
  )})
  #Outputs for first two panels
  output$mean <- renderText({input$p})
  output$std.dev <- renderText({
    p <- input$p
    round(sqrt(p*(1-p)/input$n), 4)
  })
  output$mean2 <- renderText({input$p})
  output$std.dev2 <- renderText({
    p <- input$p
    round(sqrt(p*(1-p)/input$n), 4)
  })
  output$np <- renderUI({
    HTML(paste(round(input$n * input$p, 2), 
               ifelse(input$n*input$p >= 10,
                      '<span style="color:green">&#10004;</span>', 
                      '<span style="color:red">&#10008;</span>')))
  })
  output$n_fail <- renderUI({
    HTML(paste(round(input$n * (1-input$p), 10), 
               ifelse(input$n*(1-input$p) >= 10,
                      '<span style="color:green">&#10004;</span>', 
                      '<span style="color:red">&#10008;</span>')))
  })
  output$cond <- renderUI({
    HTML(ifelse(input$n*input$p >= 10 & input$n*(1-input$p) >= 10, 
                '<span style="color:green">&#10004;</span>', 
                '<span style="color:red">&#10008;</span>'))
  })
  #Panel for sampling distribution of p-hat
  output$dist_panel <- renderUI({
    fluidPage(
      br(),
      tags$ul(
        tags$li('Mean \\( = p = \\)', textOutput('mean', inline=T)),
        tags$li('Standard Deviation \\( = \\sqrt{\\frac{p(1-p)}{n}} = \\)', 
                textOutput('std.dev', inline=T))
        ),
    plotOutput('plot', height='305px')
  )})
  output$plot <- renderPlot({
    n <- as.numeric(input$n)
    x <- 0:n
    y <- dbinom(x, n, input$p)
    par(mar=c(4.1, 4.1, .5, .5), cex.lab=1.3, cex.axis=1)
    plot(x/n, y*100, type='h', xlab='Sample Proportion', xlim=input$xlim,
         ylab='Percent of samples', col=4, ylim=c(0, max(y)*100))
  })
  #Panel for comparison with normal distribution
  output$norm_panel <- renderUI({fluidPage(
    withMathJax(),
    br(),
    tags$ul(
      tags$li('Mean \\( = p = \\)', textOutput('mean2', inline=T)),
      tags$li('Standard Deviation \\( = \\sqrt{\\frac{p(1-p)}{n}} = \\)', 
              textOutput('std.dev2', inline=T))
    ),
    plotOutput('comp_normal', height='305px'), 
    checkboxInput('calc_prob', label='Calculate probabilities that \\(\\hat{p}\\) falls 
                  in a certain range based on sampling distribution and its normal 
                  approximation'), 
    conditionalPanel('input.calc_prob == true',
     wellPanel(
      sliderInput('norm_range', label='Range to find probability', min=0, 
                max=1, value=c(0, 1), step=.001),
      tags$table(
        tags$thead(
          tags$th('Probability that \\(\\hat{p}\\) is in the range'),
          tags$th('According to which distribution?')
        ),
        tags$tr(
          tags$td(textOutput('bin_prob', inline=T)),
          tags$td('Sampling distribution of \\(\\hat{p}\\)')
        ),
        tags$tr(
          tags$td(textOutput('norm_prob', inline=T)), 
          tags$td('Normal approximation')
        )
      )
    ))
  )})
  observeEvent({input$xlim}, {
    updateSliderInput(session, 'norm_range', min=input$xlim[1], max=input$xlim[2])
  })
  output$comp_normal <- renderPlot({
    par(mar=c(4.1, 4.1, .5, .5), cex.lab=1.3, cex.axis=1)
    n <- as.numeric(input$n); p <- as.numeric(input$p)
    x <- 0:n
    y <- dbinom(x, n, p)
    par(mar=c(4.1, 4.1, .5, .5), cex.lab=1.3, cex.axis=1)
    xnorm <- seq(from=input$xlim[1], to=input$xlim[2], length=200)
    sd <- sqrt(p*(1-p)/n)
    dens.norm <- dnorm(xnorm, mean=p, sd=sd)
    mode.bin <- floor((n+1)*p)
    adj <- dbinom(mode.bin, n, p) * 100 / dnorm(mode.bin / n, mean=p, sd=sd)
    max.y <- dbinom(mode.bin, n, p) * 100 * dnorm(0, mean=0, sd=sd) / dnorm(mode.bin / n, mean=p, sd=sd)
    plot(x/n, y*100, type='h', xlab='Sample Proportion', xlim=input$xlim,
         ylab='Percent of samples', col=4, ylim=c(0, max.y))
    if(input$calc_prob==T){
      xrange <- seq(from=input$norm_range[1], to=input$norm_range[2], by=.001)
      ys <- dnorm(xrange, mean=p, sd=sd) * adj
      polygon(x=c(xrange[1], xrange, input$norm_range[2], xrange[1]), 
              y=c(0, ys, 0, 0), col=brewer.pal(9, 'Greens')[5])
      for(i in 0:n){
        if(i/n >= input$norm_range[1] & i/n <= input$norm_range[2])
          segments(x0=i/n, y0=0, y1=dbinom(i, n, p)*100, 
                   col=brewer.pal(9, 'Greens')[1])
      }
    }
    lines(xnorm, dens.norm * adj)
  })
  output$bin_prob <- renderText({
    prob <- diff(pbinom(c(input$norm_range[1]-.00001, input$norm_range[2])*input$n, 
                        input$n, input$p))
    sprintf('%5.4f', prob)
  })
  output$norm_prob <- renderText({
    prob <- diff(pnorm(input$norm_range, mean=input$p, sd=sqrt(input$p*(1-input$p)/input$n)))
    sprintf('%5.4f', prob)
  })
  observeEvent({input$xlim; input$p}, {
    if(input$p < input$xlim[1] | input$p > input$xlim[2])
      updateSliderInput(session, 'xlim', value=c(0,1))
  })
  
})
