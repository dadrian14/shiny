library(shiny)

comp.power <- function(del, sigma, n, alpha){
  ncp <- del / (sigma * sqrt(2/n))
  tquant <- qt(1-alpha/2, df=2*n-2)
  1 - pt(tquant, df=2*n-2, ncp=ncp) + pt(-tquant, df=2*n-2, ncp=ncp)
}


draw.boxplot <- function(data, gp, jit)
{
  num5 <- fivenum(data)
  segments(x0=gp-.5*jit, x1=gp+.5*jit, y0=num5[c(1,5)])
  segments(x0=gp-jit, x1=gp+jit, y0=num5[2:4])
  segments(x0=gp, y0=num5[c(1,5)], y1=num5[c(2,4)])
  segments(x0=gp+c(-jit, jit), y0=num5[2], y1=num5[4])
}

comp.t <- function(x1, x2){
  (mean(x1) - mean(x2)) / sqrt(var(x1) / length(x1) + var(x2) / length(x2))
}

sattherthwaite <- function(s1, s2, n1, n2)
{
  (s1^2/n1 + s2^2/n2)^2 / 
    ((s1^2/n1)^2 / (n1-1) + (s2^2/n2)^2 / (n2-1))
}

ui <- fluidPage(
  fluidRow(wellPanel(h2(strong('Type 1 and 2 Error, Power')),
                     'We know the truth when we simulate samples', 
                     'so we know whether a hypothesis test makes an error.')),
  fluidRow(
    column(4, wellPanel(
    radioButtons('truth', label='The truth', inline=F, choiceNames=
                   list(HTML('H<sub>0</sub>: &mu;<sub>1</sub> = &mu;<sub>2</sub>'),
                        HTML('H<sub>a</sub>: &mu;<sub>1</sub> &ne; &mu;<sub>2</sub>')),
                 choiceValues=list('Ho', 'Ha')),
    sliderInput(inputId='alpha', label=HTML('Significance level (&alpha;)'), 
                min=0.01, max=0.2, value=.05, step=.01)
    )),
    column(4, wellPanel(
    actionButton('new', strong('Draw New Sample')), br(),
    actionButton('new50', strong('Draw 50 New Samples')), hr(),
    strong('Current decision:'), 
    tags$ul(tags$li(textOutput('dec2', inline=T))),
    strong('Correct decision or Error:'), 
    tags$ul(tags$li(textOutput('error', inline=T)))
    )),
    column(4, wellPanel(
      h3(strong('Tally of results')),
      strong('Proportion of correct decisions', textOutput('power', inline=T)),
      tags$ul(tags$li(textOutput('prop.corr'))), 
      strong('Proportion of', textOutput('type', inline=T), 'Errors'),
      tags$ul(tags$li(textOutput('prop.err')))
    ))
  ),
  fluidRow(
    column(4, wellPanel(
      h3(strong('Population means')),
      conditionalPanel("input.truth == 'Ho'", 
                       sliderInput(inputId='mus_ho', 
                                   label=HTML('&mu;<sub>1</sub> = &mu;<sub>2</sub>'), 
                                   min=0, max=10, value=5, step=.1)),
      conditionalPanel("input.truth == 'Ha'",
                       sliderInput(inputId='mu1', label=HTML('&mu;<sub>1</sub>'), 
                                   min=0, max=10, value=5, step=.1), 
                       sliderInput(inputId='mu2', label=HTML('&mu;<sub>2</sub>'), 
                                   min=0, max=10, value=3, step=.1),
                       conditionalPanel("input.mu1 == input.mu2",
                                        HTML('HEY! If H<sub>a</sub> is true, then'),
                                        HTML('&mu;<sub>1</sub> &ne; &mu;<sub>2</sub>!'))),
      hr(),
      strong('Details:'),
      tags$ul(
        tags$li('Normally distributed populations'),
        tags$li('Population standard deviations', 
                HTML('&sigma;<sub>1</sub> = &sigma;<sub>2</sub> = 3')),
        tags$li('Sample sizes', 
                HTML('n<sub>1</sub> = n<sub>2</sub> = 30'))
      )
    )),
    column(4, wellPanel(
      h3(strong('Plot of last sample')), 
      plotOutput(outputId='plot')
    )),
    column(4, wellPanel(
      h3(strong('Last sample results')), 
      h5(strong('Sample 1')),
      tags$ul(
        tags$li(HTML('mean x&#772;<sub>1</sub> = '), 
                textOutput('xbar1.t8', inline=T)), 
        tags$li(HTML('standard deviation s<sub>1</sub> = '),
                textOutput('s1.t8', inline=T))
      ), 
      h5(strong('Sample 2')),
      tags$ul(
        tags$li(HTML('mean x&#772;<sub>2</sub> = '), 
                textOutput('xbar2.t8', inline=T)), 
        tags$li(HTML('standard deviation s<sub>2</sub> = '),
                textOutput('s2.t8', inline=T))
      ), 
      strong('Hypothesis Test'), br(),
      tags$ul(
        tags$li('t = ', textOutput('tstat', inline=T)),
        tags$li('p-value* = ', textOutput('pval', inline=T)),
        tags$li('Decision: ', textOutput('dec1', inline=T))
      ),
      '* For 2-tailed test; Based on Sattherwaite approximation of df'
    ))
  )
)


server <- function(input, output) {
  jit <- .20 #jitter
  sigma1 <- sigma2 <- 3
  n1 <- n2 <- 30
  num <- reactiveValues(tests=0, corr=0, err=0)
  observeEvent(input$new,{
    num$tests <- num$tests + 1
    if(input$truth == 'Ho' & results()$decision == 'Fail to reject Ho')
      num$corr <- num$corr + 1
    if(input$truth == 'Ho' & results()$decision == 'Reject Ho')
      num$err <- num$err + 1
    if(input$truth == 'Ha' & results()$decision == 'Fail to reject Ho')
      num$err <- num$err + 1
    if(input$truth == 'Ha' & results()$decision == 'Reject Ho')
      num$corr <- num$corr + 1
  })
  observeEvent(input$new50, {
    num$tests <- num$tests + 50
    if(input$truth == 'Ho'){
      n.err <- rbinom(n=1, size=50, prob=input$alpha)
      num$err <- num$err + n.err
      num$corr <- num$corr + 50 - n.err
    }
    if(input$truth == 'Ha'){
      power <- comp.power(input$mu1 - input$mu2, sigma1, n1, input$alpha)
        #uses pooled t test 
      n.corr <- rbinom(n=1, size=50, prob=power)
      num$corr <- num$corr + n.corr
      num$err <- num$err + 50 - n.corr
    }
  })
  observeEvent({
    input$alpha
    input$truth
    input$mu1; input$mu2
    },
    {num$tests <- 0; num$corr <- 0; num$err <- 0}
  )
  output$n.tests <- renderText(num$tests)
  output$n.corr <- renderText(num$corr)
  output$n.err <- renderText(num$err)
  output$power <- renderText(
    ifelse(input$truth == 'Ha', '(power)', '')
  )
  output$type <- renderText(
    ifelse(input$truth == 'Ho', 'Type 1', 'Type 2')
  )
  output$prop.corr <- renderText({
    paste(num$corr, '/', num$tests, '=', 
          ifelse(num$tests==0, '---', round(num$corr/num$tests, 3)))
  })
  output$prop.err <- renderText({
    paste(num$err, '/', num$tests, '=', 
          ifelse(num$tests==0, '---', round(num$err/num$tests, 3)))
  })
  mu <- reactive({
    if(input$truth == 'Ho') list(input$mus_ho, input$mus_ho)
    else if (input$truth == 'Ha') list(input$mu1, input$mu2)
  })
  data <- eventReactive({input$new; input$new50; input$truth 
    input$mus_ho; input$mu1; input$mu2}, {
    list(y1=rnorm(n=n1, mean=mu()[[1]], sd=sigma1), 
         y2=rnorm(n=n2, mean=mu()[[2]], sd=sigma2), 
         x1=runif(n=n1, min=1-jit, max=1+jit), 
         x2=runif(n=n2, min=2-jit, max=2+jit))
  }, ignoreInit=T)
  output$plot <- renderPlot({
    par(mar=c(4.5, 2.5, .5, .5), cex.lab=1.5, cex.axis=1.3)
    plot(c(data()$x1, data()$x2), c(data()$y1, data()$y2), 
         xlim=c(.5, 2.5), ylim=c(-10, 20), 
         xaxt='n', xlab='Samples', ylab='')
    axis(side=1, at=c(1,2))
    abline(h=seq(-10, 20, by=5), col=grey(.7), lty=3)
    draw.boxplot(data()$y1, 1, jit)
    draw.boxplot(data()$y2, 2, jit)
  })
  results <- reactive({
    xbar1 <- mean(data()$y1)
    s1 <- sd(data()$y1)
    xbar2 <- mean(data()$y2)
    s2 <- sd(data()$y2)
    t <- comp.t(data()$y1, data()$y2)
    pval <- 2 * pt(-abs(t), df=sattherthwaite(s1, s2, n1, n2))
    decision <- ifelse(pval <= input$alpha, 'Reject Ho', 'Fail to reject Ho')
    list(xbar1=xbar1, s1=s1, xbar2=xbar2, s2=s2, t=t, pval=pval, decision=decision)
  })
  output$xbar1.t8 <- renderText(round(results()$xbar1, 2))
  output$s1.t8 <- renderText(round(results()$s1, 2))
  output$xbar2.t8 <- renderText(round(results()$xbar2, 2))
  output$s2.t8 <- renderText(round(results()$s2, 2))
  output$tstat <- renderText(round(results()$t, 2))
  output$pval <- renderText(round(results()$pval,4))
  output$dec1 <- renderText(results()$decision)
  output$dec2 <- renderText(results()$decision)
  output$error <- renderText({
    ifelse(input$truth == 'Ho' & results()$decision == 'Reject Ho', 'Type 1 Error',
           ifelse(input$truth == 'Ha' & results()$decision == 'Fail to reject Ho',
                  'Type 2 Error', 'Correct decision'))
  })
}

shinyApp(ui = ui, server = server)