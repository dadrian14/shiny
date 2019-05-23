library(shiny)

shinyServer(function(input, output, session) {
  v <- reactiveValues(data=NULL)
  observeEvent(c(input$n, input$dist, input$draw), {
    if(input$dist == 'normal') v$data <- rnorm(input$n, mean=7, sd=2)
    if(input$dist == 'exp') v$data <- rexp(input$n, rate=1)
    if(input$dist == 'cauchy') v$data <- rcauchy(input$n, location=7, scale=1)
    if(input$dist == 'left') v$data <- 10 - rexp(input$n, rate=1)
  })
  output$listed <- renderUI({
    if(is.null(v$data)) return('Draw a Sample')
    i <- input$which_i; n <- as.numeric(input$n)
    data <- round(sort(v$data), 2)
    if(i>1) before <- paste0(toString(data[1:(i-1)]), ',')
    else before <- NULL
    if(i<n) at <- paste0(data[i], ',')
    else at <- data[i]
    if(i<n) after <- toString(data[(i+1):n])
    else after <- NULL
    tagList(before, strong(at), after)
  })
  output$hist <- renderPlot({
    par(mar=c(4, 4, .5, .5))
    if(is.null(v$data)) return()
    hist(v$data, xlab='Sample', ylab='Count', main='')
  })
  output$qqplot <- renderPlot({
    par(mar=c(4,4,.5,.5))
    if(is.null(v$data)) return()
    n <- as.numeric(isolate(input$n))
    percentiles <- (1:n - 0.5) / n
    x <- qnorm(percentiles)
    y <- sort(v$data)
    plot(x, y, xlab='Normal Quantiles', ylab='Sample')
    grid()
    abline(a=mean(v$data), b=sd(v$data))
      points(x[input$which_i], y[input$which_i], pch=19, col=2)
  }, height = function() {
      session$clientData$output_qqplot_width
  })
  observe({
    updateSliderInput(session, 'which_i', max=input$n)
  })
  coords <- reactive({
    i <- input$which_i; n <- as.numeric(input$n)
    perc <- (i-0.5)/n
    quant <- round(qnorm(perc), 2)
    ordered.sample.val <- round(sort(v$data)[input$which_i], 2)
    list(perc=perc, quant=quant, ordered.sample.val=ordered.sample.val)
  })
  output$coord <- renderText({
    paste0('(', coords()$quant, ', ', coords()$ordered.sample.val, ')')
  })
  output$quant <- renderText({
    coords()$quant
  })
  output$i <- renderText(input$which_i)
  output$perc <- renderText({
    coords()$perc
  })
  output$ordered_sample_val <- renderText({
    coords()$ordered.sample.val
  })
  output$norm_exp <- renderPlot({
    par(mar=c(2, .1, 2, .1))
    x <- seq(from=-3.5, to=3.5, by=.05)
    y <- dnorm(x)
    perc <- coords()$perc
    plot(x,y, type='l', yaxt='n', xaxt='n', main=paste('Area =', perc))
    quant <- coords()$quant
    segments(x0=quant, y0=0, y1=dnorm(quant))
    xcum <- seq(from=quant, to=-3.5, by=-.05)
    polygon(x=c(-3.5, quant, quant, xcum, -3.5, -3.5), 
            y=c(0,0,dnorm(quant), dnorm(xcum), dnorm(-3.5), 0), col=grey(.8))
    axis(side=1, at=quant)
  })
})
