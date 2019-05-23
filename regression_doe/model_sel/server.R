library(shiny)
library(MASS)

shinyServer(function(input, output) {
  mean.xs <- c(10,10)
  sd.xs <- 2
  beta0 <- 5; beta1 <- 2
  n <- 100; 
  sigma_y <- 5
  xs <- reactive({
    mvrnorm(n, mu=mean.xs, 
            Sigma=sd.xs^2*cbind(c(1, input$cor_xs), c(input$cor_xs, 1)), empirical=T)
  })
  mean_y <- reactive({
    beta0 + beta1 * xs()[,1] + input$beta2 * xs()[,2]
  })
  x1fit <- reactive({ #model with only x1
    X <- cbind(1, xs()[,1])
    xpx.inv <- solve(t(X) %*% X)
    mean.beta1hat <- xpx.inv[2,] %*% t(X) %*% mean_y()
    sd.beta1hat <- sigma_y * sqrt(xpx.inv[2,2])
    mean.y1 <- X[1,] %*% xpx.inv %*% t(X) %*% mean_y()
    sd.y1 <- sigma_y * sqrt(t(X[1,]) %*% xpx.inv %*% X[1,])
    list(mean.b1=mean.beta1hat, sd.b1=sd.beta1hat, mean.y1=mean.y1, sd.y1=sd.y1)
  })
  x12fit <- reactive({ #model with x1, x2
    X <- cbind(1, xs())
    xpx.inv <- solve(t(X) %*% X)
    mean.beta1hat <- xpx.inv[2,] %*% t(X) %*% mean_y()
    sd.beta1hat <- sigma_y * sqrt(xpx.inv[2,2])
    mean.y1 <- X[1,] %*% xpx.inv %*% t(X) %*% mean_y()
    sd.y1 <- sigma_y * sqrt(t(X[1,]) %*% xpx.inv %*% X[1,])
    list(mean.b1=mean.beta1hat, sd.b1=sd.beta1hat, mean.y1=mean.y1, sd.y1=sd.y1)
  })
  output$scatter_xs <- renderPlot({
    par(mar=c(4.1, 4.1, .1, .1))
    plot(xs()[,1], xs()[,2], xlab=expression(x[1]), ylab=expression(x[2]))
  })
  output$samp_betahat1 <- renderPlot({
    par(mar=c(4.1, 4.1, .5, .5))
    min.plot <- min(x1fit()$mean.b1 - 4*x1fit()$sd.b1,
                    x12fit()$mean.b1 - 4*x12fit()$sd.b1)
    max.plot <- max(x1fit()$mean.b1 + 4*x1fit()$sd.b1,
                    x12fit()$mean.b1 + 4*x12fit()$sd.b1)
    x <- seq(from=0, to=4, length=200)
    x1samp <- dnorm(x, mean=x1fit()$mean.b1, sd=x1fit()$sd.b1)
    x12samp <- dnorm(x, mean=x12fit()$mean.b1, sd=x12fit()$sd.b1)
    matplot(x, cbind(x1samp, x12samp), type='l', xlab=expression(hat(beta)[1]), 
            ylab='Density', col=c(2, 4), lty=1)
    abline(v=beta1, lty=2)
    abline(h=0)
  })
  output$beta1_table <- renderTable({
    bias.x1 <- x1fit()$mean.b1 - beta1
    rmse.x1 <- sqrt(bias.x1^2 + x1fit()$sd.b1^2)
    x1_only <- c(bias.x1, x1fit()$sd.b1, rmse.x1)
    bias.x12 <- x12fit()$mean.b1 - beta1
    rmse.x12 <- sqrt(bias.x12^2 + x12fit()$sd.b1^2)
    x1_and_x2 <- c(bias.x12, x12fit()$sd.b1, rmse.x12)
    table <- data.frame(x1_only, x1_and_x2)
    colnames(table) <- c('\\(x_1\\) only', '\\(x_1\\) and \\(x_2\\)')
    rownames(table) <- c('Bias\\((\\hat{\\beta}_1)\\)', 
                         'SD\\((\\hat{\\beta}_1)\\)', 
                         'RMSE\\((\\hat{\\beta}_1)\\)')
    table
  }, rownames=T, digits=3)
  output$underover <- renderUI({
    if(input$beta2 == 0)
      out <- withMathJax(tagList('Model with \\(x_1\\) and \\(x_2\\) is', strong('overfit')))
    else
      out <- withMathJax(tagList('Model with \\(x_1\\) only is', strong("underfit")))
    out
  })
})
