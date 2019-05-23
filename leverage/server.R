library(shiny)
library(MASS)

make.cov <- function(sd.x1, sd.x2, cor)
{
  cov <- array(dim=c(2, 2))
  cov[1,1] <- sd.x1^2
  cov[2,2] <- sd.x2^2
  cov[1,2] <- cov[2,1] <- sd.x1 * sd.x2 * cor
  cov
}

shinyServer(function(input, output, session) {
  sample <- reactiveValues(
    x = mvrnorm(n = 100, mu=c(10,10), Sigma=diag(c(4, 9)))
  )
  observeEvent(input$new_samp,{
    cov <- make.cov(input$sd_x1, input$sd_x2, input$cor)
    sample$x <- mvrnorm(n = 100, mu=c(input$mean_x1, input$mean_x2), Sigma=cov)
  }) 
  xs <- reactive({
    cbind(c(input$x1star, sample$x[,1]),
          c(input$x2star, sample$x[,2])
    )
  })
  output$plot <- renderPlot({
    par(mar=c(4.5, 4.2, .1, .1), cex.lab=1.4)
    plot(xs()[,1], xs()[,2], xlab=expression(x[1]), ylab=expression(x[2]))
    points(input$x1star, input$x2star, pch=16, cex=2, col=2)
  })
  output$lev <- renderText({
    X <- cbind(1, xs())
    xpxinv <- solve(t(X) %*% X)
    xstar <- c(1, input$x1star, input$x2star)
    lev <- as.numeric(t(xstar) %*% xpxinv %*% xstar)
    round(lev, 4)
  })
})
