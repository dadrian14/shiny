library(shiny)

shinyServer(function(input, output) {
  jit <- .15
   y <- reactiveValues(fix=array(dim=c(10, 2)), 
                       ran=array(dim=c(10, 2)))
   res <- reactiveValues(fix=array(dim=c(100,2)), ran=array(dim=c(100,2)), 
                         ifix=0, iran=0)
   observeEvent(input$gen_fix,{
     y$fix[,1] <- rnorm(10, mean=input$mu_fix + input$tau1, sd=input$sig_fix)
     y$fix[,2] <- rnorm(10, mean=input$mu_fix - input$tau1, sd=input$sig_fix)
     res$ifix <- res$ifix + 1
     ind <- res$ifix %% 100 + 1
     res$fix[ind,] <- y$fix[1:2,1]
   })
   observeEvent({input$mu_fix; input$tau1; input$sig_fix; input$clear_fix}, {
     res$fix <- array(dim=c(100,2))
   })
   output$curr_fix <- renderPlot({
     par(mar=c(4.1, 4.1, .5, .5))
     x1 <- runif(10, min=1-jit, max=1+jit)
     x2 <- runif(10, min=2-jit, max=2+jit)
     plot(c(x1, x2), c(y$fix[,1], y$fix[,2]), xlim=c(.5, 2.5), xaxt='n', ylab='y', 
          ylim=c(0, 20), xlab='Treatment', col=c(2, 4, rep(1, 18)), pch=c(16,16,rep(1,18)))
     axis(side=1, at=c(1,2))
   })
   output$cor_fix <- renderPlot({
     par(mar=c(4.1, 4.1, .5, .5))
     if(all(is.na(res$fix))){
       par(mar=c(.5, .5, .5, .5))
       plot(NA, NA, xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n')
       text(.5, .5, 'Draw some samples!', col=2)
     }
     else{
       plot(res$fix, xlab=expression(y[11]), ylab=expression(y[12]))
      ind <- res$ifix %% 100 + 1
      segments(x0=res$fix[ind,1], y0=res$fix[ind,2], y1=0, lty=2, col=2)
      segments(x0=res$fix[ind,1], y0=res$fix[ind,2], x1=0, lty=2, col=4)
     }
   })
   #Random
   observeEvent(input$gen_ran,{
     Ts <- rnorm(2, mean=0, sd=input$sigT)
     y$ran[,1] <- rnorm(10, mean=input$mu_ran + Ts[1], sd=input$sig_ran)
     y$ran[,2] <- rnorm(10, mean=input$mu_ran + Ts[2], sd=input$sig_ran)
     res$iran <- res$iran + 1
     ind <- res$iran %% 100 + 1
     res$ran[ind,] <- y$ran[1:2,1]
   })
   observeEvent({input$mu_ran; input$sigT; input$sig_ran; input$clear_ran}, {
     res$ran <- array(dim=c(100,2))
   })
   output$curr_ran <- renderPlot({
     par(mar=c(4.1, 4.1, .5, .5))
     x1 <- runif(10, min=1-jit, max=1+jit)
     x2 <- runif(10, min=2-jit, max=2+jit)
     plot(c(x1, x2), c(y$ran[,1], y$ran[,2]), xlim=c(.5, 2.5), xaxt='n', ylab='y', 
          ylim=c(0, 20), xlab='Treatment', col=c(2, 4, rep(1, 18)), 
          pch=c(16,16,rep(1,18)))
     axis(side=1, at=c(1,2))
   })
   output$cor_ran <- renderPlot({
     par(mar=c(4.1, 4.1, .5, .5))
     if(all(is.na(res$ran))){
       par(mar=c(.5, .5, .5, .5))
       plot(NA, NA, xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n')
       text(.5, .5, 'Draw some samples!', col=2)
     }
     else{
       plot(res$ran, xlab=expression(y[11]), ylab=expression(y[12]))
       ind <- res$iran %% 100 + 1
       segments(x0=res$ran[ind,1], y0=res$ran[ind,2], y1=-20, lty=2, col=2)
       segments(x0=res$ran[ind,1], y0=res$ran[ind,2], x1=-20, lty=2, col=4)
     }
   })
   output$theo_corr_rand <- renderText({
     round(input$sigT^2 / (input$sigT^2 + input$sig_ran), 4)
   })
})
