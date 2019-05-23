library(shiny)

comp.mu <- function(i, j, k, mu, a, b, c, ab, ac, bc, abc, outer1, outer2, outer3)
{
  mu + a*outer1[i] + b*outer1[j] + c*outer1[k] + ab*outer2[i,j] + 
    ac*outer2[i,k] + bc*outer2[j,k] + abc*outer3[i,j,k]
}

shinyServer(function(input, output) {
  outer1 <- c(1, -1)
  outer2 <- outer(c(1, -1), c(1,-1))
  outer3 <- outer(outer2, c(1, -1))
  mu <- reactiveValues(all=array(dim=c(2,2,2)))
  observe({
    for(i in 1:2) for(j in 1:2) for(k in 1:2) 
      mu$all[i,j,k] <- comp.mu(i, j, k, input$mu, input$a, input$b, input$c, 
                            input$ab, input$ac, input$bc, input$abc, 
                            outer1, outer2, outer3)
  })
  lapply(1:2, function(i){
    lapply(1:2, function(j){
      lapply(1:2, function(k){
        output[[paste0('mu', i, j, k)]] <- renderPlot({
          par(mar=c(.1, 2.5, 1.5, .1))
          plot(.5, mu$all[i,j,k], ylim=c(2, 18), xlim=c(0,1), xaxt='n', pch=NA, main=paste0('mu',i,j,k))
          abline(h=mu$all[i,j,k])
          text(.5, mu$all[i,j,k]+.7, mu$all[i,j,k])
        })
  })})})
})
