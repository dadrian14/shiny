library(shiny)
library(xtable)
ui <- fluidPage(
  wellPanel(h2(strong('Simple Linear Regression Model')), 
            h4(HTML('Y<sub>i</sub> = &beta;<sub>0</sub> + ',
              '&beta;<sub>1</sub>x<sub>i</sub> + E<sub>i</sub>', 
              ',  E<sub>i</sub> ~ iid N(0, &sigma;<sup>2</sup>)'))),
  fluidRow(
    column(4, wellPanel(
      strong('Fixed x\'s'), br(), 
      HTML('x<sub>1</sub> = 1, x<sub>2</sub> = 2, ..., x<sub>6</sub> = 6'), br(), br(),
      h4(strong('Specify the parameters')),
      sliderInput('beta0', label=HTML('&beta;<sub>0</sub>'), value=1, min=0, max=2, step=.1), 
      sliderInput('beta1', label=HTML('&beta;<sub>1</sub>'), value=.5, min=0, max=1, step=.1), 
      sliderInput('sigma', label=HTML('&sigma;'), value=1, min=0, max=2, step=.1), 
      hr(),
      actionButton('new_samp', label='Draw New Sample')
    )), 
    column(8, wellPanel(
      plotOutput('plot'), 
      p(strong('Black line:'), '(Truth from model) mean(Y) = ', textOutput('popline', inline=T)),
      p(strong(HTML('<font color="red"> Red line: </font>')), 
        HTML('(Estimate from sample) y&#770; = '), 
        textOutput('regline', inline=T))
    ))
  ), 
  fluidRow(wellPanel(
    tableOutput('table'), 
    strong(HTML('Estimate of &sigma; =')), textOutput('sighat', inline=T)
  ))
)

server <- function(input, output) {
  x <- 1:6
  X <- cbind(1, x)
  out <- eventReactive(input$new_samp,{
    mean_y <- input$beta0 + input$beta1 * x
    y <- rnorm(6, mean=mean_y, sd=input$sigma)
    bhats <- as.vector(solve(t(X)%*%X)%*%t(X)%*%y)
    yhat <- bhats[1] + bhats[2] * x
    e <- y - mean_y
    ehat <- y - yhat
    list(y=y, muy=mean_y, bhat=bhats, yhat=yhat, e=e, ehat=ehat)
  })
  output$plot <- renderPlot({
    if(as.numeric(input$new_samp)==0){
      par(mar=c(4, 4, .5, .5))
      return({
        plot(0,0, ann=F, type='n', axes=F)
        box()
        text(0,0, 'Click \"Draw New Sample\" \n for a scatterplot', cex=3, col=2)
      })
    }
    par(mar=c(4, 4, .5, .5))
    plot(x, out()$muy, type='l', ylab='y', ylim=c(-5, 10), lwd=2)
    grid()
    points(x, out()$y, pch=16, col=2)
    abline(a=out()$bhat[1], b=out()$bhat[2], col=2)
  })
  output$popline <- renderText({
    paste0(input$beta0, ' + ', input$beta1, 'x')
  })
  output$regline <- renderText({
    paste0(round(out()$bhat[1], 2), ' + ', round(out()$bhat[2], 2), 'x')
  })
  output$table <- renderTable({
    mat <- cbind(x, out()$y, out()$muy, out()$yhat, out()$e, out()$ehat)
    xt <- xtable(mat)
    names(xt) <- c('x', 'y', 'mean(y)',  'yhat', 'Errors', 'Residuals')
    xt
  })
  output$sighat <- renderText({
    round(sqrt(sum(out()$ehat^2/4)), 2)
  })
}

shinyApp(ui = ui, server = server)