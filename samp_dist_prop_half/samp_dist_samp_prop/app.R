library(shiny)
ui <- fluidPage(
  wellPanel(h2(strong('Sampling distribution of sample proportion'))), 
  sidebarLayout(
    sidebarPanel(
      sliderInput('p', label= h4("Population proportion (p)"), 
                  min=.01, max=.99, value=.5, step=.01),
      sliderInput("n", label = h4("Sample size (n)"), 
                  min=10, max=1000, value=100, step=10)
    ),
    mainPanel(
      plotOutput('plot'), 
      hr(),
      column(6, h4('Mean'), textOutput('mean', inline=T)),
      column(6, h4('Standard Deviation'), textOutput('std.dev')
      )
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    n <- as.numeric(input$n)
    x <- 0:n
    y <- dbinom(x, n, input$p)
    par(mar=c(4.1, 4.5, .5, .5), cex.lab=1.8, cex.axis=1.5)
    plot(x/n, y*100, type='h', xlab='Sample Proportion', 
         ylab='Percent of samples', col=4)
    
  })
  output$mean <- renderText({input$p})
  output$std.dev <- renderText({
    p <- input$p
    round(sqrt(p*(1-p)/input$n), 4)
  })
}

shinyApp(ui = ui, server = server)

