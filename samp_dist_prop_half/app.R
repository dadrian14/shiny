library(shiny)
ui <- fluidPage(
  h3('Distribution of proportions of odd rolls'), 
  sidebarLayout(
    sidebarPanel(
      selectInput("n", label = h4("Number of Rolls"), 
                choices = list("5" = 5, "10"=10, "20"=20, 
                               "50"=50, "100"=100, "250"=250), 
                selected = 1)),
    mainPanel(plotOutput('plot'))
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    n <- as.numeric(input$n)
    x <- 0:n
    y <- dbinom(x, n, .5)
    par(mar=c(4.1, 4.1, .5, .5))
    plot(x/n, y*100, type='h', xlab='Proportion of odd rolls', 
         ylab='Percent of students', col=4)
    
  })
}

shinyApp(ui = ui, server = server)

