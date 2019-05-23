library(shiny)
source('random_nested.R')
source('fixed_crossed.R')
source('fixed_nested.R')
source('mixed_crossed.R')
source('mixed_nested.R')
source('random_crossed.R')

ui <- navbarPage(title='Two factor experiments', inverse=T,
                 tabPanel('Fixed & Crossed', fixed_crossed_ui()), 
                 tabPanel('Mixed & Crossed', mixed_crossed_ui()), 
                 tabPanel('Random & Crossed', random_crossed_ui()),
                 tabPanel('Fixed & Nested', fixed_nested_ui()),
                 tabPanel('Mixed & Nested', mixed_nested_ui()),
                 tabPanel('Random & Nested', random_nested_ui())
)

server <- function(input, output)
{
  fixed_crossed_server(input, output)
  mixed_crossed_server(input, output)
  random_crossed_server(input, output)
  fixed_nested_server(input, output)
  mixed_nested_server(input, output)
  random_nested_server(input, output)
}

shinyApp(ui = ui, server = server)