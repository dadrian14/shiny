library(shiny)
library(shinythemes)
library(shinyjs)


shinyUI(fluidPage(theme = shinytheme("cerulean"),
    fluidRow(
      splitLayout(
        tags$h4("SD of Different Distributions"),
        actionButton(class = "btn action-button", style = "background-color: white ;color: black", "alldistbutton", "Click to get fresh plots"),
        uiOutput("abforanswers")
      )
    ),
    tags$hr(style="border-color: red;"),
    
    fluidRow(
      splitLayout(
        uiOutput("ans1"),
        uiOutput("ans2"),
        uiOutput("ans3"),
        uiOutput("ans4")
      )
    ),
    fluidRow(
      splitLayout(
        uiOutput("rb"),
        uiOutput("rb1"),
        uiOutput("rb2"),
        uiOutput("rb3")
      )
    ),
    mainPanel({
      splitLayout(
        plotOutput("mygraph"),
        plotOutput("myboxplot")
      )
    })
  ))

  
