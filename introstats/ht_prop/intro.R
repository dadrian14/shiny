server.intro <- function(input, output){
  
  #Makes UI
output$intro_panel <- renderUI({
fluidPage(
  wellPanel(
    h3(strong('Introduction to Hypothesis Test for a Single Proportion')),
    'Click on the tabs on the black bar at the top of the page to navigate to the different apps.
    The tabs are:',
    tags$ul(
      tags$li('Introduction (currently selected)'),
      tags$li('Test statistic'),
      tags$li('P-value'),
      tags$li('Test statistic / p-value')
    )
  )
)
})
}