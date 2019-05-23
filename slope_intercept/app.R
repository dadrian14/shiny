#Slope_intercept
library(shiny)
library(graphics)
ui <- fluidPage(
  includeCSS('styles.css'), 
  withMathJax(),
  wellPanel(
      h3(strong('Slope-intercept form of a line')),
      h4('\\(y = mx + b\\)'), 
      tags$div(
        checkboxInput('click_info', label='Click for more info!'),
        style='margin-bottom: -10px;'),
      conditionalPanel('input.click_info == true',
        uiOutput('info')
      , style='margin-bottom: -10px')
  ),
  fluidRow(
      column(3, wellPanel(
        sliderInput(inputId="m", label='Slope \\(m\\)', value=1.3, min=-3, max=3,
              step=.1, animate=list(interval=250, loop=T)), 
        br(),
        sliderInput(inputId="b", label='Y-Intercept \\(b\\)', value=1.7, min=-2, max=2, 
              step=.1, animate=list(interval=250, loop=T))
      )), 
      column(5, wellPanel(
        h4(strong('Equation:'), '\\(y = \\)', textOutput('mtxt', inline=T), '\\(x\\)',
        textOutput('btxt', inline=T)),
        br(),
          tags$head(tags$style("#mtxt{color: blue;}
                        #btxt{color: red;}"
          )),
        plotOutput("plot", height='auto')
        
      )), 
      column(4, wellPanel(
             tags$h4(strong('Explanation of Plot')),
             br(),
             tags$p(strong('Black line:'), 'The Plot of the line \\(y = mx+b\\)'), 
             tags$p(strong(HTML('<span style="color:blue;">Blue annotations:</span>')),
                    'The', strong('slope (m)'), 
                    'is the change in \\(y\\) value for a 1-unit increase in \\(x\\) value.'), 
             p(strong(HTML('<span style="color:red;">Red annotations:</span>')), 
                  'Show that the value of the', 
               strong('y-intercept \\((b)\\)'), 
               'is the \\(y\\) value at which the line cross the \\(y\\)-axis (where \\(x=0\\)).')
      ))
  )
)

server <- function(input, output, session) {
  output$info <- renderUI({fluidPage(
    'The boxes below describe the contents the corresponding panels below them in the app.',
    fluidRow(
      column(3, wellPanel(
        tags$div('Use the sliders to change the value of the slope and the y-intercept.  To 
                 animate the increase of either slider, click on the "play" button.', 
                 style='font-size: x-small')
      )),
      column(5, wellPanel(
        tags$div('Shows the equation and plot of the line for the current values of the 
                 slope and intercept.', 
                 style='font-size: x-small')
      )),
      column(4, wellPanel(
        tags$div('Describes the annotations on the plot.', 
                 style='font-size: x-small')
      ))
      , style="margin: 0px -20px;")
  )})
  x <- c(-3,0,3)
  output$plot <- renderPlot({
    par(cex.main=2, cex.lab=1.5, mar=c(4.1, 4.1, .5, .5))
    plot(x, input$m*x+input$b, type='l', ylim=c(-3, 3), 
         ylab="y", lwd=3)
    grid()
    abline(v=0); abline(h=0)
    points(0, input$b, col=2, pch=16, cex=2)
    arrows(0, input$b, 1, input$b, col=4, lty=1, lwd=2, length=.1)
    arrows(1, input$b, 1, input$b+input$m, col=4, lty=1, lwd=2, length=.1)
    if(input$m < 0) text(0.5, input$b+.3, '1')
    if(input$m > 0) text(0.5, input$b-.3, '1')
    if(input$m != 0) text(1.5, input$b + input$m/2, input$m, col=4, cex=1.5)
    arrows(0, input$b, -3.1, input$b, col=2, length=.1, lwd=2)
    text(-2.8, input$b+.3, input$b, col=2, cex=1.5)
  }, height = function() {
    session$clientData$output_plot_width
  })
  output$mtxt <- renderText({input$m})
  output$btxt <- renderText({
    ifelse(input$b >= 0, paste('+', input$b), paste('-', abs(input$b)))
  })
  
}

shinyApp(ui = ui, server = server)