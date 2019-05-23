#inputs and output have suffix .t4

slope_int_ui <- function()
{
 fluidPage(
  wellPanel(
      tags$h2(strong('Slope-intercept form of a line:  '), tags$i('y = mx + b'))
  ),
  fluidRow(
      column(3, wellPanel(
             tags$h2(strong('Manipulation')),
        tags$p('Use the following sliders to change the', tags$strong('slope (m)'), 
             'and',  strong('y-intercept (b)'), 'of the line.'),
        sliderInput(inputId="m.t4", label='Slope (m)', value=1.3, min=-3, max=3,
              step=.1), 
        sliderInput(inputId="b.t4", label='Y-Intercept (b)', value=1.7, min=-2, max=2, 
              step=.1)
      )), 
      column(5, wellPanel(
        plotOutput("plot.t4", height='auto') 
        
      )), 
      column(4, wellPanel(
             tags$h2(strong('Explanation of Plot')), 
             tags$p(strong('Black line:'), 'Plot of the line'), 
             tags$p(strong(tags$span(style="color: blue;", 'Blue annotations:')),
                    'Show that the value of the', strong('slope (m)'), 
                    'is the change in y-value for a 1-unit increase in x-value.'), 
             p(strong(tags$span(style="color: red;",'Red annotations:')), 
               'Show that the value of the', strong('y-intercept (b)'), 
               'is the y-value at which the line cross the y-axis.')
      ))
    )
  )
}

slope_int_server <- function(input, output, session) {
  x <- c(-3,0,3)
  output$plot.t4 <- renderPlot({
    par(cex.main=2.5, cex.lab=1.5)
    plot(x, input$m.t4*x+input$b.t4, type='l', ylim=c(-3, 3), 
         ylab="y", lwd=3, main=
           paste('Plot:   y = ', input$m.t4, 'x', " + ", input$b.t4, sep=''))
    grid()
    abline(v=0); abline(h=0)
    points(0, input$b.t4, col=2, pch=16, cex=2)
    arrows(0, input$b.t4, 1, input$b.t4, col=4, lty=1, lwd=2, length=.1)
    arrows(1, input$b.t4, 1, input$b.t4+input$m.t4, col=4, lty=1, lwd=2, length=.1)
    if(input$m.t4 < 0) text(0.5, input$b.t4+.3, '1')
    if(input$m.t4 > 0) text(0.5, input$b.t4-.3, '1')
    if(input$m.t4 != 0) text(1.5, input$b.t4 + input$m.t4/2, input$m.t4, col=4, cex=1.5)
    arrows(0, input$b.t4, -3.1, input$b.t4, col=2, length=.1, lwd=2)
    text(-2.8, input$b.t4+.3, input$b.t4, col=2, cex=1.5)
    }, height = function() {
    session$clientData$output_plot.t4_width
    })
}