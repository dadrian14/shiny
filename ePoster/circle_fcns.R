#inputs and output have suffix .t5

circle_ui <- function()
{
  fluidPage(
  wellPanel(
    tags$h2(strong('Equation of a Circle:  '), 
      tags$i(HTML('(x - x<sub>0</sub>)<sup>2</sup> + (y - y<sub>0</sub>)<sup>2</sup>
            = r<sup>2</sup>')))
    ),
  fluidRow(
    column(3, wellPanel(
      h2(strong('Manipulation')),
      h4(HTML('Change the <strong>center (x<sub>0</sub>, y<sub>0</sub>)</strong>')),
      sliderInput('x0.t5', label=HTML('x<sub>0</sub>'), min=-3, max=3, 
                  value=0, step=.1),
      sliderInput('y0.t5', label=HTML('y<sub>0</sub>'), min=-3, max=3, 
                  value=0, step=.1), 
      hr(),
      h4('Change the', strong('radius r')),
      sliderInput('r.t5', label=NULL, min=.5, max=3, value=1, step=.1)
    )), 
    column(5, wellPanel(
      plotOutput('plot.t5', height="auto")
    )), 
    column(4, wellPanel(
      h2(strong('Explanation')),
      p(strong('Black line:'), 'Plot of the circle.'),
      p(strong(tags$span(style="color: red;",'Red point:')), 
                  'location of the', strong('center'), 'of the circle', 
                  HTML('<strong>(x<sub>0</sub>, y<sub>0</sub>)</strong>.')),
      p(strong(tags$span(style="color: blue;", 'Blue arrow:')), 
        'length of the', strong('radius r.'))
      )   
    ))
  )
}

circle_server <- function(input, output, session) {
  output$plot.t5 <- renderPlot({
    theta <- seq(-pi, pi, by=pi/100)
    x <- input$r.t5 * cos(theta) + input$x0.t5
    y <- input$r.t5 * sin(theta) + input$y0.t5
    par(cex.main=2.5, cex.lab=1.3, cex.axis=1.1)
    plot(x=NULL, y=NULL, xlim=c(-6, 6), ylim=c(-6, 6), xlab='x', ylab='y', 
         main=substitute((x - x0)^2 + (y-y0)^2*' = '*r^2, 
                         list(x0=input$x0.t5, y0=input$y0.t5, r=input$r.t5)))
    abline(h=0, col=grey(.6))
    abline(v=0, col=grey(.6))
    arrows(input$x0.t5, input$y0.t5, input$x0.t5 + input$r.t5, input$y0.t5, 
           length=.1, col=4, lwd=2)
    points(input$x0.t5, input$y0.t5, col=2, pch=16, cex=1.5)
    abline(h=(-6:6)[-7], lty=3, col=grey(.8))
    abline(v=(-6:6)[-7], lty=3, col=grey(.8))
    lines(x, y, type='l', lwd=2)
  }, height = function() {
    session$clientData$output_plot.t5_width
  })
  
}