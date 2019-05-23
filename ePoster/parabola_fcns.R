#inputs and output have suffix .t6

parabola_ui <- function()
{
  fluidPage(
  wellPanel(
    tags$h2(strong('Equation of a Parabola:  '),
      tags$i(HTML('y = a(x - x<sub>0</sub>)<sup>2</sup> + y<sub>0</sub>')))
  ),
  fluidRow(
    column(3, wellPanel(
      h2(strong('Manipulation')),
      h4('Change the', HTML('<strong>vertex: (x<sub>0</sub>, y<sub>0</sub>)</strong>')),
      sliderInput('x0.t6', label=HTML('x<sub>0</sub>'), min=-8, max=8, 
                  value=0, step=.1),
      sliderInput('y0.t6', label=HTML('y<sub>0</sub>'), min=-8, max=8, 
                  value=0, step=.1), 
      hr(), 
      h4('Change the', strong('steepness: a')), 
      sliderInput('a.t6', label=NULL, min=-3, max=3, 
                  value=1, step=.1)
    )),
    column(5, wellPanel(
      plotOutput('plot.t6', height="auto")
    )), 
    column(4, wellPanel(
      h2(strong('Explanation')),
      p(strong('Black line:'), 'plot of parabola'), 
      p(strong(tags$span(style="color: red;", 'Red point:')), 
        'location of the', strong('vertex', 
                                  HTML('(x<sub>0</sub>, y<sub>0</sub>).'))),
      p(strong(tags$span(style="color: blue;", 'Blue line:')), 
        'line of symmetry, x = ', HTML('x<sub>0</sub>'))
      )
    ))
  )
}

parabola_server <- function(input, output, session) {
  output$plot.t6 <- renderPlot({
    x <- seq(-10, 10, by=.05)
    y <- input$a.t6 * (x - input$x0.t6)^2 + input$y0.t6
    par(mar=c(4, 4, 4, 1), cex.main=2.5, cex.lab=1.5, cex.axis=1.1)
    plot(x, y, type='l', xlim=c(-10, 10), ylim=c(-10, 10), lwd=2, 
         main=substitute('y = '*a*(x-x0)^2+y0, 
                         list(a=input$a.t6, x0=input$x0.t6, y0=input$y0.t6)))
    abline(h=(-10:10)[-11], lty=3, col=grey(.8))
    abline(v=(-10:10)[-11], lty=3, col=grey(.8))
    abline(h=0, col=grey(.7))
    abline(v=0, col=grey(.7))
    points(input$x0.t6, input$y0.t6, col=2, pch=16, cex=1.5)
    abline(v=input$x0.t6, col=4, lty=2, lwd=2)
  }, height = function() {
    session$clientData$output_plot.t6_width
  })
}
