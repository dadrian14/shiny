#Unit Circle (Trigonometry)
library(shiny); library(plotrix)
ui <- fluidPage(
  fluidRow(wellPanel(
    h1(strong('The Unit Circle')), 
    h3('Illustrating the sine and cosine functions')
  )),
  fluidRow(
    column(3, wellPanel(
      h2(strong('Manipulation')),
      p('Change the angle', HTML('&theta;'), 'in radians from the positive x-axis', 
        'between 0 and', HTML('2&pi;')),
      tags$ul(tags$li(strong("Manually:"), 'Use the slider'), 
              tags$li(strong('Animate:'), 'Push the play button')), 
      sliderInput('theta', label=HTML('Angle &theta;'), min=0, max=6.28, 
                  value=1, step=.05, 
                  animate=animationOptions(interval=200, loop=T))
    )), 
    column(5, wellPanel(
      h2(strong('The Unit Circle')),
      plotOutput('uc', height='auto')
    )), 
    column(4, wellPanel(
      h2(strong('Sine and cosine functions')),
      h4(HTML('<strong>sin(&theta;)</strong>')), 
        p('length of the',  strong('vertical blue line'), 'in the unit circle.'),
      plotOutput('sin', height='200px'), 
      #hr(),
      h4(HTML('<strong>cos(&theta;)</strong>')), 
      p('length of the', strong('horizontal red line'), 'in the unit circle.'),
      plotOutput('cos', height='200px')
    ))
  )
)

server <- function(input, output, session) {
  output$uc <- renderPlot({
    par(mar=c(4, 4, .5, .5))
    rad <- seq(0, 2*pi, by=pi/100)
    plot(cos(rad), sin(rad), type='l', xlab='x', ylab='y')
    segments(0,0, cos(input$theta), sin(input$theta))
    segments(0,0, cos(input$theta), 0, col=2)
    segments(cos(input$theta), 0, cos(input$theta), sin(input$theta), col=4)
    if(input$theta>=pi/2 & input$theta<=3*pi/2)
      segments(0,0, .2, 0)
    draw.arc(x=0, y=0, radius=.1, angle1=0, angle2=input$theta)
    text(x=.15, y=.04, expression(theta))
  }, height = function() {
    session$clientData$output_uc_width
  })
  output$sin <- renderPlot({
    par(mar=c(4, 4, .5, .5))
    rad <- seq(0, 2*pi, by=pi/100)
    plot(rad, sin(rad), type='l', xaxt='n', ylab=expression(sin(theta)), 
         col=4, xlab=expression(theta))
    axis(side=1, at=0:4*pi/2, labels=c(0, expression(frac(pi,2)), expression(pi),
              expression(frac(3*pi,2)), expression(2*pi)), padj=.5)
    points(input$theta, sin(input$theta), pch=16, col=4)
    abline(h=0, col=grey(.8), lty=3)
    segments(x0=input$theta, y0=0, y1=sin(input$theta), col=4)
  })
  output$cos <- renderPlot({
    par(mar=c(4, 4, .5, .5))
    rad <- seq(0, 2*pi, by=pi/100)
    plot(rad, cos(rad), type='l', xaxt='n', ylab=expression(cos(theta)), 
         col=2, xlab=expression(theta))
    axis(side=1, at=0:4*pi/2, labels=c(0, expression(frac(pi,2)), expression(pi),
                                       expression(frac(3*pi,2)), expression(2*pi)), padj=.5)
    points(input$theta, cos(input$theta), pch=16, col=2)
    abline(h=0, col=grey(.8), lty=3)
    segments(x0=input$theta, y0=0, y1=cos(input$theta), col=2)
  })
}

shinyApp(ui = ui, server = server)