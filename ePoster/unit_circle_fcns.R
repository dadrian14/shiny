#inputs and outputs use .t7

draw.arc <- function(r, end){
  rad <- seq(0, end, by=.05)
  lines(r*cos(rad), r*sin(rad))
}

unit_circle_ui <- function(){
  fluidPage(
  fluidRow(wellPanel(
    h2(strong('The Unit Circle: '), 'Illustrating the sine and cosine functions')
  )),
  fluidRow(
    column(3, wellPanel(
      h2(strong('Manipulation')),
      p('Change the angle', HTML('&theta;'), 'in radians from the positive x-axis', 
        'between 0 and', HTML('2&pi;')),
      tags$ul(tags$li(strong("Manually:"), 'Use the slider'), 
              tags$li(strong('Animate:'), 'Push the play button below the slider')), 
      sliderInput('theta.t7', label=HTML('Angle &theta;'), min=0, max=6.28, 
                  value=1, step=.05, 
                  animate=animationOptions(interval=200, loop=T))
    )), 
    column(5, wellPanel(
      h2(strong('The Unit Circle')),
      plotOutput('uc.t7', height='auto')
    )), 
    column(4, wellPanel(
      h2(strong('Sine and cosine functions')),
      h4(HTML('<strong>sin(&theta;)</strong>')), 
      p('length of the',  strong('vertical blue line'), 'in the unit circle.'),
      plotOutput('sin.t7', height='200px'), 
      #hr(),
      h4(HTML('<strong>cos(&theta;)</strong>')), 
      p('length of the', strong('horizontal red line'), 'in the unit circle.'),
      plotOutput('cos.t7', height='200px')
    ))
  )
)}


unit_circle_server <- function(input, output, session) {
  output$uc.t7 <- renderPlot({
    par(mar=c(4, 4, .5, .5))
    rad <- seq(0, 2*pi, by=pi/100)
    plot(cos(rad), sin(rad), type='l', xlab='x', ylab='y')
    segments(0,0, cos(input$theta.t7), sin(input$theta.t7))
    segments(0,0, cos(input$theta.t7), 0, col=2)
    segments(cos(input$theta.t7), 0, cos(input$theta.t7), sin(input$theta.t7), col=4)
    if(input$theta.t7>=pi/2 & input$theta.t7<=3*pi/2)
      segments(0,0, .2, 0)
    draw.arc(.1, input$theta.t7)
    text(x=.15, y=.04, expression(theta))
  }, height = function() {
    session$clientData$output_uc.t7_width
  })
  output$sin.t7 <- renderPlot({
    par(mar=c(4, 4, .5, .5))
    rad <- seq(0, 2*pi, by=pi/100)
    plot(rad, sin(rad), type='l', xaxt='n', ylab=expression(sin(theta)), 
         col=4, xlab=expression(theta))
    axis(side=1, at=0:4*pi/2, labels=c(0, expression(frac(pi,2)), expression(pi),
                                       expression(frac(3*pi,2)), expression(2*pi)), 
         padj=.5)
    points(input$theta.t7, sin(input$theta.t7), pch=16, col=4)
    abline(h=0, col=grey(.8), lty=3)
    segments(x0=input$theta.t7, y0=0, y1=sin(input$theta.t7), col=4)
  })
  output$cos.t7 <- renderPlot({
    par(mar=c(4, 4, .5, .5))
    rad <- seq(0, 2*pi, by=pi/100)
    plot(rad, cos(rad), type='l', xaxt='n', ylab=expression(cos(theta)), 
         col=2, xlab=expression(theta))
    axis(side=1, at=0:4*pi/2, labels=c(0, expression(frac(pi,2)), expression(pi),
                                       expression(frac(3*pi,2)), expression(2*pi)), 
         padj=.5)
    points(input$theta.t7, cos(input$theta.t7), pch=16, col=2)
    abline(h=0, col=grey(.8), lty=3)
    segments(x0=input$theta.t7, y0=0, y1=cos(input$theta.t7), col=2)
  })
}