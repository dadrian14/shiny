#inputs and outputs have suffix .t9

sattherthwaite <- function(s1, s2, n1, n2)
{
  (s1^2/n1 + s2^2/n2)^2 / 
    ((s1^2/n1)^2 / (n1-1) + (s2^2/n2)^2 / (n2-1))
}

make.ci <- function(xbar1, xbar2, s1, s2, n1, n2, conf)
{
  df <- sattherthwaite(s1, s2, n1, n2)
  tstar <- qt( (1-conf)/2, df=df, lower.tail=F)
  se <- sqrt(s1^2/n1 + s2^2/n2)
  low <- (xbar1 - xbar2) - tstar * se
  high <- (xbar1 - xbar2) + tstar * se
  c(low, high)
}


ci_ui <- function(){
  fluidPage(
  fluidRow(wellPanel(strong(h2(HTML(
    'Confidence interval for difference in population means 
    &mu;<sub>1</sub> - &mu;<sub>2</sub>'
  ))), 
  'Investigating how it depends on the sample means, standard deviations, 
  sample sizes, and confidence level')),
  fluidRow(
    column(4, wellPanel(
      sliderInput(inputId='conf.t9', label='Confidence level (%)',
                  min=80, max=99.5, value=95, step=.5),
      sliderInput(inputId='xbar1.t9', label=HTML('Sample 1 mean (x&#772;<sub>1</sub>)'),
                  min=0, max=10, value=5, step=.1),
      sliderInput(inputId='xbar2.t9', label=HTML('Sample 2 mean (x&#772;<sub>2</sub>)'),
                  min=0, max=10, value=3, step=.1),
      sliderInput(inputId='n.t9', label=HTML('Sample sizes (n<sub>1</sub> = n<sub>2</sub>)'), 
                  min=2, max=40, value=10, step=1),
      sliderInput(inputId='s.t9', label=
                    HTML('Sample std devs (s<sub>1</sub> = s<sub>2</sub>)'), 
                  min=1, max=10, value=3, step=.1)
    )),
    column(8, wellPanel(
    plotOutput('ciplot.t9', height="150px")
    ), hr(),
    column(6, wellPanel(
      h4(strong('Confidence interval formula')),
      p(style="text-align:center", tags$img(width='200px', src='ci_formula.PNG'))
    )),
    column(6, wellPanel(
      h4(strong('Parts of formula')), 
      'Difference in means', verbatimTextOutput('center.txt'), 
      'Margin of error', verbatimTextOutput('me.txt'),
      'Confidence level multiplier (t*)', verbatimTextOutput('tstar.txt'), 
      'Standard error', verbatimTextOutput('se.txt')
    ))
    )) 
    
  )
  
}

ci_server <- function(input, output) {
  center <- reactive({input$xbar1.t9 - input$xbar2.t9})
  tstar <- reactive({
    qt( (1-input$conf.t9/100)/2, df=input$n.t9-1, lower.tail=F)
  })
  se <- reactive({
    sqrt(2 * input$s.t9^2/input$n.t9)
  })
  output$center.txt <- renderText({
    round(center(), 1)
  })
  output$tstar.txt <- renderText({
    round(tstar(), 3)
  })
  output$se.txt <- renderText({
    round(se(), 3)
  })
  output$me.txt <- renderText({
    round(tstar() * se(), 3)
  })
  output$ciplot.t9 <- renderPlot({
    ci <- center() + c(-1, 1) * tstar() * se()
    par(mar=c(2, .5, 4, .5), cex.main=2, cex.lab=1.7, cex.axis=1.5) 
    col <-  ifelse(ci<0, 2, 4)
    plot(x=ci, y=c(0,0), xlim=c(-20, 20), ylim=c(-1, 1), pch=c('[', ']'), 
         cex=3, yaxt='n', xlab='', col=col, xaxt='n',
         main=substitute('CI for '*mu[1]-mu[2]*': ('*a*', '*b*')',
                         list(a=round(ci[1], 3), b=round(ci[2], 3))))
    points(x=input$xbar1.t9-input$xbar2.t9, y=0, pch='+', cex=3)
    axis(side=1, at=seq(-20, 20, by=5))
    #segments(ci[1], 0, ci[2], 0, lty=2)
    abline(v=(-20:20)[-21], lty=3, col=grey(.6))
    abline(v=0)
    if(ci[2] > 21) text(21, .5, 'Off-screen', pos=2)
    if(ci[1] < -21) text(-21, .5, 'Off-screen', pos=4)
  })
}