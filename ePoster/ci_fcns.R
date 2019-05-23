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
  fluidRow(wellPanel(h2(HTML(
    'Confidence interval for difference in population means 
    &mu;<sub>1</sub> - &mu;<sub>2</sub>'
  )), 
  'Investigating how it depends on the sample means, standard deviations, 
  sample sizes, and confidence level')),
  fluidRow(column(8, offset=2, wellPanel(
    plotOutput('ciplot.t9', height="150px")
    ))
  ),
  fluidRow(
    column(3, wellPanel(
      h3('Confidence level (%)'), 
      sliderInput(inputId='conf.t9', label=NULL,
                  min=80, max=99.5, value=95, step=.5)
    )), 
    column(3, wellPanel(
      h3('Sample means'),
      sliderInput(inputId='xbar1.t9', label='Sample 1 mean',
                  min=0, max=10, value=5, step=.1),
      sliderInput(inputId='xbar2.t9', label='Sample 2 mean',
                  min=0, max=10, value=3, step=.1)
    )),
    column(3, wellPanel(
      h3('Sample sizes'),
      p(HTML(paste0('We will assume they are equal (n', 
                    tags$sub('1'), ' = n', tags$sub('2'), ')'))),
      sliderInput(inputId='n.t9', label=NULL, 
                  min=2, max=35, value=10, step=1)
    )), 
    column(3, wellPanel(
      h3('Sample s.d.s'),
      p(HTML(paste0('We will assume they are equal (s', 
                    tags$sub('1'), ' = s', tags$sub('2'), ')'))),
      sliderInput(inputId='s.t9', label=NULL, 
                  min=1, max=10, value=3, step=.1)
    ))
  ))
}

ci_server <- function(input, output) {
  output$ciplot.t9 <- renderPlot({
    ci <- make.ci(input$xbar1.t9, input$xbar2.t9, input$s.t9, input$s.t9, input$n.t9, 
                  input$n.t9, input$conf.t9/100)
    par(mar=c(2, .5, 4, .5), cex.main=2, cex.lab=1.7, cex.axis=1.5) 
    col <-  ifelse(ci<0, 2, 4)
    plot(x=ci, y=c(0,0), xlim=c(-20, 20), ylim=c(-1, 1), pch=c('[', ']'), 
         cex=3, yaxt='n', xlab='', col=col, xaxt='n',
         main=substitute('CI for '*mu[1]-mu[2]*': ('*a*', '*b*')',
                         list(a=round(ci[1], 2), b=round(ci[2], 2))))
    points(x=input$xbar1.t9-input$xbar2.t9, y=0, pch='+', cex=3)
    axis(side=1, at=seq(-20, 20, by=5))
    #segments(ci[1], 0, ci[2], 0, lty=2)
    abline(v=(-20:20)[-21], lty=3, col=grey(.6))
    abline(v=0)
    if(ci[2] > 21) text(21, .5, 'Off-screen', pos=2)
    if(ci[1] < -21) text(-21, .5, 'Off-screen', pos=4)
  })
}