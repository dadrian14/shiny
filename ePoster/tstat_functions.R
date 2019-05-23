#inputs and output have suffix .t1

tstat <- function(xbar1, xbar2, s, n)
{
  (xbar1 - xbar2) / sqrt(2*s^2/n)
}

tstat_ui <- function(){
  fluidPage(
  fluidRow(wellPanel(
                     h2(strong('The two-sample t test statistic')), 
                     p('Investigating how it depends on the summary statistics
                       changed with the sliders in the left panel.')
          )),
  fluidRow(
    column(3, wellPanel(
      h3(strong('Summary statistics')),
      p('For simplicity, we will assume the standard deviations 
          and sample sizes of both groups are equal, i.e.', 
        HTML('s<sub>1</sub> = s<sub>2</sub> and 
             n<sub>1</sub> = n<sub>2</sub>.')),
      sliderInput('xbar1.t1', label=HTML('Sample 1 mean (x&#772;<sub>1</sub>)'), min=0, 
                  max=10, value=5, step=.1),
      sliderInput('xbar2.t1', label=HTML('Sample 2 mean (x&#772;<sub>2</sub>)'), min=0, 
                  max=10, value=3, step=.1),
      hr(), 
      sliderInput(inputId='s.t1', label=
        HTML('Sample std devs (s<sub>1</sub> = s<sub>2</sub>)'), 
                  min=1, max=10, value=3, step=.1),
      hr(),
      sliderInput(inputId='n.t1', label=
                    HTML('Sample sizes (n<sub>1</sub> = n<sub>2</sub>)'), 
                  min=5, max=200, value=30, step=5)
    )),
    column(4, wellPanel(
      h3(strong('Test statistic')), 
      plotOutput(outputId='tplot.t1', height="140px"), 
      hr(),
      h3(strong('Test statistic formula')),
      p(style="text-align:center", tags$img(width='150px', src='t_formula.PNG'))
    )),
    column(4, wellPanel(
    h3(strong('Analysis')),
      h4(strong('Two parts of formula:')), 
      tags$ul(
        tags$li(strong('Numerator:'), 'difference in means'), 
        verbatimTextOutput('diff.t1'), 
        tags$ul(
          tags$li('Which mean is greater determines sign of the test statistic'),
          tags$li('Distance between means determines size of test statistic')
          ),
        tags$li(strong('Denominator:'), 'standard error'),
        verbatimTextOutput('se.t1'), 
        tags$ul(
          tags$li('Inverse relationship with test statistic'), 
          tags$li('Larger standard deviations -> larger standard error'),
          tags$li('Larger sample sizes -> smaller standard error')
          )
        )
      )
    ))
  )
}


tstat_serv <- function(input, output){
	output$tplot.t1 <- renderPlot({
    par(mar=c(2.5, .5, 2, .5), cex.main=2, cex.lab=1.7, cex.axis=1.5)
    t.obs <- tstat(input$xbar1.t1, input$xbar2.t1, input$s.t1, input$n.t1)
    plot(x=NULL, y=NULL, xlim=c(-10, 10), ylim=c(-1, 1), yaxt='n', xlab='',
         main=substitute('t = '*t.obs, list(t.obs=round(t.obs, 2))))
    abline(v=seq(-10, 10, by=1), col=grey(.8))
    abline(v=0)
    col <- ifelse(t.obs > 0, 4, 2)
    if(t.obs!=0) arrows(0, 0, t.obs, 0, length=.1, lwd=3, col=col)
    if(t.obs==0) points(0, 0 , pch=16)
    if(t.obs < -11) text(-11, .5, 'Off-screen', pos=4)
    if(t.obs > 11) text(11, .5, 'Off-screen', pos=2)
  })
  output$diff.t1 <- renderPrint(input$xbar1.t1 - input$xbar2.t1)
  output$se.t1 <- renderPrint(
    round(sqrt(2*input$s.t1^2/input$n.t1), 2)
  )
}