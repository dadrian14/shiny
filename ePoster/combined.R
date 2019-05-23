#combined
tstat <- function(xbar1, xbar2, s, n)
{
  (xbar1 - xbar2) / sqrt(2*s^2/n)
}

sattherthwaite <- function(s1, s2, n1, n2)
{
  (s1^2/n1 + s2^2/n2)^2 / 
    ((s1^2/n1)^2 / (n1-1) + (s2^2/n2)^2 / (n2-1))
}

library(shiny)
ui <- fluidPage(
  fluidRow(
    column(3, wellPanel(style = "background-color: #ffffff;",
                h2(style="text-align:center", strong('Summary statistics')),
                hr(),
                p('For simplicity, we will assume',
                  HTML('s<sub>1</sub> = s<sub>2</sub> and 
                  n<sub>1</sub> = n<sub>2</sub>.')),
                        sliderInput('xbar1', label=HTML('Sample 1 mean (x&#772;<sub>1</sub>)'), min=0, 
                                    max=10, value=5, step=.1),
                        sliderInput('xbar2', label=HTML('Sample 2 mean (x&#772;<sub>2</sub>)'), min=0, 
                                    max=10, value=3, step=.1),
                        hr(), 
                        sliderInput(inputId='s', label=
                                      HTML('Sample std devs (s<sub>1</sub> = s<sub>2</sub>)'), 
                                    min=1, max=10, value=3, step=.1),
                        hr(),
                        sliderInput(inputId='n', label=
                                      HTML('Sample sizes (n<sub>1</sub> = n<sub>2</sub>)'), 
                                    min=5, max=200, value=30, step=5)
    )), 
    column(3, wellPanel(style = "background-color: #ffffff;",
                        h2(style="text-align:center", strong('Test statistic')), 
                        hr(),
                        p(style="text-align:center", tags$img(width='125px', src='t_formula.PNG')),
                        hr(),
                        h4(strong('Two parts of formula:')), 
                        tags$ul(
                          tags$li(strong('Numerator:'), 'difference in means'), 
                          verbatimTextOutput('diff'), 
                          tags$li(strong('Denominator:'), 'standard error'),
                          verbatimTextOutput('se')
                        ),
                        hr(),
                        h4(strong('Value of test statistic')),
                        plotOutput(outputId='tplot', height="140px")
    )), 
    column(6, wellPanel(style = "background-color: #ffffff;",
                        h2(style="text-align:center", strong('P-value')), 
                        hr(),
            fluidRow(column(4, wellPanel(style = "background-color: #ffffff;",
                        radioButtons(inputId='Ha', label=
                                       HTML('Alternative hypothesis (H<sub>a</sub>)'), 
                                     inline=F,
                                     choices=c('mu1 > mu2', 'mu1 < mu2', 'mu1 =/= mu2')),
                        hr(),
                        h5(strong('Degrees of Freedom (Sattherthwaite Approx)')),
                        verbatimTextOutput('df'), 
                        hr(), 
                        h4(strong('What to enter in TI calculator')),
                        verbatimTextOutput('TI') 
            )),
            column(8, wellPanel(style = "background-color: #ffffff;",
                        h4(strong('Picture:')),
                        plotOutput('picture', height='300px')
            )))
            
    ))
  )
)

server <- function(input, output) {
  output$diff <- renderPrint(input$xbar1 - input$xbar2)
  output$se <- renderPrint(
    round(sqrt(2*input$s^2/input$n), 2)
  )
  output$df <- renderPrint(round(
    sattherthwaite(input$s, input$s, input$n, input$n), 2))
  output$TI <- renderPrint({
    df <- 2*(input$n-1)
    t.obs <- round(tstat(input$xbar1, input$xbar2, input$s, input$n), 2)
    if(input$Ha == 'mu1 > mu2') text <- paste0('tcdf(', t.obs,', 9999, ',
                                               df, ')')
    if(input$Ha == 'mu1 < mu2') text <- paste0('tcdf(-9999, ', t.obs, ', ',
                                               df, ')')
    if(input$Ha == 'mu1 =/= mu2') text <- paste0('2*tcdf(', 
                                                 abs(t.obs),', 9999, ', df, ')')
    text
  })
  output$tplot <- renderPlot({
    par(mar=c(2.5, .5, 2, .5), cex.main=2, cex.lab=1.7, cex.axis=1.5)
    t.obs <- tstat(input$xbar1, input$xbar2, input$s, input$n)
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
  output$picture <- renderPlot({
    x <- seq(-4, 4, by=.1)
    df <- sattherthwaite(input$s, input$s, input$n, input$n)
    t <- tstat(input$xbar1, input$xbar2, input$s, input$n)
    if(input$Ha == 'mu1 > mu2') pval <- pt(t, df=df, lower.tail=F)
    if(input$Ha == 'mu1 < mu2') pval <- pt(t, df=df, lower.tail=T)
    if(input$Ha == 'mu1 =/= mu2') pval <- 2*pt(-abs(t), df=df)
    par(mar=c(4, .5, 2, .5), cex.main=2, cex.lab=1.7, cex.axis=1.5)
    plot(x, dt(x, df), type='l', lwd=1, xlab='test statistic t', xaxt='n', yaxt='n', ylab='')
    title(substitute('P-value = '*pval, 
                     list(pval=round(pval, 4))))
    if(t < -4) axis(side=1, at=-4.1, substitute('<-  '*t, list(t=round(t, 2))), 
                    hadj=0, tick=F)
    if(t > 4) axis(side=1, at=4.1, substitute(t*'  ->', list(t=round(t, 2))), 
                   hadj=1, tick=F)
    axis(side=1, at=round(t, 2))
    if(input$Ha == 'mu1 > mu2'){ if(t < 4){ 
      t.start <- round(max(t, -4), 2)
      t.seq <- seq(t.start, 4, by=.01)
      polygon(c(t.seq, 4, t.start), c(dt(t.seq, df=df), 0, 0), col=grey(.6))
    }}
    if(input$Ha == 'mu1 < mu2'){ if(t > -4){ 
      t.start <- round(min(t, 4), 2)
      t.seq <- seq(-4, t.start, by=.01)
      polygon(c(t.seq, t.start, -4), c(dt(t.seq, df=df), 0, 0), col=grey(.6))
    }}
    if(input$Ha == 'mu1 =/= mu2'){ if(abs(t) < 4){
      t.temp <- round(abs(t), 2)
      t.neg <- seq(-4, -t.temp, by=.01)
      polygon(c(t.neg, -t.temp, -4), c(dt(t.neg, df=df),0,0), col=grey(.6))
      t.pos <- seq(t.temp, 4, by=.01)
      polygon(c(t.pos, 4, t.temp), c(dt(t.pos, df=df), 0,0), col=grey(.6))
    }}
  })
}

shinyApp(ui = ui, server = server)