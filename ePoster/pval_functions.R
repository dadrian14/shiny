#inputs and output have suffix .t2

pval_ui <- function(){
  fluidPage(
    fluidRow(wellPanel(style = "background-color: #ffffff;",
                       h2(strong('P-value for two sample t-test')), 
                       'How it depends on the alternative hypotheses, 
                       the test statistic, and the degrees of freedom'
                       )),
  fluidRow(
    column(4, wellPanel(style = "background-color: #ffffff;",
          h3(strong('Manipulation')), 
          radioButtons(inputId='Ha.t2', label=
                         HTML('Alternative hypothesis (H<sub>a</sub>)'), 
                       inline=F,
                       choices=c('mu1 > mu2', 'mu1 < mu2', 'mu1 =/= mu2')),
          hr(),
          sliderInput(inputId='t.t2', label='Test statistic t', 
                      min=-3, max=3, value=1, step=.01),
          hr(),
          
          sliderInput(inputId='df.t2', label='Degrees of freedom (df)', 
                      min=1, max=30, value=5, step=1)
    )), 
    column(6, wellPanel(style = "background-color: #ffffff;",
      h3(strong('P-value')),
      verbatimTextOutput('pval.t2'), 
      hr(),
      h4(strong('What to enter in TI calculator')),
      verbatimTextOutput('TI.t2'),
      hr(),
      h4(strong('Picture:')),
      tags$ul(
        tags$li('t distribution with', textOutput('df.t2', inline=T), 
                'degrees of freedom'), 
        tags$li('p-value: shaded region')
      ),
      plotOutput('picture.t2', height='300px')
    ))
  )
)}

pval_serv <- function(input, output)
{
	output$df.t2 <- renderText(input$df.t2)
  output$pval.t2 <- renderPrint({
    if(input$Ha.t2 == 'mu1 > mu2') pval <- pt(input$t.t2, df=input$df.t2, 
                                           lower.tail=F)
    if(input$Ha.t2 == 'mu1 < mu2') pval <- pt(input$t.t2, df=input$df.t2, 
                                           lower.tail=T)
    if(input$Ha.t2 == 'mu1 =/= mu2') pval <- 2*pt(-abs(input$t.t2), 
                                               df=input$df.t2)
    pval
  })
  output$TI.t2 <- renderPrint({
    if(input$Ha.t2 == 'mu1 > mu2') text <- paste0('tcdf(', input$t.t2,', 9999, ',
                                       input$df.t2, ')')
    if(input$Ha.t2 == 'mu1 < mu2') text <- paste0('tcdf(-9999, ', input$t.t2, ', ',
                                       input$df.t2, ')')
    if(input$Ha.t2 == 'mu1 =/= mu2') text <- paste0('2*tcdf(', 
                                    abs(input$t.t2),', 9999, ', input$df.t2, ')')
    text
  })
  output$picture.t2 <- renderPlot({
    x <- seq(-4, 4, by=.1)
    df <- input$df.t2
    par(mar=c(4, .5, .5, .5), cex.main=2, cex.lab=1.7, cex.axis=1.5)
    plot(x, dt(x, df), type='l', lwd=1, xlab='test statistic t', xaxt='n', yaxt='n', ylab='')
    if(input$Ha.t2 == 'mu1 > mu2' || input$Ha.t2 == 'mu1 < mu2')
      axis(side=1, at=input$t.t2)
    else
      axis(side=1, at=c(-abs(input$t.t2), abs(input$t.t2)))
    if(input$Ha.t2 == 'mu1 > mu2'){ 
      t.seq <- seq(input$t.t2, 4, by=.01)
      polygon(c(t.seq, 4, input$t.t2), c(dt(t.seq, df=df), 0, 0), col=grey(.6))
    }
    if(input$Ha.t2 == 'mu1 < mu2'){ 
      t.seq <- seq(-4, input$t.t2, by=.01)
      polygon(c(t.seq, input$t.t2, -4), c(dt(t.seq, df=df), 0, 0), col=grey(.6))
    }
    if(input$Ha.t2 == 'mu1 =/= mu2'){
      t.temp <- abs(input$t.t2)
      t.neg <- seq(-4, -t.temp, by=.01)
      polygon(c(t.neg, -t.temp, -4), c(dt(t.neg, df=df),0,0), col=grey(.6))
      t.pos <- seq(t.temp, 4, by=.01)
      polygon(c(t.pos, 4, t.temp), c(dt(t.pos, df=df), 0,0), col=grey(.6))
    }
  })
}