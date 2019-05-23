#P-value
library(shiny)
ui <- fluidPage(
  fluidRow(
    column(4, wellPanel(style = "background-color: #ffffff;",
          h3(strong('Manipulation')), 
          radioButtons(inputId='Ha', label=
                         HTML('Alternative hypothesis (H<sub>a</sub>)'), 
                       inline=F,
                       choices=c('mu1 > mu2', 'mu1 < mu2', 'mu1 =/= mu2')),
          hr(),
          sliderInput(inputId='t', label='Test statistic t', 
                      min=-3, max=3, value=1, step=.01),
          hr(),
          
          sliderInput(inputId='df', label='Degrees of freedom (df)', 
                      min=1, max=30, value=5, step=1)
    )), 
    column(6, wellPanel(style = "background-color: #ffffff;",
      h3(strong('P-value')),
      verbatimTextOutput('pval'), 
      hr(),
      h4(strong('What to enter in TI calculator')),
      verbatimTextOutput('TI'),
      hr(),
      h4(strong('Picture:')),
      tags$ul(
        tags$li('t distribution with', textOutput('df', inline=T), 
                'degrees of freedom'), 
        tags$li('p-value: shaded region')
      ),
      plotOutput('picture', height='300px')
    ))
  )
)

server <- function(input, output) {
  output$df <- renderText(input$df)
  output$pval <- renderPrint({
    if(input$Ha == 'mu1 > mu2') pval <- pt(input$t, df=input$df, 
                                           lower.tail=F)
    if(input$Ha == 'mu1 < mu2') pval <- pt(input$t, df=input$df, 
                                           lower.tail=T)
    if(input$Ha == 'mu1 =/= mu2') pval <- 2*pt(-abs(input$t), 
                                               df=input$df)
    pval
  })
  output$TI <- renderPrint({
    if(input$Ha == 'mu1 > mu2') text <- paste0('tcdf(', input$t,', 9999, ',
                                       input$df, ')')
    if(input$Ha == 'mu1 < mu2') text <- paste0('tcdf(-9999, ', input$t, ', ',
                                       input$df, ')')
    if(input$Ha == 'mu1 =/= mu2') text <- paste0('2*tcdf(', 
                                    abs(input$t),', 9999, ', input$df, ')')
    text
  })
  output$picture <- renderPlot({
    x <- seq(-4, 4, by=.1)
    df <- input$df
    par(mar=c(4, .5, .5, .5), cex.main=2, cex.lab=1.7, cex.axis=1.5)
    plot(x, dt(x, df), type='l', lwd=1, xlab='test statistic t', xaxt='n', yaxt='n', ylab='')
    if(input$Ha == 'mu1 > mu2' || input$Ha == 'mu1 < mu2')
      axis(side=1, at=input$t)
    else
      axis(side=1, at=c(-abs(input$t), abs(input$t)))
    if(input$Ha == 'mu1 > mu2'){ 
      t.seq <- seq(input$t, 4, by=.01)
      polygon(c(t.seq, 4, input$t), c(dt(t.seq, df=df), 0, 0), col=grey(.6))
    }
    if(input$Ha == 'mu1 < mu2'){ 
      t.seq <- seq(-4, input$t, by=.01)
      polygon(c(t.seq, input$t, -4), c(dt(t.seq, df=df), 0, 0), col=grey(.6))
    }
    if(input$Ha == 'mu1 =/= mu2'){
      t.temp <- abs(input$t)
      t.neg <- seq(-4, -t.temp, by=.01)
      polygon(c(t.neg, -t.temp, -4), c(dt(t.neg, df=df),0,0), col=grey(.6))
      t.pos <- seq(t.temp, 4, by=.01)
      polygon(c(t.pos, 4, t.temp), c(dt(t.pos, df=df), 0,0), col=grey(.6))
    }
  })
}

shinyApp(ui = ui, server = server)