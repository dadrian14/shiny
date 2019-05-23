#inputs and output have suffix .t2

pval_ui <- function(){
  fluidPage(
    fluidRow(wellPanel(
                       h2(strong('P-value for two sample t-test')), 
                       'How it depends on the alternative hypotheses, 
                       the test statistic, and the degrees of freedom'
                       )),
  fluidRow(
    column(4, wellPanel(
          radioButtons(inputId='Ha.t2', label=
                         h4(HTML('Alternative hypothesis (H<sub>a</sub>)')), 
                       inline=F,
                       choiceNames=list(HTML('H<sub>a</sub>: &mu;<sub>1</sub> > &mu;<sub>2</sub>'), 
                                        HTML('H<sub>a</sub>: &mu;<sub>1</sub> < &mu;<sub>2</sub>'), 
                                        HTML('H<sub>a</sub>: &mu;<sub>1</sub> &ne; &mu;<sub>2</sub>')),
                       choiceValues=list('mu1 > mu2', 'mu1 < mu2', 'mu1 =/= mu2')),
          hr(),
          sliderInput(inputId='t.t2', label=h4('Test statistic t'), 
                      min=-4, max=4, value=1, step=.01),
          hr(),
          radioButtons('df_input', label=h4('Input type for df'), 
                       choiceNames = list('Slider', 'Text Input'), 
                       choiceValues = list(1, 2)),
          hr(),
          conditionalPanel('input.df_input == 1',
                           sliderInput(inputId='slider.df', label=h4('Degrees of Freedom (df)'), 
                                       min=1, max=30, value=5, step=1)),
          conditionalPanel('input.df_input == 2', 
                           numericInput('text.df', label=h4('Degrees of Freedom (df)'), 
                                        value=5, min=1, step=1), 
                           'Required: df > 0')
    )), 
    column(8, wellPanel(
      h4(strong('P-value')),
      verbatimTextOutput('pval.t2'), 
      hr(),
      h4(strong('What to enter in TI calculator')),
      verbatimTextOutput('TI.t2'),
      hr(),
      h4(strong('Picture:')),
      plotOutput('picture.t2', height='300px'),
      br(),
      tags$ul(
        tags$li('t distribution with', textOutput('df.t2', inline=T), 
                'degrees of freedom'), 
        tags$li('p-value: shaded region')
      )
    ))
  )
)}

pval_serv <- function(input, output)
{
	df.t2 <- reactive({
	  ifelse(input$df_input==1, input$slider.df, input$text.df)
	})  
	output$df.t2 <- renderText({
	  if(is.na(df.t2())) return('____')
	  else return(df.t2())
	  })
  output$pval.t2 <- renderText({
    if(is.na(df.t2())) return('ENTER DF')
    else if(df.t2()<=0) return('DF must be positive')
    if(input$Ha.t2 == 'mu1 > mu2') pval <- pt(input$t.t2, df=df.t2(), 
                                           lower.tail=F)
    if(input$Ha.t2 == 'mu1 < mu2') pval <- pt(input$t.t2, df=df.t2(), 
                                           lower.tail=T)
    if(input$Ha.t2 == 'mu1 =/= mu2') pval <- 2*pt(-abs(input$t.t2), 
                                               df=df.t2())
    round(pval,5)
  })
  output$TI.t2 <- renderText({
    if(is.na(df.t2())) return('ENTER DF')
    else if(df.t2()<=0) return('DF must be positive')
    if(input$Ha.t2 == 'mu1 > mu2') text <- paste0('tcdf(', input$t.t2,', 99, ',
                                                  df.t2(), ')')
    if(input$Ha.t2 == 'mu1 < mu2') text <- paste0('tcdf(-99, ', input$t.t2, ', ',
                                                  df.t2(), ')')
    if(input$Ha.t2 == 'mu1 =/= mu2') text <- paste0('2*tcdf(', 
                                    abs(input$t.t2),', 99, ', df.t2(), ')')
    text
  })
  output$picture.t2 <- renderPlot({
    x <- seq(-4.3, 4.3, by=.1)
    df <- df.t2()
    par(mar=c(4, .5, .5, .5), cex.main=2, cex.lab=1.7, cex.axis=1.5)
    if(is.na(df.t2())){
      plot(x=0,y=0,xaxt='n', xlab='', yaxt='n', type='n')
      text(0,0,'Enter df', col=2, cex=3)
    }
    else if(df.t2()<0){
      plot(x=0,y=0,xaxt='n', xlab='', yaxt='n', type='n')
      text(0,0,'Required: df > 0', col=2, cex=3)
    }
    else{
    plot(x, dt(x, df), type='l', lwd=1, xlab='test statistic t', xaxt='n', yaxt='n', ylab='')
    if(input$Ha.t2 == 'mu1 > mu2' || input$Ha.t2 == 'mu1 < mu2')
      axis(side=1, at=input$t.t2)
    else
      axis(side=1, at=c(-abs(input$t.t2), abs(input$t.t2)))
    if(input$Ha.t2 == 'mu1 > mu2'){ 
      t.seq <- seq(input$t.t2, 4.3, by=.01)
      polygon(c(t.seq, 4.3, input$t.t2), c(dt(t.seq, df=df), 0, 0), col=grey(.6))
    }
    if(input$Ha.t2 == 'mu1 < mu2'){ 
      t.seq <- seq(-4.3, input$t.t2, by=.01)
      polygon(c(t.seq, input$t.t2, -4.3), c(dt(t.seq, df=df), 0, 0), col=grey(.6))
    }
    if(input$Ha.t2 == 'mu1 =/= mu2'){
      t.temp <- abs(input$t.t2)
      t.neg <- seq(-4.3, -t.temp, by=.01)
      polygon(c(t.neg, -t.temp, -4.3), c(dt(t.neg, df=df),0,0), col=grey(.6))
      t.pos <- seq(t.temp, 4.3, by=.01)
      polygon(c(t.pos, 4.3, t.temp), c(dt(t.pos, df=df), 0,0), col=grey(.6))
    }
    }
  })
}