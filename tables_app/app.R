library(shiny)
ui <- navbarPage(title='Tables for STA 215', inverse=T,
                 tabPanel('A.1', 
                          wellPanel(h3('Table A.1: Confidence level multiplier z*')), 
                          fluidRow(
                            column(4, wellPanel(
                              numericInput('zstarconf', label='Confidence level C (%)',
                                           value=95, min=0, max=100)
                            )), 
                            column(8, wellPanel(
                              h4('z* Value'), 
                              textOutput('zstar', inline=T)
                            ))
                          )),
                 tabPanel('A.2', 
                          wellPanel(h3('Table A.2: P-value for z test statistic')),
                          fluidRow(
                            column(4, wellPanel(
                              numericInput('zstat', label='z Test statistic', 
                                           value=NA, step=.01), 
                              hr(),
                              radioButtons('ztail', label='Type of test', 
                                           choiceNames=list('two-tailed', 'right-tailed', 'left-tailed'), 
                                           choiceValues=list(1,2,3))
                            )), 
                            column(8, wellPanel(
                              h4('P-value'),
                              textOutput('z_pval', inline=T)
                            ))
                          )), 
                 tabPanel('A.3', wellPanel(h3('Table A.3: Standard normal probabilities')),
                          fluidRow(
                            column(4, wellPanel(
                              numericInput('z', label='z', value=NA, step=.01)
                            )), 
                            column(8, wellPanel(
                              h4('Left-tailed Probability'),
                              textOutput('z_prob', inline=T)
                            ))
                          )),
                 tabPanel('A.4', wellPanel(h3('Table A.4: Confidence level multiplier t*')),
                          fluidRow(
                            column(4, wellPanel(
                              numericInput('tstarconf', label='Confidence level C (%)',
                                           value=95, min=0, max=100), 
                              numericInput('df_tstar', label='Degrees of freedom (DF)', 
                                           value=NA, min=1, step=1)
                            )), 
                            column(8, wellPanel(
                              h4('t* Value'), 
                              textOutput('tstar', inline=T)
                            ))
                          )),
                 tabPanel('A.5', wellPanel(h3('Table A.5: P-value for t test statistic')),
                          fluidRow(
                            column(4, wellPanel(
                              numericInput('tstat', label='t Test statistic', 
                                           value=NA, step=.01), 
                              numericInput('df_ttest', label='Degrees of freedom (DF)', 
                                           value=NA, min=1, step=1),
                              hr(),
                              radioButtons('ttail', label='Type of test', 
                                           choiceNames=list('two-tailed', 'right-tailed', 'left-tailed'), 
                                           choiceValues=list(1,2,3))
                            )), 
                            column(8, wellPanel(
                              h4('P-value'),
                              textOutput('t_pval', inline=T)
                            ))
                          )),
                 tabPanel('A.6', wellPanel(h3(HTML('Table A.6: P-value for chi-squared (&chi;<sup>2</sup>) test statistic'))),
                          fluidRow(
                            column(4, wellPanel(
                              numericInput('x2', label=HTML('Chi-squared (&chi;<sup>2</sup>) test statistic'), 
                                           value=NA, min=0, step=.01), 
                              numericInput('df_x2', label='Degrees of freedom (DF)', 
                                           value=NA, min=1, step=1)
                            )), 
                            column(8, wellPanel(
                              h4('P-value'),
                              textOutput('x2_pval', inline=T)
                            ))
                          )),
                 tabPanel('A.7', wellPanel(h3('Table A.7: P-value for F test statistic')),
                          fluidRow(
                            column(4, wellPanel(
                              numericInput('f', label='F Test statistic', 
                                           value=NA, min=0,step=.01),
                              numericInput('df1', label='Numerator df', 
                                           value=NA, min=1,step=1),
                              numericInput('df2', label='Denominator df', 
                                           value=NA, min=1,step=1)
                            )), 
                            column(8, wellPanel(
                              h4('P-value'),
                              textOutput('f_pval', inline=T)
                            ))
                          ))
)

server <- function(input, output) {
  output$zstar <- renderText({ #A.1
    if(is.na(input$zstarconf)) return('Enter confidence level C')
    else if(input$zstarconf>0 & input$zstarconf<100)
      return(round(qnorm(1-(100-input$zstarconf)/200), 3))
    else return('Invalid confidence level C')
  })
  output$z_pval <- renderText({ #A.2
    if(is.na(input$zstat)) return('Enter z test statistic')
    else{
      if(input$ztail==1) pval <- 2*pnorm(-abs(input$zstat))
      else if(input$ztail==2) pval <- 1 - pnorm(input$zstat)
      else if(input$ztail==3) pval <- pnorm(input$zstat)
      if(pval < .001) return('< 0.001')
      else if(pval > .999) return('> 0.999')
      else return(round(pval, 3))
    }
  })
  output$z_prob <- renderText({ #A.3
    if(is.na(input$z)) return('Enter z')
    else{
      prob <- pnorm(input$z)
      if(prob < .0001) return('< 0.0001')
      else if(prob > .9999) return('> 0.9999')
      else return(round(prob, 4))
    }
  })
  output$tstar <- renderText({ #A.4
    if(is.na(input$tstarconf) | is.na(input$df_tstar)) 
      return('Enter Confidence level and DF')
    else if((input$tstarconf <=0 | input$tstarconf >= 100) & input$df_tstar >0)
      return('Invalid confidence level')
    else if((input$tstarconf >0 & input$tstarconf < 100) & input$df_tstar <=0)
      return('Invalid DF')
    else if((input$tstarconf <=0 | input$tstarconf >= 100) & input$df_tstar <=0)
      return('Invalid confidence level and DF')
    else{
      tstar <- qt(1-(100-input$tstarconf)/200, df=input$df_tstar)
      return(round(tstar, 3))
    }
  })
  output$t_pval <- renderText({ #A.5
    if(is.na(input$tstat) | is.na(input$df_ttest)) 
      return('Enter t test statistic and DF')
    else if(input$df_ttest <=0)
      return('Invalid DF')
    else{
      if(input$ttail==1) pval <- 2*pt(-abs(input$tstat), df=input$df_ttest)
      else if(input$ttail==2) pval <- 1 - pt(input$tstat, df=input$df_ttest)
      else if(input$ttail==3) pval <- pt(input$tstat, df=input$df_ttest)
      if(pval < .001) return('< 0.001')
      else if(pval > .999) return('> 0.999')
      else return(round(pval, 3))
    }
  })
  output$x2_pval <- renderText({ #A.6
    if(is.na(input$x2) | is.na(input$df_x2)) 
      return('Enter chi-squared test statistic and DF')
    else if(input$x2 < 0 & input$df_x2 <=0)
      return('Invalid chi-squared test statistic and DF')
    else if(input$x2 < 0 & input$df_x2 >0)
      return('Invalid chi-squared test statistic')
    else if(input$x2 >= 0 & input$df_x2 <= 0)
      return('Invalid DF')
    else{
      pval <- 1 - pchisq(input$x2, df=input$df_x2)
      if(pval < .001) return('< 0.001')
      else return(round(pval, 3))
    }
  })
  output$f_pval <- renderText({ #A.6
    if(is.na(input$f) | is.na(input$df1) | is.na(input$df2)) 
      return('Enter F test statistic, the numerator df, and the denominator df')
    else if(input$f < 0 | input$df1 < 0 | input$df2 < 0)
      return('Invalid input value(s)')
    else{
      pval <- 1 - pf(input$f, input$df1, input$df2)
      if(pval < .001) return('< 0.001')
      return(round(pval, 3))
    }
  })
}

shinyApp(ui = ui, server = server)