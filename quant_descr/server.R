library(shiny)
library(rhandsontable)
library(ggplot2)

plot.outlier <- function(x, qs){ #x=outlier, qs=c(q1, q3)
  iqr <- diff(qs)
  if(any(c(qs[1]-x, x-qs[2]) > 3*iqr)) points(x=x, y=1, pch='*', cex=2.5)
  else points(x=x, y=1, cex=2)
}

shinyServer(function(input, output, session) {
  output$info <- renderUI({
  tagList(
    'For a list of numbers that you enter, this app displays numerical summaries 
    (statistics) and graphical summaries (histogram and boxplot). The descriptions 
    in the boxes below match the panels in their corresponding positions in 
    the app.',
    fluidRow(
      column(2, wellPanel(
        tags$div('Enter a set of numbers into the column \"Data\" as you would 
                 in Microsoft Excel.  The number and graphical summaries in the 
                 other panels will 
                 dynamically adjust to changes in your entered data.  
                 If necessary, you can add rows by clicking on the 
                 number 30 and dragging the square in the lower right corner of 
                 the outlined box downwards.', 
                 style='font-size: x-small;')
      )),
      column(5, wellPanel(
        tags$div('Displays a histogram of the current data.  You can change whether 
                 the y-axis represents the counts of observations in each bin 
                 (making it a frequency histogram) or proportions (relative 
                 frequency histogram).  Also you can choose whether to have the app 
                 automatically determine the number of bins or to determine the 
                 number yourself with the slider.  (Note: observations falling on 
                 the dividing line between two bins are included in the right
                 of the two bins.)', 
                 style='font-size: x-small;')
      )), 
      column(5, wellPanel(
        tags$div('Displays a boxplot of the current data.  You can specify whether 
                 the boxplot type is simple (outliers not shown separately; 
                 whiskers always extend to max/min) or modified (outliers shown as 
                 separate points: circles for minor outliers and asterisks for 
                 major outliers).  Whether an observation is a minor or major 
                 outlier depends on its position relative to the \"fences\", 
                 which will appear if you click the checkbox.  Minor outliers 
                 are between the inner and outer fences, while minor outliers are 
                 outside the outer fences.  (Note: the whiskers on a modified 
                 boxplot extend to the most extreme observations that are not outliers.)', 
                 style='font-size: x-small;')
      )),
      column(6, wellPanel(
        tags$div('This panel displays numerical summaries (statistics) for 
                 the current data.  Note that the set of statistics in the right 
                 table constitute the', strong('five number summary.'), 
                 style='font-size: x-small;')
      )),
      column(4, wellPanel(
        tags$div('Lists the current data sorted from smallest to largest.  
                 This can be helpful for calculating the median or how many 
                 observations are in a given interval on the histogram.', 
                 style='font-size: x-small;')
      ))
      , style="margin: 0px -20px;")               
  )})
  output$table <- renderRHandsontable({
    df <- data.frame(Data=as.numeric(rep(NA, 30)))
    rhandsontable(df) %>% hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
  })
  x <- reactive({
    req(input$table)
    x <- hot_to_r(input$table)$Data
    x[!is.na(x)]
  })
  #Table of stats
  output$stats_table <- renderUI({
    fluidPage(
      withMathJax(),
      column(6, 
             tags$table(
               tags$thead(
                 tags$th('Statistic'), 
                 tags$th('Value')
               ), 
               tags$tr(
                 tags$td('Sample size \\((n)\\)'), 
                 tags$td(textOutput('n', inline=T))
               ), 
               tags$tr(
                 tags$td('Mean \\((\\bar{x})\\)'), 
                 tags$td(textOutput('xbar', inline=T))
               ), 
               tags$tr(
                 tags$td('Standard deviation \\((s)\\)'), 
                 tags$td(textOutput('s', inline=T))
               ),
               tags$tr(
                 tags$td('Range'), 
                 tags$td(textOutput('range', inline=T))
               ), 
               tags$tr(
                 tags$td('Interquartile Range (IQR)'), 
                 tags$td(textOutput('iqr', inline=T))
               )
             )  
      ), 
      column(6, 
             tags$table(
               tags$thead(
                 tags$th('Statistic'), 
                 tags$th('Value')
               ), 
               tags$tr(
                 tags$td('Minimum'), tags$td(textOutput('min', inline=T))
               ), 
               tags$tr(
                 tags$td('First quartile (Q1)'), 
                 tags$td(textOutput('Q1', inline=T))
               ), 
               tags$tr(
                 tags$td('Median'), 
                 tags$td(textOutput('med', inline=T))
               ),
               tags$tr(
                 tags$td('Third quartile (Q3)'), 
                 tags$td(textOutput('Q3', inline=T))
               ), 
               tags$tr(
                 tags$td('Maximum'), tags$td(textOutput('max', inline=T))
               )
             ),
             style='padding-left:5px;'
      )
    )
    
  })
  
  output$n <- renderText({
    length(x())
  })
  output$xbar <- renderText({
    if(length(x())==0) return('')
    else return(round(mean(x(), na.rm=T), 3))
  })
  output$s <- renderText({
    if(length(x())<=1) return('')
    else return(round(sd(x(), na.rm=T), 3))
  })
  output$min <- renderText({
    if(length(x())==0) return('')
    else return(min(x(), na.rm=T))
  })
  output$Q1 <- renderText({
    if(length(x())==0) return('')
    else return(boxplot.stats(x())$stats[2])
  })
  output$med <- renderText({
    if(length(x())==0) return('')
    else return(median(x(), na.rm=T))
  })
  output$Q3 <- renderText({
    if(length(x())==0) return('')
    else return(boxplot.stats(x())$stats[4])
  })
  output$max <- renderText({
    if(length(x())==0) return('')
    else return(max(x(), na.rm=T))
  })
  output$range <- renderText({
    if(length(x())<=1) return('')
    else return(diff(range(x(), na.rm=T)))
  })
  output$iqr <- renderText({
    if(length(x())<=1) return('')
    else return(diff(boxplot.stats(x())$stats[c(2,4)]))
  })
  output$hist <- renderPlot({
    if(length(x())==0){
      par(mar=c(.1, .1, .1, .1))
      plot(x=0,y=0, type='n', xlab='', ylab='', xaxt='n', yaxt='n')
      text(x=0,y=0, 'Enter Data!', cex=3, col=2)
    }
    if(length(x())>=1){
      par(mar=c(2, 4, 1, 1))
      if(input$hist_type=='counts'){ 
          if(input$auto_bins == 'Automatically') 
            ggplot(data.frame(x()), aes(x())) + 
              geom_histogram(fill=grey(.7), col=1, breaks=pretty(x()), closed='left') + 
              labs(x="Data")
          else
            ggplot(data.frame(x()), aes(x())) +
              geom_histogram(fill=grey(.7), col=1, 
                breaks=seq(from=min(x()), to=max(x()), length=input$n_bins+1), 
                closed='left') + 
              labs(x='Data')
      }
      else{ # input$hist_type=='
        if(input$auto_bins == 'Automatically') 
          ggplot(data.frame(x()), aes(x())) + 
          geom_histogram(aes(y=..count../sum(..count..)), 
                         fill=grey(.7), col=1, breaks=pretty(x()), closed='left') + 
          labs(x="Data", y='proportion')
        else
          ggplot(data.frame(x()), aes(x())) +
          geom_histogram(aes(y=..count../sum(..count..)),
                         fill=grey(.7), col=1, 
                         breaks=seq(from=min(x()), to=max(x()), length=input$n_bins+1), 
                         closed='left') + 
          labs(x='Data', y='proportion')
      }
    }
  })
  output$box <- renderPlot({
    if(length(x())<=1){
      par(mar=c(.1, .1, .1, .1))
      plot(x=0,y=0, type='n', xlab='', ylab='', xaxt='n', yaxt='n')
      text(x=0,y=0, 'Sample size \nmust be at least 2', cex=3, col=2)
    }
    if(length(x())>=2){
      par(mar=c(2,0.5,0.5,0.5))
      if(input$box_type=='simple')
        boxplot(x(), range=0, horizontal=T, col=grey(.7), whisklty=1)
      else{
        if(input$fences==F)
          bx <- boxplot(x(), range=1.5, horizontal=T, col=grey(.7), outline=F, 
                      ylim=range(x()), whisklty=1)
        else{
          temp <- boxplot.stats(x())$stats
          q1 <- temp[2]; q3 <- temp[4]; iqr <- q3 - q1
          inner <- c(q1 - 1.5*iqr, q3 + 1.5*iqr)
          outer <- c(q1 - 3*iqr, q3 + 3*iqr)
          ylim <- c(min(min(x()), outer[1]), 
                    max(max(x()), outer[2]))
          bx <- boxplot(x(), range=1.5, horizontal=T, col=grey(.7), outline=F, 
                        ylim=ylim, whisklty=1)
          abline(v=outer, col=2, lty=2)
          abline(v=inner, col=4, lty=2)
        }
        if(length(bx$out)>0) sapply(bx$out, plot.outlier, bx$stats[c(2,4),1])
      }
    }
  })
  output$fences_table <- renderUI({
    tags$table(
      tags$tr(
        tags$td('Inner fences', tags$span('(blue)', style='color:blue;'),
                rowspan="2"), 
        tags$td('Q1 - 1.5 * IQR'), 
        tags$td(textOutput('lower_inner'))
      ),
      tags$tr(
        tags$td('Q3 + 1.5 * IQR'), 
        tags$td(textOutput('upper_inner'))
      ), 
      tags$tr(
        tags$td('Outer fences', tags$span('(red)', style='color:red;'),
                rowspan="2"), 
        tags$td('Q1 - 3 * IQR'), 
        tags$td(textOutput('lower_outer'))
      ),
      tags$tr(
        tags$td('Q3 + 3 * IQR'), 
        tags$td(textOutput('upper_outer'))
      ) 
    )
  })
  fences <- reactive({
    temp <- boxplot.stats(x())$stats
    q1 <- temp[2]; q3 <- temp[4]; iqr <- q3 - q1
    inner <- c(q1 - 1.5*iqr, q3 + 1.5*iqr)
    outer <- c(q1 - 3*iqr, q3 + 3*iqr)
    list(inner=inner, outer=outer)
  })
  output$lower_inner <- renderText({
    fences()$inner[1]
  })
  output$upper_inner <- renderText({
    fences()$inner[2]
  })
  output$lower_outer <- renderText({
    fences()$outer[1]
  })
  output$upper_outer <- renderText({
    fences()$outer[2]
  })
  output$sorted <- renderText({
    toString(sort(x()))
  })
})
