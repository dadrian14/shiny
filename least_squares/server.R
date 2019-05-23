library(shiny)
library(RColorBrewer); library(DT)

shinyServer(function(input, output, session) {
  output$info <- renderUI({fluidPage(
    p(tags$u('Big concept:'), 'The least squares regression line has the',
      strong('smallest sum of squared residuals'),
      '(i.e. least squares) out of all possible lines.'), 
    'The boxes below each describe a panel in the app:',
    fluidRow(
      column(4, wellPanel(
        tags$div(strong('Which dataset?'), 'The datasets available all have 10 
                 observations.  In the "Houses" dataset, the explanatory variable (x) 
                 is square feet and the response variable (y) is price (in $1000s).  "Data 1" 
                 to "Data 5" are simulated datasets that will be the same for each 
                 instance of the app.  For the random option, 10 pairs of \\(x\\) and \\(y\\)
                 are randomly selected.  In this option, draw a new random sample by clicking 
                 on the "Draw New" button.',
                 style='font-size: x-small')
      ), 
      wellPanel(
        tags$div(strong('Guess the LS line!'), 'Specify your guessed least squares 
                 line by entering your guesses for the y-intercept and slope.  (You 
                 can also change these values using the "up" or "down" keys.)',
                 style='font-size: x-small')
      ),
      wellPanel(
        tags$div(strong('Show which lines?'), 'This changes which lines are shown
                 on the scatterplot (see the legend in upper left of the 
                 scatterplot).  When you think you\'ve got a good guess, check 
                 it by showing the true LS regression line.',
                 style='font-size: x-small')
      )),
      column(4, wellPanel(
        tags$div(strong('Table:'), 'This table shows the 10 observations of the current dataset.
                 The columns shown depend on the options chosen in the "Show which lines" panel. 
                 If no options are chosen, only the values of \\(x\\) and \\(y\\) are shown.  
                 If "Your Guess" is chosen, the table will also display the "yhat" column, 
                 which provides the predicted \\(y\\) values according to the current guessed 
                 line.  In addition, if "Residuals" is chosen, the table will also display 
                 the "resid" and "resid.sq" columns, which are the residual and squared 
                 residual values for the current guessed line.  (Residuals are 
                 calculated as \\(y - \\hat{y}\\).)',
                 style='font-size: x-small')
      ), 
      wellPanel(
        tags$div(strong('Sum of squared residuals:'), 'If you have selected the "Residuals" 
                 option in the "Show which lines?" panel, this panel will display the 
                 value of the sum of squared residuals for the current guessed line, 
                 along with a colored bar representing this sum.  Note that the length 
                 of each of the colored rectangles making up this bar is propoortional to the 
                 size of the squared residual in the row of the table with the same color.', 
                         style='font-size: x-small')
      )),
      column(4, wellPanel(
        tags$div(strong('Scatterplot:'), 'shows a scatterplot of the current dataset, with color-coded 
                 points correspoding to the colored rows of the table.  It also shows lines depending on the 
                 options chosen in the "Show which lines?" panel:', 
                 tags$ul(
                  tags$li('Your guessed LS line (dashed line)'),
                  tags$li('Residuals (dotted lines): These represent the distance between 
                          each \\(y\\) value and its predicted \\(y\\) value according 
                          to your guessed line.'),
                  tags$li('The true LS regression line (solid line)')
                 ),
                 style='font-size: x-small')
      ))
    , style="margin: 0px -20px;")
  )})
  runif(1)
  old.seed <- .Random.seed
  colors <- c(grey(.5), brewer.pal(8, 'Set1'), grey(.85))
  set.seed(24602)
  data15 <- array(dim=c(5, 10, 2))
  for(i in 1:5){
    data15[i,,] <- sim1slrdata(n=10, mean.x=7, mean.y=7, sd.x=2, sd.y=2, 
                               min.rho=-.99, max.rho=.99)
  }
  .GlobalEnv$.Random.seed <- old.seed
  data <- reactiveValues(x=rep(0, 10), y=rep(0, 10))
  observeEvent(c(input$dataset, input$draw),{
    if(input$dataset == 'Houses'){
      data$x <- c(1480, 1581, 1915, 2676, 960, 1080, 2488, 2600, 2104, 616)
      data$y <- c(130, 200, 260, 212, 168, 130, 320, 280, 223, 157)
    }
    for(i in 1:5){
      if(input$dataset == paste('Data', i)){
        data$x <- round(data15[i,,1], 2)
        data$y <- round(data15[i,,2], 2)
      }
    }
    if(input$dataset == 'Random'){
      sim.data <- sim1slrdata(n=10, mean.x=7, mean.y=7, sd.x=2, sd.y=2, 
                              min.rho=-.99, max.rho=.99)
      data$x <- round(sim.data[,1], 2)
      data$y <- round(sim.data[,2], 2)
    }
    updateNumericInput(session, 'yint_guess', value=mean(data$y), 
      step=signif(diff(range(data$y))/50, 1))
    updateNumericInput(session, 'slope_guess', value=0, 
      step=signif(diff(range(data$y))/diff(range(data$x))/50, 1))
  })
  stats <- eventReactive(c(input$dataset, input$draw), {  
    xy <- cbind(data$x, data$y)
    xy <- xy[order(data$x),]
    x <- xy[,1]; y <- xy[,2]
    xbar <- mean(x)
    ybar <- mean(y)
    fit <- lm(y~x)
    list(x=x, y=y, xbar=xbar, ybar=ybar, fit=fit)
  })
  res <- reactive({
    m <- input$slope_guess
    yint <- input$yint_guess
    mb <- c(m, yint)
    yhat <- yint + m * stats()$x
    e <- stats()$y - yhat
    list(mb=mb, yhat=yhat, e=e)
  })
  output$scatter <- renderPlot({
    par(mar=c(4.1, 4.1, .1, .1), cex.lab=1.3)
    ylims <- c(min(stats()$y), max(stats()$y+.2*diff(range(stats()$y))))
    plot(stats()$x, stats()$y, 
         xlab=ifelse(input$dataset == 'Houses', 'Square feet', 'x'), 
         ylab=ifelse(input$dataset == 'Houses', 'Price (in $1000s)', 'y'), 
         col=colors, pch=19, cex=2, ylim=ylims)
    grid()
    guess.txt <- 'Guessed LS line'
    if(input$guess_line){
      abline(a=res()$mb[2], b=res()$mb[1], col=1, lty=2)
      legend('topleft', guess.txt, col=1, lty=2)
      if(input$show_res){
        segments(x0=stats()$x, y0=stats()$y, y1=res()$yhat, col=colors, lty=3)
        legend('topleft', c(guess.txt, 'Residuals'), col=1, lty=c(2,3))
      }
    }
    if(input$true_line){
      abline(a=stats()$fit$coef[1], b=stats()$fit$coef[2], col=1)
      if(input$guess_line & input$show_res)
        legend('topleft', c(guess.txt, 'Residuals', 'True LS line'),
               col=1, lty=c(2,3, 1))
    }
  })
  output$SS <- renderPlot({
    par(mar=c(2.1, 1, .5, 1))
    SSE <- sum(stats()$fit$residuals^2)
    barplot(cbind(res()$e^2), beside=F, main='',
            col=colors, horiz=T, xlim=c(0,SSE*4))
  })
  output$SS.guess <- renderText({
    round.dig <- ifelse(input$dataset=='Houses', 0, 2)
    format(round(sum(res()$e^2), round.dig), big.mark=',')
  })
  output$SS.true <- renderText({
    round.dig <- ifelse(input$dataset=='Houses', 0, 2)
    format(round(sum(stats()$fit$residuals^2), round.dig), big.mark=',')
  })
  output$table <- renderDT({
    mat <- cbind(stats()$x, stats()$y)
    if(input$dataset == 'Houses') colnames(mat) <- c('SqFt', 'Price')
    else colnames(mat) <- c('x', 'y')
    round.dig <- ifelse(input$dataset=='Houses', 1, 3)
    if(input$guess_line) 
      mat <- cbind(mat, yhat=round(res()$yhat, round.dig))
    if(input$show_res) 
      mat <- cbind(mat, resid=round(res()$e, round.dig), resid.sq=round(res()$e^2, round.dig))
    xcolname <- ifelse(input$dataset == 'Houses', 'SqFt', 'x')
    datatable(mat,
              options = list(dom = 't')) %>%
      formatStyle(xcolname, target='row', backgroundColor = styleEqual(stats()$x, colors))
  })
  output$guess_eq <- renderUI({
    withMathJax(paste0('\\(\\hat{y} = ', 
      input$yint_guess, ifelse(input$slope_guess>=0, '+', '-'), 
      abs(input$slope_guess), 'x\\)'
    ))
  })
  output$true_eq <- renderUI({
    yint <- signif(stats()$fit$coef[1], 4)
    slope <- signif(stats()$fit$coef[2], 4)
    withMathJax(paste0('\\(\\hat{y} = ',
      yint, ifelse(slope>=0, '+', '-'),
      abs(slope), 'x\\)'
    ))
  })
})


sim1slrdata <- function(n, mean.x, mean.y, sd.x, sd.y, min.rho, max.rho)
{
  rho <- runif(1, min=min.rho, max=max.rho)
  x <- rnorm(n, mean=mean.x, sd=sd.x)
  cond.mean.y <- mean.y + rho * sd.y * (x - mean.x) / sd.x
  cond.sd.y <- sd.y * sqrt(1-rho^2)
  y <- rnorm(n, mean=cond.mean.y, sd=cond.sd.y)
  cbind(x, y)
}
