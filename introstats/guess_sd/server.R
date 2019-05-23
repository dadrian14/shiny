library(shiny)
shinyServer(function(input, output){
#Uniform Distribution
  observeEvent(input$alldistbutton, {
    sample14 = sample(1:4,4, replace = FALSE)
    sample2100 = sample(2:100,1)
    sample1100 = sample(1:100,1)
    sample2100B = sample(2:100,1)
    sample0100low = sample(0:90,1)
    sample0100high = sample((sample0100low+2):100,1)
    probabsample = round(runif(1, min = .2, max = .8), 2)
    if (sample14[1] == 1) {
      myrunifmin = sample(1,10,1)
      myrunifmax = myrunifmin + round(runif(1, min = sqrt(12), max = 5*sqrt(12)))
    }
    if (sample14[1] == 2) {
      myrunifmin = sample(1,10,1)
      myrunifmax = myrunifmin + round(runif(1, min = 11*sqrt(12), max = 15*sqrt(12)))
    }
    if (sample14[1] == 3) {
      myrunifmin = sample(1,10,1)
      myrunifmax = myrunifmin + round(runif(1, min = 21*sqrt(12), max = 25*sqrt(12)))
    }
    if (sample14[1] == 4) {
      myrunifmin = sample(1,10,1)
      myrunifmax = myrunifmin + round(runif(1, min = 31*sqrt(12), max = 35*sqrt(12)))
    }
    runifdist <- runif(sample2100, min = myrunifmin , max = myrunifmax)

    output$ans1 = renderText("")
    output$ans2 = renderText("")
    output$ans3 = renderText("")
    output$ans4 = renderText("")
    
    real.sd1 <<- round(sd(runifdist))
    #Exponential Dist
    if (sample14[2] == 1){
      myrexprate = runif(1, min = 0.2, max = 1)
    }
    if (sample14[2] == 2){
      myrexprate = runif(1, min = 1/15, max = 1/11)
    }
    if (sample14[2] == 3){
      myrexprate = runif(1, min = 1/25, max = 1/21)
    }
    if (sample14[2] == 4){
      myrexprate = runif(1, min = 1/35, max = 1/31)
    }
    rexpdist <- rexp(sample2100, rate = myrexprate)
    real.sd2 <<- round(sd(rexpdist))
    
    #Chisquared Dist
    if (sample14[3] == 1){
      mychisqdf = runif(1, min = 1/2, max = 25/2)
    }
    if (sample14[3] == 2){
      mychisqdf = runif(1, min = 121/2, max = 225/2)
    }
    if (sample14[3] == 3){
      mychisqdf = runif(1, min = 441/2, max = 625/2)
    }
    if (sample14[3] == 4){
      mychisqdf = runif(1, min = 961/2, max = 1225/2)
    }
    rchisqdist <- rchisq(sample2100, mychisqdf)
    real.sd3<<- round(sd(rchisqdist))
    
    #Binomial Dist
    if (sample14[4] == 1){
      myrbinomsize = runif(1, min = 16, max = 100)
    }
    if (sample14[4] == 2){
      myrbinomsize = runif(1, min = 484 , max = 900)
    }
    if (sample14[4] == 3){
      myrbinomsize = runif(1, min = 1764, max = 2500)
    }
    if (sample14[4] == 4){
      myrbinomsize = runif(1, min = 3844, max = 4900)
    }
    
    rbinomdist <- rbinom(sample2100, as.integer(myrbinomsize), 0.5)
    real.sd4 <<- round(sd(rbinomdist))
    
    output$mygraph = renderPlot({
      par(mfrow = c(2,2))
      hist(runifdist, main = "Uniform Distribution", col = "orange")
      hist(rexpdist, main = "Exponential Distribution", col = "purple")
      boxplot(runifdist, col = "orange")
      boxplot(rexpdist, col = "purple")

    })
    output$myboxplot = renderPlot({
      par(mfrow = c(2,2))
      hist(rchisqdist, main = "Chi-squared Distribution", col = "green")
      hist(rbinomdist, main = "Binomial Distribution", col = "wheat4")
      boxplot(rchisqdist, col = "green")
      boxplot(rbinomdist, col = "wheat4")
      
    })

  })
###############################################################################################################    
  
#Uniform Distribution
  observeEvent(input$singulargetanswersbutton,{
    if (input$radio == real.sd1){
      output$ans1 <- renderText(c("<b>",'Standard Deviation:',"</b>","<font color=\"#00EB00\"><b>", real.sd1,  "</b></font>"))
    } else {
      output$ans1 <- renderText(c("<b>",'Standard Deviation:',"</b>","<font color=\"#FF000\"><b>", real.sd1,  "</b></font>"))
    }
    if (input$radio1 == real.sd2){
      output$ans2 <- renderText(c("<b>",'Standard Deviation:',"</b>","<font color=\"#00EB00\"><b>", real.sd2,  "</b></font>"))
    } else {
      output$ans2 <- renderText(c("<b>",'Standard Deviation:',"</b>","<font color=\"#FF000\"><b>", real.sd2,  "</b></font>"))
    }
    if (input$radio2 == real.sd3){
      output$ans3 <- renderText(c("<b>",'Standard Deviation:',"</b>","<font color=\"#00EB00\"><b>", real.sd3,  "</b></font>"))
    } else {
      output$ans3 <- renderText(c("<b>",'Standard Deviation:',"</b>","<font color=\"#FF000\"><b>", real.sd3,  "</b></font>"))
    }
    if (input$radio3 == real.sd4){
      output$ans4 <- renderText(c("<b>",'Standard Deviation:',"</b>","<font color=\"#00EB00\"><b>", real.sd4,  "</b></font>"))
    } else {
      output$ans4 <- renderText(c("<b>",'Standard Deviation:',"</b>","<font color=\"#FF000\"><b>", real.sd4,  "</b></font>"))
    }
  })
  
  observeEvent(input$alldistbutton, {
    output$abforanswers = renderUI({
      actionButton("singulargetanswersbutton", "Click to get answers",
                   class = "btn action-button",
                   style = "color: black;
                   background-color: white")
    })
  })
  
  observeEvent(input$alldistbutton,{
    listofbagels = c(real.sd1, real.sd2, real.sd3, real.sd4)
    sortedlistofbagels = sort(listofbagels)
    output$rb <- renderUI({
      radioButtons("radio", label = h5("Uniform Dist SD?"),
                   choices = as.list(c('None Selected', sortedlistofbagels[1], sortedlistofbagels[2], sortedlistofbagels[3], sortedlistofbagels[4])))
    })
    output$rb1 <- renderUI({
      radioButtons("radio1", label = h5("Exponential Dist SD?"),
                   choices = as.list(c('None Selected', sortedlistofbagels[1], sortedlistofbagels[2], sortedlistofbagels[3], sortedlistofbagels[4])))
    })
    output$rb2 <- renderUI({
      radioButtons("radio2", label = h5("Chi-Squared Dist SD?"),
                   choices = as.list(c('None Selected',  sortedlistofbagels[1], sortedlistofbagels[2], sortedlistofbagels[3], sortedlistofbagels[4])))
    })
    output$rb3 <- renderUI({
      radioButtons("radio3", label = h5("Binomial Dist SD?"),
                   choices = as.list(c('None Selected',  sortedlistofbagels[1], sortedlistofbagels[2], sortedlistofbagels[3], sortedlistofbagels[4])))
    })
    
  })
  
  })
