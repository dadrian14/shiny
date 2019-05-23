skewed.pop <- function(shape, dir, mean, max, mult, N)
{
  scale <- mean/shape
  x <- rgamma(N, shape=shape, scale=scale)
  x <- x*mult
  x <- x[x<max & x>0]
  if(dir=='l') x <- max - x
  x
}

library(shiny)
ui <- fluidPage(
  wellPanel(
    tags$h3(strong('Sampling Distribution of the Sample Mean')),
    tags$h4('And the Central Limit Theorem')
  ), 
  fluidRow(
    column(4, wellPanel(
      h3('Choose the population'),
      sliderInput('which.pop', label='Skewness (see below)', min=1, 
                  max=12, value=1, step=1, ticks=T),
      '<< More skewed right',
      p('More skewed left >>', style="text-align:right"),
      hr(),
      h4('Distribution of the population'),
      plotOutput("hist_pop", height="300px"), br(),
      tags$p('Population mean ', HTML('&mu;'), ' = ', textOutput("pop_mean", inline=T)), 
      tags$p('Population standard deviation', HTML('&sigma;'), 
                    ' = ', textOutput("pop_sd", inline=T))
    )), 
    column(8, wellPanel(
      tags$h3('1000 Samples from the Population'),
      sliderInput("n", 'Size of each sample (n)', min=1, max=50, 
                    value=1),
      'We take the mean', HTML('x&#772;'), 'of each of the 1000 samples.', br(),
      'The sampling distribution of sample mean is the distribution of those means.',
      hr(),
        tags$h4('Sampling distribution of sample mean'),
        plotOutput("hist_sampdist", height="300px"), br(),
        tags$p('Mean of sample means = ', textOutput("sampdist_mean", inline=T), 
               ' ( about', HTML('&mu;'), ')'),
        tags$p('standard deviation of sample means = ', textOutput("sampdist_sd", inline=T), 
               ' ( about', HTML('&sigma;/&radic;n'), ')')
      ))
))

server <- function(input, output){
  N <- 1000
  pops <- array(dim=c(N, 12))
  shap <- c(.1, .5, 1, 3, 6, 25)
  mult <- c(1, 1, 2, 3, 4, 5)
  mults <- c(mult, rev(mult))
  shapes <- c(shap, rev(shap))
  dir <- c(rep('r', 6), rep('l', 6))
  mean <- 10; max <- 100
  for(i in 1:12){ 
    x <- skewed.pop(shapes[i], dir[i], mean, max, mults[i], N)
    pops[,i] <- c(x, rep(NA, N-length(x)))
  }
  output$hist_pop <- renderPlot({
    par(mar=c(4.1,4.1,2,0.5))
    hist(pops[,input$which.pop], xlab='Population values', xlim=c(0, max), 
         main="Histogram of population")
  })
  output$pop_mean <- renderText({
    return(round(mean(pops[,input$which.pop], na.rm=T), 1))
  })
  output$pop_sd <- renderText({
    return(round(sd(pops[,input$which.pop], na.rm=T), 1))
  })
  xbars <- reactive({
    Npop <- N - sum(is.na(pops[,input$which.pop]))
    samples <- array(dim=c(1000, input$n))
    for(i in 1:1000)
      samples[i,] <- sample(pops[1:Npop,input$which.pop], size=input$n, replace=T)
    apply(samples, 1, mean)
  })
  output$hist_sampdist <- renderPlot({
    par(mar=c(4.1,4.1,2,0))
    hist(xbars(), xlim=c(0, max), xlab='Sample means', 
         main="Histogram of sampling distribution of sample mean")
  })
  output$sampdist_mean <- renderText({
    round(mean(xbars()), 1)
  })
  output$sampdist_sd <- renderText({
    round(sd(xbars()), 1)
  })
}

shinyApp(ui = ui, server = server)