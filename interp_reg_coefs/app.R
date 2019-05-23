library(shiny)

pred <- function(var, terms, b0, x1_coef, x1, x2_coef, x2, x1x2_coef, x3, 
                 DA_coef, DB_coef, forplot)
{
  yhat <- b0
  if('x1' %in% terms) yhat <- yhat + x1_coef * x1
  if('x2' %in% terms) yhat <- yhat + x2_coef * x2
  if('x3' %in% terms) yhat <- yhat + (x3 == 'A') * DA_coef + (x3 == 'B') * DB_coef
  if('x1x2' %in% terms) yhat <- yhat + x1x2_coef * x1 * x2
  if(forplot==T & !any(grepl(var, terms)) & var == 'x3') return(rep(yhat,3))
  if(forplot==T & !any(grepl(var, terms))) return(rep(yhat,11))
  yhat
}


ui <- fluidPage(
  wellPanel(h2(strong('Interpreting Regression Coefficients'))),
  fluidRow(
    column(3, wellPanel(
      checkboxGroupInput('terms', label='Terms in the model', 
                         choiceValues = list('x1', 'x2', 'x3', 'x1x2'), 
                         choiceNames = list(HTML('x<sub>1</sub>'), HTML('x<sub>2</sub>'), 
                                            HTML('x<sub>3</sub>'),
                                            HTML('x<sub>1</sub>x<sub>2</sub>')), inline=T),
      HTML('x<sub>1</sub>, x<sub>2</sub>, and x<sub>4</sub> are quantitative;'),
      HTML('x<sub>3</sub>'), 'is categorical with categories A, B, and C.',
      hr(),
      h4('Coefficient values'),
      sliderInput('b0', label='Intercept', min=-1, max=1, value=0.5, step=.1),
      conditionalPanel("input.terms.indexOf('x1') != -1", 
        sliderInput('x1_coef', label=HTML('x<sub>1</sub> Coefficient'), min=-1, max=1, value=.5, 
                    step=.1) 
      ),
      conditionalPanel("input.terms.indexOf('x2') != -1", 
                       sliderInput('x2_coef', label=HTML('x<sub>2</sub> Coefficient'), min=-1, max=1, value=.5, 
                                   step=.1) 
      ),
      conditionalPanel("input.terms.indexOf('x3') != -1", 
                       sliderInput('DA_coef', label=HTML('D<sub>A</sub> Coefficient'), min=-1, max=1, value=.5, 
                                   step=.1),
                       sliderInput('DB_coef', label=HTML('D<sub>B</sub> Coefficient'), min=-1, max=1, value=.5, 
                                   step=.1) 
      ),
      conditionalPanel("input.terms.indexOf('x1x2') != -1", 
                       sliderInput('x1x2_coef', label=HTML('x<sub>1</sub>x<sub>2</sub> Coefficient'), 
                                   min=-1, max=1, value=.5, step=.1) 
      )
    )),
    column(9, wellPanel(
      h4('Regression line:'), HTML('y&#770; = '), uiOutput('reg_line', inline=T),
      hr(),
      h4('Profiles of Regression Line'),
      fluidRow(
        column(4, 
             plotOutput('x1_profile', height='200px'), 
             sliderInput('x1', label=HTML('x<sub>1</sub>'), min=0, max=1, value=0, step=.1)
        ),
        column(4, 
               plotOutput('x2_profile', height='200px'), 
               sliderInput('x2', label=HTML('x<sub>2</sub>'), min=0, max=1, value=0, step=.1)
        ), 
        column(4, 
          plotOutput('x3_profile', height='200px'),
          radioButtons('x3', label=HTML('x<sub>3</sub>'), choices=list('A', 'B', 'C'), 
                       inline=T)
        )
    )
    ))
  )
)

server <- function(input, output, session) {
  output$reg_line <- renderUI({
    x1part <- x2part <- x1x2part <- x3part <- NULL
    if('x1' %in% input$terms) 
      x1part <- paste(ifelse(input$x1_coef >= 0, '+', '-'), abs(input$x1_coef),
                            'x<sub>1</sub>')
    if('x2' %in% input$terms) 
      x2part <- paste(ifelse(input$x2_coef >= 0, '+', '-'), abs(input$x2_coef),
                      'x<sub>2</sub>')
    if('x3' %in% input$terms){
      DApart <- paste(ifelse(input$DA_coef >= 0, '+', '-'), abs(input$DA_coef),
                      'D<sub>A</sub>')
      DBpart <- paste(ifelse(input$DB_coef >= 0, '+', '-'), abs(input$DB_coef),
                      'D<sub>B</sub>')
      x3part <- paste(DApart, DBpart)
    }
    if('x1x2' %in% input$terms) 
      x1x2part <- paste(ifelse(input$x1x2_coef >= 0, '+', '-'), abs(input$x1x2_coef),
                      'x<sub>1</sub>x<sub>2</sub>')
    HTML(paste(input$b0, x1part, x2part, x3part, x1x2part))
  })
  output$x1_profile <- renderPlot({
    par(mar=c(4, 4.5, .1, .1), cex.lab=1.3)
    x1 <- seq(0, 1, by=.1)
    yhat <- pred('x1',input$terms, input$b0, input$x1_coef, x1, 
                 input$x2_coef, input$x2, input$x1x2_coef, input$x3, 
                 input$DA_coef, input$DB_coef, forplot=T)
    plot(x1, yhat, type='l', xlab=expression(x[1]), ylab=expression(hat(y)), 
         ylim=c(-1,1)*(1+length(input$terms)))
    grid()
    points(input$x1, pred('x1', input$terms, input$b0, input$x1_coef, input$x1, 
                          input$x2_coef, input$x2, input$x1x2_coef, input$x3, 
                          input$DA_coef, input$DB_coef, forplot=F), pch=16, col=2)
  })
  output$x2_profile <- renderPlot({
    par(mar=c(4, 4.5, .1, .1), cex.lab=1.3)
    x2 <- seq(0, 1, by=.1)
    yhat <- pred('x2', input$terms, input$b0, input$x1_coef, input$x1, 
                 input$x2_coef, x2, input$x1x2_coef, input$x3, 
                 input$DA_coef, input$DB_coef, forplot=T)
    plot(x2, yhat, type='l', xlab=expression(x[2]), ylab=expression(hat(y)), 
         ylim=c(-1,1)*(1+length(input$terms)))
    grid()
    points(input$x2, pred('x2', input$terms, input$b0, input$x1_coef, input$x1, 
                          input$x2_coef, input$x2, input$x1x2_coef, input$x3, 
                          input$DA_coef, input$DB_coef, forplot=F), pch=16, col=2)
  })
  output$x3_profile <- renderPlot({
    par(mar=c(4, 4.5, .1, .1), cex.lab=1.3)
    x3 <- c('A', 'B', 'C')
    yhat <- pred('x3', input$terms, input$b0, input$x1_coef, input$x1, 
                 input$x2_coef, x2, input$x1x2_coef, x3, 
                 input$DA_coef, input$DB_coef, forplot=T)
    plot(1:3, yhat, xlab=expression(x[3]), ylab=expression(hat(y)), 
         ylim=c(-1,1)*(1+length(input$terms)), xaxt='n')
    x3.num <- which(input$x3 == LETTERS[1:3])
    points(x3.num, pred('x2', input$terms, input$b0, input$x1_coef, input$x1, 
                         input$x2_coef, input$x2, input$x1x2_coef, input$x3, 
                         input$DA_coef, input$DB_coef, forplot=F), pch=16, col=2)
    axis(side=1, at=1:3, labels=x3)
  })
  observe({
    x3.num <- which(input$x3 == LETTERS[1:3])
    print(x3.num)
  })
}

shinyApp(ui = ui, server = server)