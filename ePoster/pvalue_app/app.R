draw1 <- function(bags){
  bag <- sample(1:2, 1)
  voucher <- sample(bags[,bag], 1)
  num <- sum(bags[,1] >= voucher)
  pval <- mean(bags[,1] >= voucher)
  list(bag=bag, voucher=voucher, pval=pval, num=num)
}

library(shiny)
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      td {
        padding: 5px;
        text-align: right;
      }
    "))
  ),
  fluidRow(wellPanel(
    h2(strong('P-values'))
  )), 
  fluidRow(
  column(4, wellPanel(
    h3(strong('Two bags')),
    tags$table(border='1', cellpadding='20', 
      tags$tr(tags$td(), 
              tags$td(tags$img(src='bag_a.PNG', height='100px')), 
              tags$td(tags$img(src='bag_b.PNG', height='100px')) 
              ), 
      tags$tr(tags$td('Value'), 
              tags$td(colspan='2', style='text-align: center', 'How many vouchers?')),
      tags$tr(tags$td('-1000'), tags$td('3'), tags$td('0')),
      tags$tr(tags$td('10'), tags$td('24'), tags$td('1')),
      tags$tr(tags$td('20'), tags$td('19'), tags$td('1')),
      tags$tr(tags$td('30'), tags$td('6'), tags$td('6')),
      tags$tr(tags$td('40'), tags$td('6'), tags$td('6')),
      tags$tr(tags$td('50'), tags$td('1'), tags$td('19')),
      tags$tr(tags$td('60'), tags$td('1'), tags$td('24')),
      tags$tr(tags$td('1000'), tags$td('0'), tags$td('3'))
    )
    )), 
  column(7, wellPanel(
    #h4('You pick one voucher from a bag, but you don\'t know whether it is Bag A 
    #    or Bag B'),
    actionButton('pick', label='Pick a voucher'), 
    tags$img(src='bag_question.PNG', height='100px'), 
    br(), br(),
    uiOutput('result')
    # p(wellPanel(style = "background-color: #ffffff;", 
    #             'Voucher value = ', 
    # textOutput('value1', inline=T))),
    # p(HTML('H<sub>0</sub>: Voucher is from Bag A  vs.  
    #        H<sub>1</sub>: Voucher is from Bag B')),
    # p(wellPanel(style = "background-color: #ffffff;",
    #             HTML('P-Value = P(Voucher value &ge; '), textOutput('value2', inline=T),
    #             ' | bag A)', br(),
    #             '= ', textOutput('num', inline=T), ' / 60 = ',
    #             textOutput('pval', inline=T)))
  ))
  )
)

server <- function(input, output) {
  bag.a <- c(rep(-1000, 3), rep(10, 24), rep(20, 19), rep(30, 6), 
             rep(40, 6), 50, 60)
  bag.b <- c(rep(1000, 3), 10, 20, rep(30, 6), rep(40, 6), 
             rep(50, 19), rep(60, 24))
  bags <- cbind(bag.a, bag.b)
  out <- eventReactive(input$pick, {draw1(bags)})
  output$result <- renderUI({
    if(input$pick==0) return('')
    list(p(wellPanel(style = "background-color: #ffffff;", 
                             paste0('Voucher value = ', out()$voucher))),
    p(HTML('H<sub>0</sub>: Voucher is from Bag A  vs.  
            H<sub>1</sub>: Voucher is from Bag B')),
    p(wellPanel(style = "background-color: #ffffff;",
                HTML(paste0('P-Value = P(Voucher value &ge; ', out()$voucher,
                            ' if voucher from bag A)')), br(), 
                paste0('= ', out()$num, ' / 60 = ', round(out()$pval, 3))))
    )
    
  })
  output$value1 <- renderText({out()$voucher})
  output$value2 <- renderText({out()$voucher})
  output$pval <- renderText({round(out()$pval, 3)})
  output$num <- renderText({out()$num})
  
  
}

shinyApp(ui = ui, server = server)