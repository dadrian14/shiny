library(shiny)
ui <- fluidPage(
  fluidRow(wellPanel(h2(strong('How well can you spel?')),
                     h4('Pick the correct spelling')
  )),
  fluidRow(column(4, wellPanel(
    lapply(1:7, function(i){
      tagList(uiOutput(paste0('choices',i)),
              uiOutput(paste0('res',i)))
    })
  )),
  column(4, wellPanel(
    lapply(8:14, function(i){
      tagList(uiOutput(paste0('choices',i)),
              uiOutput(paste0('res',i)))
    })
  )),
  column(4, wellPanel(
    lapply(15:21, function(i){
      tagList(uiOutput(paste0('choices',i)),
              uiOutput(paste0('res',i)))
    })
  )),
  hr(),
  actionButton('checkans', label='Check Answers!'), 
  actionButton('reset', label='RESET'),
  br(),'Number correct = ', textOutput('tally', inline=T), br(),
  'Percent correct = ', textOutput('percent', inline=T)
                     
))

server <- function(input, output, session) {
  words <- rbind(c('Asiduous', 'Assiduous'), 
                 c('Changeable', 'Changeble'), 
                 c('Maelstrom', 'Malstrom'), 
                 c('Munificent', 'Munificient'), 
                 c('Panacea', 'Panecea'), 
                 c('Hierarchy', 'Heirarchy'), 
                 c('Plenitude', 'Plentitude'), 
                 c('Philantropic', 'Philanthropic'), 
                 c('Pugnecious', 'Pugnacious'), 
                 c('Minuscule', 'Miniscule'), 
                 c('Privilage', 'Privilege'), 
                 c('Supersede', 'Supercede'), 
                 c('Threshold', 'Threshhold'), 
                 c('Vaccum', 'Vacuum'), 
                 c('Millennium', 'Milennium'), 
                 c('Possession', 'Posession'), 
                 c('Indispensable', 'Indespensable'), 
                 c('Concsientious', 'Conscientious'), 
                 c('Leisure', 'Liesure'), 
                 c('Perseverance', 'Persaverance'), 
                 c('Greateful', 'Grateful'))
  correct <- c(2, 1, 1, 1, 1, 1, 1, 2, 2, 1, 2, 1, 1, 2, 1, 
               1, 1, 2, 1, 1, 2)
  n <- nrow(words)
  res <- reactiveValues(rightvec=rep(0,n))
  lapply(1:n, function(i){
    output[[paste0('choices', i)]] <- renderUI({
      radioButtons(paste0('w',i), label=paste0(i,'.'), 
                 choices=c(words[i,], 'None selected'), selected='None selected', inline=T)
    })
  })
  observe({})
  observeEvent(input$checkans, {
    lapply(1:n, function(i){ 
      res$rightvec[i] <- ifelse(input[[paste0('w',i)]] == words[i,correct[i]], 1, 0)
      output[[paste0('res', i)]] <- renderUI({
        if(res$rightvec[i] == 1) out <- tags$div('Correct!', style="color:green")
        else out <- tags$div('WRONG', style="color:red")
        out
      })
    })
  })
  output$tally <- renderText({sum(res$rightvec)})
  output$percent <- renderText({paste0(round(sum(res$rightvec)/n*100,1), '%')})
  observeEvent(input$reset, {
    res$rightvec <- rep(0, n)
    lapply(1:n, function(i){
      updateRadioButtons(session, paste0('w',i), label=paste0(i,'.'), 
                         choices=c(words[i,], 'None selected'), selected='None selected', inline=T)
      output[[paste0('res', i)]] <- renderUI({})
    })
  })
}

shinyApp(ui = ui, server = server)
