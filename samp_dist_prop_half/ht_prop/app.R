setwd('C:\\Users\\adriand1\\Desktop\\STA 215 W17\\Apps\\ht_prop')
source('tstat.R')
source('pval.R')
source('combined.R')
library(shiny); library(graphics)
ui <- navbarPage(title='Test for population proportion', inverse=T,
  tabPanel('Test statistic', tstat.ui()),
  tabPanel('P-value', pval.ui()), 
  tabPanel('Test statistic and p-value', combined.ui())
)

server <- function(input, output){
  tstat.serv(input, output)
  pval.serv(input, output)  
  combined.serv(input, output)
}

shinyApp(ui = ui, server = server)