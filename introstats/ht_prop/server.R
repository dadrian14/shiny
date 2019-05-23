library(shiny)
source('intro.R')
source('tstat.R')
source('pval.R')
source('combined.R')

shinyServer(function(input, output) {
  server.intro(input, output)
  server.tstat(input, output)
  server.pval(input, output)
  server.combined(input, output)
})
