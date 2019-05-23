library(shiny); library(graphics)
source('./sim2sample_fcns.R')
source('./tstat_functions.R')
source('./pval_functions.R')
source('./combined_functions.R')
source('./ci_fcns.R')

ui <- navbarPage(title='Two Sample Apps', inverse=T,
                 tabPanel('Simulate 2 samples', sim2sample_ui()),
                 navbarMenu(title='Two-sample t test',
                   tabPanel('Test statistic', tstat_ui()),
                   tabPanel('P-value', pval_ui()), 
                   tabPanel('Test statistic and p-value', combined_ui())
                 ),
                 tabPanel('Confidence interval for difference in means', ci_ui())
)

server <- function(input, output, session) {
  sim2sample_server(input, output)
  tstat_serv(input, output)
  pval_serv(input, output)
  combined_serv(input, output)
  ci_server(input, output)
}

shinyApp(ui = ui, server = server)