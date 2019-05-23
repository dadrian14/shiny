#setwd('C:\\Users\\adriand1\\Desktop\\Shiny Apps\\ePoster')
library(shiny); library(graphics)
source('./tstat_functions.R')
source('./pval_functions.R')
source('./combined_functions.R')
source('./slope_int_fcns.R')
source('./circle_fcns.R')
source('./parabola_fcns.R')
source('./unit_circle_fcns.R')
source('./sim2sample_fcns.R')
source('./ci_fcns.R')
source('./fixed_fcns.R')
source('./mixed_fcns.R')
source('./random_fcns.R')
source('./intro.R')

ui <- navbarPage(title='Shiny Apps', inverse=T,
  tabPanel('Introduction', intro_ui()),
  tabPanel('Unit Circle', unit_circle_ui()),
  navbarMenu(title='Algebra', 
    tabPanel('Slope-intercept form', slope_int_ui()),
    tabPanel('Circle', circle_ui()),
    tabPanel('Parabola', parabola_ui())
  ),
  navbarMenu(title='Two-sample t test',
    tabPanel('Simulate 2 samples', sim2sample_ui()),
    tabPanel('Test statistic', tstat_ui()),
    tabPanel('P-value', pval_ui()), 
    tabPanel('Test statistic and p-value', combined_ui()),
    tabPanel('Confidence interval for difference in means', ci_ui())
  ), 
  navbarMenu(title='ANOVA models with random effects', 
    tabPanel('Fixed effects', fixed_ui()),
    tabPanel('Mixed effects', mixed_ui()),
    tabPanel('Random effects', random_ui())
  )
)

server <- function(input, output, session) {
  slope_int_server(input, output, session)
  circle_server(input, output, session)
  parabola_server(input, output, session)
  unit_circle_server(input, output, session)
  sim2sample_server(input, output)
  tstat_serv(input, output)
  pval_serv(input, output)
  combined_serv(input, output)
  ci_server(input, output)
  fixed_server(input, output)
  mixed_server(input, output)
  random_server(input, output)
}

shinyApp(ui = ui, server = server)