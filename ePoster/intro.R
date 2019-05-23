#introduction

intro_ui <- function()
{
  fluidPage(
    wellPanel(fluidRow(
      h1(strong('Shiny Apps for Teaching and Learning Statistics')),
      h2('Daniel W. Adrian'), 
      h4('Department of Statistics')
     )), 
    fluidRow(
       column(6, wellPanel(
         h2(style="text-align:center", strong('Background')), 
         hr(),
         column(6, wellPanel(style = "background-color: #ffffff;",
           p(style="text-align:center", tags$img(height='125px', src='java.jpg')),
           p('Similar in principle to', strong('Java Applets'), 'but without 
           the', strong('technical glitches.'))
         )),
         column(6, wellPanel(style = "background-color: #ffffff;",
           p(style="text-align:center", tags$img(height='125px', src='r_orb.png')), 
           p('Written in R, which is popular among 
             statistics and', strong('free'), 'to download.')
         )), 
         hr(),
         p('Students use a web browser to operate Shiny Apps.  Programming
           is', strong('not necessary.'))
       )), 
       column(6, wellPanel(
         h2(style="text-align:center", strong('Two parts of a Shiny App')), 
         hr(),
         p(style="text-align:center", tags$img(height='180px', src='two_parts.png')),
         fluidRow(column(6, wellPanel(style = "background-color: #ffffff;",
                             h3(strong('Server')),
                             'Computer running R behind the scenes that updates 
                             the webpage')),
         column(6, wellPanel(style = "background-color: #ffffff;",
                             h3(strong('User Interface')), 
                             'What is shown on the webpage and how the user
                             (student) interacts with it'))
         )
       ))
    ), 
    wellPanel(fluidRow(
      column(9, h3('Teaching and Learning with Technology Symposium')), 
      column(3, p(style="text-align:right", 
                  tags$img(height='45px', src='gv_logo.jpg')))
    ))
  )
}