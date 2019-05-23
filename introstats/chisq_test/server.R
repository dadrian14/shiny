library(shiny)
library(rhandsontable)
source('helpers.R')



shinyServer(function(input, output, session) {
  init <- 10
  output$info <- renderUI({fluidPage(
    withMathJax(),
    'The boxes below supply information about the corresponding panels below in the app.',
    fluidRow(
      column(4, wellPanel(
        tags$div('Specify the number of rows and columns for the two-way table of counts
                 and type in the counts themselves in the table below.', 
                 style='font-size: x-small')
      ), 
      wellPanel(
        tags$div('This table displays the current values of the observed and expected counts, 
                 as well as the total counts in each row and column and the overall total.  
                 The expected counts are calculated under the assumption of no association 
                 between the categorical variables representing the rows and columns, according 
                 to the formula 
                 $$\\mathrm{Expected\\ Count} = \\frac{\\mathrm{Row\\ Total}\\times\\mathrm{Column\\ Total}}{\\mathrm{Overall\\ Total}}$$', 
                 style='font-size: x-small')
      )),
      column(8, wellPanel(
        tags$div('Click on the tabs at the top of this panel (Conditions, Test statistic, P-value, 
                 or Graphical Interpretation) to display different information about the Chi-
                 squared test based on the current two-way table of counts.  (The information 
                 below will adjust to which tab is selected).', br(), br(), 
                 conditionalPanel('input.tab == "Conditions"', 
                    strong('Conditions:'), 'The expected counts must be \"large enough\" for 
                    the \\(\\chi^2\\) test distribution to follow the \\(\\chi^2\\) distribution 
                    under \\(H_0\\).  Here we will required that both of the following 
                    conditions are satisfied:', 
                    tags$ul(
                      tags$li('No expected cell counts are less than 1.'), 
                      tags$li('No more than 20% of the expected cell counts are less than 5.')
                    )
                 ),
                 conditionalPanel('input.tab == "Test statistic"', 
                   strong('Test statistic:'), 'The \\(\\chi^2\\) test statistic has the formula 
                   $$\\chi^2 = \\sum\\frac{(O-E)^2}{E},$$
                   where \\(O\\) and \\(E\\) refer to the observed and expected counts, 
                   respectively, and the \\(\\sum\\) symbol signifies that the results from each 
                   cell in the two-way table must be added together.', br(), 
                   'Note that the \\(\\chi^2\\) test statistic value is always positive, with 
                   small \\(\\chi^2\\) values occuring when the observed and expected counts are close together 
                   and large \\(\\chi^2\\) values occuring when the observed and expected counts 
                   are far apart.'
                 ),
                 conditionalPanel('input.tab == "P-value"',
                   strong('P-value:'), 'A Chi-squared test is testing whether there is an 
                   association between two categorical variables -- the ones represented by 
                   the rows and columns of the two-way contingency table.  The hypotheses are',
                   tags$ul(
                      tags$li('\\(H_0:\\) The variables are not associated (i.e. are independent).'),
                      tags$li('\\(H_a:\\) The variables are associated (i.e. are not independent).')
                   ), 
                   'The definition of the p-value is the probability of the test statistic observed 
                   or one more extreme in the direction of \\(H_a\\), assuming \\(H_0\\) is true. 
                   This probability is based on the fact that the test statistic follows the 
                   \\(\\chi^2\\) distribution under \\(H_0\\) with degrees of freedom equal to 
                   \\((r-1)(c-1)\\), where \\(r\\) and \\(c\\) stand for the number of rows and 
                   columns in the two-way table.  The p-value is the probability that this 
                   \\(\\chi^2\\) distribution takes values greater than the test statistic (greater 
                   because larger values of the test statistic are expected under \\(H_a\\).'
                 ),
                 conditionalPanel('input.tab == "Graphical Interpretation"', 
                   strong('Graphical Interpretation:'), 'Note that the rectangles are laid out 
                   like the cells of the two-way table such that the area of each rectangle is 
                   proportional to the relative size of its count.  Also, for both the observed and 
                   expected counts, the areas of each row is proportional to its row total count.
                   For the expected counts, the columns always line up because under the assumption 
                   of no association, the proportion in each column is the same for each row.  The
                   degree from which the rectangles representing the observed counts deviate from 
                   the grid representing the expected counts represents how strong the 
                   association is.'               
                 ),
                 style='font-size: x-small')
      ))
      , style="margin: 0px -20px;")
  )})
  rv <- reactiveValues(
    cur.mat=matrix(as.integer(init), nrow=2, ncol=2, dimnames=list(1:2, 1:2)), 
    last.mat=matrix(as.integer(init), nrow=2, ncol=2, dimnames=list(1:2, 1:2)),
    cur.nrow=2, last.nrow=2, cur.ncol=2, last.ncol=2, 
    exp=matrix(init, nrow=2, ncol=2)
  )
  output$table <- renderRHandsontable({
    rhandsontable(rv$cur.mat)
  })
  observeEvent(input$nrow, {
    req(input$table)
    ncol <- as.numeric(input$ncol)
    rv$last.nrow <- rv$cur.nrow
    rv$cur.nrow <- as.numeric(input$nrow)
    rv$last.mat <- as.matrix(hot_to_r(input$table))
    if(rv$cur.nrow < rv$last.nrow){
      rv$cur.mat <- rv$last.mat[1:rv$cur.nrow,]
      rownames(rv$cur.mat) <- 1:rv$cur.nrow
    }
    if(rv$cur.nrow > rv$last.nrow){
      add0 <- matrix(init, nrow=rv$cur.nrow - rv$last.nrow, ncol=ncol)
      rv$cur.mat <- rbind(rv$last.mat, add0)
      rownames(rv$cur.mat) <- 1:rv$cur.nrow
    }
    rv$cur.mat <- apply (rv$cur.mat, c (1, 2), function (x) {(as.integer(x))})
  })
  observeEvent(input$ncol, {
    req(input$table)
    nrow <- as.numeric(input$nrow)
    rv$last.ncol <- rv$cur.ncol
    rv$cur.ncol <- as.numeric(input$ncol)
    rv$last.mat <- as.matrix(hot_to_r(input$table))
    if(rv$cur.ncol < rv$last.ncol){
      rv$cur.mat <- rv$last.mat[,1:rv$cur.ncol]
      colnames(rv$cur.mat) <- 1:rv$cur.ncol
    }
    if(rv$cur.ncol > rv$last.ncol){
      add0 <- matrix(init, nrow=nrow, ncol=rv$cur.ncol - rv$last.ncol)
      rv$cur.mat <- cbind(rv$last.mat, add0)
      colnames(rv$cur.mat) <- 1:rv$cur.ncol
    }
    rownames(rv$cur.mat) <- 1:nrow
    rv$cur.mat <- apply (rv$cur.mat, c (1, 2), function (x) {(as.integer(x))})
  })
  observeEvent({input$table; rv$cur.mat}, {
    req(input$table)
    obs.cts <- rv$exp <- as.matrix(hot_to_r(input$table))
    obs.cts <- apply (obs.cts, c (1, 2), function (x) {(as.double(x))})
    rowtot <- apply(obs.cts, 1, sum)
    coltot <- apply(obs.cts, 2, sum)
    for(i in 1:nrow(obs.cts))
      for(j in 1:ncol(obs.cts))
        rv$exp[i,j] <- rowtot[i]*coltot[j]/sum(coltot)
  })
  output$twoway <- renderUI({
    req(input$table)
    req(rv$exp)
    obs <- as.matrix(hot_to_r(input$table))
    nrow <- nrow(obs); ncol <- ncol(obs)
    rowtot <- apply(obs, 1, sum)
    coltot <- apply(obs, 2, sum)
    colheads <- tagList(tags$td('Observed', br(), 
                                     HTML('<font color="red">Expected</font>')))
    for(j in 1:ncol) colheads[[j+1]] <- tags$th(paste0('Col ',j))
    colheads[[ncol+2]] <- tags$th('Total')
    bodytable <- tagList()
    for(i in 1:nrow){
      onerow <- tagList(tags$th(paste0('Row ',i)))
      for(j in 1:ncol) 
        onerow[[j+1]] <- tags$td(obs[i,j], br(), 
                                  HTML(paste0('<font color="red">',
                                              round(rv$exp[i,j], 1),' </font>')), 
                                 style='font-size: x-small;')
        onerow[[ncol+2]] <- tags$th(rowtot[i])
        bodytable[[i]] <- tags$tr(onerow)
    }
    lastrow <- tagList(tags$th('Total'))
    for(j in 1:ncol) lastrow[[j+1]] <- tags$th(coltot[j])
    lastrow[[ncol+2]] <- tags$th(sum(coltot))
    tags$table(
      tags$tr(colheads),
      bodytable,
      tags$tr(lastrow)
    )
  })
  #conditions panel
  output$conditions <- renderUI({fluidPage(
    br(),
    h4(strong('Checking conditions')),
    tags$ul(
      tags$li('No expected cell counts are < 1.', br(), uiOutput('cond1'), br()), 
      tags$li('No more than 20% of the expected cell counts are < 5.', br(), 
              uiOutput('cond2'))
    ), 
    hr(),
    strong('Overall'), 
    uiOutput('overall_cond')
  )})
  output$cond1 <- renderUI({
    min.exp <- round(min(rv$exp), 1)
    HTML(ifelse(min.exp < 1, 
           paste0('<span style="color:red">&#10008;</span> Minimum expected cell count 
                  is ', min.exp),
           paste0('<span style="color:green">&#10004;</span> Minimum expected cell count
                  is ', min.exp) 
    ))
  })
  output$cond2 <- renderUI({
    tot <- nrow(rv$exp) * ncol(rv$exp)
    perc <- sum(rv$exp < 5) / tot * 100
    fail <- paste0('<span style="color:red">&#10008; </span>', round(perc, 1), '% of 
                 expected cell counts are < 5.')
    succ <- paste0('<span style="color:green">&#10004; </span>', round(perc, 1), '% of 
                 expected cell counts are < 5.')
    HTML(ifelse(perc > 20, fail, succ))            
  })
  output$cond_tf <- reactive({
    min.exp <- round(min(rv$exp), 1)
    perc <- sum(rv$exp < 5) / (nrow(rv$exp) * ncol(rv$exp)) * 100
    return(min.exp >=1 & perc <= 20)
  })
  outputOptions(output, "cond_tf", suspendWhenHidden = FALSE)
  output$overall_cond <- renderUI({
    overall <- round(min(rv$exp), 1) >= 1 & 
      sum(rv$exp < 5) / (nrow(rv$exp) * ncol(rv$exp)) * 100 <=20
    res <- ifelse(overall==T, 
           '<span style="color:green">&#10004; </span> Both conditions are satisfied.', 
           '<span style="color:red">&#10008; </span> At least one condition is violated.')
    HTML(res)
  })
  #Test statistic tab
  output$test_stat <- renderUI({fluidPage(
    br(),
    withMathJax(),
    h4(strong('Chi-squared \\((\\chi^2)\\) test statistic')), 
    conditionalPanel('output.cond_tf == false', 
      'It\'s not appropriate to calculate the test statistic because the conditions are 
      not satisfied.', strong('(See conditions tab)')
    ),
    conditionalPanel('output.cond_tf == true', 
    strong('Value'), br(),
    '\\(\\chi^2 = \\)', textOutput('x2', inline=T), 
    hr(), 
    strong('Calculation'), br(),
    tags$table(tags$tr(
      tags$td('\\(\\chi^2 = \\sum \\frac{(O - E)^2}{E} = \\)', style='border-style: none'), 
      tags$td(uiOutput('test_stat_calc', inline=T), style='border-style: none')
    ), style='border-style: none')
    )
  )})
  output$x2 <- renderText({
    req(input$table)
    obs <- as.matrix(hot_to_r(input$table))
    x2 <- sum((obs - rv$exp)^2 / rv$exp)
    round(x2, 2)
  })
  output$test_stat_calc <- renderUI({
    req(input$table)
    obs <- as.matrix(hot_to_r(input$table))
    exp <- round(rv$exp, 1)
    nrow <- nrow(obs); ncol <- ncol(obs)
    calc <- '\\('
    for(i in 1:nrow){
      for(j in 1:ncol){
        txt <- paste0('\\frac{(', obs[i,j], '-', exp[i,j], ')^2}{', exp[i,j], '}')
        if(!(j==ncol & i==nrow)) 
          calc <- paste0(calc, txt, '+')
        else
          calc <- paste0(calc, txt)
        if(j==ncol & i!=nrow)
          calc <- paste0(calc, '\\)<br>\\(')
        
      }
    }
    calc <- paste0(calc, '\\)')
    withMathJax(HTML(calc))
  })
  output$pval_tab <- renderUI({fluidPage(
    br(),
    withMathJax(),
    h4(strong('P-value for Chi-squared Test')),
    br(),
    conditionalPanel('output.cond_tf == false', 
                     'It\'s not appropriate to calculate the p-value because the conditions are 
                     not satisfied.', strong('(See conditions tab)')
    ),
    conditionalPanel('output.cond_tf == true', 
    strong('Null distribution'), br(),
    '\\(\\chi^2\\) distribution with df = ', textOutput('df', inline=T), 
    hr(), 
    strong('P-value'), br(), 
    textOutput('pval', inline=T)
    )
  )})
  output$df <- renderText({
    res()$df
  })
  output$pval <- renderText({
    ifelse(res()$pval < .0001, '<.0001', sprintf("%.4f", res()$pval))
  })
  output$graph_tab <- renderUI({fluidPage(
    br(),
      radioButtons('which_rect', label='Show rectangles representing which counts?', 
        choiceNames=list('Observed Counts (black)', 
                         HTML('Expected Counts <span style=color:red>(red)</span>'), 
                         'Both Observed and Expected Counts'), 
        choiceValues=list('obs', 'exp', 'both')
      ),
      plotOutput('mosaic')
  )})
  output$mosaic <- renderPlot({
    req(input$table)
    obs <- as.matrix(hot_to_r(input$table))
    par(mar=c(0, 0, 0, 0), plt=c(0,1,0,1))
    plot(.5, .5, xlim=c(0,1), ylim=c(0,1), type='n', frame.plot=F, 
         axes=F, ann=F, xaxs='i', yaxs='i')
    if(input$which_rect == 'obs'){
      draw.obs.rect(obs, cts=T)
    }
    if(input$which_rect == 'exp'){
      draw.exp.rect(obs, rv, cts=T)
    }
    if(input$which_rect == 'both'){
        draw.obs.rect(obs, cts=F)
        draw.exp.rect(obs, rv, cts=F)
    }
  })
  res <- reactive({
    req(input$table)
    obs <- as.matrix(hot_to_r(input$table))
    x2 <- sum((obs - rv$exp)^2 / rv$exp)
    df <- (as.numeric(input$nrow)-1) * (as.numeric(input$ncol)-1)
    pval <- pchisq(x2, df=df, lower.tail=F)
    list(x2=x2, df=df, pval=pval)
  })
  output$x2 <- renderText({
    round(res()$x2, 2)
  })
  output$pval_pic_tab <- renderUI({fluidPage(
    withMathJax(),
      wellPanel(
        h3(strong('Picture of the p-value for the \\(\\chi^2\\) test')), 
        tags$div(
          checkboxInput('check_info_pval', label='Click for more info!'), 
          style='margin-bottom: -10px'),
        conditionalPanel('input.check_info_pval', 
          uiOutput('pval_tab_info')              
        , style='margin-bottom: -10px;')
      ),
      column(4, wellPanel(
        numericInput('df', label="Degrees of freedom (df)", min=1, value=4),
        sliderInput('stat', label="\\(\\chi^2\\) test statistic", min=0, max=20, value=7.5, 
                    step=.1),
        hr(),
        checkboxInput('change_max', label='Change maximum of test statistic'),
        conditionalPanel("input.change_max == true",
            numericInput('max_stat', label="Maximum", min=1, value=20)
        )
      )),
      column(8, wellPanel(
        plotOutput('plot', height='350px')
      ))
  )})
  output$pval_tab_info <- renderUI({fluidPage(
    withMathJax(),
    fluidRow(
      column(4, wellPanel(
        tags$div('Change the number of degrees of freedom of the \\(\\chi^2\\) 
                 distribution, which is calculated in practice as \\(df = (r-1)(c-1)\\), where 
                 \\(r\\) is the number of rows and \\(c\\) is the number of columns in the 
                 two-way table.  You can also change the value of the \\(\\chi^2\\) test 
                 statistic with the slider.  Because the \\(\\chi^2\\) distribution tends to 
                 take on larger values as the df increases, there is the option to change 
                 the maximum value shown on the test statistic slider (as well as the maximum 
                 value shown on the plot).', 
                 style='font-size: x-small')
      )),
      column(8, wellPanel(
        tags$div('This shows a picture of the \\(\\chi^2\\) distribution with the specified 
                 degrees of freedom and shows the p-value as the area under the curve 
                 for values greater than the test statistic value.', 
                 style='font-size: x-small')
      ))
      , style="margin: 0px -20px;")
  )})
    output$plot <- renderPlot({
      x <- seq(0, input$max_stat, by=.01)
      par(cex.main=1.5, cex.lab=1.3, cex.axis=1, mar=c(4.1, .1, 2.5, .1))
      plot(x, dchisq(x, df=input$df), type='l', ylab='', yaxt='n', 
           xlab='Chi-squared statistic')
      axis(side=1, at=input$stat, col.axis=2)
      segments(x0=input$stat, y0=0, y1=dchisq(input$stat, df=input$df))
      x.seq <- seq(input$stat, input$max_stat, by=.01)
      x.poly <- c(input$stat, x.seq, input$max_stat, input$stat)
      y.poly <- c(0, dchisq(x.seq, df=input$df),0,0)
      polygon(x.poly, y.poly, col=grey(.8))
      pval <- pchisq(input$stat, df=input$df, lower.tail=F)
      pval <- ifelse(pval < .0001, '<.0001', sprintf('%.4f', pval))
      title(paste0('P-value = ', pval))
    })
    observe({
      updateSliderInput(session, 'stat', value=input$stat, min=0, max=input$max_stat, step=.1)
    })
})
