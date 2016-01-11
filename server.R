
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
## Uncomment the next line to install devtools and shiny if needed
#install.packages(c("devtools","shiny"))
library(devtools)
install_github("EMIjess/evalwaterfallr",
               options(download.file.method = "libcurl")) # requires this package
library(shiny)
`%then%` <- shiny:::`%OR%`
shinyServer(function(input, output, session) {
  mym <- reactive({ # number of parameters
    m <- ifelse(is.null(input$nparams),1, input$nparams)
  })
  
  myparams <- reactive({
    mycutoff <- max(1, mym(), na.rm=TRUE) #never less than 1
    validate(
      need(input$p1value >=0, 'Parameters cannot be negative.') %then%
        need(input$p2value >=0, 'Parameters cannot be negative.') %then%
        need(input$p3value >=0, 'Parameters cannot be negative.') %then%
        need(input$p4value >=0, 'Parameters cannot be negative.') %then%
        need(input$p5value >=0, 'Parameters cannot be negative.') %then%
        need(input$p6value >=0, 'Parameters cannot be negative.') 
    )
    df <- data.frame(
      param.names = c(input$p1name, input$p2name, input$p3name, 
                      input$p4name, input$p5name, input$p6name),
      values = c(input$p1value, input$p2value, input$p3value, 
                 input$p4value, input$p5value, input$p6value),
      stringsAsFactors=FALSE)
    
    df <- df[c(1:mycutoff),] # limit to the number of parameters
  }) #myparams
  mygiven <- reactive({
    #  if (input$submit > 0) {
    tdf <- data.frame(
      variables = c("Gross.XA","NTG.XA","NTG.XP"),
      values = c(input$gross.xa, input$ntg.xa, input$ntg.xp),
      stringsAsFactors=FALSE)
  }) #mygiven
  
  #calculate the table
  mytable <- eventReactive(input$button, {
    library(evalwaterfallr)
    mygiven <- mygiven()
    mytab <- evalwaterfallr::waterfallPrep(myparams(), 
                                           mygiven$value[1],
                                           mygiven$value[2],
                                           mygiven$value[3],
                                           output=input$selectTab)
  }) #table
  
  # make the titles
  tblcaption <- eventReactive(input$button,{
    
    tblcaption <- ifelse(input$selectTab=="none", "Given Values Table",
                         ifelse(input$selectTab=="gross", "Gross Permutation Table",
                                ifelse(input$selectTab=="net", "Net Permutation Table",
                                       "Table")))
  })
  figcaption<- eventReactive(input$button,{
    figcaption <- ifelse(input$selectTab=="none", "Given Values Plot",
                         ifelse(input$selectTab=="gross", "Gross Waterfall",
                                ifelse(input$selectTab=="net", "Net Waterfall",
                                       "Figure"))) 
  })
  output$tblcaption <- renderUI({ 
    HTML(tblcaption())
  })
  output$figcaption <- renderUI({ 
    HTML(figcaption())
  })
  
  output$table <- renderTable({ #print the table
    mytable()[c("variable","total","base","decrease","increase")]}, 
    include.rownames=FALSE)
  
  mypallette <- reactive({
    py <- c(ifelse(is.null(input$color1),"lightblue3", input$color1),
            ifelse(is.null(input$color2),"navy", input$color2))
    
  })
  myaxisl <- reactive({
    axisl <- c(ifelse(is.null(input$xaxisl),"", input$xaxisl),
               ifelse(is.null(input$yaxisl),"", input$yaxisl))
    
  })
  
  
  #output$myPlot <- renderPlot({
  createPlot <- eventReactive(input$button, {
    library(evalwaterfallr)
    mygiven <- mygiven()
    mypallette <- mypallette() # user-defined or default
    myaxisl <- myaxisl() # user-defined or default
    waterfallPlot(
      waterfallPrep(
        myparams(),
        mygiven$value[1],
        mygiven$value[2],
        mygiven$value[3],
        output = input$selectTab
      ),
      palette = mypallette,
      xlab = myaxisl[1],
      ylab = myaxisl[2]
    ) 
  })
  output$myPlot <- renderPlot({
    createPlot()
    
  }) # distPlot
  
})
