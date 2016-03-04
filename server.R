
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
## Uncomment the next line to install devtools and shiny if needed
# if(!require(devtools)){
#     install.packages("devtools")
#     library(devtools)
# }
library(devtools)
library(shiny)
library(evalwaterfallr)
#install_github("EMIjess/evalwaterfallr",
#               options(download.file.method = "libcurl")) # requires this package

# if(!require(shiny)){
#     install.packages("shiny")
#     library(shiny)
# }

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
    # function that capitalizes
    capwords <- function(s, strict = FALSE) {
      cap <- function(s) paste(toupper(substring(s, 1, 1)),
                               {s <- substring(s, 2); if(strict) tolower(s) else s},
                               sep = "", collapse = " " )
      sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
    }
    colnames(mytab) <- capwords(colnames(mytab))
    mytab$Given[mytab$Given==mytab$Total] <- ""
    colnames(mytab)[colnames(mytab) == 'Given'] <- 'Impact Parameter'
    mytab$Change <- ifelse(is.na(mytab$Decrease), 0,
                           ifelse(mytab$Decrease > 0, mytab$Decrease*(-1), 
                                  mytab$Increase)) #blank or neg or increase
    customRound <- function(value){
      mydigits <- ifelse(abs(value) <= 1, 2, # less than 1, 2 digits
                     ifelse(abs(value) <= 10, 1,
                            0)) # more than 10, no digits
      round(value, digits = mydigits)
    }
    
    mytab$Change <- customRound(mytab$Change) # round the Change value
    mytab$Change[mytab$Change==0] <- ""
    mytab <- mytab[c("Variable","Total","Impact Parameter","Change")]
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
                         ifelse(input$selectTab=="gross", "Gross Permutation Plot",
                                ifelse(input$selectTab=="net", "Net Permutation Plot",
                                       "Figure"))) 
  })
  output$tblcaption <- renderUI({ 
    HTML(tblcaption())
  })
  output$figcaption <- renderUI({ 
    HTML(figcaption())
  })
  output$tblcaption_self <- renderUI({ 
    HTML(input$tblcaption_self)
  })
  output$figcaption_self <- renderUI({ 
    HTML(input$figcaption_self)
  })
  
  output$table <- renderTable({ #print the table
    mytable()}, 
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
      ylab = myaxisl[2],
      xtextangle = 0
    ) 
  })
  output$myPlot <- renderPlot({
    createPlot()
    
  }) # distPlot
  
})
