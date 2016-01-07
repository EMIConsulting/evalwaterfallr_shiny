
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
## Uncomment the next line to install devtools and shiny if needed
#install.packages(c("devtools","shiny"))
library(devtools)
install_github("EMIjess/evalwaterfallr") # requires this package
library(shiny)

shinyServer(function(input, output, session) {
  mym <- reactive({ # number of parameters
     m <- ifelse(is.null(input$nparams),1, input$nparams)
  })

  myparams <- reactive({
    mycutoff <- max(1, mym(), na.rm=TRUE) #never less than 1
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
  
  
   output$table <- renderTable({
    library(evalwaterfallr)
    mygiven <- mygiven()
    mytab <- evalwaterfallr::waterfallPrep(myparams(), 
                                           mygiven$value[1],
                                           mygiven$value[2],
                                           mygiven$value[3],
                          output=input$selectTab)
  }) #table
  
  
  output$myPlot <- renderPlot({
  library(evalwaterfallr)
    mygiven <- mygiven()

    waterfallPlot(waterfallPrep(myparams(), 
                                           mygiven$value[1],
                                           mygiven$value[2],
                                           mygiven$value[3],
                          output=input$selectTab))

  }) # distPlot

})
