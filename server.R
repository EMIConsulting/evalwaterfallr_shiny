
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
#                options(download.file.method = "libcurl")) # requires this package

# if(!require(shiny)){
#     install.packages("shiny")
#     library(shiny)
# }

`%then%` <- shiny:::`%OR%`
shinyServer(function(input, output, session) {

#################
#
# get the key inputs
#
################
  mym <- reactive({ # number of parameters
    m <- ifelse(is.null(input$nparams),1, input$nparams)
  })
  myaddm <- reactive({ # is it additive or multiplicative?
        validate(
    need(input$add_mult !="", "Please put in the details under Data")
    )
    #  if (input$submit > 0) {
     if(input$add_mult == "Additive"){
       type <- "add"
     }else if(input$add_mult == "Multiplicative"){
       type <- "mult"
     }
    return(type)
  }) #mytype 
  
  myparams <- reactive({
    mycutoff <- max(1, mym(), na.rm=TRUE) #never less than 1 param

    df <- data.frame(
      param.names = c(input$p1name, input$p2name, input$p3name, 
                      input$p4name, input$p5name, input$p6name),
      values = c(input$p1value, input$p2value, input$p3value, 
                 input$p4value, input$p5value, input$p6value),
      stringsAsFactors=FALSE)
    df$values <- as.numeric(df$values) # may look like string
    
    df <- df[c(1:mycutoff),] # limit to the number of parameters
    return(df)
  }) #myparams
  
  mygiven <- reactive({ # these are the input gross and NTG values or default
    #  if (input$submit > 0) {
    tdf <- data.frame(
      variables = c("Gross.XA","NTG.XA","NTG.XP"),
      values = c(input$gross.xa, input$ntg.xa, input$ntg.xp),
      stringsAsFactors=FALSE)
    tdf$values <- as.numeric(tdf$values) #may look like string
    return(tdf)
  }) #mygiven

#################
#
# #calculate the tables
#
################ 
  
  mytables <- eventReactive(input$button, {

    library(evalwaterfallr)
    type <- myaddm()
    mygiven <- mygiven()
    if(type == "add"){
      mytab <- evalwaterfallr::addwaterfallPrep(myparams(), 
                                           mygiven$values[1],
                                           mygiven$values[2],
                                           mygiven$values[3]) #get all output
    }else if(type == "mult"){
      mytab <- evalwaterfallr::waterfallPrep(myparams(), 
                                           mygiven$values[1],
                                           mygiven$values[2],
                                           mygiven$values[3]) #get all output
    }
    return(mytab) #this is a list of four: none, gross, net, hybrid
  }) 

  output$testtab <- renderTable({ #print the table
    myparams()}, 
    include.rownames=FALSE)
  output$testtab2 <- renderTable({ #print the table
    mygiven()}, 
    include.rownames=FALSE)
    
  #tables
  output$gross_tab <- renderTable({ #print the table
    data.frame(mytables()[[2]])}, 
    include.rownames=FALSE)
  output$net_tab <- renderTable({
     mytables()[[3]]}, rownames = FALSE)
  output$hybrid_tab <- renderTable({
     mytables()[[4]]}, rownames = FALSE)
  

#################
#
# allow some title customization
#
################ 
  
#################
#
# make the plots
#
################ 
  
  mypallette <- reactive({
    py <- c(ifelse(is.null(input$color1),"#CC3300", input$color1),
            ifelse(is.null(input$color2),"#009900", input$color2))
    
  })
  myaxisl <- reactive({ # these are reactive axis labels
    axisl <- c(ifelse(is.null(input$xaxisl),"Savings and Adjustments", input$xaxisl),
               ifelse(is.null(input$yaxisl),"", input$yaxisl))
    
  })
  
  
  createPlot <- eventReactive(input$button, {

    library(evalwaterfallr)
    mypallette <- mypallette() # user-defined or default
    myaxisl <- myaxisl() # user-defined or default
    gwp <- waterfallPlot(mytables()[[2]],
      palette = mypallette,
      xlab = myaxisl[1],
      ylab = myaxisl[2]#,
    ) 
    nwp <- waterfallPlot(mytables()[[3]],
      palette = mypallette,
      xlab = myaxisl[1],
      ylab = myaxisl[2]#,
    ) 
    hwp <- waterfallPlot(mytables()[[4]],
      palette = mypallette,
      xlab = myaxisl[1],
      ylab = myaxisl[2]#,
    ) 
    theseplots <- list(gwp, nwp, hwp)
  })
  
  
  output$myPlotg <- renderPlot({
    createPlot()[[1]]}) 
  output$myPlotn <- renderPlot({
    createPlot()[[2]]})
  output$myPloth <- renderPlot({
    createPlot()[[3]]})

#################
#
# download handlers
#
################   
  
  output$downloadDataTab <- downloadHandler(
    filename = 'permuted_tables.zip',
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())

      fs <- c("gross_tab.csv", "net_tab.csv", "hybrid_tab.csv")
      write.csv(mytables()[[2]], file = "gross_tab.csv")
      write.csv(mytables()[[3]], file = "net_tab.csv")
      write.csv(mytables()[[4]], file = "hybrid_tab.csv")
      print (fs)
      #fsf <- c("gross_fig.png", "net_fig.png", "hybrid_fig.png")

      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    },
    contentType = "application/zip"
    )
    output$downloadDataFig<- downloadHandler(
    filename = 'permuted_figures.zip',
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())

      fs <- c("gross_fig.png", "net_fig.png", "hybrid_fig.png")
      #png(createPlot()[[1]], file = "gross_fig.png")
      png(file = "gross_fig.png")
      print(createPlot()[[1]])
      dev.off()
      png(file = "net_fig.png")
      print(createPlot()[[2]])
      dev.off()
      png(file = "hybrid_fig.png")
      print(createPlot()[[3]])
      dev.off()
      

      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    },
    contentType = "application/zip"
    )
    
  
})

