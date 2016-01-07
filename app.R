# shiny application for evalwaterfallr package
## Uncomment the next line to install devtools and shiny if needed
#install.packages(c("devtools","shiny"))
library(devtools)
install_github("EMIjess/evalwaterfallr") # requires this package
# server.R content
server <- function(input, output, session) {
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
  
}

# ui.R content
ui <- fluidPage(
  # Application title
  titlePanel("Waterfall for Evaluation"),
  fluidRow(
    p("This application creates a waterfall table and plot for viewing impact evaluation results, specifically with energy efficiency savings programs in mind.")
  ),
  fluidRow(
    # Sidebar with a slider input for number of bins
    column(4,
           h4("What are Your Key Values?"),
           numericInput("gross.xa","Gross Reported",100), # gross.xa
           numericInput("ntg.xa", "NTG Reported", 1), # ntg.xa
           numericInput("ntg.xp", "NTG Evaluated", 1) # ntg.xp
    ), # key values column
    column(4,
           h4("How many Impact Parameters do you have? (up to 6)"),
           sliderInput(
             "nparams", "Number of Parameters:",
             min = 1,
             max = 6, #reasonable stop
             value = 4),
           p("This controls the number of parameters you can enter data for and how many will be permuted.")
    ), #nparams column
    column(4,
           radioButtons("selectTab", "View:", 
                        c("No Permutation" = "none",
                          "Gross Permutation" = "gross",
                          "Net Permutation" = "net"), 
                        selected = "Gross Permutation", inline = FALSE, width = NULL
           ), # end select input
           p("Gross Permutation is Gross Ex Ante, Parameters, Gross Ex Post, Net Ex Post"),
           p("Net Permutation is Gross Ex Ante, Net Ex Ante, Parameters, Net Ex Post")
    ) # plot column
  ),
  fluidRow(h4("Impact Parameters"),
           p("Enter the parameter names, like 'ISR' or 'HOU', and their values here.")),
  fluidRow(
    column(2,
           textInput("p1name","Param 1 Name","Param 1"),
           numericInput("p1value","Param 1 Value",1)
    ),
    column(2,
           textInput("p2name","Param 2 Name","Param 2"),
           numericInput("p2value","Param 2 Value",1)
    ),
    column(2,
           conditionalPanel(# param 3
             condition = "input.nparams >= 3",
             textInput("p3name","Param 3 Name","Param 3"),
             numericInput("p3value","Param 3 Value",1)
           )
    ),
    column(2,
           conditionalPanel(# param 4
             condition = "input.nparams >= 4",
             textInput("p4name","Param 4 Name","Param 4"),
             numericInput("p4value","Param 4 Value",1)
           )
    ),
    column(2,
           conditionalPanel(#param 5
             condition = "input.nparams >= 5",
             textInput("p5name","Param 5 Name","Param 5"),
             numericInput("p5value","Param 5 Value",1)
           )
    ),
    column(2,
           conditionalPanel(#param 6
             condition = "input.nparams >= 6",
             textInput("p6name","Param 6 Name","Param 6"),
             numericInput("p6value","Param 6 Value",1)
           )
    )
  ), # end impact parameter row
  fluidRow(
    p("The plot and table update with each change. Scroll down to see the output.")
  ),
  hr(),
  # Here is the start of the output
  h4("The Waterfall Plot"),
  fluidRow(plotOutput("myPlot")),
  hr(),
  h4("The Permutation Table"),
  fluidRow(tableOutput("table")
  ),
  
  # Here is the start of the credits
  hr(),
  fluidRow(
    column(3,
           img(src='emilogo.jpg', align = "right",height=140,width=200)
    ),
    column(9,
           p("This application was developed by EMI Consulting, in collaboration with PG&E."),
           p("For full reference, please see the ", a("emiwaterfallr package on GitHub", href="https://github.com/EMIjess/evalwaterfallr.git")), 
           p("For more information on the motivation for this package, see Kasman, Robert, Adam Scheer, Rachel Sackman, Rafael Friedmann, and Janice Berman. 2015. “Development of Order-Independent Waterfall Graphics to Enable Comprehensive Understanding of Impact Evaluation Results.” Proceedings of the 2015 International Energy Program Evaluation Conference", a("at the IEPEC proceedings website.", href="http://www.iepec.org/wp-content/uploads/2015/papers/022.pdf")))
  ) # end of credits
)

# call shiny
shinyApp(ui = ui, server = server)

# end of file
