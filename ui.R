
# if(!require(shiny)){
#     install.packages("shiny")
#     library(shiny)
# }



shinyUI(
  fluidPage(theme = "bootstrap.css",
    # Application title
    h1("Waterfall for Evaluation"),
    fluidRow(
      p("This application creates a waterfall table and plot for viewing impact evaluation results, specifically with energy efficiency savings programs in mind.",
        style = "color: #808080; text-align: left; padding: 0px 10px")
    ),
    
    # first we need key values
    fluidRow(
      h4("What are Your Key Values?",
       style = "text-align: left; padding: 0px 10px"),
      column(4,
             numericInput("gross.xa","Gross Reported",100,
                          min=0), # gross.xa
             p(style="font-size:60%; color: #808080","The Gross Reported value is equivalent to Ex Ante Gross savings.")),
      column(4,
             numericInput("ntg.xa", "NTG Reported", 1,
                          min=0, max=2, step=.1), # ntg.xa
             p(style="font-size:60%; color: #808080","The fraction of the ex ante gross savings that is predicted to occur due to program influence. The function defaults to 1.")),
      column(4,
             numericInput("ntg.xp", "NTG Evaluated", 1,
                          min=0, max=2, step=.1), # ntg.xp
             p(style="font-size:60%; color: #808080","The fraction of the gross savings that are found by evaluation to occur due to program influence. The function defaults to 1."))
    ), # key values row
    
    # Now for impact parameters
    
    fluidRow( # impact parameters rows

      h4("Impact Parameters",
       style = "text-align: left; padding: 0px 10px"),
      column(5, 
      sliderInput(
        "nparams", "How many Impact Parameters do you have?",
        min = 2,
        max = 6, #reasonable stop
        value = 4)),
      column(7,
      p(style="font-size:80%; color: #808080","This controls the number of impact parameters that will be permuted. Typically, there will be 3 or 4. The evalwaterfallr package can handle up to 10. This application can handle up to 6."),
      p(style="font-size:80%; color: #808080","Enter the parameter names, like 'ISR' or 'HOU', and their values below. Parameters are expected as realization ratios: (Evaluated Value/Expected Value). For example, a parameter with value 0.7 had evaluated results that were 70% of expected. All parameters default to 1: the evaluated value was the same as expected.")
      )),
  fluidRow(
    column(2,
           textInput("p1name","Param 1 Name","Param 1"),
           numericInput("p1value","Param 1 Value",1,
                        min=0, max=NA, step=.1)
    ),
    column(2,
           textInput("p2name","Param 2 Name","Param 2"),
           numericInput("p2value","Param 2 Value",1,
                        min=0, max=NA, step=.1)
    ),
    column(2,
           conditionalPanel(# param 3
             condition = "input.nparams >= 3",
             textInput("p3name","Param 3 Name","Param 3"),
             numericInput("p3value","Param 3 Value",1,
                          min=0, max=NA, step=.1)
           )
    ),
    column(2,
           conditionalPanel(# param 4
             condition = "input.nparams >= 4",
             textInput("p4name","Param 4 Name","Param 4"),
             numericInput("p4value","Param 4 Value",1,
                          min=0, max=NA, step=.1)
           )
    ),
    column(2,
           conditionalPanel(#param 5
             condition = "input.nparams >= 5",
             textInput("p5name","Param 5 Name","Param 5"),
             numericInput("p5value","Param 5 Value",1,
                          min=0, max=NA, step=.1)
           )
    ),
    column(2,
           conditionalPanel(#param 6
             condition = "input.nparams >= 6",
             textInput("p6name","Param 6 Name","Param 6"),
             numericInput("p6value","Param 6 Value",1,
                          min=0, max=NA, step=.1)
           )
    )
  ), # end impact parameter row
  fluidRow(
    h4("Adjustments",
       style = "text-align: left; padding: 0px 10px"),
    p(" Minor adjustments to the plot visualization can be made here. Scroll down to see the output plot and table.",
      style = "color: #808080; text-align: left; padding: 0px 10px"),
    
    
    column(4,
           radioButtons("selectTab", "Plot & Table View:", 
                        c("No Permutation" = "none",
                          "Gross Permutation" = "gross",
                          "Net Permutation" = "net"), 
                        selected = "gross", inline = FALSE, width = NULL
           ), # end select input
           p(style="font-size:80%; color: #808080", em("Gross Permutation")," is Gross Ex Ante, Parameters, Gross Ex Post, Net Ex Post",em("Net Permutation"),"is Gross Ex Ante, Net Ex Ante, Parameters, Net Ex Post."),
           actionButton("button","Update Table & Plot")
    ), # permutation column
    column(4,
           textInput("xaxisl","X axis Title",""),
           textInput("yaxisl","Y axis Title","")
           
    ),
    column(4,
           selectInput("color1","Decrease Color:",
                       c("l. blue" = "lightblue3",
                         "green" = "darkolivegreen3",
                         "orange" = "darkorange1",
                         "red" = "firebrick1"), selected="lightblue3"),
           
           selectInput("color2","Increase Color:",
                       c("d. blue" = "navy",
                         "d. green" = "springgreen4",
                         "brown" = "tan4",
                         "purple" = "purple3"), selected="navy")
    )),
  hr(),
  # Here is the start of the output
  fluidRow(
    
    column(4,
          h4(htmlOutput("tblcaption"),
          #h5("The Permutation Table",
          style = "text-align: left; padding: 0px 10px"),
          tableOutput("table"),
          p("This table can be copied and pasted into Excel or another software tool if you prefer to make your plots there.", 
            style="font-size:80%; color: #808080")#output
          ),
    column(8,
           h4(htmlOutput("figcaption"),
          #h5("The Waterfall Plot",
          style = "text-align: left; padding: 0px 10px"),
          plotOutput("myPlot"), #output
          p("The ", a("emiwaterfallr package on GitHub", href="https://github.com/EMIjess/evalwaterfallr.git")," allows more control over the plot than this application.", 
            style="font-size:80%; color: #808080")
          )
    ),

  hr(),
 

  # Here is the start of the credits
  hr(),
  fluidRow(
    column(3,
           img(src='emilogo.jpg', align = "right",height=140,width=200)
    ),
    column(9,
           p("This application was developed by ", a("EMI Consulting", href="http://emiconsulting.com"), "in collaboration with PG&E.",em("If you use this application for your evaluation efforts, please give us credit, like so: 'This table/plot was produced with the evalwaterfallr package developed by EMI Consulting and PG&E.'")),
           p("For full reference, please see the ", a("emiwaterfallr package on GitHub", href="https://github.com/EMIjess/evalwaterfallr.git")), 
           p("For more information on the motivation for this package, see Kasman, Robert, Adam Scheer, Rachel Sackman, Rafael Friedmann, and Janice Berman. 2015. “Development of Order-Independent Waterfall Graphics to Enable Comprehensive Understanding of Impact Evaluation Results.” Proceedings of the 2015 International Energy Program Evaluation Conference", a("at the IEPEC proceedings website.", href="http://www.iepec.org/wp-content/uploads/2015/papers/022.pdf")))
  ) # end of credits
)
)
