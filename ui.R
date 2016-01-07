
library(shiny)


shinyUI(
  fluidPage(
    # Application title
    titlePanel("Waterfall for Evaluation"),
    fluidRow(
      p("This application creates a waterfall table and plot for viewing impact evaluation results, specifically with energy efficiency savings programs in mind.")
    ),
    fluidRow(
      # Sidebar with a slider input for number of bins
      column(4,
        h4("What are Your Key Values?"),
        numericInput("gross.xa","Gross Reported",100,
                     min=0), # gross.xa
        numericInput("ntg.xa", "NTG Reported", 1,
                     min=0, max=2, step=.1), # ntg.xa
        numericInput("ntg.xp", "NTG Evaluated", 1,
                     min=0, max=2, step=.1) # ntg.xp
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
          selected = "gross", inline = FALSE, width = NULL
      ), # end select input
      p(style="font-size:80%", em("Gross Permutation")," is Gross Ex Ante, Parameters, Gross Ex Post, Net Ex Post"),
      p(style="font-size:80%", em("Net Permutation"),"is Gross Ex Ante, Net Ex Ante, Parameters, Net Ex Post")
    ) # plot column
  ),
  fluidRow(h4("Impact Parameters"),
           p("Enter the parameter names, like 'ISR' or 'HOU', and their values here.")),
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
  h4("Plot Adjustments"),
  p("The plot and table update with each change. Scroll down to see the output."),
  column(4,
    textInput("xaxisl","X axis Title",""),
    textInput("yaxisl","Y axis Title","")
    ),
    column(4,
      selectInput("color1","Decrease Color:",
            c("l. blue" = "lightblue3",
              "green" = "darkolivegreen3",
              "orange" = "darkorange1",
              "red" = "firebrick1"), selected="lightblue3")
    ),
  column(4,
       selectInput("color2","Increase Color:",
            c("d. blue" = "navy",
              "d. green" = "springgreen4",
              "brown" = "tan4",
              "purple" = "purple3"), selected="navy")
    )),
    hr(),
  # Here is the start of the output
    h4("The Waterfall Plot"),
    fluidRow(plotOutput("myPlot")),
    hr(),
    h4("The Permutation Table"),
    fluidRow(
      column(3,
             p("This table can be copied and pasted into Excel or another software tool if you prefer to make your plots there. Also, the evalwaterfallr package in R allows more control over the plot than this application. See credits for the package link.")
             ),
      column(9,
             tableOutput("table")
             )),
    
  # Here is the start of the credits
  hr(),
  fluidRow(
    column(3,
    img(src='emilogo.jpg', align = "right",height=140,width=200)
    ),
    column(9,
           p("This application was developed by ", a("EMI Consulting", href="http://emiconsulting.com"), "in collaboration with PG&E."),
           p("For full reference, please see the ", a("emiwaterfallr package on GitHub", href="https://github.com/EMIjess/evalwaterfallr.git")), 
p("For more information on the motivation for this package, see Kasman, Robert, Adam Scheer, Rachel Sackman, Rafael Friedmann, and Janice Berman. 2015. “Development of Order-Independent Waterfall Graphics to Enable Comprehensive Understanding of Impact Evaluation Results.” Proceedings of the 2015 International Energy Program Evaluation Conference", a("at the IEPEC proceedings website.", href="http://www.iepec.org/wp-content/uploads/2015/papers/022.pdf")))
  ) # end of credits
  )
)
