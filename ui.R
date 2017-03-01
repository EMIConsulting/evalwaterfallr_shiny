
# if(!require(shiny)){
#     install.packages("shiny")
#     library(shiny)
# }
library(markdown)

navbarPage(theme = "bootstrap.css", "Waterfall for Evaluation",
           tabPanel("Overview",
      tags$head(tags$script(HTML('
        var fakeClick = function(tabName) {
          var dropdownList = document.getElementsByTagName("a");
          for (var i = 0; i < dropdownList.length; i++) {
            var link = dropdownList[i];
            if(link.getAttribute("data-value") == tabName) {
              link.click();
            };
          }
        };
      '))),
                    fluidPage(
                              fluidRow(
                                h2("Overview"),
                                p("This application creates waterfall tables and plots with order-independent steps for viewing energy efficiency impact evaluation results. This overview briefly describes the types of evaluation data considered for this application, describes the underlying calculations, shows example output, and describes how to use the application for your data."),
                                p("If you already know what you are doing here, skip straight to the Data tab and start entering data.",
                              shiny::actionButton(inputId = "o1",label="Data", icon = icon("tasks", lib="glyphicon"), onclick = "fakeClick('Data')"))),
                              fluidRow(
                                h2("Types of Evaluation Data Considered"),
                                p("There are two kinds of evaluation data considered for this application: additive and multiplicative."),
                                column(width = 6,
                                       withTags({
                                       div(class = "evalguidancebox",
                                       p("Additive Parameters",
                                         class = "evalguidancetitle"),
                                       div(
                                         ul(
                                           li("Parameters are not interdependent."),
                                           li("Parameters that represent multiple programs in a portfolio, such as Lighting, HVAC, and Appliances.")
                                         ),
                                         class = "evalguidancetext")
                                       )
                                       }) # end with tags
                                ), # end additive column
                                column(width = 6,
                                       withTags({
                                       div(class = "evalguidancebox",
                                       p("Multiplicative Parameters",
                                         class = "evalguidancetitle"),
                                       div(
                                         ul(
                                           li("Dimensionless factors that represent the ratio of ex post to ex ante engineering parameters that are used to calculate savings, such as Hours of Use and Installation Rate."),
                                           li("The Gross Realization Rate (GRR) is calculated by multiplying these factors.")
                                         ),
                                         class = "evalguidancetext")
                                       )
                                       }) # end with tags
                                ) # end multiplicative column
                              ),  # end types of eval parameters considered
                              fluidRow(
                                h2("Waterfall Graphics"),
                                p("There are three kinds of waterfall graphics calculated by this application: Gross, Net, and Hybrid. The functional form of the calculation varies depending on whether the parameters are additive or multiplicative. These waterfalls all contain order independent steps. The permutation computational procedure in Kasman et al. is the basis for ensuring order-independence. Please note that this application can handle up to 6 parameters. For problems with more than 6 parameters, one can download the evalwaterfallr package and run the analysis on your local machine."),
                                column(width = 4,
                                       withTags({div(class = "evalguidancebox",
                                       p("Gross Waterfall", class = "evalguidancetitle"),
                                       div(class = "evalguidancetext",
                                        "Gives the ex ante/ex post discrepancies in the gross domain, showing the path from ex ante gross to ex post gross. Subsequent application of the ex post net-to-gross yields ex post net.")
                                       )}) # end with tags
                                ), # end Gross column
                                column(width = 4,
                                       withTags({div(class = "evalguidancebox",
                                       p("Net Waterfall", class = "evalguidancetitle"),
                                       div(class = "evalguidancetext",
                                           "Applies ex ante net-to-gross to convert ex ante gross to ex ante net savings. The path from ex ante net to ex post net then includes the impact parameter adjustments and the ratio of ex post to ex ante net-to-gross, or the net-to-gross realization rate")
                                       )}) # end with tags
                                ), # end Net column
                                column(width = 4,
                                       withTags({div(class = "evalguidancebox",
                                       p("Hybrid Waterfall", class = "evalguidancetitle"),
                                       div(class = "evalguidancetext",
                                           "Connects ex ante gross directly to ex post net via the impact parameter adjustments along with the ex ante net-to-gross and the net-to-gross realization rate. No ex post gross or ex ante net ‘stopping points’ are computed. For readers most interested in the ex ante/ex post discrepancies from ex ante gross to ex post net, the Hybrid waterfall provides the most accurate picture.")
                                       )}) # end with tags
                                ) # end Hybrid column
                              ), # end Permutations
# begin examples in overview                              
                              fluidRow(
                                h2("Example Output"),
                                p("The application outputs both tables of savings adjustments and the corresponding waterfall plots. Here, we show examples of the output for arbitrary data.")
                              ),
                              fluidRow(
                                h3("Additive Example"),
                                p("Assume ex ante gross reported savings are100 (arbitrary units). Three additive adjustments, A, B, and C link ex ante gross to ex post gross. The ex ante Net to Gross (NTG) is 0.8, and an ex post NTG is 0.6. We input these data into the evalwaterfallr, and the order-independent gross, net and hybrid waterfall graphics as shown below." ),
                                column(4,
                                       #DT::dataTableOutput("add_gross_tab")
                                       withTags({div(class = "evalguidancebox",
                                       p("Gross Plot", class = "evalguidancetitle"),
                                       img(src='add_gross.png', style = "max-width:100%;max-height:99%;")
                                       )})),
                                column(4,
                                       withTags({div(class = "evalguidancebox",
                                       p("Net Plot", class = "evalguidancetitle"),
                                       #DT::dataTableOutput("add_net_tab")
                                       img(src='add_net.png', style = "max-width:100%;max-height:99%;")
                                       )})),
                                column(4,
                                       withTags({div(class = "evalguidancebox",
                                       p("Hybrid Plot", class = "evalguidancetitle"),
                                       #DT::dataTableOutput("add_hybrid_tab")
                                       img(src='add_hybrid.png', style = "max-width:100%;max-height:99%;")
                                       )}))
                              ), # end additive example
                                fluidRow(
                                h3("Multiplicative Example"),
                                p("Again assume ex ante gross is 100. Three multiplicative parameters (Hours of Use (HOU), delta Watts, and Installation Rate (ISR)) are used to calculate savings. The ex post to ex ante ratio of these parameters are multiplicative adjustments that connect ex ante gross to ex post  gross. The ex ante Net to Gross (NTG) is 0.65, and ex post NTG is 0.85. Upon inputting these data into the evalwaterfallr, the following order-independent waterfall plots are generated." ),
                                column(4,
                                       #DT::dataTableOutput("add_gross_tab")
                                       withTags({div(class = "evalguidancebox",
                                       p("Gross Plot", class = "evalguidancetitle"),
                                       img(src='mult_gross.png', style = "max-width:100%;max-height:99%;")
                                       )})),
                                column(4,
                                       withTags({div(class = "evalguidancebox",
                                       p("Net Plot", class = "evalguidancetitle"),
                                       #DT::dataTableOutput("add_net_tab")
                                       img(src='mult_net.png', style = "max-width:100%;max-height:99%;")
                                       )})),
                                column(4,
                                       withTags({div(class = "evalguidancebox",
                                       p("Hybrid Plot", class = "evalguidancetitle"),
                                       img(src='mult_hybrid.png', style = "max-width:100%;max-height:99%;")
                                       )}))
                                ), # end Multiplicative Example
                               # end Example Output
#                        
# Here is the start of HOW TO USE on the Overview page
#                              
                              fluidRow(
                                h2("How to Use"),
                                h3("Step 1: Enter your Data"),
                                       p("Go to the 'Data' tab and enter your data. Enter all data on the Data tab. This requires that you know if your data are additive or multiplicative, that you have the reported gross, reported NTG, evaluated NTG, and parameter values. There is some guidance on the Data tab to help you fill it in."),
                                img(src='example_data.png', style = "max-width:50%;max-height:49%;"),
                                h3("Step 2: Check your Output"),
                                       p("Go to the 'Output' tab and click the 'Update Tables & Plots' button. When you first go to the 'Output' tab, there won't be any output: "), 
                                img(src='example_output1.png', style = "max-width:50%;max-height:49%;"),
                                p("The page will reload with your data. Review that the tables and plots are what you expect - focus on the 'Given' values in the tables (this is what you entered) and the final Ex Post Net value. If anything is not right, go back to the 'Data' tab and revise. You can rerun the output multiple times."),
                                img(src='example_output2.png', style = "max-width:50%;max-height:49%;"),
                                h3("Step 3: Download your Output"),
                                       p("When you are satisfied with your tables and plots, click the 'Download Tables' button to download a zip file of the three permutation tables (as .csv) for your data and/or the 'Download Plots' button to download a zip file of the three permutation waterfall plots (as .png) for your data."),
                                h2("You are ready - Go enter some Data!",
                                shiny::actionButton(inputId = "o2", label="Data", icon = icon("tasks", lib="glyphicon"), onclick = "fakeClick('Data')"))
                              ) # end How to Use
                              )     
                    ), #end of overview tab

########### 
#
# beginning data entry tab
#
###########
                    tabPanel("Data",icon = icon("tasks", lib="glyphicon"),
      tags$head(tags$script(HTML('
        var fakeClick = function(tabName) {
          var dropdownList = document.getElementsByTagName("a");
          for (var i = 0; i < dropdownList.length; i++) {
            var link = dropdownList[i];
            if(link.getAttribute("data-value") == tabName) {
              link.click();
            };
          }
        };
      '))),
                             
                        fluidPage(
                          p("Enter your Data on this Page, then go to look at your Output.",
                            shiny::actionButton(inputId = "d1", label="Output", icon = icon("object-align-top", lib="glyphicon"), onclick = "fakeClick('Output')")),
# data start by choose Additive or Multiplicative
                                fluidRow( # start with Add/Mult
                                  div(class = "evalguidancebox",
                                      p(class = "evalguidancetitle", "Type of Parameters"),
                                       div(class = "evalguidancetext",
                                        fluidRow(
                                        column(4, 
                                           "Is your data: additive or multiplicative?",
                                        selectInput("add_mult", "", 
                                            c("Choose one" = "", c("Additive","Multiplicative")))
                                        ),
                                       column(8,
                                       p(style="font-size:80%; color: #808080","Select whether your parameters are additive or multiplicative. These two kinds of parameters rely on different permutation calculations. Additive parameters may be from different programs, while multiplicative parameters are likely from the same program or measure, such as the Hours of Use and Installation Rate for a measure.")                      
                                      ))))), # done with Add/Mult
# data enter key values                                
                                fluidRow( # key values
                                  div(class = "evalguidancebox",
                                      p(class = "evalguidancetitle", "Key Values"),
                                       div(class = "evalguidancetext",
                                           fluidRow( # put the key value entries side by side
                                            column(4,
                                      numericInput("gross.xa","Gross Reported",100, min=0), # gross.xa
             p(style="font-size:60%; color: #808080","The Gross Reported value is equivalent to Ex Ante Gross savings.")),
                                            column(4,
                                      numericInput("ntg.xa", "NTG Reported", 1, min=0, max=2, step=.1), # ntg.xa
             p(style="font-size:60%; color: #808080","The fraction of the ex ante gross savings that is predicted to occur due to program influence. The function defaults to 1.")),
                                            column(4,
                                      numericInput("ntg.xp", "NTG Evaluated", 1,min=0, max=2, step=.1), # ntg.xp
             p(style="font-size:60%; color: #808080","The fraction of the gross savings that are found by evaluation to occur due to program influence. The function defaults to 1."))
                                      )))), # done with key values

# data enter impact parameters                                
                                fluidRow( # Impact Parameters
                                  div(class = "evalguidancebox",
                                      p(class = "evalguidancetitle", "Impact Parameters"),
                                       div(class = "evalguidancetext",
                                           fluidRow(
                                        column(4, 
                                            sliderInput(
                                             "nparams", "How many Impact Parameters do you have?",
                                             min = 2,
                                             max = 6, #reasonable stop
                                             value = 4)
                                        ),
                                       column(8,
                                           p(style="font-size:80%; color: #808080","This controls the number of impact parameters that will be permuted. Typically, there will be 3 or 4. The evalwaterfallr package can handle up to 10. This application can handle up to 6."))),
                                           p(style="font-size:80%; color: #808080","Enter the parameter names, like 'ISR' or 'HOU', and their values below. All parameters default to 1.", strong("Additive parameters")," are gross adjustments (which can take positive or negative values).", strong("Multiplicative parameters")," are expected as realization ratios: (Evaluated Value/Expected Value). Multiplicative parameters should be greater than zero. For example, a multiplicative parameter with value 0.7 had evaluated results that were 70% of expected."),
  fluidRow(
    column(2,
           textInput("p1name","Param 1 Name","Param 1"),
           textInput("p1value","Param 1 Value",1)
    ),
    column(2,
           textInput("p2name","Param 2 Name","Param 2"),
           textInput("p2value","Param 2 Value",1)
    ),
    column(2,
           conditionalPanel(# param 3
             condition = "input.nparams >= 3",
             textInput("p3name","Param 3 Name","Param 3"),
             textInput("p3value","Param 3 Value",1)
           )
    ),
    column(2,
           conditionalPanel(# param 4
             condition = "input.nparams >= 4",
             textInput("p4name","Param 4 Name","Param 4"),
             textInput("p4value","Param 4 Value",1)
           )
    ),
    column(2,
           conditionalPanel(#param 5
             condition = "input.nparams >= 5",
             textInput("p5name","Param 5 Name","Param 5"),
             textInput("p5value","Param 5 Value",1)
           )
    ),
    column(2,
           conditionalPanel(#param 6
             condition = "input.nparams >= 6",
             textInput("p6name","Param 6 Name","Param 6"),
             textInput("p6value","Param 6 Value",1)
           )
    )
  ) # end impact parameter row
                                      )
                                  ) # done with Impact Parameters
                                
                                   ))), # end of data entry tab

########### 
#
# beginning Output tab
#
###########
                    tabPanel("Output", icon = icon("object-align-top", lib="glyphicon"),
                             fluidPage(
                               fluidRow(
                                 div(h4("When you finish entering or making adjustments to data or appearance, rerun the tables and plots: ",
                                       actionButton("button","Update Tables & Plots")))),
                                       
                               fluidRow(h4("Minor adjustments to the plot appearance can be made here:"),
                                 column(6, h4("Change the Axis Titles")), column(6, h4("Change the Bar Colors"))),
                               fluidRow(
                                 column(3,textInput("xaxisl","X axis Title","")),
                                 column(3,textInput("yaxisl","Y axis Title","")),
                                 column(3,selectInput("color1","Decrease Color:",
                                                    c("red" = "#CC3300",
                                                      "l. blue" = "lightblue3",
                                                      "green" = "darkolivegreen3",
                                                      "orange" = "darkorange1"), selected="#CC3300")),
                                 column(3, selectInput("color2","Increase Color:",
                                                    c("green" = "#009900",
                                                      "d. blue" = "navy",
                                                      "brown" = "tan4",
                                                      "purple" = "purple3"), selected="#009900"))
                                 ),
                               div(h4("When you are happy with the output, you can download all three tables and/or plots: ",
                                       downloadButton('downloadDataTab', 'Download Tables'),
                                       downloadButton('downloadDataFig', 'Download Plots')
                                 )),
                               fluidRow(
                                       verticalLayout( 
                                         div(class = "evalguidancebox",
                                          p(class = "evalguidancetitle", "Gross Permutation Table and Plot"),
                                          div(class = "evalguidancetext",
                                              div(splitLayout(tableOutput("gross_tab"), 
                                               plotOutput("myPlotg"))))),
                                         div(class = "evalguidancebox",
                                          p(class = "evalguidancetitle", "Net Permutation Table and Plot"),
                                          div(class = "evalguidancetext",
                                              div(splitLayout(
                                         tableOutput("net_tab"),
                                         plotOutput("myPlotn"))))),
                                         div(class = "evalguidancebox",
                                          p(class = "evalguidancetitle", "Hybrid Permutation Table and Plot"),
                                          div(class = "evalguidancetext",
                                              div(splitLayout(
                                         tableOutput("hybrid_tab"), 
                                         plotOutput("myPloth")))))
                                       )
                             ))

                    ), # end of output



########### 
#
# beginning Help tab
#
###########                    
                    tabPanel("Help", icon = icon("wrench", lib="glyphicon"),
                             fluidRow(
                               withTags({
                                       div(class = "evalguidancebox",
                                       p("FAQs",
                                         class = "evalguidancetitle"),
                                       div(
                                         ul(
                                           li(class="myblankspace",strong("I'm getting a value error: "), "You may have set negative parameters for a multiplicative permutation. If you are using a multiplicative permutation, the parameters must be greater than 0; values of less than 1 reference a lower than reported value."),
                                           li(class="myblankspace",strong("I'm getting a net evaluated value of zero: "), "If you have a multiplicative parameter that has value 0, the final result is 0. If the reported and evaluated parameter impacts are the same, the parameter value should be 1. The same will occur if either the evaluated NTG or reported NTG are zero."),
                                            li(class="myblankspace",strong("The Date Modified of my downloaded tables and plots does not match my timezone: "), "The downloaded files have a timestamp of GMT by default."),
                                           li(class="myblankspace",strong("I'm getting huge values: "), "You may have identified multiplicative when you need additive permutation. Additive parameters are the values that you would add or subtract from the Gross or Net Reported values to get to the Evaluated values."),
                                           li(class="myblankspace",strong("The page is all disorganized: "), "We may have a browser issue. This application was tested on Chrome. There are known issues with bootstrap renders on Safari and Internet Explorer. Can you try again with another browser?"),
                                           li(class="myblankspace",strong("I have more than 6 parameters, how can I get a permutation: "), "Because of the time it takes to permute more than 6 parameters (this becomes more than 8 parameters when we consider a hybrid permutation), this application does not include it. You can download the evalwaterfallr package from Github (see link in footer) and work from there. If you have many parameters, you may need to modify the wParamPermute() function and ensure that you are running multiple cores."),
                                           li(class="myblankspace",strong("I have another question: "), "Please submit an issue on the Github page for the evalwaterfallr package. We will try to help you as soon as practical.")
                                         ),
                                         class = "evalguidancetext")
                                       )
                                       }) # end with tags
                               ), # end FAQs
          div(class="myblankspace",            
          source('footer_credits.R', local=TRUE)$value)

                    ) # End of Help TabPanel
           ) # end of NavbarPage
           
           
