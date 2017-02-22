
# if(!require(shiny)){
#     install.packages("shiny")
#     library(shiny)
# }
library(markdown)

navbarPage(theme = "bootstrap.css", "Waterfall for Evaluation",
           tabPanel("Overview",
                    fluidPage(
                              fluidRow(
                                h2("Overview"),
                                p("This application creates permuted waterfall tables and plots for viewing impact evaluation results, specifically with energy efficiency savings programs in mind. This overview briefly describes the types of evaluation data considered for this application, describes the permutations, shows example output, and describes how to use the application for your data."),
                                p("If you already know what you are doing here, skip straight to the Data tab and start entering data.")
                              ),
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
                                           li("Parameters that are not dependent upon each other."),
                                           li("Parameters that represent mulitple programs in a portfolio, such as Lighting and Pumps.")
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
                                           li("Parameters that are dependent upon each other."),
                                           li("Parameters that represent use for a single program, such as Hours of Use (HOU) and Installation Rate (ISR).")
                                         ),
                                         class = "evalguidancetext")
                                       )
                                       }) # end with tags
                                ) # end multiplicative column
                              ),  # end types of eval parameters considered
                              fluidRow(
                                h2("Permutations"),
                                p("There are three kinds of permutations calculated by this application: Gross, Net, and Hybrid. The functional form of the permutation varies depending on whether the parameters are additive or multiplicative. However, the focus of each permutation is consistent. Please note that this application can handle up to 6 parameters. If you have more than 6 parameters, please download the evalwaterfallr package and run the analysis on your local machine."),
                                column(width = 4,
                                       withTags({div(class = "evalguidancebox",
                                       p("Gross Permutation", class = "evalguidancetitle"),
                                       div(
                                         ul(
                                           li("Focus is on the gross realization rate."),
                                           li("Permutes the mulitplicative parameters with each other."),
                                           li("There is no permutation for additive parameters.")
                                         ),
                                         class = "evalguidancetext")
                                       )
                                       }) # end with tags
                                ), # end Gross column
                                column(width = 4,
                                       withTags({div(class = "evalguidancebox",
                                       p("Net Permutation", class = "evalguidancetitle"),
                                       div(
                                         ul(
                                           li("Focus is from the net reported to the net evaluated savings, or the net realization rate."),
                                           li("Permutes the multiplicative parameters with each other and the ratio of the NTG evaluated to the NTG reported."),
                                           li("Permutes the additive parameters with the ratio of the NTG evaluated to the NTG reported.")
                                         ),
                                         class = "evalguidancetext")
                                       )
                                       }) # end with tags
                                ), # end Net column
                                column(width = 4,
                                       withTags({div(class = "evalguidancebox",
                                       p("Hybrid Permutation", class = "evalguidancetitle"),
                                       div(
                                         ul(
                                           li("Focus is from the gross reported to the net evaluated savings, or the overall realization rate."),
                                           li("Permutes the multiplicative parameters with each other, the NTG reported, and the ratio of the NTG evaluated to the NTG reported."),
                                           li("Permutes the additive parameters withthe NTG reported and the ratio of the NTG evaluated to the NTG reported.")
                                         ),
                                         class = "evalguidancetext")
                                       )
                                       }) # end with tags
                                ) # end Hybrid column
                              ), # end Permutations
# begin examples in overview                              
                              fluidRow(
                                h2("Example Output"),
                                p("The application outputs both permuted tables and the related waterfall plots. Here, we show examples of the output for arbitrary data.")
                              ),
                              fluidRow(
                                h3("Additive Example"),
                                p("Let's assume we have three programs, A, B, and C with gross reported savings of 100 arbtrary units, an overall reported Net to Gross (NTG) of 0.8, and an evaluated NTG of 0.6. We input these data into the evalwaterfallr, and we get the waterfall plots of gross, net. and hybrid permutations shown below. The theory and equations are in the supporting papers. Here, we just care that the outputs (and their possible interpretations) are different based on the order in which we go from the gross reported savings to the net evaluated savings." ),
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
                                p("Let's assume we have results from a lighting program with three multiplicative parameters (Hours of Use (HOU), delta Watts, and Installation Rate (ISR)) with gross reported savings of 100 arbtrary units, an overall reported Net to Gross (NTG) of 0.65, and an evaluated NTG of 0.85. We input these data into the evalwaterfallr, and we get the waterfall plots of gross, net. and hybrid permutations shown below. The theory and equations are in the supporting papers. Here, we just care that the outputs (and their possible interpretations) are different based on the order in which we go from the gross reported savings to the net evaluated savings." ),
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
                                h2("You are ready - Go enter some Data!")
                              ) # end How to Use
                              )     
                    ), #end of overview tab

########### 
#
# beginning data entry tab
#
###########
                    tabPanel("Data",icon = icon("tasks", lib="glyphicon"),
                        fluidPage(
                          p("Enter your Data on this Page, then go to the Output tab."),
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
                                            sliderInput(
                                             "nparams", "How many Impact Parameters do you have?",
                                             min = 2,
                                             max = 6, #reasonable stop
                                             value = 4),
                                           p(style="font-size:80%; color: #808080","This controls the number of impact parameters that will be permuted. Typically, there will be 3 or 4. The evalwaterfallr package can handle up to 10. This application can handle up to 6."),
                                           p(style="font-size:80%; color: #808080","Enter the parameter names, like 'ISR' or 'HOU', and their values below. Additive parameters are gross adjustments. Multiplicative parameters are expected as realization ratios: (Evaluated Value/Expected Value). For example, a parameter with value 0.7 had evaluated results that were 70% of expected. All parameters default to 1."),
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
                             fluidPage(div("When you finish entering or making adjustments to data,  ",
                                       actionButton("button","Update Tables & Plots")),
                                       p("You can make further changes to the data and try again (verify the 'Given' column matches what you expect), or you can download all three tables and/or plots: "),
                                       downloadButton('downloadDataTab', 'Download Tables'),
                                       downloadButton('downloadDataFig', 'Download Plots'),
                                       #tableOutput("testtab"),
                                       #tableOutput("testtab2"),
                                       verticalLayout( 
                                         div(class = "evalguidancebox",
                                          p(class = "evalguidancetitle", "Gross Permutation Table and Plot"),
                                          div(class = "evalguidancetext",
                                              splitLayout(tableOutput("gross_tab"), 
                                               plotOutput("myPlotg")))),
                                         div(class = "evalguidancebox",
                                          p(class = "evalguidancetitle", "Net Permutation Table and Plot"),
                                          div(class = "evalguidancetext",
                                              splitLayout(
                                         tableOutput("net_tab"),
                                         plotOutput("myPlotn")))),
                                         div(class = "evalguidancebox",
                                          p(class = "evalguidancetitle", "Hybrid Permutation Table and Plot"),
                                          div(class = "evalguidancetext",
                                         tableOutput("hybrid_tab"), #try without split
                                         plotOutput("myPloth")))
                                       )
                             )

                    ),



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
                                           li(strong("I'm getting a value error: "), "You may have set negative parameters for a multiplicative permutation. If you are using a multiplicative permutation, the parameters must be at least 0; values of less than 1 reference a lower than reported value."),
                                           li(strong("I'm getting huge values: "), "You may have identified multiplicative when you need additive permutation. Additive parameters are the values that you would add or subtract from the Gross or Net Reported values to get to the Evaluated values."),
                                           li(strong("The page is all disorganized: "), "We may have a browser issue. This application was tested on Chrome. There are known issues with bootstrap renders on Safari and Internet Explorer. Can you try again with another browser?"),
                                           li(strong("I have more than 6 parameters, how can I get a permutation: "), "Because of the time it takes to permute more than 6 parameters (this becomes more than 8 parameters when we consider a hybrid permutation), this application does not include it. You can download the evalwaterfallr package from Github (see link in footer) and work from there. If you have many parameters, you may need to modify the wParamPermute() function and ensure that you are running multiple cores."),
                                           li(strong("I have another question: "), "Please submit an issue on the Github page for the evalwaterfallr package. We will try to help you as soon as practical.")
                                         ),
                                         class = "evalguidancetext")
                                       )
                                       }) # end with tags
                               ), # end FAQs
                             
                             #                        
# Here is the start of the credits
#
                                hr(),
                                fluidRow(
                                  column(3,
                                         img(src='emilogo.jpg', align = "right",height=140,width=200),
                                         img(src='https://www.pge.com/pge_global/common/images/pge-spot-full-rgb-pos-lg.png',
                                             align = "right", style = "margin-right: 40px", height = 100, width = 100)
                                  ),
                                  column(9,
                                         p("This application was developed by ", a("EMI Consulting", href="http://emiconsulting.com"), "in collaboration with PG&E.",em("If you use this application for your evaluation efforts, please give us credit, like so: 'This table/plot was produced with the evalwaterfallr package developed by EMI Consulting and PG&E.'")),
                                         p("For full reference, please see the ", a("evalwaterfallr package on GitHub", href="https://github.com/EMIjess/evalwaterfallr.git")), 
                                         p("For more information on the motivation for this package, see Kasman, Robert, Adam Scheer, Rachel Sackman, Rafael Friedmann, and Janice Berman. 2015. 'Development of Order-Independent Waterfall Graphics to Enable Comprehensive Understanding of Impact Evaluation Results.' Proceedings of the 2015 International Energy Program Evaluation Conference", a("at the IEPEC proceedings website.", href="http://www.iepec.org/wp-content/uploads/2015/papers/022.pdf")),
                                         p("A more recent whitepaper includes the additive parameters, see Kasman, Robert and Adam Scheer. 2017.'Whitepaper: Development of Order-Independent Waterfall Graphics to Enable Comprehensive Understanding of Impact Evaluation Results.' ", a("Available for review at CPUC's Energy Data Web.", href="http://www.energydataweb.com/cpuc/deliverableView.aspx?did=1632&uid=0&cid=&tid="))
                                  )
                                ) # end of credits

                    ) # End of Help TabPanel
           ) # end of NavbarPage
           
           
