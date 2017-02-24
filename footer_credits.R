# Here is the start of the credits
#
                             
                                fluidRow(
                                  column(3,
                                         div(
                                         img(src='emilogo.jpg', height=140,width=200),
                                         img(src='https://www.pge.com/pge_global/common/images/pge-spot-full-rgb-pos-lg.png',
                                             height = 70, width = 70)
                                  )),
                                  column(9,
                                         p("This application was developed by ", a("EMI Consulting", href="http://emiconsulting.com"), "in collaboration with PG&E.",em("If you use this application for your evaluation efforts, please give us credit, like so: 'This table/plot was produced with the evalwaterfallr package developed by EMI Consulting and PG&E.'")),
                                         p("For full reference, please see the ", a("evalwaterfallr package on GitHub", href="https://github.com/EMIjess/evalwaterfallr.git")), 
                                         p("For more information on the motivation for this package, see Kasman, Robert, Adam Scheer, Rachel Sackman, Rafael Friedmann, and Janice Berman. 2015. 'Development of Order-Independent Waterfall Graphics to Enable Comprehensive Understanding of Impact Evaluation Results.' Proceedings of the 2015 International Energy Program Evaluation Conference", a("at the IEPEC proceedings website.", href="http://www.iepec.org/wp-content/uploads/2015/papers/022.pdf")),
                                         p("A more recent whitepaper includes the additive parameters, see Kasman, Robert and Adam Scheer. 2017.'Whitepaper: Development of Order-Independent Waterfall Graphics to Enable Comprehensive Understanding of Impact Evaluation Results.' ", a("Available for review at CPUC's Energy Data Web.", href="http://www.energydataweb.com/cpuc/deliverableView.aspx?did=1632&uid=0&cid=&tid="))
                                  )
                                ) # end of credits
