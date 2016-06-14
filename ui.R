require(shiny)
require(DT)
require(Cairo)
require(shinysky)
require(shinyBS)
ui <- navbarPage("Navbar!",
                 tabPanel("Uploading Files",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput('file1', 'Choose CSV File',
                                        accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv')),
                              tags$hr(),
                              checkboxInput('header', 'Header', TRUE),
                              radioButtons('sep', 'Separator',
                                           c(Comma=',',
                                             Semicolon=';',
                                             Tab='\t'),
                                           ','),
                              radioButtons('quote', 'Quote',
                                           c(None='',
                                             'Double Quote'='"',
                                             'Single Quote'="'"),
                                           '"'),
                              numericInput('combos', 'Compound Counts', 3,
                                           min = 1, max = 9),
                              bsTooltip(id = 'combos', title = "After the #. of stop model determined", 
                                        placement = "bottom", trigger = "hover"),
                              numericInput('backbones', 'Backbone Counts', 4,
                                           min = 1, max = 9)
                             
                            ),
                            mainPanel(
                              DT::dataTableOutput('contents')
                            )
                          )
                 ),
                 tabPanel("Summary",
                          verbatimTextOutput("summary1")
                 ),
                 tabPanel("Plot",
                          selectInput('select','model selection',choices=c("AutoSelection","Hierarchy")),
                          actionButton("do","update"),
                          busyIndicator("Plot In Progress",wait=0),
                          plotOutput("plot1", hover=hoverOpts(id="plothover")),
                          verbatimTextOutput("summary2")
                 ), 
                 tabPanel("Simulated Trial",
                          sidebarLayout(
                            sidebarPanel(
                              tags$div(title="Click here to slide through years",
                                       sliderInput("slider_year", "YEAR:", 
                                                   min = 2001, max = 2011, value = 2009, 
                                                   format="####", locale="us"
                                       )
                              ),
                              numericInput('combos.sim', 'Compound Counts', 3,
                                           min = 1, max = 9),
                              
                              numericInput('backbones.sim', 'Backbone Counts', 4,
                                           min = 1, max = 9),
                              numericInput('k.itm', 'Number of interim look', 3,
                                                                          min = 1, max = 3),
                              numericInput('npitm', 'Number of sub. per interim look', 10,
                                           min = 5, max = 15)
                              
                              ,tags$div(title="Determin model after Num. of interim looks",
                               numericInput('stop.itm', 'model selection at Num. of interim look', 1,
                                           min = 1, max = 3)
                               )
                              ,checkboxGroupInput('Models', 'Model candidates:',
                                                 c("Mod1-interaction","Mod2-additive main effect"
                                                   ,"Mod3-Main Comp. effect","Mod4-Hierarchy interaction",
                                                   "Mod5-Hierarchy additive","Mod6-Hierarchy main effect"), selected = c("Mod1-interaction","Mod2-additive main effect"
                                                                                                                         ,"Mod3-Main Comp. effect","Mod4-Hierarchy interaction",
                                                                                                                         "Mod5-Hierarchy additive","Mod6-Hierarchy main effect")),
                              actionButton("sim.start","Simulation")
                            ),
                            mainPanel(
                               h4("Simulation Input/Output")
                              ,div(class="well container-fluid", "Various Bayesian models are used as candidates.  
                                   DIC based model selection criterion will be used to select the best model for fitting. 
                                   Please update compound number, backbone number, event prob. (predp) and other simulation 
                                   parameters and click simulation.")
                              ,hotable("hotable1")
                              ,busyIndicator("Simulation In Progress",wait=0)
                              ,verbatimTextOutput("summary3")
                              ,plotOutput("plotsim")
                            )
                            
                        )         
  
                 ) 
    )
  

  
  
