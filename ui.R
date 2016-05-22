ui <- fluidPage(
  
  #  Application title
  titlePanel("Sliders"), 
  actionButton("do","update"),
  
  # Sidebar with sliders that demonstrate various available
  # options
  sidebarLayout(
    sidebarPanel(
      sliderInput (inputId = "p1", 
                     label ="Choose a number",
                     value =0.5, min =0.01, max =1.0),
      sliderInput (inputId = "p2", 
                   label ="Choose a number",
                   value =0.5, min =0.01, max =1.0),
      
      sliderInput (inputId = "p3", 
                   label ="Choose a number",
                   value =0.5, min =0.01, max =1.0),
      
      
      sliderInput (inputId = "p4", 
                   label ="Choose a number",
                   value =0.5, min =0.01, max =1.0),
      
      
      sliderInput (inputId = "p5", 
                   label ="Choose a number",
                   value =0.5, min =0.01, max =1.0),
      
      
      sliderInput (inputId = "p6", 
                   label ="Choose a number",
                   value =0.5, min =0.01, max =1.0),
      
      
      sliderInput (inputId = "p7", 
                   label ="Choose a number",
                   value =0.5, min =0.01, max =1.0),
      
      
      sliderInput (inputId = "p8", 
                   label ="Choose a number",
                   value =0.5, min =0.01, max =1.0),
      
      
      sliderInput (inputId = "p9", 
                   label ="Choose a number",
                   value =0.5, min =0.01, max =1.0),
      
      
      
      sliderInput (inputId = "p10", 
                   label ="Choose a number",
                   value =0.5, min =0.01, max =1.0),
      
      
      sliderInput (inputId = "p11", 
                   label ="Choose a number",
                   value =0.5, min =0.01, max =1.0),
      
      
      sliderInput (inputId = "p12", 
                   label ="Choose a number",
                   value =0.5, min =0.01, max =1.0)
    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      plotOutput("plot1"),
      verbatimTextOutput("summary")
    )
  )
)
  
  
