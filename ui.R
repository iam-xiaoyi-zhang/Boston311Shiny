nbh <- c("Dorchester","Alston","Brighton","Brookline")
shinyUI(fluidPage(theme="style-superhero.css",
  
  ## Title
  tags$img(src='title4_final.png',width="100%"),
  
  tabsetPanel(
    id="Topics",
    
    ## Home Page 
    tabPanel("Home Page",
             tags$img(src='untitled.png',width="100%"),
             style = "background-color: #BCC6CC;"
             ),
    
    ## Scatter Plot
    tabPanel("Scatter Plots",
             titlePanel("Time Duration"),
             plotlyOutput("scatter_time_consume"),
             titlePanel("Efficiency"),
             plotlyOutput("scatter_efficiency"),
             titlePanel("On-time Rate"),
             plotlyOutput("scatter_ontime_rate"),
             titlePanel("Requests Volume"),
             plotlyOutput("scatter_requests_number")
    ),
    
    ## Bubble Charts
    tabPanel("Bubble Charts",
             tabsetPanel(id="in Bubble Charts",
                         tabPanel("All Departments",
                                  h1(strong("Quality of Service by Department")),
                                  h4("Bubbles with the same color are requsets from different neighborhoods to one department."),
                                  plotlyOutput("B_department")
                         ),
                         tabPanel("Within Department",
                                  sidebarLayout(
                                    sidebarPanel(
                                      selectInput("select one department",label = h3("select one department"),
                                                  choices=dpt)
                                      ),
                                    mainPanel(
                                      h1(strong("Quality of Service in Single Department")),
                                      h4("Each bubble is a different neighborhood, and bubble size represents request volume (sizemode: area.)"),
                                      plotlyOutput("B_dpt_quality"),
                                      h1(strong("Association between Quality of Service and Popolation Density")),
                                      h4("Each bubble is a different neighborhood, and bubble size represents request volume (sizemode: area.)"),
                                      plotlyOutput("B_dpt_density"),
                                      h1(strong("Association between Quality of Service and Income")),
                                      h4("Each bubble is a different neighborhood, and bubble size represents request volume (sizemode: area.)"),
                                      plotlyOutput("B_dpt_income")
                                      )
                                    )
                                  )
                         )
             ),
    
    tabPanel("Map",
             titlePanel("Boston Map"),
             
             fluidRow(
               column(width=4,
                      h3("Census Information"),
                      selectInput("var1", label = ("Choose one..."),
                                  choices = list("Population"= 2, 
                                                 "Density"= 3, 
                                                 "Median Income" = 4),
                                  selected = 3),
                      h5("*Resources:"),
                      p("2010 Census, Median Income in 2013"),
                      plotOutput("map")),
               
               column(width=4,
                      h3("Departments Service Quality"),
                      selectInput("var2", "Choose a department",
                                  choices = list("Public Works Department"= 2,
                                                 "Boston Public School" = 3,
                                                 "Boston Water & Sewer Commission" = 4,
                                                 "Inspectional Services" = 5,
                                                 "Mayor's 24 Hour Hotline"= 6,
                                                 "Parks & Recreation Department"= 7,
                                                 "Property Management"= 8,
                                                 "Transportation - Traffic Division"= 9),
                                  selected = 2),
                      selectInput("dataset", "Choose a quality measurement",
                                  choices = c("Duration", "Efficiency", "Ontime Rate", "Requests Volume"),
                                  selected = "Duration"),
                      plotOutput("quality")),
               
               column(width=4,
                      h3("Reasons of Requests"),
                      selectInput("ty", "Choose a reason",
                                  choices = list("Public Works Department"= 
                                                   c("Highway Maintenance"= 2,
                                                     "Recycling"= 3,
                                                     "Sanitation" = 4,
                                                     "Street Cleaning" = 5,
                                                     "Street Lights" = 6),
                                                 "Inspectional Services" = 
                                                   c("Building" = 7,
                                                     "Code Enforcement" = 8,
                                                     "Environmental Services" = 9,
                                                     "Housing" = 10),
                                                 "Transportation - Traffic Division"= 
                                                   c("Enforcement & Abandoned Vehicles" = 11,
                                                     "Signs & Signals" =12)),
                                  selected = 2),
                      selectInput("qos", "Choose a quality measurement",
                                  choices = list("Duration"= 1,
                                                 "Efficiency" = 2,
                                                 "Ontime Rate"= 3,
                                                 "Requests Volume"= 4),
                                  selected = 1),
                      plotOutput("type"))
             )
             ),
    
    ## Metrics: efficiency
    tabPanel("Metrics: Efficiency",
             sidebarPanel(
               splitLayout(
                 checkboxGroupInput(inputId = "department",
                                    label   = "Department",
                                    choices = dpt,
                                    selected = "Public Works Department")
               )
             ),
             
             mainPanel(
               tabsetPanel(id="In metrics",
                           tabPanel("Unified r",
                                    sliderInput("r",
                                                "Target Time Ajusted Factor(r)", 
                                                min = 0,
                                                max = 2.0,step=0.01, 
                                                value = 1),
                                    plotOutput("ecdf")
                           ),
                           tabPanel("Multiple Sliders",
                                    flowLayout(
                                      sliderInput("r1","r for Publc Works Department",min=0,max=2.0,value=1,step=0.01),
                                      sliderInput("r2","r for Transportation - Traffic Division",min=0,max=2.0,value=1,step=0.01),
                                      sliderInput("r3","r for Inspectional Services",min=0,max=2.0,value=1,step=0.01),
                                      sliderInput("r4","r for Parks & Recreation Department",min=0,max=2.0,value=1,step=0.01),
                                      sliderInput("r5","r for Property Management",min=0,max=2.0,value=1,step=0.01),
                                      sliderInput("r6","r for Boston Public School",min=0,max=2.0,value=1,step=0.01),
                                      sliderInput("r7","r for Mayor's 24 Hour Hotline",min=0,max=2.0,value=1,step=0.01),
                                      sliderInput("r8","r for Boston Water & Sewer Commision",min=0,max=2.0,value=1,step=0.01)
                                    ),
                                    plotOutput("ecdf_dpt")
                           )
               )
             )
    )
    
    

    )
  )
  )