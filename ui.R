#
# This is the User Interface Script. It controls what the users are presented with. 
# 
# To start the application the "Run App" button should be clicked. This is above the
#  script and may not appear until the shiny package is installed.
#
#......................................................................................
#

fluidPage(
  titlePanel(
    h2("Welcome to the Transport Infrastructure Information Platform: London Tube Network Edition.",align = "centre")
    ),
  
  sidebarLayout(
    
    sidebarPanel(
      h2("Citywide Relationships"),
      h6("Disclaimer: There are some extreme data points that may have affected the reliability of the relationships presented below. To improve their reliability, theses extreme points are removed from consideration. This is standard practice in statistics.
         Also, please visit https://youtu.be/t8uyiZKw-DA for a short tutorial on interpreting this data."),
      textOutput("pointSelect"),
      plotOutput("relationPlot"),
      h1(" ",align="center"),
      h6(textOutput("relation"),align = "center"),
      h1(" ",align="center"),
      sliderInput("alpha","How sure do you want to be about the results? You can never reach 100% certainty with these models, sorry",75,99.99,75, step = 0.01, round = FALSE, locale = "us", ticks = TRUE, animate = FALSE),
      h1(" ",align = "center"),
      h6(textOutput("Alpha")),
      h1(" ",align = "center"),
      h6("Scientists generally like to be at least 95% sure before suggesting that relationships 
            might actually exist. If they can only be less than 95% sure, scientists usually won't 
         say that a relationship exists.")
      #plotOutput("KernelDensityDistribution")
    ),
  
    mainPanel(
      h1("Map",algin="center"),
      selectInput("dependentVariable","Which social index would you like to assess?", 
                  choices = list("Mean Household Income"=1,
                                 "Mean Age"=2,
                                 "Household Car Ownership"=3,
                                 "Population"=4,
                                 "Unemployment"=5,
                                 "Bike Use" = 6,
                                 "House Price" = 7,
                                 "Number of Employed People" = 8,
                                 "Percentage of Black, Asian and/or Minority Ethnicity People"= 9,
                                 "Percentage of Dwellings That Are Social Housing"=10,
                                 "Percentage of Population on Job Seekers Allowance"=11,
                                 "Voter Turnout"=12,
                                 "Number of Working Age People"=13,
                                 "Average Deprivation Rank"=14,
                                 "Crime Rate"=15
                                 ),selected = NULL,selectize=TRUE),
      h1(" ",align="center"),
      h5(textOutput("selectionNotification"),align="left"),
      #textOutput("selectionNotification2"),
      #textOutput("selectionNotification3"),
      #textOutput("selectionNotification4"),
      leafletOutput(outputId = "zoomPlot"),
      h6(textOutput("legendTitle"),align = "left"),
      h6("For a tutorial on using the map component of this tool, visit https://youtu.be/N6okR-tYeLM"),
      textOutput("citations")
    )
    
  )
)


