fluidPage(
  titlePanel("Welcome to the London Transport Infrastructure Analysis Platform"),
  
  sidebarLayout(
    sidebarPanel(
      h1("Citywide Relationships"),
      selectInput("dependentVariable","Which social index would you like to assess?", choices = list("Mean Household Income"=1,"Mean Age"=2,"Household Car Ownership"=3,"Population Density"=4,"Employment Rate"=5),selected = NULL,selectize=TRUE),
      sliderInput("alpha","How sure do you want to be about the results?",50,99.99,80, step = 0.01, round = FALSE, format = "#,##0.#####", locale = "us", ticks = TRUE, animate = FALSE),
      h6("You can never reach 100% certainty with these models, sorry"),
      plotOutput("relationPlot"),
      textOutput("relation"),
      textOutput("Alpha")
    ),
    
    mainPanel(
      h1("Map",algin="center"),
      h1(" ",align="center"),
      textOutput("selectionNotification"),
      h1(" ",align="center"),
      h4("Public Transport Access Level (PTAL)", align = "left"),
      plotOutput("PTAL"),
      textOutput("dependentPlotTitle"),
      plotOutput("DEPVAR")
      
      
      #h4("Car Ownership per Household", align = "center"),
      #img(src="cars.png",collapse = " "),height = 390,width=495,align="center"),
      #h4("Employment Rate", align = "center"),
      #img(src="employ.png",height = 390,width=495,align="center"),
      #h4("Mean Household Income", align = "center"),
      #img(src="inc.png",height = 390,width=495,align="center"),
      #h4("Mean Age", align = "center"),
      #img(src="mage.png",height = 390,width=495,align="center"),
      #h4("Population Density", align = "center"),
      #img(src="cars.png",height = 390,width=495,align="center"),
      #h4("Low Resolution PTAL", align = "center"),
      #img(src="PTALm.png",height = 390,width=495,align="center")
      
    )
    
  )
)


