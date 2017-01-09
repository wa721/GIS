function(input,output){

  output$selectionNotification <- renderText({
    selection <- as.numeric(input$dependentVariable)
    variableNames <- list("Mean Household Income",
                          "Mean Age",
                          "Household Car Ownership",
                          "Population",
                          "Unemployment Rate",
                          "Bike Use",
                          "House Price",
                          "Number of Employed People",
                          "Percentage of Black, Asian and/or Minority Ethnicity People",
                          "Percentage of Dwellings That Are Social Housing",
                          "Percentage of Population on Job Seekers Allowance",
                          "Voter Turnout",
                          "Number of Working Age People",
                          "Average Deprivation Rank",
                          "Crime Rate")
    
    paste("You have chosen to compare", tolower(variableNames[selection]), " around tube stations with how connected tube stations are to the rest of the tube network.")
  })
  
  output$relation <- renderText({
    variableNames <- list("Mean Household Income",
                          "Mean Age",
                          "Household Car Ownership",
                          "Population",
                          "Unemployment Rate",
                          "Bike Use",
                          "House Price",
                          "Number of Employed People",
                          "Percentage of Black, Asian and/or Minority Ethnicity People",
                          "Percentage of Dwellings That Are Social Housing",
                          "Percentage of Population on Job Seekers Allowance",
                          "Voter Turnout",
                          "Number of Working Age People",
                          "Average Deprivation Rank",
                          "Crime Rate")
    rsquaredVal <- as.character(round(as.numeric(variableCodes[[as.numeric(input$dependentVariable)]]$r.squared)*100,
                                      digits = 2))
    
    paste('The centrality of a Tube Station can explain ',
          rsquaredVal,
          "% of the variation in",
          variableNames[as.numeric(input$dependentVariable)]
          )
  })
  
  output$relationPlot <- renderPlot({
    click = clickOpts(id="relationPlot_click", clip = TRUE)
    rplot <- RelationList[as.numeric(input$dependentVariable)]
    rplot
  })
  
  output$pointSelect <- renderText({
    paste(input$relationPlot_click)
  })
  output$zoomPlot <- renderLeaflet({
    leafList[[as.numeric(input$dependentVariable)]]
  })
  
  output$Alpha <- renderText({
    alphaval <- (100- input$alpha)/100
    if (as.numeric(variableCodes[[as.numeric(input$dependentVariable)]]$coefficients[2,4] < alphaval)){
      paste("You can be at least", input$alpha,
            "% sure that the relationship isn't just a coincidence.")
      }
    else {
      paste("You CANNOT be ", input$alpha, "% sure that the relationship isnt just a coincidence.")
    }
  })
  
  output$legendTitle <- renderText({
    titleList <- list("average income of households closest to station",
                      "average age of people closest to station",
                      "average cars owned of households closest to Station",
                      "number of people closest to station",
                      "unemployment rate in areas closest to station",
                      "average percentage of people who use bikes closest to station",
                      "average house prices in area closest to station",
                      "number of employed people",
                      "percentage of population of black, asian or minority ethnicities",
                      "percentatge of population on job seekers allowance",
                      "voter turnout in the 2011 mayoral election",
                      "number of working age people (16-64 years)",
                      "average UK-wide deprivation rank",
                      "average crime rate")
    
    paste("The size of each circle represents the average time it takes to travel to each other station (adjusted for how central the desitination is) and colour represents the ",titleList[as.numeric(input$dependentVariable)],".")
  })
  output$citations <- renderText({
    citationList <- list(
      "Greater London Authority Unit Update.",
      "ONS Mid-year estimates, 2013.",
      "Census 2011",
      "GLA Population Projection data, and Mid year estimates 2015.",
      "Census 2011",
      "Census 2011",
      "Greater London Authority calculations based on Land Registry",
      "Census2011",
      "Census2011",
      "Census2011",
      "Department of Work and Pensions, GLA Intelligence Unit Calculations",
      "London Elects",
      "GLA Population Projection data, and Mid year estimates 2015.",
      "Indicies of Deprivation DCLG",
      "Metropolitan Police Servies")
    
    paste("Sources: TfL Unified API, 'Shiny' (Rstudio, 2016), 'Leaflet' (Cheng % Xie,2016), OpenStreetMap, ",citationList[as.numeric(input$dependentVariable)])
  })

  output$KernelDensityDistribution <- renderPlot({
    connectivityDist
  })
}