function(input,output){
  output$selectionNotification <- renderText({
    paste("You have chosen to compare", variables[as.integer(input$dependentVariable)], "with access to public transport.")
  })
  output$relation <- renderText({
    paste("Access to public transport explains",
          as.character(round((summary(lm(as.formula(paste(variableCodes[as.integer(input$dependentVariable)],"~ptal",collapse="+"))))$r.squared)*100,digits=0)),
                       "% of the variation in",
                       variables[as.integer(input$dependentVariable)]
          )
  })
  output$relationPlot <- renderPlot({
    plot(as.numeric(unlist(variableindex[as.integer(input$dependentVariable)])),ptal,xlab=variables[as.integer(input$dependentVariable)],ylab="Public Transportation Access Level")
  })
  output$DEPVAR <- renderPlot({
    image.plot(interpolatedcars)
  })
  output$PTAL <- renderPlot({
    image.plot(interpolatedPTALData)
  })
  output$dependentPlotTitle <- renderText({
    paste(variables[as.integer(input$dependentVariable)])
  })
  output$Alpha <- renderText({
    paste("The significance level is", as.character(round((100-as.numeric(input$alpha))/100,digits=4)))
  })
}