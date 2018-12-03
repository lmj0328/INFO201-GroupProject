library(shiny)
library(ggplot2)
library(dplyr)
library(usmap)
library(lubridate)
library(urbnmapr)

source("../Script/ReadData.R")

shinyServer(
  function(input, output) {
    ## Mengjiao's Code

    ## Render Map's Plot
    output$yearSalary <- renderPlot({
      RawData <- read.csv(paste0("../Data/SOITaxData/", input$selectYear, ".csv"))
      
      filteredData <- RawData %>%
        filter(STATE == "WA") %>%
        filter(COUNTYNAME != "Washington") %>%
        filter(agi_stub == input$selectSalary)
      
      # Render Table
      # output$yearSalaryChart <- renderTable({
      #   head(filteredData, n = filteredData$N1)
      # })
  
      colnames(filteredData)[4] <- "county_name" 
      filteredData$county_name <- as.character(filteredData$county_name)
      
      joinedData <- filteredData %>%
        left_join(counties, by = "county_name") %>% 
        filter(state_name =="Washington")
      
      datasetInput <- reactive({
        switch(input$selectDataset,
               "N1" = joinedData$N1,
               "A06500" = joinedData$A06500,
               "A04800" = joinedData$A04800,
               "A11901" = joinedData$A11901,
               "A00200" = joinedData$A00200)
      })

      # Map's Code
      ggplot(joinedData, aes(long, lat, group = group, fill = datasetInput())) +
        geom_polygon(color = "#ffffff", size = 0.05) +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
        labs(fill = "Number", labels = scales::comma) +
        scale_fill_gradientn(colours = terrain.colors(10), 
                             limits=c(input$rangeSlider[1], input$rangeSlider[2]))
    })
    
    #Display selected Choice
    output$chosedDataset <- renderText(
      if(input$selectDataset == "N1") {
        paste("You have chosen to display data from 'Number of Returns'. 
              The map's color will be based on the amount of people. ")
      } else if (input$selectDataset == "A06500") {
        paste("You have chosen to display data from 'Income Tax Amount'.  
              The map will indicate the amount of income tax peopel have to pay. ")
      } else if (input$selectDataset == "A04800") {
        paste("You have chosen to display data from 'Taxable Income Amount'.  
              The map will indicate the amount of income that are taxable. ")
      } else if (input$selectDataset == "A11901") {
        paste("You have chosen to display data from 'Tax Due at Time of Filing'.  
              The map will indicate the amount of tax people have to pay at time of filing")
      } else if (input$selectDataset == "A00200") {
        paste("You have chosen to display data from 'Salaries and Wages Amount'.
              The map will indicate the amount of salaries and wages. ")
      }
    )
    
    output$chosedYear <- renderText(
      paste("You have chosen to display data from", input$selectYear, ".")
    )
    
    output$chosedSalary <- renderText(
      paste("You have chosen to people who have salaries from ", input$selectSalary, ".")
    )
    
    output$chosedRange <- renderText(
      paste("You have chosen to display a range of", input$rangeSlider[1], "to", input$rangeSlider[2], ".")
    )
    
    
    output$yearPlot <- renderPlot(
      {
        RawData <- read.csv(paste0("../Data/SOITaxData/", input$selectYear, ".csv"))
        
        WashingtonState <- RawData %>%
          filter(STATE == "WA") %>%
          filter(COUNTYNAME == "Washington")
        
        salaryList <- list("$1 under $10,000",
                           "$10,000 under $25,000",
                           "$25,000 under $50,000",
                           "$50,000 under $75,000",
                           "$75,000 under $100,000",
                           "$100,000 under $200,000",
                           "$200,000 or more")
        
        output$yearInfo <- renderText(
          {
            if(input$selectSalary == 2) {
              salary <- "$1 under $10,000"
            } else if (input$selectSalary == 3) {
              salary <- "$10,000 under $25,000"
            } else if (input$selectSalary == 4) {
              salary <- "$25,000 under $50,000"
            } else if (input$selectSalary == 5) {
              salary <- "$50,000 under $75,000"
            } else if (input$selectSalary == 6) {
              salary <- "$75,000 under $100,000"
            } else if (input$selectSalary == 6) {
              salary <- "$75,000 under $100,000"
            } else if (input$selectSalary == 6) {
              salary <- "$100,000 under $200,000"
            } else if (input$selectSalary == 6) {
              salary <- "$200,000 or more"
            }
            
            paste0("In ", input$selectYear, ", there are a total of ", WashingtonState$N1, " people earned ", salaryList, ".")
          }
        )
      }
    )
    
    
  
    ## Put ur codes here.
  }
)
