library(shiny)
library(ggplot2)
library(dplyr)
library(usmap)
library(lubridate)
library(urbnmapr)

source("../Script/ReadData.R")

shinyServer(
  function(input, output, session) {
    
    ## Mengjiao's Code

    ## Render Map's Plot
    output$yearSalary <- renderPlot({
      RawData <- read.csv(paste0("../Data/SOITaxData/", input$selectYear, ".csv"))
      
      filteredData <- RawData %>%
        filter(STATE == "WA") %>%
        filter(COUNTYNAME != "Washington") %>%
        filter(agi_stub == input$selectSalary)
  
      colnames(filteredData)[4] <- "county_name" 
      filteredData$county_name <- as.character(filteredData$county_name)
      
      joinedData <- filteredData %>%
        left_join(counties, by = "county_name") %>% 
        filter(state_name =="Washington")
      
      datasetInput <- reactive({
        switch(input$selectDataset,
               "N1" = joinedData$N1,
               "A04800" = joinedData$A04800,
               "A00200" = joinedData$A00200)
      })
      


      output$chosedRange <- renderText(
        if(input$selectDataset == "N1") {
          paste("You have chosen to display a range of Number of Returns from", input$rangeSlider[1], "to", input$rangeSlider[2], ".")
        } else if (input$selectDataset == "A04800") {
          paste("You have chosen to display a range of Taxable Income Amount from", input$rangeSlider[1], "to", input$rangeSlider[2], ".")
        } else if (input$selectDataset == "A00200") {
          paste("You have chosen to display a range of Salaries and Wages Amount from", input$rangeSlider[1], "to", input$rangeSlider[2], ".")
        }
      )
      
      # Map's Code
      ggplot(joinedData, aes(long, lat, group = group, fill = datasetInput())) +
        geom_polygon(color = "#ffffff", size = 0.05) +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
        labs(fill = "Number", labels = scales::comma) +
        scale_fill_gradientn(colours = terrain.colors(10), 
                             limits=c(input$rangeSlider[1], input$rangeSlider[2]))
    })
    
    # Chart's Code
    output$chartTable <- DT::renderDataTable({
      RawData <- read.csv(paste0("../Data/SOITaxData/", input$selectYear, ".csv"))
      
      filteredData <- RawData %>%
        filter(STATE == "WA") %>%
        filter(COUNTYNAME != "Washington") %>%
        filter(agi_stub == input$selectSalary) 
      
      renameData <- filteredData %>%
        select(STATE, COUNTYNAME, N1, A04800, A00200)
      
      colnames(renameData)[3] <- "Number of Returns" 
      colnames(renameData)[4] <- "Taxable Income Amount" 
      colnames(renameData)[5] <- "Salaries and Wages Amount" 
      
      DT::datatable(renameData, options = list(orderClasses = TRUE, paging = FALSE, searching = FALSE))
    })
    
    #Display selected Choice
    output$chosedDataset <- renderText(
      if(input$selectDataset == "N1") {
        paste("You have chosen to display data from 'Number of Returns'. 
              The map's color will be based on the amount of people. ")
      } else if (input$selectDataset == "A04800") {
        paste("You have chosen to display data from 'Taxable Income Amount'.  
              The map will indicate the amount of income that are taxable. ")
      } else if (input$selectDataset == "A00200") {
        paste("You have chosen to display data from 'Salaries and Wages Amount'.
              The map will indicate the amount of salaries and wages. ")
      }
    )
    
    output$chosedYear <- renderText(
      paste("You have chosen to display data from", input$selectYear, ".")
    )
    
    output$chosedSalary <- renderText(
      if(input$selectSalary == 2) {
        paste("You have chosen people who have salaries from $1 to $10,000.")
      } else if (input$selectSalary == 3) {
        paste("You have chosen people who have salaries from $10,000 to $25,000.")
      } else if (input$selectSalary == 4) {
        paste("You have chosen people who have salaries from $25,000 to $50,000.")
      } else if (input$selectSalary == 5) {
        paste("You have chosen people who have salaries from $50,000 to $75,000.")
      } else if (input$selectSalary == 6) {
        paste("You have chosen people who have salaries from $75,000 to $100,000.")
      } else if (input$selectSalary == 7) {
        paste("You have chosen people who have salaries from $100,000 to $200,000.")
      } else if (input$selectSalary == 7) {
        paste("You have chosen people who have salaries from $200,000 to more.")
      }
    )
    
    # observe code for county's action button
    observe({
      if (input$UncheckCounty > 0) {
        if (input$UncheckCounty %% 2 == 0){
          updateCheckboxGroupInput(session=session,
                                   inputId="selectCounty",
                                   choices = ListOfCounties$COUNTYNAME,
                                   selected = ListOfCounties$COUNTYNAME)
          
        } else {
          updateCheckboxGroupInput(session=session,
                                   inputId="selectCounty",
                                   choices = ListOfCounties$COUNTYNAME,
                                   selected = "")
        }
      }
    })
    
    # # observe code for year's action button
    # observe({
    #   if (input$UncheckYear > 0) {
    #     if (input$UncheckYear %% 2 == 0){
    #       updateCheckboxGroupInput(session=session,
    #                                inputId="selectYear2",
    #                                choices = listOfYear,
    #                                selected = listOfYear)
    #       
    #     } else {
    #       updateCheckboxGroupInput(session=session,
    #                                inputId="selectYear2",
    #                                choices = listOfYear,
    #                                selected = "")
    #       
    #     }
    #   }
    # })
    
    output$chartTable2 <- DT::renderDataTable({
      FilteredChartData <- AllChartData %>%
        # filter(year %in% input$selectYear2) %>%
        filter(COUNTYNAME %in% input$selectCounty) %>%
        filter(agi_stub == input$selectSalary2) %>%
        select(STATE, COUNTYNAME, year, N1, A04800, A00200)
      
      colnames(FilteredChartData)[1] <- "State" 
      colnames(FilteredChartData)[2] <- "County" 
      colnames(FilteredChartData)[3] <- "Year" 
      colnames(FilteredChartData)[4] <- "Number of Returns" 
      colnames(FilteredChartData)[5] <- "Taxable Income Amount" 
      colnames(FilteredChartData)[6] <- "Salaries and Wages Amount" 
      
      DT::datatable(FilteredChartData, options = list(orderClasses = TRUE, paging = FALSE))
    })
    
    output$yearPlot <- renderPlot({
      BarPlot %>% filter(Shape %in% input$checkGroup)
      
      
      # Render a barplot
      barplot(WorldPhones[,input$region]*1000, 
              main=input$region,
              ylab="Number of Telephones",
              xlab="Year")
    })
    
    ## Put ur codes here.
  }
)
