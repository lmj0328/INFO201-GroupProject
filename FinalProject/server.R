library(shiny)
library(ggplot2)
library(dplyr)
library(usmap)
library(lubridate)
library(urbnmapr)
library(tidyverse)

source("../Script/ReadData.R")

shinyServer(
  function(input, output, session) {
    
    ## Mengjiao's Code

    # OUTPUT MAP FOR PANEL 3
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
      
      # ADD REACTIVE FUNCTION FOR DATASET
      datasetInput <- reactive({
        switch(input$selectDataset,
               "N1" = joinedData$N1,
               "A04800" = joinedData$A04800,
               "A00200" = joinedData$A00200)
      })
      
      # OUTPUT SELECTED RANGE FOR PANEL 3
      output$chosedRange <- renderText(
        if(input$selectDataset == "N1") {
          paste("You have chosen to display a range of Number of Returns from", input$rangeSlider[1], "to", input$rangeSlider[2], ".")
        } else if (input$selectDataset == "A04800") {
          paste("You have chosen to display a range of Taxable Income Amount from", input$rangeSlider[1], "to", input$rangeSlider[2], ".")
        } else if (input$selectDataset == "A00200") {
          paste("You have chosen to display a range of Salaries and Wages Amount from", input$rangeSlider[1], "to", input$rangeSlider[2], ".")
        }
      )
      
      ggplot(joinedData, aes(long, lat, group = group, fill = datasetInput())) +
        geom_polygon(color = "#ffffff", size = 0.05) +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
        labs(fill = "Number", labels = scales::comma) +
        scale_fill_gradientn(colours = terrain.colors(10), 
                             limits=c(input$rangeSlider[1], input$rangeSlider[2]))
    })
    
    # OUTPUT DATA TABLE FOR PANEL 3
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
    
    # OUTPUT SELECT DATASET FOR PANEL 3
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
    
    # OUTPUT SELECT YEAR FOR PANEL 3
    output$chosedYear <- renderText(
      paste("You have chosen to display data from", input$selectYear, ".")
    )
    
    # OUTPUT SELECT SALARY FOR PANEL 3
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
    
    # OBSERVE COUNTY'S ACTION BUTTON FOR PANEL 2
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

    # OUTPUT DATASET FOR PANEL 2
    output$chartTable2 <- DT::renderDataTable({
      FilteredChartData <- AllChartData %>%
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
    
    # OUTPUT BAR PLOT FOR PANEL 2
    output$yearBarPlot <- renderPlot({
      
      FilteredBarData <- AllChartData %>%
        filter(COUNTYNAME %in% input$selectCounty) %>%
        filter(agi_stub == input$selectSalary2)
      
      selectCountyName <- FilteredBarData %>% 
        filter(STATE == "WA") %>%
        group_by(COUNTYNAME) %>%
        summarise(n_distinct(N1)) %>%
        select(COUNTYNAME) %>%
        filter(COUNTYNAME != "Washington")
      
      adjustedData <- data.frame(CountyName=rep(selectCountyName$COUNTYNAME, each = 5),
                        Years=rep(c("2012", "2013", "2014", "2015", "2016"), length(selectCountyName$COUNTYNAME)),
                        NumberOfReturns=FilteredBarData$N1,
                        TaxableIncomeAmount=FilteredBarData$A04800,
                        SalariesAndWagesAmount=FilteredBarData$A00200)

      # ADD REACTIVE FUNCTION FOR DATASET
      datasetInput2 <- reactive({
        switch(input$selectDataset2,
               "N1" = adjustedData$NumberOfReturns,
               "A04800" = adjustedData$TaxableIncomeAmount,
               "A00200" = adjustedData$SalariesAndWagesAmount)
      })

      plot <- ggplot(data=adjustedData, aes(x=Years, y=datasetInput2(), fill=CountyName)) +
        geom_bar(stat="identity", position=position_dodge()) +
        theme_minimal()
      
      return(plot)
    })
      #OUTPUT FOR TABPANEL 1 (COMPARE COUNTIES)
      output$distPlot <- renderPlot({
        select_data <- read.csv(paste0("../Data/OORHousingData/", input$yearSelect,"-", input$bdrmSelect, ".csv"))
        select_data$monthly_rent <- as.numeric(gsub("[^0-9.]", "",select_data$monthly_rent))
        plot <- ggplot(select_data, aes(x = reorder(COUNTY, -monthly_rent), y = monthly_rent, 
                                        fill = monthly_rent)) +
          geom_bar(stat = "identity") + theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_fill_gradient(low = "green", high = "red") +
          labs(x = "County", y = "Monthly Rent (USD)", title = paste0(input$bdrmSelect, " monthly rent by counties in Washington in ", input$yearSelect))
        print(plot)
      })
      
      #OUTPUT FOR TABPANEL 2 (COMPARE YEARS)
      output$trendPlot <- renderPlot({
        select_data_2011 <- read.csv(paste0("../Data/OORHousingData/", 2011, "-", input$bdrmSelect, ".csv"))
        select_data_2012 <- read.csv(paste0("../Data/OORHousingData/", 2012, "-", input$bdrmSelect, ".csv"))
        select_data_2013 <- read.csv(paste0("../Data/OORHousingData/", 2013, "-", input$bdrmSelect, ".csv"))
        select_data_2014 <- read.csv(paste0("../Data/OORHousingData/", 2014, "-", input$bdrmSelect, ".csv"))
        select_data_2015 <- read.csv(paste0("../Data/OORHousingData/", 2015, "-", input$bdrmSelect, ".csv"))
        select_data_2016 <- read.csv(paste0("../Data/OORHousingData/", 2016, "-", input$bdrmSelect, ".csv"))
        rename_data_2011 <- select_data_2011 %>% 
          rename("2011_monthly_rent" = "monthly_rent")
        rename_data_2012 <- select_data_2012 %>% 
          rename("2012_monthly_rent" = "monthly_rent")
        rename_data_2013 <- select_data_2013 %>% 
          rename("2013_monthly_rent" = "monthly_rent")
        rename_data_2014 <- select_data_2014 %>% 
          rename("2014_monthly_rent" = "monthly_rent")
        rename_data_2015 <- select_data_2015 %>% 
          rename("2015_monthly_rent" = "monthly_rent")
        rename_data_2016 <- select_data_2016 %>% 
          rename("2016_monthly_rent" = "monthly_rent")
        join_trend_data <- list(rename_data_2011, rename_data_2012, rename_data_2013, rename_data_2014,
                                rename_data_2015, rename_data_2016) %>% reduce(full_join, by = "COUNTY")
        filtered_trend_data <- join_trend_data %>% 
          filter(COUNTY == input$countySelect)
        filtered_flip_data <- data.frame(t(filtered_trend_data[-1]))
        colnames(filtered_flip_data) <- filtered_trend_data[, 1]
        filtered_flip_data <- filtered_flip_data %>% 
          mutate(year = c(2011, 2012, 2013, 2014, 2015, 2016)) %>% 
          rename(amount = input$countySelect) %>% 
          mutate(county = "county")
        
        trend_plot <- ggplot(filtered_flip_data, aes(year, amount, group = county, colour = county)) +
          geom_point() + 
          geom_line() +
          xlab("Year") + 
          ylab(paste0("Monthly Rent of ", input$bdrmSelect2, " in ", input$countySelect))
        print(trend_plot)
      })
      
  }
)
