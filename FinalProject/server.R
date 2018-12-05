library(shiny)
library(ggplot2)
library(dplyr)
library(usmap)
library(lubridate)
library(urbnmapr)
library(tidyverse)
library(rsconnect)
library(plotly)
library(RColorBrewer)

source("Script/ReadData.R")

shinyServer(
  function(input, output, session) {
    
    ## Mengjiao's Code

    # OUTPUT MAP FOR PANEL 3
    output$yearSalary <- renderPlot({
      RawData <- read.csv(paste0("Data/SOITaxData/", input$selectYear, ".csv"))
      filteredData <- RawData %>%
        dplyr::filter(STATE == "WA") %>%
        dplyr::filter(COUNTYNAME != "Washington") %>%
        dplyr::filter(agi_stub == input$selectSalary)
  
      colnames(filteredData)[4] <- "county_name" 
      filteredData$county_name <- as.character(filteredData$county_name)
      joinedData <- filteredData %>%
        left_join(counties, by = "county_name") %>% 
        dplyr::filter(state_name =="Washington")
      
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
        labs(fill = "Area of Interest", labels = scales::comma) +
        scale_fill_gradientn(colours = terrain.colors(10), 
                             limits=c(input$rangeSlider[1], input$rangeSlider[2]))
    })
    
    # OUTPUT DATA TABLE FOR PANEL 3
    output$chartTable <- DT::renderDataTable({
      RawData <- read.csv(paste0("Data/SOITaxData/", input$selectYear, ".csv"))
      
      filteredData <- RawData %>%
        dplyr::filter(STATE == "WA") %>%
        dplyr::filter(COUNTYNAME != "Washington") %>%
        dplyr::filter(agi_stub == input$selectSalary) 
      
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
        dplyr::filter(COUNTYNAME %in% input$selectCounty) %>%
        dplyr::filter(agi_stub == input$selectSalary2) %>%
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
        dplyr::filter(COUNTYNAME %in% input$selectCounty) %>%
        dplyr::filter(agi_stub == input$selectSalary2) %>%
        arrange(year, STATE, COUNTYNAME, agi_stub)
      
      selectCountyName <- FilteredBarData %>% 
        dplyr::filter(STATE == "WA") %>%
        group_by(COUNTYNAME) %>%
        summarise(n_distinct(N1)) %>%
        select(COUNTYNAME) %>%
        dplyr::filter(COUNTYNAME != "Washington") %>%
        arrange(COUNTYNAME)
      
      adjustedData <- data.frame(CountyName=rep(selectCountyName$COUNTYNAME, 5),
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
          theme_minimal() +
          labs(y="Area of Intereest", labels = scales::comma)
        
        return(plot)
      })

      
      
    # Poverty Tab Server Code
    readData <- reactive({
      wa_poverty_data <- read.csv("Data/PovertyData/wa_poverty_data.csv", stringsAsFactors = FALSE)
      wa_poverty_data <- wa_poverty_data %>% dplyr::select(county_name, Percent, Year)
      wa_county_data <- read.csv("Data/PovertyData/wa_county_data.csv", stringsAsFactors = FALSE)
      wa_county_data <- wa_county_data %>% dplyr::select(long, lat, order, group, state_abbv, state_fips, county_name)
      wa_final_data <- left_join(wa_county_data, wa_poverty_data, by="county_name")
      if (input$selectPovYear == "2012"){
        filteredPOVData <- wa_final_data %>% dplyr::filter(Year == "2012")
      } else if (input$selectPovYear == "2013") {
        filteredPOVData <- wa_final_data %>% dplyr::filter(Year == "2013")
      } else if (input$selectPovYear == "2014") {
        filteredPOVData <- wa_final_data %>% dplyr::filter(Year == "2014")
      } else if (input$selectPovYear == "2015") {
        filteredPOVData <- wa_final_data %>% dplyr::filter(Year == "2015")
      } else{
        filteredPOVData <- wa_final_data %>% dplyr::filter(Year == "2016")
      }
      filteredPOVData
    })
    
    output$yearPOV <- renderPlot({
      mapData <- readData()
      
      ## Tidying up the map plot
      ditch_the_axes <- theme(
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()
      )
      
      ## Create a color palette using RColorBrewer
      palette2colors=function(name){
        brewer.pal(brewer.pal.info[rownames(brewer.pal.info)==name, "maxcolors"],name)}
      
      
      ggplot(mapData, aes(long, lat, group = group, fill = Percent)) +
        geom_polygon(color = "#ffffff", size = 0.05) +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
        ggtitle("Washington Poverty Rate For", input$selectPovYear) + 
        scale_fill_gradientn(colours = palette2colors("OrRd")) +
        theme(text = element_text(face = "bold")) + 
        theme(plot.title=element_text(size=32)) +
        
        labs(fill = "% Below Poverty Line") + theme_bw() + ditch_the_axes
      ##scale_fill_gradientn(colours = terrain.colors(7))
    })
    
    output$povertyTable <-  DT::renderDataTable({
      wa_poverty_data <- read.csv("Data/PovertyData/wa_poverty_data.csv", stringsAsFactors = FALSE)
      wa_poverty_data <- wa_poverty_data %>% select(county_name, Percent, Year)
      data <- wa_poverty_data[, c("county_name", "Percent", "Year")]
      DT::datatable(data, options = list(lengthMenu = c(9, 17, 25), pageLength = 9), rownames=FALSE)
    })
    readDataA <- reactive({
      wa_poverty_data <- read.csv("Data/PovertyData/wa_poverty_data.csv", stringsAsFactors = FALSE)
      wa_poverty_data <- wa_poverty_data %>% dplyr::select(county_name, Percent, Year)
      wa_county_data <- read.csv("Data/PovertyData/wa_county_data.csv", stringsAsFactors = FALSE)
      wa_county_data <- wa_county_data %>% dplyr::select(long, lat, order, group, state_abbv, state_fips, county_name)
      wa_final_data <- left_join(wa_county_data, wa_poverty_data, by="county_name")
      if (input$selectBarYear == "2012"){
        filteredPOVData <- wa_final_data %>% dplyr::filter(Year == "2012")
      } else if (input$selectBarYear == "2013") {
        filteredPOVData <- wa_final_data %>% dplyr::filter(Year == "2013")
      } else if (input$selectBarYear == "2014") {
        filteredPOVData <- wa_final_data %>% dplyr::filter(Year == "2014")
      } else if (input$selectBarYear == "2015") {
        filteredPOVData <- wa_final_data %>% dplyr::filter(Year == "2015")
      } else{
        filteredPOVData <- wa_final_data %>% dplyr::filter(Year == "2016")
      }
      filteredPOVData
    })
    output$povertyPlot <- renderPlot({
      barData <- readDataA()
      barData$pov_z <- round((barData$Percent - mean(barData$Percent))/sd(barData$Percent), 2)
      barData$pov_type <- ifelse(barData$pov_z < 0, "above", "below")  # above / below avg flag
      barData$county_names <- barData$county_name
      ggplot(barData, aes(x=county_names, y=barData$pov_z, label=pov_z)) + 
        geom_bar(stat='identity', aes(fill=pov_type), width=.5)  +
        scale_fill_manual(name="WA Poverty Avg ", 
                          labels = c("Below Average", "Above Average"), 
                          values = c("above"="#00ba38", "below"="#f8766d")) + 
        labs(subtitle="WA Poverty Visual", 
             title= "Diverging Bars") + 
        coord_flip()
    })



      #OUTPUT FOR TABPANEL 1 (COMPARE COUNTIES)
       output$distPlot <- renderPlotly({
         select_data <- read.csv(paste0("Data/", input$yearSelect,"-", input$bdrmSelect, ".csv"))
         select_data$monthly_rent <- as.numeric(gsub("[^0-9.]", "",select_data$monthly_rent))
         county <- reorder(select_data$COUNTY, -select_data$monthly_rent)
         plot <- ggplot(select_data, aes(x = county, y = monthly_rent, 
                                         fill = monthly_rent)) +
           geom_bar(stat = "identity") + theme_minimal() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
           scale_fill_gradient(low = "green", high = "red") +
           labs(x = "County", y = "Monthly Rent (USD)", title = paste0(input$bdrmSelect, " monthly rent by counties in Washington in ", input$yearSelect))
         plot <- ggplotly(plot,tooltip = c("x", "y"))
         print(plot)
       })
      
      
      #OUTPUT FOR TABPANEL 2 (COMPARE YEARS)
       output$trendPlot <- renderPlotly({
         select_data_2011 <- read.csv(paste0("Data/", 2011, "-", input$bdrmSelect2, ".csv"))
         select_data_2012 <- read.csv(paste0("Data/", 2012, "-", input$bdrmSelect2, ".csv"))
         select_data_2013 <- read.csv(paste0("Data/", 2013, "-", input$bdrmSelect2, ".csv"))
         select_data_2014 <- read.csv(paste0("Data/", 2014, "-", input$bdrmSelect2, ".csv"))
         select_data_2015 <- read.csv(paste0("Data/", 2015, "-", input$bdrmSelect2, ".csv"))
         select_data_2016 <- read.csv(paste0("Data/", 2016, "-", input$bdrmSelect2, ".csv"))
         
         select_data_2011$monthly_rent <- as.numeric(gsub("[^0-9.]", "",select_data_2011$monthly_rent))
         select_data_2012$monthly_rent <- as.numeric(gsub("[^0-9.]", "",select_data_2012$monthly_rent))
         select_data_2013$monthly_rent <- as.numeric(gsub("[^0-9.]", "",select_data_2013$monthly_rent))
         select_data_2014$monthly_rent <- as.numeric(gsub("[^0-9.]", "",select_data_2014$monthly_rent))
         select_data_2015$monthly_rent <- as.numeric(gsub("[^0-9.]", "",select_data_2015$monthly_rent))
         select_data_2016$monthly_rent <- as.numeric(gsub("[^0-9.]", "",select_data_2016$monthly_rent))
         
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
           dplyr::filter(COUNTY == input$countySelect)
         filtered_flip_data <- data.frame(t(filtered_trend_data[-1]))
         colnames(filtered_flip_data) <- filtered_trend_data[, 1]
         filtered_flip_data <- filtered_flip_data %>% 
           mutate(Year = c(2011, 2012, 2013, 2014, 2015, 2016)) %>% 
           rename(Rent = input$countySelect) %>% 
           mutate(county = "county")
         
         trend_plot <- ggplot(filtered_flip_data, aes(Year, Rent, color = county)) +
           geom_point() + 
           geom_line() +
           scale_color_manual(values=c("green")) + 
           xlab("Year") + 
           ylab(paste0("Monthly Rent of ", input$bdrmSelect2, " in ", input$countySelect, " (USD)"))
         trend_plot <- ggplotly(trend_plot,tooltip = c("x", "y"))
         print(trend_plot)
       })
})
