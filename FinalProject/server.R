library(shiny)
library(ggplot2)
library(dplyr)
library(usmap)
library(lubridate)
library(urbnmapr)





shinyServer(
  function(input, output) {
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
      
      # taxType <- as.factor(paste0("joinedData$", input$selectTax))

      ggplot(joinedData, aes(long, lat, group = group, fill = N1)) +
        geom_polygon(color = "#ffffff", size = 0.05) +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
        labs(fill = "Income tax") + 
        scale_fill_gradientn(colours = terrain.colors(7))
      })
  }
)
