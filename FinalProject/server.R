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
    output$yearPOV <- renderPlot({
      wa_poverty_data <- read.csv("../data/PovertyData/wa_poverty_data.csv", stringsAsFactors = FALSE)
      wa_poverty_data <- wa_poverty_data %>% select(county_name, Percent, Year)
      wa_county_data <- read.csv("../data/PovertyData/wa_county_data.csv", stringsAsFactors = FALSE)
      wa_county_data <- wa_county_data %>% select(long, lat, order, group, state_abbv, state_fips, county_name)
      wa_poverty_final_data <- left_join(wa_county_data, wa_poverty_data, by="county_name")
      
      ditch_the_axes <- theme(
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()
      )
      
      
      filteredPOVData <- wa_poverty_final_data %>% 
          filter(Year == input$selectPovYear)
     
      
      
      ggplot(filteredPOVData, aes(long, lat, group = group, fill = Percent)) +
        geom_polygon(color = "#ffffff", size = 0.05) +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
        ggtitle("Washington Poverty Rate For", input$selectPovYear) + 
        theme(text = element_text(face = "bold")) + 
        theme(plot.title=element_text(size=32)) +
        
        labs(fill = "% Below Poverty Line") + theme_bw() + ditch_the_axes
        ##scale_fill_gradientn(colours = terrain.colors(7))
      
    })
    
      output$povertyTable <-  DT::renderDataTable({
        data <- wa_poverty_data[, c("county_name", "Percent", "Year")]
        DT::datatable(data, options = list(lengthMenu = c(9, 17, 25), pageLength = 9), rownames=FALSE)
    })
    
    
  }
  
)
