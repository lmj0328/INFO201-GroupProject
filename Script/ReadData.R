library(devtools)
library(dplyr)
library(urbnmapr)
library(ggplot2)
 
# Data2011 <- read.csv("../Data/SOITaxData/2011.csv")
# Data2012 <- read.csv("../Data/SOITaxData/2012.csv")
# Data2013 <- read.csv("../Data/SOITaxData/2013.csv")
# Data2014 <- read.csv("../Data/SOITaxData/2014.csv")
# Data2015 <- read.csv("../Data/SOITaxData/2015.csv")
# Data2016 <- read.csv("../Data/SOITaxData/2016.csv")

RawData <- Data2016

ListOfCounties <- RawData %>% 
  filter(STATE == "WA") %>%
  group_by(COUNTYNAME) %>%
  summarise(n_distinct(N1)) %>%
  select(COUNTYNAME) %>%
  filter(COUNTYNAME != "Washington")

filteredData <- RawData %>% 
  filter(STATE == "WA") %>%
  filter(agi_stub == 8) %>%
  filter(COUNTYNAME != "Washington")

colnames(filteredData)[4] <- "county_name" 
filteredData$county_name <- as.character(filteredData$county_name)


joinedData <- filteredData %>%
  left_join(counties, by = "county_name") %>%
  filter(state_name =="Washington")

WashingtonState <- RawData %>%
  filter(STATE == "WA") %>%
  filter(COUNTYNAME == "Washington")

graph1 <- ggplot(joinedData, aes(long, lat, group = group, fill = A06500, label = A06500)) +
  geom_polygon(color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  labs(fill = "Income tax")
  


print(graph1)