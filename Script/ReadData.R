library(devtools)
library(dplyr)
library(urbnmapr)
library(ggplot2)
 
Data2012 <- read.csv("../Data/SOITaxData/2012.csv")
Data2013 <- read.csv("../Data/SOITaxData/2013.csv")
Data2014 <- read.csv("../Data/SOITaxData/2014.csv")
Data2015 <- read.csv("../Data/SOITaxData/2015.csv")
Data2016 <- read.csv("../Data/SOITaxData/2016.csv")

WashingtonState2012 <- Data2012 %>%
  filter(STATE == "WA") %>%
  filter(COUNTYNAME != "Washington") %>%
  select(STATE, COUNTYNAME, N1, A04800, A00200)

colnames(WashingtonState2012)[3] <- "2012N1" 
colnames(WashingtonState2012)[4] <- "2012A04800" 
colnames(WashingtonState2012)[5] <- "2012A00200" 

WashingtonState2013 <- Data2013 %>%
  filter(STATE == "WA") %>%
  filter(COUNTYNAME != "Washington") %>%
  select(N1, A04800, A00200)

colnames(WashingtonState2013)[1] <- "2013N1" 
colnames(WashingtonState2013)[2] <- "2013A04800" 
colnames(WashingtonState2013)[3] <- "2013A00200" 

WashingtonState2014 <- Data2014 %>%
  filter(STATE == "WA") %>%
  filter(COUNTYNAME != "Washington") %>%
  select(N1, A04800, A00200)

colnames(WashingtonState2014)[1] <- "2014N1" 
colnames(WashingtonState2014)[2] <- "2014A04800" 
colnames(WashingtonState2014)[3] <- "2014A00200" 

WashingtonState2015 <- Data2015 %>%
  filter(STATE == "WA") %>%
  filter(COUNTYNAME != "Washington") %>%
  select(N1, A04800, A00200)

colnames(WashingtonState2015)[1] <- "2015N1" 
colnames(WashingtonState2015)[2] <- "2015A04800" 
colnames(WashingtonState2015)[3] <- "2015A00200" 

WashingtonState2016 <- Data2016 %>%
  filter(STATE == "WA") %>%
  filter(COUNTYNAME != "Washington") %>%
  select(N1, A04800, A00200)

colnames(WashingtonState2016)[1] <- "2016N1" 
colnames(WashingtonState2016)[2] <- "2016A04800" 
colnames(WashingtonState2016)[3] <- "2016A00200" 

remove(Data2012, Data2013, Data2014, Data2015, Data2016)
         
BarPlot <- data.frame(WashingtonState2012, WashingtonState2013, WashingtonState2014, WashingtonState2015, WashingtonState2016)

ListOfCounties <- RawData %>% 
  filter(STATE == "WA") %>%
  group_by(COUNTYNAME) %>%
  summarise(n_distinct(N1)) %>%
  select(COUNTYNAME) %>%
  filter(COUNTYNAME != "Washington")

testFilteredData <- RawData %>% 
  filter(STATE == "WA") %>%
  filter(agi_stub == 8) %>%
  filter(COUNTYNAME != "Washington")

ListOfMaxMin <- RawData %>%
  filter(STATE == "WA") %>%
  filter(COUNTYNAME != "Washington")

listOfMax <- apply(ListOfMaxMin, 2, max)
listOfYear <- c(2012,2013,2014,2015,2016)

NeedMax <- listOfMax[c("N1", "A04800", "A00200")]

outputMax <- function(year, columnName, agi) {
  RawData <- read.csv(paste0("../Data/SOITaxData/", year, ".csv"))
  filtered <- RawData %>% 
    filter(STATE == "WA") %>%
    filter(COUNTYNAME != "Washington") %>%
    filter(agi_stub == agi) %>%
    select(columnName)
  max <- apply(filtered, 2, max)
  return(max)
}


colnames(testFilteredData)[4] <- "county_name" 
testFilteredData$county_name <- as.character(testFilteredData$county_name)


testJoinedData <- testFilteredData %>%
  left_join(counties, by = "county_name") %>%
  filter(state_name =="Washington")

WashingtonState <- RawData %>%
  filter(STATE == "WA") %>%
  filter(COUNTYNAME == "Washington")


testGraph1 <- ggplot(testJoinedData, aes(long, lat, group = group, fill = A06500, label = A06500)) +
  geom_polygon(color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  labs(fill = "Income tax")
  
