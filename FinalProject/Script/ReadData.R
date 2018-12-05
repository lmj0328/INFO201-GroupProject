library(devtools)
library(dplyr)
library(urbnmapr)
library(ggplot2)
library(tibble)

Data2012 <- read.csv("Data/SOITaxData/2012.csv")
Data2013 <- read.csv("Data/SOITaxData/2013.csv")
Data2014 <- read.csv("Data/SOITaxData/2014.csv")
Data2015 <- read.csv("Data/SOITaxData/2015.csv")
Data2016 <- read.csv("Data/SOITaxData/2016.csv")

RawData <- Data2012

WashingtonState2012 <- Data2012 %>%
  dplyr::filter(STATE == "WA") %>%
  dplyr::filter(COUNTYNAME != "Washington") %>%
  select(STATE, COUNTYNAME, agi_stub, N1, A04800, A00200) %>%
  mutate(year = 2012)

WashingtonState2013 <- Data2013 %>%
  dplyr::filter(STATE == "WA") %>%
  dplyr::filter(COUNTYNAME != "Washington") %>%
  select(STATE, COUNTYNAME, agi_stub, N1, A04800, A00200) %>%
  mutate(year = 2013)

WashingtonState2014 <- Data2014 %>%
  dplyr::filter(STATE == "WA") %>%
  dplyr::filter(COUNTYNAME != "Washington") %>%
  select(STATE, COUNTYNAME, agi_stub, N1, A04800, A00200) %>%
  mutate(year = 2014)

addMissedData <- data.frame(STATE = rep("WA", each = 14), COUNTYNAME = rep(c("Pend Oreille County", "Grays Harbor County"), 7), agi_stub = rep(2:8, 2), N1 = 0, A04800 = 0, A00200 = 0, year = 2014)

WashingtonState2014 <- rbind(WashingtonState2014, addMissedData)

WashingtonState2015 <- Data2015 %>%
  dplyr::filter(STATE == "WA") %>%
  dplyr::filter(COUNTYNAME != "Washington") %>%
  select(STATE, COUNTYNAME, agi_stub, N1, A04800, A00200) %>%
  mutate(year = 2015)

WashingtonState2016 <- Data2016 %>%
  dplyr::filter(STATE == "WA") %>%
  dplyr::filter(COUNTYNAME != "Washington") %>%
  select(STATE, COUNTYNAME, agi_stub, N1, A04800, A00200) %>%
  mutate(year = 2016)

ListOfCounties <- Data2012 %>%
  dplyr::filter(STATE == "WA") %>%
  group_by(COUNTYNAME) %>%
  summarise(n_distinct(N1)) %>%
  select(COUNTYNAME) %>%
  dplyr::filter(COUNTYNAME != "Washington")

AllChartData <- rbind(WashingtonState2012, WashingtonState2013, WashingtonState2014, WashingtonState2015, WashingtonState2016)

FilteredBarData <- AllChartData %>%
  dplyr::filter(COUNTYNAME %in% ListOfCounties$COUNTYNAME) %>%
  dplyr::filter(agi_stub == 2) 
  



remove(Data2012, Data2013, Data2014, Data2015, Data2016)


adjustedData <- data.frame(CountyName=rep(ListOfCounties$COUNTYNAME, each = 5),
                           Years=rep(c("2012", "2013", "2014", "2015", "2016"), length(ListOfCounties$COUNTYNAME)),
                           NumberOfReturns=FilteredBarData$N1,
                           TaxableIncomeAmount=FilteredBarData$A04800,
                           SalariesAndWagesAmount=FilteredBarData$A00200)

plot <- ggplot(data=adjustedData, aes(x=Years, y=NumberOfReturns, fill=CountyName)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal()

# print(plot)

remove(WashingtonState2012, WashingtonState2013, WashingtonState2014, WashingtonState2015, WashingtonState2016)




testFilteredData <- RawData %>%
  dplyr::filter(STATE == "WA") %>%
  dplyr::filter(agi_stub == 8) %>%
  dplyr::filter(COUNTYNAME != "Washington")

ListOfMaxMin <- RawData %>%
  dplyr::filter(STATE == "WA") %>%
  dplyr::filter(COUNTYNAME != "Washington")

listOfMax <- apply(ListOfMaxMin, 2, max)
listOfYear <- c(2012,2013,2014,2015,2016)

NeedMax <- listOfMax[c("N1", "A04800", "A00200")]

outputMax <- function(year, columnName, agi) {
  RawData <- read.csv(paste0("Data/SOITaxData/", year, ".csv"))
  filtered <- RawData %>% 
    dplyr::filter(STATE == "WA") %>%
    dplyr::filter(COUNTYNAME != "Washington") %>%
    dplyr::filter(agi_stub == agi) %>%
    select(columnName)
  max <- apply(filtered, 2, max)
  return(max)
}


colnames(testFilteredData)[4] <- "county_name"
testFilteredData$county_name <- as.character(testFilteredData$county_name)


testJoinedData <- testFilteredData %>%
  left_join(counties, by = "county_name") %>%
  dplyr::filter(state_name =="Washington")

WashingtonState <- RawData %>%
  dplyr::filter(STATE == "WA") %>%
  dplyr::filter(COUNTYNAME == "Washington")


testGraph1 <- ggplot(testJoinedData, aes(long, lat, group = group, fill = A06500, label = A06500)) +
  geom_polygon(color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Income tax")
