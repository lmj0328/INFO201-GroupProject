## How I merged all of my .csv files into one
## mypath is the path to folder with .csv files
multmerge=function(mypath) {
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist= lapply(filenames, function (x) read.csv(file=x, header=TRUE))
  Reduce(function(x,y) rbind(x,y), datalist)}

## Install urbnmapr (need to install devtools prior)
devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

## Get the county data
county_data <- left_join(countydata, counties, by="county_fips")
head(county_data)
## Filter out all but Washington conties
county_data <- county_data %>% filter(state_abbv == "WA")

## Make a map of washington
countydata %>% left_join(counties, by = "county_fips") %>% filter(state_name =="Washington") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = horate)) + 
  geom_polygon(color = "#ffffff", size = .25) + scale_fill_gradientn(labels = scales::percent,
  guide = guide_colorbar(title.position = "top"), colors = "blue") + 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  theme(legend.title = element_text(), legend.key.width = unit(.5, "in")) + labs(fill = "Homeownership rate")

