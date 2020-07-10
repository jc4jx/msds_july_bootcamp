library(dplyr)
library(lubridate)
library(ggplot2)


covid_data_raw <- read.csv("C:/Users/chojo/Downloads/owid-covid-data.csv", header=TRUE)
head(covid_data_raw)

View(covid_data_raw)


#Line Plot
covid_data <- covid_data_raw %>% select(location,date,total_cases_per_million)


filtered_country_data <- covid_data %>% filter(location == "South Korea") %>% group_by(location) %>% summarise(sum(total_cases_per_million))
#filtered_country_data <- group_by( filter(covid_data,location == "South Korea"))

filtered_country_data <- covid_data %>% filter(location == "South Korea", as.Date(date) > "2020-04-12",as.Date(date) < "2020-07-01" )

ggplot(filtered_country_data,aes(x = as.Date(date), y = total_cases_per_million)) + geom_line() 

ggplot(filtered_country_data,aes(x = as.Date(date), y = total_cases_per_million)) + geom_bar() 


#+ scale_y_continuous(trans = 'log2')


filtered_country_data <- covid_data %>% filter(location == "South Korea"|location == "Brazil"|location == "United States")
ggplot(filtered_country_data,aes(x = as.Date(date), y = total_cases_per_million)) + geom_line(aes(color = location)) + scale_y_continuous(trans = 'log2') + ggtitle("newplot")

ggplot(filtered_country_data,aes(x = as.Date(date), y = total_cases_per_million)) + geom_line() + scale_y_continuous(trans = 'log2')


ggplot(covid_data,aes(x = as.Date(date), y = total_cases_per_million)) + geom_line(aes(color = location)) + scale_y_continuous(trans = 'log2')





#wrapping it into a funciton
graph_country_plot <-  function(country_name){
  filtered_country_data <- covid_data %>% filter(location == country_name)
  ggplot(filtered_country_data,aes(x = as.Date(date), y = total_cases_per_million)) + geom_line()
}

graph_country_plot("Brazil")


Brazil <- graph_country_plot("Brazil")
Brazil


# map visualization
library(leaflet)
library(rgdal)

globe_shapes <- readOGR("C:/Users/chojo/Desktop/UIA_World_Countries_Boundaries-shp/World_Countries__Generalized_.shp")
shapeData <- spTransform(globe_shapes, CRS("+proj=longlat +datum=WGS84 +no_defs"))


covid_data_2 <- covid_data_raw %>% select(location,new_cases_per_million) %>% group_by(location) %>% summarise(total_cases_per_million = sum(new_cases_per_million,na.rm = TRUE) )
View(covid_data_2)



shapeData@data <- inner_join(shapeData@data,covid_data_2,by = c("COUNTRY" = "location"))


pal <- colorNumeric(
  palette = "Blues",
  domain = covid_data_2$total_cases_per_million)

leaflet(shapeData) %>% addTiles() %>% addPolygons(color = ~pal(total_cases_per_million),weight = .5,fillOpacity = 1)



leaflet() %>% addTiles()







