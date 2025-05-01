
library(tidyverse)
library(terra)
library(sf)
library(leaflet) #new
library(leaflet.extras) #new
library(tmap)

#data connection
counties <- sf::read_sf("../data/static_mapping/oh_counties.gpkg")
ohdata <- read_csv("../data/static_mapping/oh_counties_DP2020.csv")

ohdata <- dplyr::filter(ohdata,name!="Ohio")
joined.table <- left_join(counties, oh2020, by=c("GEOIDFQ"="geoid"))

ohio <- joined.table %>% mutate(area=st_area(geom))
ohio <- ohio %>% mutate(pop_dens=(poptotal/as.numeric(area)*1e6))
ohio <- st_transform(ohio, crs = 4326)

pal = colorNumeric("YlOrRd", domain = ohio$pop_dens,n=5)

leaflet(data = ohio) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = ohio, fillColor = ~pal(pop_dens), stroke = TRUE, fillOpacity = 0.9,color = "black",
              weight = 0.3, popup = ~paste0(NAME, ": ", pop_dens)) %>%
  addLegend(pal = pal, values = ~pop_dens, title = "Population Density", position = "bottomright", bins = 5) %>%  
  addMiniMap() %>%
  addMarkers(lng = -81.327, lat = 41.159, popup = "Kent State University", label = "Locate me")


