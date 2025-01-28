library(tidyverse)
library(sf)

p.counties <- "./data/County_Boundaries.shp"
p.stations <- "./data/Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"

d.counties <- sf::read_sf(p.counties)
d.stations <- sf::read_sf(p.stations)

# Get all the counties in Delaware
del.counties <- d.counties %>% dplyr::filter(STATEFP10 == 10)

# Projections
d.counties %>% sf::st_crs()
d.stations %>% sf::st_crs()

    # Double check that they match exactly - formal test
d.counties %>% sf::st_crs() == d.stations %>% sf::st_crs()

# Find the stations in Delaware
de.stations <- sf::st_intersection(d.stations, del.counties)

plot(de.stations)

st_area()
