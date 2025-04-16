

library(sf)
library(terra)
library(tmap)
library(spData) 
library(tidyverse)


Ohio_Counties <- st_read("./data/static_mapping/oh_counties.gpkg")

Ohio_CSV <- read_csv("./data/static_mapping/oh_counties_DP2020.csv")

Joined <- left_join(Ohio_Counties, Ohio_CSV, by = c("GEOIDFQ" = "geoid"))

Group_1 <- tm_shape(Joined) + 
  tm_polygons(fill = "medianage",
      lty = 5,
      lwd = 2,
      fill.scale = tm_scale_intervals(style = "equal", values = "BuGn"),
      fill.legend = tm_legend(title = "Median Age")) +
  tm_scalebar()


Group_1
