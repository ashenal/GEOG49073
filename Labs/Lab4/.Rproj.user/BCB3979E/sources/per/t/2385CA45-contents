#library packages
library(sf)
library(terra)
library(tmap)
library(spData) # new package - you'll likely need to install
library(tidyverse)

#data connection
oh2020 <- read_csv("../data/static_mapping/oh_counties_DP2020.csv")
#ohio.counties <- sf::read_sf("../data/static_mapping/oh_counties.gpkg")

maindt <- sf::read_sf("../data/static_mapping/oh_places.gpkg")
counties <- sf::read_sf("../data/static_mapping/oh_counties.gpkg")
parks <- sf::read_sf("../data/static_mapping/oh_parks.gpkg")
streams <- sf::read_sf("../data/static_mapping/oh_rivers.gpkg")

rtcounty <- filter(counties, NAME %in% c("Summit", "Portage"))
dat <- st_intersection(maindt, rtcounty)
parkcr <- st_transform(parks, crs = st_crs(dat))
streamcr <- st_transform(streams, crs = st_crs(parkcr))

#removing extra information
oh2020 <- dplyr::filter(oh2020,name!="Ohio")

#joining the files
joined.table <- left_join(counties, oh2020, by=c("GEOIDFQ"="geoid"))

#mapping
ohio <- tm_shape(joined.table) + tm_polygons(fill = "medianage",lty=5,lwd=5,fill.scale = tm_scale_intervals(style = "equal", values = "BuGn"))+
                         tm_scalebar(breaks = c(0, 100, 200), text.size = 1, position = c("left", "top"))
ohio

#Code of group 2
sf::sf_use_s2(FALSE)
parkdat <- st_intersection(parkcr, rtcounty)
streamdat <- st_intersection(streamcr, rtcounty)

streammap <- tm_shape(streamdat) + tm_lines(lwd = 10, col = "darkblue")
#streammap
boundary <- tm_shape(rtcounty) +tm_polygons()

parkmap <- tm_shape(parkdat) +tm_polygons(fill = "FEATTYPE", palette = "Greens")

finmap <- boundary + parkmap + streammap
finmap

#group 3
install.packages("grid")
library(grid)
#Step one is to import the tif file 
neoh_dem <- terra::rast("../data/static_mapping/neoh_dem.tif")
#Next, import the counties
ohcounties <- st_read("../data/static_mapping/oh_counties.gpkg")

plot(neoh_dem)


st_crs(neoh_dem)
st_crs(ohcounties)

neoh_dem_rp <- project(neoh_dem, crs(ohcounties))

#We need to subset Portage and Summit then clip 
portsum <- ohcounties %>% dplyr::filter(NAME=="Portage" | NAME=="Summit")
plot(portsum)

x <- vect(portsum)

demposu <- terra::crop(neoh_dem_rp, x)

plot(demposu)
#This is the correct area, but it isn't cropped?

map1 <- tm_shape(portsum) + tm_polygons() +
  tm_shape(demposu) + tm_raster(col_alpha = 0.8) +  tm_compass(position = c("right", "bottom"))

map1

PortSum<- map1 + parkmap + streammap
PortSum

norm_dim = function(obj){
  bbox = st_bbox(obj)
  width = bbox[["xmax"]] - bbox[["xmin"]]
  height = bbox[["ymax"]] - bbox[["ymin"]]
  w = width / max(width, height)
  h = height / max(width, height)
  return(unit(c(w, h), "snpc"))
}
main_dim = norm_dim(finmap)
ins_dim = norm_dim(ohio)

main_vp = viewport(width = main_dim[1], height = main_dim[2])

ins_vp = viewport(width = ins_dim[1] * 0.5, height = ins_dim[2] * 0.5,
                  x = unit(1, "npc") - unit(0.5, "cm"), y = unit(0.5, "cm"),
                  just = c("right", "bottom")) 

grid.newpage()
print(PortSum, vp = main_vp)
pushViewport(main_vp)
print(ohio, vp = ins_vp)
