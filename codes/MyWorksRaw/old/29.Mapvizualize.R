library(sf)
library(leaflet)
library(dplyr)
library(rgdal)
library(dplyr)

canada <- readOGR("./gpr_000b11a_e/gpr_000b11a_e.shp") # shapefile included with sf package

canada_latlon <- spTransform(canada, CRS("+proj=longlat +datum=WGS84"))
ns <- subset(canada_latlon, PRUID == "12")
ns_lakes <- readOGR("./data/processed/ns_lakes.shp")%>%
  spTransform(CRS("+proj=longlat +datum=WGS84"))
ns_lakes_buffer <- readOGR("./check_arcGIS/ns_lake_buffer.shp")%>%
  spTransform(CRS("+proj=longlat +datum=WGS84"))
ns_fp <- readOGR("./check_arcGIS/ns_lake_home_500.shp")%>%
  spTransform(CRS("+proj=longlat +datum=WGS84"))
  

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = ns, # borders of all counties
              color = "black", 
              fill = NA, 
              weight = 1.5)%>%
  addPolygons(data = ns_lakes,
              color = "blue",
              #fill = "blue",
              weight = 1)%>%
  addPolygons(data = ns_lakes_buffer,
              color = "red",
              fill = NA,
              weight = 1.5)%>%
  addPolygons(data = ns_fp,
              color = "black",
              fill = "black",
              weight = 0.5)