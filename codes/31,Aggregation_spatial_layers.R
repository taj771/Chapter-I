library(sf)
library(dplyr)
library(tidyr)

ns_lake <- st_read("./data/processed/ns_lakes.shp")
sk_lake <- st_read("./data/processed/sk_lakes.shp")
ab_lake <- st_read("./data/processed/ab_lakes.shp")
bc_lake <- st_read("./data/processed/bc_lakes.shp")
mb_lake <- st_read("./data/processed/mb_lakes.shp")
nl_lake <- st_read("./data/processed/nl_lakes.shp")
nt_lake <- st_read("./data/processed/nt_lakes.shp")
nu_lake <- st_read("./data/processed/nu_lakes.shp")
on_lake <- st_read("./data/processed/on_lakes.shp")
pe_lake <- st_read("./data/processed/pe_lakes.shp")
qc_lake <- st_read("./data/processed/qc_lakes.shp")
yt_lake <- st_read("./data/processed/yt_lakes.shp")

can_lakes <- rbind(ns_lake,sk_lake,ab_lake,bc_lake,mb_lake,nl_lake,nt_lake,
                   nu_lake,on_lake,pe_lake,qc_lake,yt_lake)


can_lake <- st_read("./check_arcGIS/CA/can_lakes.shp")%>%
  select(name_id,geometry)

st_write(can_lake, "./check_arcGIS/CA/can_lake.shp", delete_dsn = T)

leaflet() %>%
  addTiles()%>%
  addPolygons(data = can_lakes, # borders of all counties
              color = "blue", 
              fill = NA, 
              weight = 1.5)
