rm(list = ls())
# load library - CanVec
library(rcanvec)
# load library - Canada Census
library(cancensus)
# other basic libraries for spatial data
library(sf)
library(sp)
library(rgdal)
library(sp)
library(tidyverse)
library(spatialEco)
library(FIESTA)



sa_lake <- st_read("./data/processed/sas_lakes.shp")
ab_lake <- st_read("./data/processed/alb_lakes.shp")
bc_lake <- st_read("./data/processed/bc_lakes.shp")
mnb_lake <- st_read("./data/processed/mnb_lakes.shp")
on_lake <- st_read("./data/processed/on_lakes.shp")
nb_lake <- st_read("./data/processed/nb_lakes.shp")
nl_lake <- st_read("./data/processed/nl_lakes.shp")
ns_lake <- st_read("./data/processed/ns_lakes.shp")
pe_lake <- st_read("./data/processed/pe_lakes.shp")
qc_lake <- st_read("./data/processed/qc_lakes.shp")

can_lakes <- rbind(sa_lake, ab_lake, bc_lake, mnb_lake, on_lake, nb_lake, nl_lake, ns_lake, pe_lake, qc_lake)

can <- st_read("./gpr_000b11a_e/gpr_000b11a_e.shp")
can_lakes_CRS <- st_transform(can_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")
can_CRS <- st_transform(can, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(can_CRS)==st_crs(can_lakes_CRS)


df <- subset(on_lake,name_en == "Bass Lake")
df_CRS <- st_transform(df, crs = "+proj=longlat +datum=WGS84 +no_defs")



ggplot()+
  geom_sf(data = df_CRS)+
  geom_sf(data = on_CRS, col = "red")+
  coord_sf()
