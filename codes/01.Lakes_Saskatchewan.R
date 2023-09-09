################################################################################
# Model 1 - Lakes in Saskatchewan
# Province Saskatchewan
# Intially processed as provincial level and later will aggregate as necessary
###############################################################################
# clean memory
rm(list = ls())
library(sf)
library(sp)
library(rgdal)
library(sp)
library(tidyverse)
library(spatialEco)
library(FIESTA)
library(dplyr)
library(lwgeom)


# read the shape file 
sas_lakes <- st_read("./shapefile/canvec_50K_SK_Hydro/waterbody_2.shp")%>%
  subset(!is.na(namelk1en))

# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
sas_lakes <- st_crop(sas_lakes, c(xmin= -111, ymin = 48.99883, xmax = -101, ymax = 56))


# merge lakes with same name (CanVec has framed large lakes into few polygons)

sas_lakes1 <- group_by(sas_lakes, name_id)%>%
  summarise(do_union = TRUE)

# calculate polygon area
sas_lakes1$area <- st_area(sas_lakes1)

# convert to square kilometers
sas_lakes1 <- mutate(sas_lakes1,area_km2 = area/1000000)

# drop lakes less than 1 km2

sas_lakes1$area_km2<- as.numeric(sas_lakes1$area_km2, units="km")

sas_lakes1 <- sas_lakes1%>%
  filter(area_km2 > 1)

# joint lake names


sas_lakes2 <- as.data.frame(sas_lakes)%>%
  select(-geometry)

sas_lakes2 <-  sas_lakes2%>%
  distinct(name_id, .keep_all = TRUE)

sas_lakes <- left_join(sas_lakes1, sas_lakes2, by='name_id')


#define projection 
sas_lakes_CRS <- st_transform(sas_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")

# read Saskatchewan Provincial boundary

sas <- st_read("./gpr_000b11a_e/gpr_000b11a_e.shp")
sas <- subset(sas, PRNAME == "Saskatchewan")
sas_CRS <- st_transform(sas, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(sas_CRS)==st_crs(sas_lakes_CRS)

# save Saskatchewan lake files (consider set)

st_write(sas_lakes,"./data/processed/sas_lakes.shp")

sas_lake <- st_read("./data/processed/sas_lakes.shp")

# map

ggplot()+
  geom_sf(data = qc_CRS)+
  geom_sf(data = qc_lakes_CRS, col = "red")+
  coord_sf()