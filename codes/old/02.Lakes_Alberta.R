################################################################################
# Model 2 : Lakes in Alberta
# Province Alberta 
################################################################################

# clean memory
# Load Library
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
alb_lakes <- st_read("./shapefile/canvec_50K_AB_Hydro/waterbody_2.shp")%>%
  subset(!is.na(namelk1en))

# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
alb_lakes <- st_crop(alb_lakes, c(xmin= -121, ymin = 48.99702, xmax = -110, ymax = 56))


# merge lakes with same name (CanVec has framed large lakes into few polygons)
alb_lakes1 <- alb_lakes %>% 
  group_by(name_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()


# calculate polygon area
alb_lakes1$area <- st_area(alb_lakes1)

# convert to square kilometers
alb_lakes1 <- mutate(alb_lakes1,area_km2 = area/1000000)

# drop lakes less than 1 km2

alb_lakes1$area_km2<- as.numeric(alb_lakes1$area_km2, units="km")

alb_lakes1 <- alb_lakes1%>%
  filter(area_km2 > 1)

# joint lake names

alb_lakes2 <- as.data.frame(alb_lakes)%>%
  select(-geometry)

alb_lakes2 <-  alb_lakes2%>%
  distinct(name_id, .keep_all = TRUE)

alb_lakes <- left_join(alb_lakes1, alb_lakes2, by='name_id')


#define projection 
alb_lakes_CRS <- st_transform(alb_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")

# read Alberta Provincial boundary

alb <- st_read("./gpr_000b11a_e/gpr_000b11a_e.shp")
alb <- subset(alb, PRNAME == "Alberta")
alb_CRS <- st_transform(alb, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(alb_CRS)==st_crs(alb_lakes_CRS)

# save Alberta lake files (consider set)

st_write(alb_lakes,"./data/processed/alb_lakes.shp")
###############################################################################

al_lake <- st_read("./data/processed/alb_lakes.shp")
