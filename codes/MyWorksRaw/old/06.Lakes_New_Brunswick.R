################################################################################
# Model 06 : Lakes in New Brunswick
# Province New Brunswick
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

nb_lakes <- st_read("./shapefile/canvec_50K_NB_Hydro/waterbody_2.shp")%>%
  subset(!is.na(namelk1en))

# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
nb_lakes <- st_crop(nb_lakes, c(xmin= -69.50135, ymin = 44, xmax = -63.5, ymax = 56))


# merge lakes with same name (CanVec has framed large lakes into few polygons)
sf_use_s2(TRUE)


nb_lakes1 <- nb_lakes %>% 
  group_by(name_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

# calculate polygon area
nb_lakes1$area <- st_area(nb_lakes1)

# convert to square kilometers
nb_lakes1 <- mutate(nb_lakes1,area_km2 = area/1000000)

# drop lakes less than 1 km2

nb_lakes1$area_km2<- as.numeric(nb_lakes1$area_km2, units="km")

nb_lakes1 <- nb_lakes1%>%
  filter(area_km2 > 1)

# joint lake names

nb_lakes2 <- as.data.frame(nb_lakes)%>%
  select(-geometry)

nb_lakes2 <-  nb_lakes2%>%
  distinct(name_id, .keep_all = TRUE)

nb_lakes <- left_join(nb_lakes1, nb_lakes2, by='name_id')


#define projection 
nb_lakes_CRS <- st_transform(nb_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")

# read Alberta Provincial boundary

nb <- st_read("./gpr_000b11a_e/gpr_000b11a_e.shp")
nb <- subset(nb, PRUID == "13")
nb_CRS <- st_transform(nb, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(nb_CRS)==st_crs(nb_lakes_CRS)

# save Alberta lake files (consider set)

st_write(nb_lakes,"./data/processed/nb_lakes.shp")
################################################################################

nb_lake <- st_read("./data/processed/nb_lakes.shp")