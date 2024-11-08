################################################################################
# Model 5 : Lakes in Ontario
# Province Ontario
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

on_lakes_1 <- st_read("./shapefile/canvec_50K_ON_Hydro/waterbody_2_1.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
on_lakes_1 <- st_crop(on_lakes_1, c(xmin= -95.5, ymin = 44.99136, xmax = -74.14911, ymax = 56))

on_lakes_2 <- st_read("./shapefile/canvec_50K_ON_Hydro/waterbody_2_2.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
on_lakes_2 <- st_crop(on_lakes_2, c(xmin= -85, ymin = 41.67656, xmax = -74, ymax = 56))

on_lakes_3 <- st_read("./shapefile/canvec_50K_ON_Hydro/waterbody_2_3.shp")%>%
  subset(!is.na(namelk1en))

on_lakes_4 <- st_read("./shapefile/canvec_50K_ON_Hydro/waterbody_2_4.shp")%>% #
  subset(!is.na(namelk1en))

on_lakes_5 <- st_read("./shapefile/canvec_50K_ON_Hydro/waterbody_2_5.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
on_lakes_5 <- st_crop(on_lakes_5, c(xmin= -95.91349, ymin = 49.82117, xmax = -90.5, ymax = 56))

# merge lakes
on_lakes <- rbind(on_lakes_1,on_lakes_2, on_lakes_5)

# merge lakes with same name (CanVec has framed large lakes into few polygons)

on_lakes1 <- on_lakes %>% 
  group_by(name_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()


# calculate polygon area
sf_use_s2(FALSE)
on_lakes1$area <- st_area(on_lakes1)

# convert to square kilometers
on_lakes1 <- mutate(on_lakes1,area_km2 = area/1000000)

# drop lakes less than 1 km2

on_lakes1$area_km2<- as.numeric(on_lakes1$area_km2, units="km")

on_lakes1 <- on_lakes1%>%
  filter(area_km2 > 1)

# joint lake names

on_lakes2 <- as.data.frame(on_lakes)%>%
  select(-geometry)

on_lakes2 <-  on_lakes2%>%
  distinct(name_id, .keep_all = TRUE)

on_lakes <- left_join(on_lakes1, on_lakes2, by='name_id')


#define projection 
on_lakes_CRS <- st_transform(on_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")

# read Ontario Provincial boundary

on <- st_read("./gpr_000b11a_e/gpr_000b11a_e.shp")
on <- subset(on, PRNAME == "Ontario")
on_CRS <- st_transform(on, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(on_CRS)==st_crs(on_lakes_CRS)

# save Ontario lake files (consider set)

st_write(on_lakes,"./data/processed/on_lakes.shp")
################################################################################

on_lake <- st_read("./data/processed/on_lakes.shp")