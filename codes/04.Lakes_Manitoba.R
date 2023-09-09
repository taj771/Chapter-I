################################################################################
# Model 4 : Lakes in Manitoba
#Province Manitoba
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
mnb_lakes_1 <- st_read("./shapefile/canvec_50K_MB_Hydro/waterbody_2_1.shp")

mnb_lakes_1 <- mnb_lakes_1%>%
  subset(!is.na(namelk1en))

mnb_lakes_2 <- st_read("./shapefile/canvec_50K_MB_Hydro/waterbody_2_2.shp")

mnb_lakes_2 <- mnb_lakes_2%>%
  subset(!is.na(namelk1en))

mnb_lakes_4 <- st_read("./shapefile/canvec_50K_MB_Hydro/waterbody_2_4.shp")

mnb_lakes_4 <- mnb_lakes_4%>%
  subset(!is.na(namelk1en))

mnb_lakes <- rbind(mnb_lakes_1,mnb_lakes_2, mnb_lakes_4)

# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
mnb_lakes <- st_crop(mnb_lakes, c(xmin= -103, ymin = 48.99921, xmax = -88, ymax = 56))


# merge lakes with same name (CanVec has framed large lakes into few polygons)

mnb_lakes1 <- group_by(mnb_lakes, name_id)%>%
  summarise(do_union = TRUE)

# calculate polygon area
mnb_lakes1$area <- st_area(mnb_lakes1)

# convert to square kilometers
mnb_lakes1 <- mutate(mnb_lakes1,area_km2 = area/1000000)

# drop lakes less than 1 km2

mnb_lakes1$area_km2<- as.numeric(mnb_lakes1$area_km2, units="km")

mnb_lakes1 <- mnb_lakes1%>%
  filter(area_km2 > 1)

# joint lake names

mnb_lakes2 <- as.data.frame(mnb_lakes)%>%
  select(-geometry)

mnb_lakes2 <-  mnb_lakes2%>%
  distinct(name_id, .keep_all = TRUE)

mnb_lakes <- left_join(mnb_lakes1, mnb_lakes2, by='name_id')


#define projection 
mnb_lakes_CRS <- st_transform(mnb_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")

# read Manitoba Provincial boundary

mnb <- st_read("./gpr_000b11a_e/gpr_000b11a_e.shp")
mnb <- subset(mnb, PRNAME == "Manitoba")
mnb_CRS <- st_transform(mnb, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(mnb_CRS)==st_crs(mnb_lakes_CRS)

# save Manitoba lake files (consider set)

st_write(mnb_lakes,"./data/processed/mnb_lakes.shp")

mb_lake <- st_read("./data/processed/mnb_lakes.shp")
