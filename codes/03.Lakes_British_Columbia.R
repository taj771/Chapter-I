################################################################################
# Model3 : Lakes in British Columbia
# ritish Columbia
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
bc_lakes <- st_read("./shapefile/canvec_50K_BC_Hydro/waterbody_2.shp")%>%
  subset(!is.na(namelk1en))

# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
bc_lakes <- st_crop(bc_lakes, c(xmin= -138.5571, ymin = 48.22456, xmax = -114, ymax = 56))


# merge lakes with same name (CanVec has framed large lakes into few polygons)

bc_lakes1 <- group_by(bc_lakes, name_id)%>%
  summarise(do_union = TRUE)

# calculate polygon area
bc_lakes1$area <- st_area(bc_lakes1)

# convert to square kilometers
bc_lakes1 <- mutate(bc_lakes1,area_km2 = area/1000000)

# drop lakes less than 1 km2

bc_lakes1$area_km2<- as.numeric(bc_lakes1$area_km2, units="km")

bc_lakes1 <- bc_lakes1%>%
  filter(area_km2 > 1)

# joint lake names

bc_lakes2 <- as.data.frame(bc_lakes)%>%
  select(-geometry)

bc_lakes2 <-  bc_lakes2%>%
  distinct(name_id, .keep_all = TRUE)

bc_lakes <- left_join(bc_lakes1, bc_lakes2, by='name_id')


#define projection 
bc_lakes_CRS <- st_transform(bc_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")

# read British Columbia Provincial boundary

bc <- st_read("./gpr_000b11a_e/gpr_000b11a_e.shp")
bc <- subset(bc, PRUID == "59")
bc_CRS <- st_transform(bc, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(bc_CRS)==st_crs(bc_lakes_CRS)

# save British Columbia lake files (consider set)

st_write(bc_lakes,"./data/processed/bc_lakes.shp")

###############################################################################
bc_lake <- st_read("./data/processed/bc_lakes.shp")
