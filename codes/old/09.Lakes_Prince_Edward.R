################################################################################
# Model 9 : Lakes in Prince Edward
# Province Prince Edward
# ##############################################################################

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

pe_lakes <- st_read("./shapefile/canvec_50K_PE_Hydro/waterbody_2.shp")%>%
  subset(!is.na(namelk1en))

# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
pe_lakes <- st_crop(pe_lakes, c(xmin= -65, ymin = 45.60189, xmax = -61.5, ymax = 56))


# merge lakes with same name (CanVec has framed large lakes into few polygons)
#sf_use_s2(TRUE)
pe_lakes1 <- pe_lakes %>% 
  group_by(name_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

# calculate polygon area
pe_lakes1$area <- st_area(pe_lakes1)

# convert to square kilometers
pe_lakes1 <- mutate(pe_lakes1,area_km2 = area/1000000)

# drop lakes less than 1 km2

pe_lakes1$area_km2<- as.numeric(pe_lakes1$area_km2, units="km")

pe_lakes1 <- pe_lakes1%>%
  filter(area_km2 > 1)

# joint lake names

pe_lakes2 <- as.data.frame(pe_lakes)%>%
  select(-geometry)

pe_lakes2 <-  pe_lakes2%>%
  distinct(name_id, .keep_all = TRUE)

pe_lakes <- left_join(pe_lakes1, pe_lakes2, by='name_id')


#define projection 
pe_lakes_CRS <- st_transform(pe_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")

# read Alberta Provincial boundary

pe <- st_read("./gpr_000b11a_e/gpr_000b11a_e.shp")
pe <- subset(pe, PRUID == "11")
pe_CRS <- st_transform(pe, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(pe_CRS)==st_crs(pe_lakes_CRS)

# save Alberta lake files (consider set)

st_write(pe_lakes,"./data/processed/pe_lakes.shp")

pe_lake <- st_read("./data/processed/pe_lakes.shp")
