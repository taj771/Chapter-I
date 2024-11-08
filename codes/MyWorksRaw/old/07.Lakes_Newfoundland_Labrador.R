################################################################################
# Model 7 : Lakes in Newfoundland & Labrador
# Province Newfoundland & Labrador
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


nl_lakes_1 <- st_read("./shapefile/canvec_50K_NL_Hydro/waterbody_2_1.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
nl_lakes_1 <- st_crop(nl_lakes_1, c(xmin= -68.12127, ymin = 51.25, xmax = -60.15642, ymax = 56))

nl_lakes_2 <- st_read("./shapefile/canvec_50K_NL_Hydro/waterbody_2_2.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
nl_lakes_2 <- st_crop(nl_lakes_2, c(xmin= -60.5, ymin = 46.25, xmax = -52.5, ymax = 56))

nl_lakes_3 <- st_read("./shapefile/canvec_50K_NL_Hydro/waterbody_2_3.shp")%>%
  subset(!is.na(namelk1en))

nl_lakes_4 <- st_read("./shapefile/canvec_50K_NL_Hydro/waterbody_2_4.shp")%>% #
  subset(!is.na(namelk1en))

nl_lakes_4 <- st_crop(nl_lakes_4, c(xmin= -64.1561, ymin = 51.25, xmax = -60.08546, ymax = 56))

nl_lakes_5 <- st_read("./shapefile/canvec_50K_NL_Hydro/waterbody_2_5.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
nl_lakes_5 <- st_crop(nl_lakes_5, c(xmin= -65.5, ymin = 51.44702, xmax = -57.5, ymax = 56))

# merge lakes
nl_lakes <- rbind(nl_lakes_1,nl_lakes_2, nl_lakes_4, nl_lakes_5)

# merge lakes with same name (CanVec has framed large lakes into few polygons)

nl_lakes1 <- nl_lakes %>% 
  group_by(name_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()


# calculate polygon area
#sf_use_s2(FALSE)
nl_lakes1$area <- st_area(nl_lakes1)

# convert to square kilometers
nl_lakes1 <- mutate(nl_lakes1,area_km2 = area/1000000)

# drop lakes less than 1 km2

nl_lakes1$area_km2<- as.numeric(nl_lakes1$area_km2, units="km")

nl_lakes1 <- nl_lakes1%>%
  filter(area_km2 > 1)

# joint lake names

nl_lakes2 <- as.data.frame(nl_lakes)%>%
  select(-geometry)

nl_lakes2 <-  nl_lakes2%>%
  distinct(name_id, .keep_all = TRUE)

nl_lakes <- left_join(nl_lakes1, nl_lakes2, by='name_id')


#define projection 
nl_lakes_CRS <- st_transform(nl_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")

# read Ontario Provincial boundary

nl <- st_read("./gpr_000b11a_e/gpr_000b11a_e.shp")
nl <- subset(nl, PRUID == "10")
nl_CRS <- st_transform(nl, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(nl_CRS)==st_crs(nl_lakes_CRS)

# save Ontario lake files (consider set)

st_write(nl_lakes,"./data/processed/nl_lakes.shp")

nl_lake <- st_read("./data/processed/nl_lakes.shp")