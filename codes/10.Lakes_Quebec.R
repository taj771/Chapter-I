################################################################################
# Model 10 : Lakes in Qubec
# Province Qubec
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

qc_lakes_1 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_1.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
qc_lakes_1 <- st_crop(qc_lakes_1, c(xmin= -77.5, ymin = 53.75, xmax = -68.5, ymax = 56))

qc_lakes_2 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_2.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
qc_lakes_2 <- st_crop(qc_lakes_2, c(xmin= -68.5, ymin = 46.5, xmax = -57, ymax = 56))

qc_lakes_3 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_3.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
qc_lakes_3 <- st_crop(qc_lakes_3, c(xmin= -80, ymin = 53.75, xmax = -74, ymax = 56))

qc_lakes_4 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_4.shp")%>% #
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
qc_lakes_4 <- st_crop(qc_lakes_4, c(xmin= -80.2812, ymin = 45.74881, xmax = -68.60391, ymax = 56))

qc_lakes_5 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_5.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
qc_lakes_5 <- st_crop(qc_lakes_5, c(xmin= -68.5, ymin = 53.5, xmax = -63, ymax = 56))

qc_lakes_6 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_6.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
qc_lakes_6 <- st_crop(qc_lakes_6, c(xmin= -79, ymin = 54.22686, xmax = -63.01094, ymax = 56))

qc_lakes_7 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_7.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
qc_lakes_7 <- st_crop(qc_lakes_7, c(xmin= -80.5, ymin = 53.75, xmax = -68.5, ymax = 56))

qc_lakes_8 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_8.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
qc_lakes_8 <- st_crop(qc_lakes_8, c(xmin= -80.5, ymin = 44.83273, xmax = -68.5, ymax = 56))

qc_lakes_10 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_10.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
qc_lakes_10 <- st_crop(qc_lakes_10, c(xmin= -80.5, ymin = 44.83273, xmax = -68.5, ymax = 56))

qc_lakes_12 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_12.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
qc_lakes_12 <- st_crop(qc_lakes_12, c(xmin= -65.66291, ymin = 54.25, xmax = -63, ymax = 56))

qc_lakes_13 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_13.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
qc_lakes_13 <- st_crop(qc_lakes_13, c(xmin= -67.01, ymin = 58.5, xmax = -65.5, ymax = 56))

qc_lakes_17 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_17.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
qc_lakes_17 <- st_crop(qc_lakes_17, c(xmin= -64.5, ymin = 47.25, xmax = -62.88333, ymax = 56))

qc_lakes_22 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_22.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
qc_lakes_22 <- st_crop(qc_lakes_22, c(xmin= -77.5, ymin = 56.25, xmax = -77, ymax = 56))

# merge lakes
qc_lakes <- rbind(qc_lakes_1,qc_lakes_2, qc_lakes_3, qc_lakes_4, qc_lakes_5, qc_lakes_6,qc_lakes_8, qc_lakes_12)

# merge lakes with same name (CanVec has framed large lakes into few polygons)

qc_lakes1 <- qc_lakes %>% 
  group_by(name_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()


# calculate polygon area
sf_use_s2(FALSE)
qc_lakes1$area <- st_area(qc_lakes1)

# convert to square kilometers
qc_lakes1 <- mutate(qc_lakes1,area_km2 = area/1000000)

# drop lakes less than 1 km2

qc_lakes1$area_km2<- as.numeric(qc_lakes1$area_km2, units="km")

qc_lakes1 <- qc_lakes1%>%
  filter(area_km2 > 1)

# joint lake names

qc_lakes2 <- as.data.frame(qc_lakes)%>%
  select(-geometry)

qc_lakes2 <-  qc_lakes2%>%
  distinct(name_id, .keep_all = TRUE)

qc_lakes <- left_join(qc_lakes1, qc_lakes2, by='name_id')


#define projection 
qc_lakes_CRS <- st_transform(qc_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")

# read Ontario Provincial boundary

qc <- st_read("./gpr_000b11a_e/gpr_000b11a_e.shp")
qc <- subset(qc, PRUID == "24")
qc_CRS <- st_transform(qc, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(qc_CRS)==st_crs(qc_lakes_CRS)

# save Ontario lake files (consider set)

st_write(qc_lakes,"./data/processed/qc_lakes.shp")
################################################################################

qc_lake <- st_read("./data/processed/qc_lakes.shp")