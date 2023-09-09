################################################################################
# Model 8: Select lakes of each province
# Lakes are larger than 1 km2
# Lakes with name
# ##############################################################################

# clean memory
# Load Library
library(sf)
library(sp)
library(rgdal)
library(sp)
library(tidyverse)
library(spatialEco)
library(FIESTA)
library(dplyr)
library(lwgeom)

# Nova Scotia
rm(list = ls())
ns_lakes <- st_read("./shapefile/canvec_50K_NS_Hydro/waterbody_2.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
ns_lakes <- st_crop(ns_lakes, c(xmin= -67, ymin = 43.25, xmax = -58.5, ymax = 56))
# merge lakes with same name (CanVec has framed large lakes into few polygons)
#sf_use_s2(TRUE)
ns_lakes1 <- ns_lakes %>% 
  group_by(name_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

# calculate polygon area
ns_lakes1$area <- st_area(ns_lakes1)
# convert to square kilometers
ns_lakes1 <- mutate(ns_lakes1,area_km2 = area/1000000)
# drop lakes less than 1 km2
ns_lakes1$area_km2<- as.numeric(ns_lakes1$area_km2, units="km")
ns_lakes1 <- ns_lakes1%>%
  filter(area_km2 > 1)
# joint lake names
ns_lakes2 <- as.data.frame(ns_lakes)%>%
  select(-geometry)
ns_lakes2 <-  ns_lakes2%>%
  distinct(name_id, .keep_all = TRUE)
ns_lakes <- left_join(ns_lakes1, ns_lakes2, by='name_id')
#define projection 
ns_lakes_CRS <- st_transform(ns_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")
# read  Provincial boundary
ns <- st_read("./gpr_000b11a_e/gpr_000b11a_e.shp")
ns <- subset(ns, PRUID == "12")
ns_CRS <- st_transform(ns, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(ns_CRS)==st_crs(ns_lakes_CRS)
# save  lake files (consider set)
st_write(ns_lakes,"./data/processed/ns_lakes.shp")
########################################################################
# Saskatchewan
rm(list = ls())
sk_lakes <- st_read("./shapefile/canvec_50K_SK_Hydro/waterbody_2.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
sk_lakes <- st_crop(sk_lakes, c(xmin= -111, ymin = 48.99883, xmax = -101, ymax = 56))
# merge lakes with same name (CanVec has framed large lakes into few polygons)
#sf_use_s2(TRUE)
sk_lakes1 <- sk_lakes %>% 
  group_by(name_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

# calculate polygon area
sk_lakes1$area <- st_area(sk_lakes1)
# convert to square kilometers
sk_lakes1 <- mutate(sk_lakes1,area_km2 = area/1000000)
# drop lakes less than 1 km2
sk_lakes1$area_km2<- as.numeric(sk_lakes1$area_km2, units="km")
sk_lakes1 <- sk_lakes1%>%
  filter(area_km2 > 1)
# joint lake names
sk_lakes2 <- as.data.frame(sk_lakes)%>%
  select(-geometry)
sk_lakes2 <-  sk_lakes2%>%
  distinct(name_id, .keep_all = TRUE)
sk_lakes <- left_join(sk_lakes1, sk_lakes2, by='name_id')
#define projection 
sk_lakes_CRS <- st_transform(sk_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")
# save  lake files (consider set)
st_write(sk_lakes,"./data/processed/sk_lakes.shp")
#################################################################################
#Alberta
rm(list = ls())
ab_lakes <- st_read("./shapefile/canvec_50K_AB_Hydro/waterbody_2.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
ab_lakes <- st_crop(ab_lakes, c(xmin= -121, ymin = 48.99702, xmax = -110, ymax = 56))
# merge lakes with same name (CanVec has framed large lakes into few polygons)
#sf_use_s2(TRUE)
ab_lakes1 <- ab_lakes %>% 
  group_by(name_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

# calculate polygon area
ab_lakes1$area <- st_area(ab_lakes1)
# convert to square kilometers
ab_lakes1 <- mutate(ab_lakes1,area_km2 = area/1000000)
# drop lakes less than 1 km2
ab_lakes1$area_km2<- as.numeric(ab_lakes1$area_km2, units="km")
ab_lakes1 <- ab_lakes1%>%
  filter(area_km2 > 1)
# joint lake names
ab_lakes2 <- as.data.frame(ab_lakes)%>%
  select(-geometry)
ab_lakes2 <-  ab_lakes2%>%
  distinct(name_id, .keep_all = TRUE)
ab_lakes <- left_join(ab_lakes1, ab_lakes2, by='name_id')
#define projection 
ab_lakes_CRS <- st_transform(ab_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")
# save  lake files (consider set)
st_write(ab_lakes,"./data/processed/ab_lakes.shp")
################################################################################################3
# British Colombia
rm(list = ls())
bc_lakes <- st_read("./shapefile/canvec_50K_BC_Hydro/waterbody_2.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
bc_lakes <- st_crop(bc_lakes, c(xmin= -138.5571, ymin = 48.22456, xmax = -114, ymax = 56))
# merge lakes with same name (CanVec has framed large lakes into few polygons)
#sf_use_s2(TRUE)
bc_lakes1 <- bc_lakes %>% 
  group_by(name_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

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
# save  lake files (consider set)
st_write(bc_lakes,"./data/processed/bc_lakes.shp")
##################################################################################
# Manitoba
rm(list = ls())
mb_lakes_1 <- st_read("./shapefile/canvec_50K_MB_Hydro/waterbody_2_1.shp")
mb_lakes_2 <- st_read("./shapefile/canvec_50K_MB_Hydro/waterbody_2_2.shp")
mb_lakes_4 <- st_read("./shapefile/canvec_50K_MB_Hydro/waterbody_2_4.shp")
mb_lakes <- rbind(mb_lakes_1,mb_lakes_2,mb_lakes_4)%>%
  subset(!is.na(namelk1en))
st_write(mb_lakes, "shapefile/Canvec_50K_MB_Hydro/waterbody_mb.shp")
mb_lakes <- st_read("shapefile/Canvec_50K_MB_Hydro/waterbody_mb.shp")
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
mb_lakes <- st_crop(mb_lakes, c(xmin= -103, ymin = 48.99886, xmax = -89.0981, ymax = 56))
# merge lakes with same name (CanVec has framed large lakes into few polygons)
#sf_use_s2(TRUE)
mb_lakes1 <- mb_lakes %>% 
  group_by(name_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()
# calculate polygon area
mb_lakes1$area <- st_area(mb_lakes1)
# convert to square kilometers
mb_lakes1 <- mutate(mb_lakes1,area_km2 = area/1000000)
# drop lakes less than 1 km2
mb_lakes1$area_km2<- as.numeric(mb_lakes1$area_km2, units="km")
mb_lakes1 <- mb_lakes1%>%
  filter(area_km2 > 1)
# joint lake names
mb_lakes2 <- as.data.frame(mb_lakes)%>%
  select(-geometry)
mb_lakes2 <-  mb_lakes2%>%
  distinct(name_id, .keep_all = TRUE)
mb_lakes <- left_join(mb_lakes1, mb_lakes2, by='name_id')
#define projection 
mb_lakes_CRS <- st_transform(mb_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")
# save  lake files (consider set)
st_write(mb_lakes,"./data/processed/mb_lakes.shp")
################################################################################
# New Brunswick
rm(list = ls())
nb_lakes <- st_read("./shapefile/canvec_50K_NB_Hydro/waterbody_2.shp")%>%
  subset(!is.na(namelk1en))
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
nb_lakes <- st_crop(nb_lakes, c(xmin= -69.50135, ymin = 44, xmax = -63.5, ymax = 56))
# merge lakes with same name (CanVec has framed large lakes into few polygons)
#sf_use_s2(TRUE)
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
# save  lake files (consider set)
st_write(nb_lakes,"./data/processed/nb_lakes.shp")
###############################################################################
# Newfoundland Labrador
rm(list = ls())
nl_lakes_1 <- st_read("./shapefile/canvec_50K_NL_Hydro/waterbody_2_1.shp")
nl_lakes_2 <- st_read("./shapefile/canvec_50K_NL_Hydro/waterbody_2_2.shp")
nl_lakes_3 <- st_read("./shapefile/canvec_50K_NL_Hydro/waterbody_2_3.shp")
nl_lakes_4 <- st_read("./shapefile/canvec_50K_NL_Hydro/waterbody_2_4.shp")
nl_lakes_5 <- st_read("./shapefile/canvec_50K_NL_Hydro/waterbody_2_5.shp")
nl_lakes <- rbind(nl_lakes_1,nl_lakes_2,nl_lakes_3,nl_lakes_4,nl_lakes_5)%>%
  subset(!is.na(namelk1en))
st_write(nl_lakes, "shapefile/Canvec_50K_NL_Hydro/waterbody_nl.shp")
nl_lakes <- st_read("shapefile/Canvec_50K_NL_Hydro/waterbody_nl.shp")
# we are interested in lakes located near populated area so we cut the upper 
# boundary at latitude 56
nl_lakes <- st_crop(nl_lakes, c(xmin= -68.06609, ymin = 46.61535, xmax = -52.65647, ymax = 56))
# merge lakes with same name (CanVec has framed large lakes into few polygons)
#sf_use_s2(TRUE)
nl_lakes1 <- nl_lakes %>% 
  group_by(name_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()
# calculate polygon area
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
# save lake files (consider set)
st_write(nl_lakes,"./data/processed/nl_lakes.shp")
################################################################################
# Northwest Territories 
rm(list = ls())
nt_lakes_1 <- st_read("./shapefile/canvec_50K_NT_Hydro/waterbody_2_1.shp")
nt_lakes_2 <- st_read("./shapefile/canvec_50K_NT_Hydro/waterbody_2_2.shp")
nt_lakes_3 <- st_read("./shapefile/canvec_50K_NT_Hydro/waterbody_2_3.shp")
nt_lakes_4 <- st_read("./shapefile/canvec_50K_NT_Hydro/waterbody_2_4.shp")
nt_lakes_9 <- st_read("./shapefile/canvec_50K_NT_Hydro/waterbody_2_9.shp")
nt_lakes <- rbind(nt_lakes_1, nt_lakes_2, nt_lakes_3, nt_lakes_4,nt_lakes_9)%>%
  subset(!is.na(namelk1en))
st_write(nt_lakes, "shapefile/Canvec_50K_NT_Hydro/waterbody_nt.shp")
nt_lakes <- st_read("shapefile/Canvec_50K_NT_Hydro/waterbody_nt.shp")
nt_lakes <- st_crop(nt_lakes, c(xmin= -136.4956, ymin = 59.62833, xmax = -102, ymax = 56))
# merge lakes with same name (CanVec has framed large lakes into few polygons)
#sf_use_s2(TRUE)
nt_lakes1 <- nt_lakes %>% 
  group_by(name_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()
# calculate polygon area
nt_lakes1$area <- st_area(nt_lakes1)
# convert to square kilometers
nt_lakes1 <- mutate(nt_lakes1,area_km2 = area/1000000)
# drop lakes less than 1 km2
nt_lakes1$area_km2<- as.numeric(nt_lakes1$area_km2, units="km")
nt_lakes1 <- nt_lakes1%>%
  filter(area_km2 > 1)
# joint lake names
nt_lakes2 <- as.data.frame(nt_lakes)%>%
  select(-geometry)
nt_lakes2 <-  nt_lakes2%>%
  distinct(name_id, .keep_all = TRUE)
nt_lakes <- left_join(nt_lakes1, nt_lakes2, by='name_id')
#define projection 
nt_lakes_CRS <- st_transform(nt_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")
# save lake files (consider set)
st_write(nt_lakes,"./data/processed/nt_lakes.shp")
###############################################################################
rm(list = ls())
nu_lakes_1 <- st_read("./shapefile/canvec_50K_NU_Hydro/waterbody_2_1.shp")
nu_lakes_11 <- st_read("./shapefile/canvec_50K_NU_Hydro/waterbody_2_11.shp")
nu_lakes_15 <- st_read("./shapefile/canvec_50K_NU_Hydro/waterbody_2_15.shp")
nu_lakes_18 <- st_read("./shapefile/canvec_50K_NU_Hydro/waterbody_2_18.shp")
nu_lakes_2_2 <- st_read("./shapefile/canvec_50K_NU_Hydro/waterbody_2_2.shp")
nu_lakes_2_21 <- st_read("./shapefile/canvec_50K_NU_Hydro/waterbody_2_21.shp")
nu_lakes_2_3 <- st_read("./shapefile/canvec_50K_NU_Hydro/waterbody_2_3.shp")
nu_lakes_2_4 <- st_read("./shapefile/canvec_50K_NU_Hydro/waterbody_2_4.shp")
nu_lakes_2_5 <- st_read("./shapefile/canvec_50K_NU_Hydro/waterbody_2_5.shp")
nu_lakes_2_6 <- st_read("./shapefile/canvec_50K_NU_Hydro/waterbody_2_6.shp")
nu_lakes_2_7 <- st_read("./shapefile/canvec_50K_NU_Hydro/waterbody_2_7.shp")
nu_lakes_2_8 <- st_read("./shapefile/canvec_50K_NU_Hydro/waterbody_2_8.shp")
nu_lakes_2_9 <- st_read("./shapefile/canvec_50K_NU_Hydro/waterbody_2_9.shp")
nu_lakes <- rbind(nu_lakes_1,nu_lakes_11,nu_lakes_15,nu_lakes_18,nu_lakes_2_2,nu_lakes_2_21,
                  nu_lakes_2_3,nu_lakes_2_4,nu_lakes_2_5,nu_lakes_2_6, nu_lakes_2_7, nu_lakes_2_8, 
                  nu_lakes_2_9)%>%
  subset(!is.na(namelk1en))
st_write(nu_lakes, "shapefile/Canvec_50K_NU_Hydro/waterbody_nu.shp")
nu_lakes <- st_read("shapefile/Canvec_50K_NU_Hydro/waterbody_nu.shp")
nu_lakes <- st_crop(nu_lakes, c(xmin= -119.9047, ymin = 51.01523, xmax = -61.76674, ymax = 56))
# merge lakes with same name (CanVec has framed large lakes inuo few polygons)
#sf_use_s2(TRUE)
nu_lakes1 <- nu_lakes %>% 
  group_by(name_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()
# calculate polygon area
nu_lakes1$area <- st_area(nu_lakes1)
# convert to square kilometers
nu_lakes1 <- mutate(nu_lakes1,area_km2 = area/1000000)
# drop lakes less than 1 km2
nu_lakes1$area_km2<- as.numeric(nu_lakes1$area_km2, units="km")
nu_lakes1 <- nu_lakes1%>%
  filter(area_km2 > 1)
# joinu lake names
nu_lakes2 <- as.data.frame(nu_lakes)%>%
  select(-geometry)
nu_lakes2 <-  nu_lakes2%>%
  distinct(name_id, .keep_all = TRUE)
nu_lakes <- left_join(nu_lakes1, nu_lakes2, by='name_id')
#define projection 
nu_lakes_CRS <- st_transform(nu_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")
# save lake files (consider set)
st_write(nu_lakes,"./data/processed/nu_lakes.shp")

#############################################################################
# Ontario
rm(list = ls())
on_lakes_2_1 <- st_read("./shapefile/canvec_50K_ON_Hydro/waterbody_2_1.shp")
on_lakes_2_2 <- st_read("./shapefile/canvec_50K_ON_Hydro/waterbody_2_2.shp")
on_lakes_2_3 <- st_read("./shapefile/canvec_50K_ON_Hydro/waterbody_2_3.shp")
on_lakes_2_4 <- st_read("./shapefile/canvec_50K_ON_Hydro/waterbody_2_4.shp")
on_lakes_2_5 <- st_read("./shapefile/canvec_50K_ON_Hydro/waterbody_2_5.shp")
on_lakes <- rbind(on_lakes_2_1,on_lakes_2_2,on_lakes_2_3,on_lakes_2_4,on_lakes_2_5)%>%
  subset(!is.na(namelk1en))
st_write(on_lakes, "shapefile/Canvec_50K_ON_Hydro/waterbody_on.shp")
on_lakes <- st_read("shapefile/Canvec_50K_ON_Hydro/waterbody_on.shp")
on_lakes <- st_crop(on_lakes, c(xmin= -95.60892, ymin = 41.94916, xmax = -74, ymax = 56))
# merge lakes with same name (CanVec has framed large lakes inuo few polygons)
#sf_use_s2(TRUE)
on_lakes1 <- on_lakes %>% 
  group_by(name_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()
# calculate polygon area
sf_use_s2(F)
on_lakes1$area <- st_area(on_lakes1)
# convert to square kilometers
on_lakes1 <- mutate(on_lakes1,area_km2 = area/1000000)
# drop lakes less than 1 km2
on_lakes1$area_km2<- as.numeric(on_lakes1$area_km2, units="km")
on_lakes1 <- on_lakes1%>%
  filter(area_km2 > 1)
# joion lake names
on_lakes2 <- as.data.frame(on_lakes)%>%
  select(-geometry)
on_lakes2 <-  on_lakes2%>%
  distinct(name_id, .keep_all = TRUE)
on_lakes <- left_join(on_lakes1, on_lakes2, by='name_id')
#define projection 
on_lakes_CRS <- st_transform(on_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")
# save lake files (consider set)
st_write(on_lakes,"./data/processed/on_lakes.shp")
###################################################################################################
# Prince Edward Island
rm(list = ls())
pe_lakes <- st_read("./shapefile/canvec_50K_PE_Hydro/waterbody_2.shp")%>%
  subset(!is.na(namelk1en))
pe_lakes <- st_crop(pe_lakes, c(xmin= -65, ymin = 45.60189, xmax = -61.5, ymax = 56))
# merge lakes with same name (CanVec has framed large lakes inuo few polygons)
sf_use_s2(TRUE)
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
# joion lake names
pe_lakes2 <- as.data.frame(pe_lakes)%>%
  select(-geometry)
pe_lakes2 <-  pe_lakes2%>%
  distinct(name_id, .keep_all = TRUE)
pe_lakes <- left_join(pe_lakes1, pe_lakes2, by='name_id')
#define projection 
pe_lakes_CRS <- st_transform(pe_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")
# save  lake files (consider set)
st_write(pe_lakes,"./data/processed/pe_lakes.shp")
#########################################################################################
# Quebec
rm(list = ls())
qc_lakes_2_1 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_1.shp")
qc_lakes_2_2 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_2.shp")
qc_lakes_2_3 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_3.shp")
qc_lakes_2_4 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_4.shp")
qc_lakes_2_5 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_5.shp")
qc_lakes_2_6 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_6.shp")
qc_lakes_2_7 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_7.shp")
qc_lakes_2_8 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_8.shp")
qc_lakes_2_10 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_10.shp")
qc_lakes_2_12 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_12.shp")
qc_lakes_2_13 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_13.shp")
qc_lakes_2_17 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_17.shp")
qc_lakes_2_22 <- st_read("./shapefile/canvec_50K_QC_Hydro/waterbody_2_22.shp")
qc_lakes <- rbind(qc_lakes_2_1,qc_lakes_2_2,qc_lakes_2_3,qc_lakes_2_4,qc_lakes_2_5,qc_lakes_2_6,
                  qc_lakes_2_7,qc_lakes_2_8,qc_lakes_2_10,qc_lakes_2_12,qc_lakes_2_13,qc_lakes_2_17,qc_lakes_2_22)%>%
  subset(!is.na(namelk1en))
st_write(qc_lakes, "shapefile/Canvec_50K_QC_Hydro/waterbody_qc.shp")
qc_lakes <- st_read("shapefile/Canvec_50K_QC_Hydro/waterbody_qc.shp")
qc_lakes <- st_crop(qc_lakes, c(xmin= -80.21842, ymin = 44.83273, xmax = -57, ymax = 56))
# merge lakes with same name (CanVec has framed large lakes inuo few polygons)
sf_use_s2(TRUE)
qc_lakes1 <- qc_lakes %>% 
  group_by(name_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()
# calculate polygon area
sf_use_s2(F)
qc_lakes1$area <- st_area(qc_lakes1)
# convert to square kilometers
qc_lakes1 <- mutate(qc_lakes1,area_km2 = area/1000000)
# drop lakes less than 1 km2
qc_lakes1$area_km2<- as.numeric(qc_lakes1$area_km2, units="km")
qc_lakes1 <- qc_lakes1%>%
  filter(area_km2 > 1)
# joion lake names
qc_lakes2 <- as.data.frame(qc_lakes)%>%
  select(-geometry)
qc_lakes2 <-  qc_lakes2%>%
  distinct(name_id, .keep_all = TRUE)
qc_lakes <- left_join(qc_lakes1, qc_lakes2, by='name_id')
#define projection 
qc_lakes_CRS <- st_transform(qc_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")
# save lake files (consider set)
st_write(qc_lakes,"./data/processed/qc_lakes.shp")
########################################################################
# Yukon
rm(list = ls())
yt_lakes <- st_read("./shapefile/canvec_50K_YT_Hydro/waterbody_2.shp")%>%
  subset(!is.na(namelk1en))
yt_lakes <- st_crop(yt_lakes, c(xmin= -141.0028, ymin = 59.5, xmax = -123.5, ymax = 56))
# merge lakes with same name (CanVec has framed large lakes inuo few polygons)
sf_use_s2(TRUE)
yt_lakes1 <- yt_lakes %>% 
  group_by(name_id) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()
# calculate polygon area
yt_lakes1$area <- st_area(yt_lakes1)
# convert to square kilometers
yt_lakes1 <- mutate(yt_lakes1,area_km2 = area/1000000)
# drop lakes less than 1 km2
yt_lakes1$area_km2<- as.numeric(yt_lakes1$area_km2, units="km")
yt_lakes1 <- yt_lakes1%>%
  filter(area_km2 > 1)
# joion lake names
yt_lakes2 <- as.data.frame(yt_lakes)%>%
  select(-geometry)
yt_lakes2 <-  yt_lakes2%>%
  distinct(name_id, .keep_all = TRUE)
yt_lakes <- left_join(yt_lakes1, yt_lakes2, by='name_id')
#define projection 
yt_lakes_CRS <- st_transform(yt_lakes, crs = "+proj=longlat +datum=WGS84 +no_defs")
# save lake files (consider set)
st_write(yt_lakes,"./data/processed/yt_lakes.shp")







