# Model 15 - AB + lakes + buffer + building foot print + census
# Linking lakes data of NS building foot print + census data

# creating buffer around water body. Need to define buffer zones as
# our objectives. For now I defined 500m buffer zone in Lake Winnnipeg

rm(list = ls())
# load library - CanVec
library(rcanvec)
# load library - Canada Census
library(cancensus)
# other basic libraries for spatial data
library(sf)
library(sp)
library(rgdal)
library(sp)
library(tidyverse)
library(spatialEco)
library(FIESTA)


# read shape file
ab_lake <- st_read("./data/processed/alb_lakes.shp")

# projection - make sure planer
ab_lakes_epsg <- st_transform(ab_lake, crs = 3348)

# creating buffer - meters (projection ensure this)
ab_lake_buffer <- st_buffer(ab_lakes_epsg, dist = 500)

# write shape file
st_write(ab_lake_buffer, "./check_arcGIS/ab_lake_buffer.shp")

# read NS building foot print
ab_fp <- st_read("./Building_footprint/AB/ab.shp")

#projection - for map purpose
ab_lake_buffer <- st_transform(ab_lake_buffer, crs = "+proj=longlat +datum=WGS84 +no_defs")
ab_fp <- st_transform(ab_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(ab_lake_buffer)==st_crs(ab_fp)

# clip - building foot prints within buffer

ab_lake_buffer_epsg <- st_transform(ab_lake_buffer, crs = 3348)
ab_fp_epsg <- st_transform(ab_fp, crs = 3348)

# This data layer consist with lake id and adjacent
ab_lake_home_500 <- st_intersection(ab_lake_buffer_epsg, ab_fp_epsg)
st_write(ab_lake_home_500, "./check_arcGIS/ab_lake_home_500.shp")

# read census file db or da

ab_census_db <- st_read( "./Census/AB/census_ab_db.shp")
ab_census_da <- st_read( "./Census/AB/census_ab_da.shp")


# clip - DB or DA capture those buffer - need to get the census characteristics of those areas
# since at the moment we have dwelling values at DA will depend on DA, but if we figure out forDB data
# need to updat
ab_census_da_epsg <- st_transform(ab_census_da, crs = 3348)

ab_lh_500_cda <- st_intersection(ab_lake_home_500, ab_census_da_epsg)

st_write(ab_lh_500_cda, "./check_arcGIS/ab_lh_cda.shp")

# select variables 

colnames(ab_lh_500_cda)

ab_lk500 <- ab_lh_500_cda%>%
  select(name_id, area_km2, feature_id, perm, name_en, ShapeAr, Type, Hoshlds, 
         Dwllngs, GeoUID, Popultn, Ar.skm., v_CATpd, v_CA1P2, v_Avod.)


# count house 
ab_lk500_count <- ab_lk500%>%
  count(GeoUID)


# calculate the polygon area of building foot print. This can be used to filter 
# out the single homes (remove buildings that do not represent homes)
# subjective call need discussion

# calculate polygon area - square meters
ab_lk500_count$building_area <- st_area(ab_lk500_count)

# merge back census data including average dwelling values 

colnames(ab_census_da)

df1 <-ab_census_da%>%
  select(Type, Hoshlds, Dwllngs, GeoUID, Popultn, Ar.skm., v_CA1P2, v_CA1P2, v_Avod.)%>%
  as.data.frame()%>%
  select(-geometry)

ab_lk500_count <- left_join(ab_lk500_count, df1, by = "GeoUID")


#save file
# This layer has number of houses within 500m buffer (need to filter based on building area)
# and census data attached to each DA level
# For example if a house locate within a buffer that located within DA called 001, then I attacged the
# census data of DA 001 to that house
st_write(ab_lk500_count, "./check_arcGIS/ab_lk500_count.shp")