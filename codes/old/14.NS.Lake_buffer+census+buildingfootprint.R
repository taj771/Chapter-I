# Model 14 - NS + lakes + buffer + building foot print + census
# Linking lakes data of NS building foot print + census data

# creating buffer around water body. Need to define buffer zones as
# our objectives. For now I defined 500m buffer zone in Lake Winnnipeg

# clean memory
rm(list = ls())

# other basic libraries for spatial data
library(sf)
library(sp)
library(rgdal)
library(sp)
library(tidyverse)
library(spatialEco)
library(FIESTA)
library(cancensus)


# read shape file
ns_lake <- st_read("./data/processed/ns_lakes.shp")
sk_lake <- st_read("./data/processed/sk_lakes.shp")
ab_lake <- st_read("./data/processed/ab_lakes.shp")
bc_lake <- st_read("./data/processed/bc_lakes.shp")
mb_lake <- st_read("./data/processed/mb_lakes.shp")
nl_lake <- st_read("./data/processed/nl_lakes.shp")
nt_lake <- st_read("./data/processed/nt_lakes.shp")
nu_lake <- st_read("./data/processed/nu_lakes.shp")
on_lake <- st_read("./data/processed/on_lakes.shp")
pe_lake <- st_read("./data/processed/pe_lakes.shp")
qc_lake <- st_read("./data/processed/qc_lakes.shp")
yt_lake <- st_read("./data/processed/yt_lakes.shp")

# projection - make sure planer
ns_lakes_epsg <- st_transform(ns_lake, crs = 3348)
sk_lakes_epsg <- st_transform(sk_lake, crs = 3348)
ab_lakes_epsg <- st_transform(ab_lake, crs = 3348)
bc_lakes_epsg <- st_transform(bc_lake, crs = 3348)
mb_lakes_epsg <- st_transform(mb_lake, crs = 3348)
nl_lakes_epsg <- st_transform(nl_lake, crs = 3348)
nt_lakes_epsg <- st_transform(nt_lake, crs = 3348)
nu_lakes_epsg <- st_transform(nu_lake, crs = 3348)
on_lakes_epsg <- st_transform(on_lake, crs = 3348)
pe_lakes_epsg <- st_transform(pe_lake, crs = 3348)
qc_lakes_epsg <- st_transform(qc_lake, crs = 3348)
yt_lakes_epsg <- st_transform(yt_lake, crs = 3348)

# creating buffer - meters (projection ensure this)
ns_lake_buffer <- st_buffer(ns_lakes_epsg, dist = 500)
sk_lake_buffer <- st_buffer(sk_lakes_epsg, dist = 500)
ab_lake_buffer <- st_buffer(ab_lakes_epsg, dist = 500)
bc_lake_buffer <- st_buffer(bc_lakes_epsg, dist = 500)
mb_lake_buffer <- st_buffer(mb_lakes_epsg, dist = 500)
nl_lake_buffer <- st_buffer(nl_lakes_epsg, dist = 500)
nt_lake_buffer <- st_buffer(nt_lakes_epsg, dist = 500)
nu_lake_buffer <- st_buffer(nu_lakes_epsg, dist = 500)
on_lake_buffer <- st_buffer(on_lakes_epsg, dist = 500)
pe_lake_buffer <- st_buffer(pe_lakes_epsg, dist = 500)
qc_lake_buffer <- st_buffer(qc_lakes_epsg, dist = 500)
yt_lake_buffer <- st_buffer(yt_lakes_epsg, dist = 500)


# write shape file - visualizing purpose
st_write(ns_lake_buffer, "./check_arcGIS/ns_lake_buffer.shp", delete_dsn = T)
st_write(sk_lake_buffer, "./check_arcGIS/sk_lake_buffer.shp", delete_dsn = T)
st_write(ab_lake_buffer, "./check_arcGIS/ab_lake_buffer.shp", delete_dsn = T)
st_write(bc_lake_buffer, "./check_arcGIS/bc_lake_buffer.shp", delete_dsn = T)
st_write(mb_lake_buffer, "./check_arcGIS/mb_lake_buffer.shp", delete_dsn = T)
st_write(nl_lake_buffer, "./check_arcGIS/nl_lake_buffer.shp", delete_dsn = T)
st_write(nt_lake_buffer, "./check_arcGIS/nt_lake_buffer.shp", delete_dsn = T)
st_write(nu_lake_buffer, "./check_arcGIS/nu_lake_buffer.shp", delete_dsn = T)
st_write(on_lake_buffer, "./check_arcGIS/on_lake_buffer.shp", delete_dsn = T)
st_write(pe_lake_buffer, "./check_arcGIS/pe_lake_buffer.shp", delete_dsn = T)
st_write(qc_lake_buffer, "./check_arcGIS/qc_lake_buffer.shp", delete_dsn = T)
st_write(yt_lake_buffer, "./check_arcGIS/yt_lake_buffer.shp", delete_dsn = T)

# read NS building foot print
ns_fp <- st_read("./Building_footprint/NS/ns.shp")
sk_fp <- st_read("./Building_footprint/SK/sk.shp")
ab_fp <- st_read("./Building_footprint/AB/ab.shp")
bc_fp <- st_read("./Building_footprint/BC/bc.shp")
mb_fp <- st_read("./Building_footprint/MB/mb.shp")
nl_fp <- st_read("./Building_footprint/NL/nl.shp")
nt_fp <- st_read("./Building_footprint/NT/nt.shp")
nu_fp <- st_read("./Building_footprint/NU/nu.shp")
on_fp <- st_read("./Building_footprint/ON/on.shp")
pe_fp <- st_read("./Building_footprint/PE/pe.shp")
qc_fp <- st_read("./Building_footprint/QC/qc.shp")
yt_fp <- st_read("./Building_footprint/YU/yu.shp")

#projection - for map purpose
ns_lake_buffer <- st_transform(ns_lake_buffer, crs = "+proj=longlat +datum=WGS84 +no_defs")
ns_fp <- st_transform(ns_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(ns_lake_buffer)==st_crs(ns_fp)
sk_lake_buffer <- st_transform(sk_lake_buffer, crs = "+proj=longlat +datum=WGS84 +no_defs")
sk_fp <- st_transform(sk_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(sk_lake_buffer)==st_crs(sk_fp)
ab_lake_buffer <- st_transform(ab_lake_buffer, crs = "+proj=longlat +datum=WGS84 +no_defs")
ab_fp <- st_transform(ab_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(ab_lake_buffer)==st_crs(ab_fp)
bc_lake_buffer <- st_transform(bc_lake_buffer, crs = "+proj=longlat +datum=WGS84 +no_defs")
bc_fp <- st_transform(bc_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(bc_lake_buffer)==st_crs(bc_fp)
mb_lake_buffer <- st_transform(mb_lake_buffer, crs = "+proj=longlat +datum=WGS84 +no_defs")
mb_fp <- st_transform(mb_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(mb_lake_buffer)==st_crs(mb_fp)
nl_lake_buffer <- st_transform(nl_lake_buffer, crs = "+proj=longlat +datum=WGS84 +no_defs")
nl_fp <- st_transform(nl_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(nl_lake_buffer)==st_crs(nl_fp)
nt_lake_buffer <- st_transform(nt_lake_buffer, crs = "+proj=longlat +datum=WGS84 +no_defs")
nt_fp <- st_transform(nt_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(nt_lake_buffer)==st_crs(nt_fp)
nu_lake_buffer <- st_transform(nu_lake_buffer, crs = "+proj=longlat +datum=WGS84 +no_defs")
nu_fp <- st_transform(nu_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(nu_lake_buffer)==st_crs(nu_fp)
on_lake_buffer <- st_transform(on_lake_buffer, crs = "+proj=longlat +datum=WGS84 +no_defs")
on_fp <- st_transform(on_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(on_lake_buffer)==st_crs(on_fp)
pe_lake_buffer <- st_transform(pe_lake_buffer, crs = "+proj=longlat +datum=WGS84 +no_defs")
pe_fp <- st_transform(pe_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(pe_lake_buffer)==st_crs(pe_fp)
qc_lake_buffer <- st_transform(qc_lake_buffer, crs = "+proj=longlat +datum=WGS84 +no_defs")
qc_fp <- st_transform(qc_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(qc_lake_buffer)==st_crs(qc_fp)
yt_lake_buffer <- st_transform(yt_lake_buffer, crs = "+proj=longlat +datum=WGS84 +no_defs")
yt_fp <- st_transform(yt_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(yt_lake_buffer)==st_crs(yt_fp)

# clip - building foot prints within buffer
ns_lake_buffer_epsg <- st_transform(ns_lake_buffer, crs = 3348)
ns_fp_epsg <- st_transform(ns_fp, crs = 3348)
sk_lake_buffer_epsg <- st_transform(sk_lake_buffer, crs = 3348)
sk_fp_epsg <- st_transform(sk_fp, crs = 3348)
ab_lake_buffer_epsg <- st_transform(ab_lake_buffer, crs = 3348)
ab_fp_epsg <- st_transform(ab_fp, crs = 3348)
bc_lake_buffer_epsg <- st_transform(bc_lake_buffer, crs = 3348)
bc_fp_epsg <- st_transform(bc_fp, crs = 3348)
mb_lake_buffer_epsg <- st_transform(mb_lake_buffer, crs = 3348)
mb_fp_epsg <- st_transform(mb_fp, crs = 3348)
nl_lake_buffer_epsg <- st_transform(nl_lake_buffer, crs = 3348)
nl_fp_epsg <- st_transform(nl_fp, crs = 3348)
nt_lake_buffer_epsg <- st_transform(nt_lake_buffer, crs = 3348)
nt_fp_epsg <- st_transform(nt_fp, crs = 3348)
nu_lake_buffer_epsg <- st_transform(nu_lake_buffer, crs = 3348)
nu_fp_epsg <- st_transform(nu_fp, crs = 3348)
on_lake_buffer_epsg <- st_transform(on_lake_buffer, crs = 3348)
on_fp_epsg <- st_transform(on_fp, crs = 3348)
pe_lake_buffer_epsg <- st_transform(pe_lake_buffer, crs = 3348)
pe_fp_epsg <- st_transform(pe_fp, crs = 3348)
qc_lake_buffer_epsg <- st_transform(qc_lake_buffer, crs = 3348)
qc_fp_epsg <- st_transform(qc_fp, crs = 3348)
yt_lake_buffer_epsg <- st_transform(yt_lake_buffer, crs = 3348)
yt_fp_epsg <- st_transform(yt_fp, crs = 3348)


# This data layer consist with lake id and adjacent homes
ns_lake_home_500 <- st_intersection(ns_lake_buffer_epsg, ns_fp_epsg)
sk_lake_home_500 <- st_intersection(sk_lake_buffer_epsg, sk_fp_epsg)
ab_lake_home_500 <- st_intersection(ab_lake_buffer_epsg, ab_fp_epsg)
bc_lake_home_500 <- st_intersection(bc_lake_buffer_epsg, bc_fp_epsg)
mb_lake_home_500 <- st_intersection(mb_lake_buffer_epsg, mb_fp_epsg)
nl_lake_home_500 <- st_intersection(nl_lake_buffer_epsg, nl_fp_epsg)
nt_lake_home_500 <- st_intersection(nt_lake_buffer_epsg, nt_fp_epsg)
nu_lake_home_500 <- st_intersection(nu_lake_buffer_epsg, nu_fp_epsg)
on_lake_home_500 <- st_intersection(on_lake_buffer_epsg, on_fp_epsg)
pe_lake_home_500 <- st_intersection(pe_lake_buffer_epsg, pe_fp_epsg)
qc_lake_home_500 <- st_intersection(qc_lake_buffer_epsg, qc_fp_epsg)
yt_lake_home_500 <- st_intersection(yt_lake_buffer_epsg, yt_fp_epsg)

# write shapefile to vizualizing purpose 
st_write(ns_lake_home_500, "./check_arcGIS/ns_lake_home_500.shp",delete_dsn = T)
st_write(sk_lake_home_500, "./check_arcGIS/sk_lake_home_500.shp",delete_dsn = T)
st_write(ab_lake_home_500, "./check_arcGIS/ab_lake_home_500.shp",delete_dsn = T)
st_write(bc_lake_home_500, "./check_arcGIS/bc_lake_home_500.shp",delete_dsn = T)
st_write(mb_lake_home_500, "./check_arcGIS/mb_lake_home_500.shp",delete_dsn = T)
st_write(nl_lake_home_500, "./check_arcGIS/nl_lake_home_500.shp",delete_dsn = T)
st_write(nt_lake_home_500, "./check_arcGIS/nt_lake_home_500.shp",delete_dsn = T)
st_write(nu_lake_home_500, "./check_arcGIS/nu_lake_home_500.shp",delete_dsn = T)
st_write(on_lake_home_500, "./check_arcGIS/on_lake_home_500.shp",delete_dsn = T)
st_write(pe_lake_home_500, "./check_arcGIS/pe_lake_home_500.shp",delete_dsn = T)
st_write(qc_lake_home_500, "./check_arcGIS/qc_lake_home_500.shp",delete_dsn = T)
st_write(yt_lake_home_500, "./check_arcGIS/yt_lake_home_500.shp",delete_dsn = T)

# read census file db or da
ns_census_da <- st_read( "./Census/NS/census_ns_da.shp")
sk_census_da <- st_read( "./Census/SK/census_sk_da.shp")
ab_census_da <- st_read( "./Census/AB/census_ab_da.shp")
bc_census_da <- st_read( "./Census/BC/census_bc_da.shp")
mb_census_da <- st_read( "./Census/MB/census_mb_da.shp")
nl_census_da <- st_read( "./Census/NL/census_nl_da.shp")
nt_census_da <- st_read( "./Census/NT/census_nt_da.shp")
nu_census_da <- st_read( "./Census/NU/census_nu_da.shp")
on_census_da <- st_read( "./Census/ON/census_on_da.shp")
pe_census_da <- st_read( "./Census/PE/census_pe_da.shp")
qc_census_da <- st_read( "./Census/QC/census_qc_da.shp")
yt_census_da <- st_read( "./Census/YT/census_yt_da.shp")

# clip - DB or DA capture those buffer - need to get the census characteristics of those areas
# since at the moment we have dwelling values at DA will depend on DA, but if we figure out for DB data
# need to update
# confirm projection
ns_census_da_epsg <- st_transform(ns_census_da, crs = 3348)
sk_census_da_epsg <- st_transform(sk_census_da, crs = 3348)
ab_census_da_epsg <- st_transform(ab_census_da, crs = 3348)
bc_census_da_epsg <- st_transform(bc_census_da, crs = 3348)
mb_census_da_epsg <- st_transform(mb_census_da, crs = 3348)
nl_census_da_epsg <- st_transform(nl_census_da, crs = 3348)
nt_census_da_epsg <- st_transform(nt_census_da, crs = 3348)
nu_census_da_epsg <- st_transform(nu_census_da, crs = 3348)
on_census_da_epsg <- st_transform(on_census_da, crs = 3348)
pe_census_da_epsg <- st_transform(pe_census_da, crs = 3348)
qc_census_da_epsg <- st_transform(qc_census_da, crs = 3348)
yt_census_da_epsg <- st_transform(yt_census_da, crs = 3348)


# intersection
ns_lh_500_cda <- st_intersection(ns_lake_home_500, ns_census_da_epsg)
sk_lh_500_cda <- st_intersection(sk_lake_home_500, sk_census_da_epsg)
ab_lh_500_cda <- st_intersection(ab_lake_home_500, ab_census_da_epsg)
bc_lh_500_cda <- st_intersection(bc_lake_home_500, bc_census_da_epsg)
mb_lh_500_cda <- st_intersection(mb_lake_home_500, mb_census_da_epsg)
nl_lh_500_cda <- st_intersection(nl_lake_home_500, nl_census_da_epsg)
nt_lh_500_cda <- st_intersection(nt_lake_home_500, nt_census_da_epsg)
nu_lh_500_cda <- st_intersection(nu_lake_home_500, nu_census_da_epsg)
on_lh_500_cda <- st_intersection(on_lake_home_500, on_census_da_epsg)
pe_lh_500_cda <- st_intersection(pe_lake_home_500, pe_census_da_epsg)
qc_lh_500_cda <- st_intersection(qc_lake_home_500, qc_census_da_epsg)
yt_lh_500_cda <- st_intersection(yt_lake_home_500, yt_census_da_epsg)


#write shape file 
st_write(ns_lh_500_cda, "./check_arcGIS/ns_lh_cda.shp")
st_write(sk_lh_500_cda, "./check_arcGIS/sk_lh_cda.shp")
st_write(ab_lh_500_cda, "./check_arcGIS/ab_lh_cda.shp")
st_write(bc_lh_500_cda, "./check_arcGIS/bc_lh_cda.shp")
st_write(mb_lh_500_cda, "./check_arcGIS/mb_lh_cda.shp")
st_write(nl_lh_500_cda, "./check_arcGIS/nl_lh_cda.shp")
st_write(nt_lh_500_cda, "./check_arcGIS/nt_lh_cda.shp")
st_write(nu_lh_500_cda, "./check_arcGIS/nu_lh_cda.shp")
st_write(on_lh_500_cda, "./check_arcGIS/on_lh_cda.shp")
st_write(pe_lh_500_cda, "./check_arcGIS/pe_lh_cda.shp")
st_write(qc_lh_500_cda, "./check_arcGIS/qc_lh_cda.shp")
st_write(yt_lh_500_cda, "./check_arcGIS/yt_lh_cda.shp")


# select variables 
# need to filterout which variables we are interested 

colnames(ns_lh_500_cda)

ns_lk500 <- ns_lh_500_cda%>%
  select(name_id, area_km2, feature_id, perm, name_en, ShapeAr, Type, Hoshlds, 
         Dwllngs, GeoUID, Popultn, Ar.skm., v_CATpd, v_CA1P2, v_Avod.)
sk_lk500 <- sk_lh_500_cda%>%
  select(name_id, area_km2, feature_id, perm, name_en, ShapeAr, Type, Hoshlds, 
         Dwllngs, GeoUID, Popultn, Ar.skm., v_CATpd, v_CA1P2, v_Avod.)
ab_lk500 <- ab_lh_500_cda%>%
  select(name_id, area_km2, feature_id, perm, name_en, ShapeAr, Type, Hoshlds, 
         Dwllngs, GeoUID, Popultn, Ar.skm., v_CATpd, v_CA1P2, v_Avod.)
bc_lk500 <- bc_lh_500_cda%>%
  select(name_id, area_km2, feature_id, perm, name_en, ShapeAr, Type, Hoshlds, 
         Dwllngs, GeoUID, Popultn, Ar.skm., v_CATpd, v_CA1P2, v_Avod.)
mb_lk500 <- mb_lh_500_cda%>%
  select(name_id, area_km2, feature_id, perm, name_en, ShapeAr, Type, Hoshlds, 
         Dwllngs, GeoUID, Popultn, Ar.skm., v_CATpd, v_CA1P2, v_Avod.)
nl_lk500 <- nl_lh_500_cda%>%
  select(name_id, area_km2, feature_id, perm, name_en, ShapeAr, Type, Hoshlds, 
         Dwllngs, GeoUID, Popultn, Ar.skm., v_CATpd, v_CA1P2, v_Avod.)
nt_lk500 <- nt_lh_500_cda%>%
  select(name_id, area_km2, feature_id, perm, name_en, ShapeAr, Type, Hoshlds, 
         Dwllngs, GeoUID, Popultn, Ar.skm., v_CATpd, v_CA1P2, v_Avod.)
nu_lk500 <- nu_lh_500_cda%>%
  select(name_id, area_km2, feature_id, perm, name_en, ShapeAr, Type, Hoshlds, 
         Dwllngs, GeoUID, Popultn, Ar.skm., v_CATpd, v_CA1P2, v_Avod.)
on_lk500 <- on_lh_500_cda%>%
  select(name_id, area_km2, feature_id, perm, name_en, ShapeAr, Type, Hoshlds, 
         Dwllngs, GeoUID, Popultn, Ar.skm., v_CATpd, v_CA1P2, v_Avod.)
pe_lk500 <- pe_lh_500_cda%>%
  select(name_id, area_km2, feature_id, perm, name_en, ShapeAr, Type, Hoshlds, 
         Dwllngs, GeoUID, Popultn, Ar.skm., v_CATpd, v_CA1P2, v_Avod.)
qc_lk500 <- qc_lh_500_cda%>%
  select(name_id, area_km2, feature_id, perm, name_en, ShapeAr, Type, Hoshlds, 
         Dwllngs, GeoUID, Popultn, Ar.skm., v_CATpd, v_CA1P2, v_Avod.)
yt_lk500 <- yt_lh_500_cda%>%
  select(name_id, area_km2, feature_id, perm, name_en, ShapeAr, Type, Hoshlds, 
         Dwllngs, GeoUID, Popultn, Ar.skm., v_CATpd, v_CA1P2, v_Avod.)
# count house 
ns_lk500_count <- ns_lk500%>%
  count(GeoUID)
sk_lk500_count <- sk_lk500%>%
  count(GeoUID)
ab_lk500_count <- ab_lk500%>%
  count(GeoUID)
bc_lk500_count <- bc_lk500%>%
  count(GeoUID)
mb_lk500_count <- mb_lk500%>%
  count(GeoUID)
nl_lk500_count <- nl_lk500%>%
  count(GeoUID)
nt_lk500_count <- nt_lk500%>%
  count(GeoUID)
nu_lk500_count <- nu_lk500%>%
  count(GeoUID)
on_lk500_count <- on_lk500%>%
  count(GeoUID)
pe_lk500_count <- pe_lk500%>%
  count(GeoUID)
qc_lk500_count <- qc_lk500%>%
  count(GeoUID)
yt_lk500_count <- yt_lk500%>%
  count(GeoUID)
# calculate the polygon area of building foot print. This can be used to filter 
# out the single homes (remove buildings that do not represent homes)
# subjective call need discussion

# calculate polygon area - square meters
ns_lk500_count$building_area <- st_area(ns_lk500_count)
sk_lk500_count$building_area <- st_area(sk_lk500_count)
ab_lk500_count$building_area <- st_area(ab_lk500_count)
bc_lk500_count$building_area <- st_area(bc_lk500_count)
mb_lk500_count$building_area <- st_area(mb_lk500_count)
nl_lk500_count$building_area <- st_area(nl_lk500_count)
nt_lk500_count$building_area <- st_area(nt_lk500_count)
nu_lk500_count$building_area <- st_area(nu_lk500_count)
on_lk500_count$building_area <- st_area(on_lk500_count)
pe_lk500_count$building_area <- st_area(pe_lk500_count)
qc_lk500_count$building_area <- st_area(qc_lk500_count)
yt_lk500_count$building_area <- st_area(yt_lk500_count)


# merge back census data including average dwelling values 
colnames(ns_census_da)
df1 <-ns_census_da%>%
  select(Type, Hoshlds, Dwllngs, GeoUID, Popultn, Ar.skm., v_CA1P2, v_CA1P2, v_Avod.)%>%
  as.data.frame()%>%
  select(-geometry)
df2 <-sk_census_da%>%
  select(Type, Hoshlds, Dwllngs, GeoUID, Popultn, Ar.skm., v_CA1P2, v_CA1P2, v_Avod.)%>%
  as.data.frame()%>%
  select(-geometry)
df3 <-ab_census_da%>%
  select(Type, Hoshlds, Dwllngs, GeoUID, Popultn, Ar.skm., v_CA1P2, v_CA1P2, v_Avod.)%>%
  as.data.frame()%>%
  select(-geometry)
df4 <-bc_census_da%>%
  select(Type, Hoshlds, Dwllngs, GeoUID, Popultn, Ar.skm., v_CA1P2, v_CA1P2, v_Avod.)%>%
  as.data.frame()%>%
  select(-geometry)
df5 <-mb_census_da%>%
  select(Type, Hoshlds, Dwllngs, GeoUID, Popultn, Ar.skm., v_CA1P2, v_CA1P2, v_Avod.)%>%
  as.data.frame()%>%
  select(-geometry)
df6 <-nl_census_da%>%
  select(Type, Hoshlds, Dwllngs, GeoUID, Popultn, Ar.skm., v_CA1P2, v_CA1P2, v_Avod.)%>%
  as.data.frame()%>%
  select(-geometry)
df7 <-nt_census_da%>%
  select(Type, Hoshlds, Dwllngs, GeoUID, Popultn, Ar.skm., v_CA1P2, v_CA1P2, v_Avod.)%>%
  as.data.frame()%>%
  select(-geometry)
df8 <-nu_census_da%>%
  select(Type, Hoshlds, Dwllngs, GeoUID, Popultn, Ar.skm., v_CA1P2, v_CA1P2, v_Avod.)%>%
  as.data.frame()%>%
  select(-geometry)
df9 <-on_census_da%>%
  select(Type, Hoshlds, Dwllngs, GeoUID, Popultn, Ar.skm., v_CA1P2, v_CA1P2, v_Avod.)%>%
  as.data.frame()%>%
  select(-geometry)
df10 <-pe_census_da%>%
  select(Type, Hoshlds, Dwllngs, GeoUID, Popultn, Ar.skm., v_CA1P2, v_CA1P2, v_Avod.)%>%
  as.data.frame()%>%
  select(-geometry)
df11 <-qc_census_da%>%
  select(Type, Hoshlds, Dwllngs, GeoUID, Popultn, Ar.skm., v_CA1P2, v_CA1P2, v_Avod.)%>%
  as.data.frame()%>%
  select(-geometry)
df12 <-yt_census_da%>%
  select(Type, Hoshlds, Dwllngs, GeoUID, Popultn, Ar.skm., v_CA1P2, v_CA1P2, v_Avod.)%>%
  as.data.frame()%>%
  select(-geometry)

# Left join
ns_lk500_count <- left_join(ns_lk500_count, df1, by = "GeoUID")
sk_lk500_count <- left_join(sk_lk500_count, df2, by = "GeoUID")
ab_lk500_count <- left_join(ab_lk500_count, df3, by = "GeoUID")
bc_lk500_count <- left_join(bc_lk500_count, df4, by = "GeoUID")
mb_lk500_count <- left_join(mb_lk500_count, df5, by = "GeoUID")
nl_lk500_count <- left_join(nl_lk500_count, df6, by = "GeoUID")
nt_lk500_count <- left_join(nt_lk500_count, df7, by = "GeoUID")
nu_lk500_count <- left_join(nu_lk500_count, df8, by = "GeoUID")
on_lk500_count <- left_join(on_lk500_count, df9, by = "GeoUID")
pe_lk500_count <- left_join(pe_lk500_count, df10, by = "GeoUID")
qc_lk500_count <- left_join(qc_lk500_count, df11, by = "GeoUID")
yt_lk500_count <- left_join(yt_lk500_count, df12, by = "GeoUID")

#save file
# This layer has number of houses within 500m buffer (need to filter based on building area)
# and census data attached to each DA level
# For example if a house locate within a buffer that located within DA called 001, then I attacged the
# census data of DA 001 to that house
st_write(ns_lk500_count, "./check_arcGIS/ns_lk500_count.shp")
st_write(sk_lk500_count, "./check_arcGIS/sk_lk500_count.shp")
st_write(ab_lk500_count, "./check_arcGIS/ab_lk500_count.shp")
st_write(bc_lk500_count, "./check_arcGIS/bc_lk500_count.shp")
st_write(mb_lk500_count, "./check_arcGIS/mb_lk500_count.shp")
st_write(nl_lk500_count, "./check_arcGIS/nl_lk500_count.shp")
st_write(nt_lk500_count, "./check_arcGIS/nt_lk500_count.shp")
st_write(nu_lk500_count, "./check_arcGIS/nu_lk500_count.shp")
st_write(on_lk500_count, "./check_arcGIS/on_lk500_count.shp")
st_write(pe_lk500_count, "./check_arcGIS/pe_lk500_count.shp")
st_write(qc_lk500_count, "./check_arcGIS/qc_lk500_count.shp")
st_write(yt_lk500_count, "./check_arcGIS/yt_lk500_count.shp")















##############################################################################
df <- subset(ns_lk500_count,GeoUID == "12010030")

st_write(df, "./check_arcGIS/GeoUID_12010030.shp")


