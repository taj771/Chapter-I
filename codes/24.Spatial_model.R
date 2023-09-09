# Model : Spatially integrate census data, building footprint and elasticity estimate
# Load library 
rm(list = ls())

library(pacman)
pacman::p_load(sf,sp,rgdal,tidyverse,spatialEco,FIESTA,lwgeom) 

# parameter definition
nonwater_front <- 500
waterfront <- 50
individual_houses_ub <- 167 #square meters
individual_houses_lb <- 70 #square meters


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


# creating buffer - meters (projection ensure this) - 50m
ns_lake_buffer_50 <- st_buffer(ns_lakes_epsg, dist = waterfront)
sk_lake_buffer_50 <- st_buffer(sk_lakes_epsg, dist = waterfront)
ab_lake_buffer_50 <- st_buffer(ab_lakes_epsg, dist = waterfront)
bc_lake_buffer_50 <- st_buffer(bc_lakes_epsg, dist = waterfront)
mb_lake_buffer_50 <- st_buffer(mb_lakes_epsg, dist = waterfront)
nl_lake_buffer_50 <- st_buffer(nl_lakes_epsg, dist = waterfront)
nt_lake_buffer_50 <- st_buffer(nt_lakes_epsg, dist = waterfront)
nu_lake_buffer_50 <- st_buffer(nu_lakes_epsg, dist = waterfront)
on_lake_buffer_50 <- st_buffer(on_lakes_epsg, dist = waterfront)
pe_lake_buffer_50 <- st_buffer(pe_lakes_epsg, dist = waterfront)
qc_lake_buffer_50 <- st_buffer(qc_lakes_epsg, dist = waterfront)
yt_lake_buffer_50 <- st_buffer(yt_lakes_epsg, dist = waterfront)

#write shape file - 50m
st_write(ns_lake_buffer_50, "./check_arcGIS/NS/ns_lake_buffer_50.shp", delete_dsn = T)
st_write(sk_lake_buffer_50, "./check_arcGIS/SK/sk_lake_buffer_50.shp", delete_dsn = T)
st_write(ab_lake_buffer_50, "./check_arcGIS/AB/ab_lake_buffer_50.shp", delete_dsn = T)
st_write(bc_lake_buffer_50, "./check_arcGIS/BC/bc_lake_buffer_50.shp", delete_dsn = T)
st_write(mb_lake_buffer_50, "./check_arcGIS/MB/mb_lake_buffer_50.shp", delete_dsn = T)
st_write(nl_lake_buffer_50, "./check_arcGIS/NL/nl_lake_buffer_50.shp", delete_dsn = T)
st_write(nt_lake_buffer_50, "./check_arcGIS/NT/nt_lake_buffer_50.shp", delete_dsn = T)
st_write(nu_lake_buffer_50, "./check_arcGIS/NU/nu_lake_buffer_50.shp", delete_dsn = T)
st_write(on_lake_buffer_50, "./check_arcGIS/ON/on_lake_buffer_50.shp", delete_dsn = T)
st_write(pe_lake_buffer_50, "./check_arcGIS/PE/pe_lake_buffer_50.shp", delete_dsn = T)
st_write(qc_lake_buffer_50, "./check_arcGIS/QC/qc_lake_buffer_50.shp", delete_dsn = T)
st_write(yt_lake_buffer_50, "./check_arcGIS/YT/yt_lake_buffer_50.shp", delete_dsn = T)

#projection - for map purpose - 50m
ns_lake_buffer_50 <- st_transform(ns_lake_buffer_50, crs = "+proj=longlat +datum=WGS84 +no_defs")
ns_fp <- st_transform(ns_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(ns_lake_buffer_50)==st_crs(ns_fp)
sk_lake_buffer_50 <- st_transform(sk_lake_buffer_50, crs = "+proj=longlat +datum=WGS84 +no_defs")
sk_fp <- st_transform(sk_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(sk_lake_buffer_50)==st_crs(sk_fp)
ab_lake_buffer_50 <- st_transform(ab_lake_buffer_50, crs = "+proj=longlat +datum=WGS84 +no_defs")
ab_fp <- st_transform(ab_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(ab_lake_buffer_50)==st_crs(ab_fp)
bc_lake_buffer_50 <- st_transform(bc_lake_buffer_50, crs = "+proj=longlat +datum=WGS84 +no_defs")
bc_fp <- st_transform(bc_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(bc_lake_buffer_50)==st_crs(bc_fp)
mb_lake_buffer_50 <- st_transform(mb_lake_buffer_50, crs = "+proj=longlat +datum=WGS84 +no_defs")
mb_fp <- st_transform(mb_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(mb_lake_buffer_50)==st_crs(mb_fp)
nl_lake_buffer_50 <- st_transform(nl_lake_buffer_50, crs = "+proj=longlat +datum=WGS84 +no_defs")
nl_fp <- st_transform(nl_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(nl_lake_buffer_50)==st_crs(nl_fp)
nt_lake_buffer_50 <- st_transform(nt_lake_buffer_50, crs = "+proj=longlat +datum=WGS84 +no_defs")
nt_fp <- st_transform(nt_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(nt_lake_buffer_50)==st_crs(nt_fp)
nu_lake_buffer_50 <- st_transform(nu_lake_buffer_50, crs = "+proj=longlat +datum=WGS84 +no_defs")
nu_fp <- st_transform(nu_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(nu_lake_buffer_50)==st_crs(nu_fp)
on_lake_buffer_50 <- st_transform(on_lake_buffer_50, crs = "+proj=longlat +datum=WGS84 +no_defs")
on_fp <- st_transform(on_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(on_lake_buffer_50)==st_crs(on_fp)
pe_lake_buffer_50 <- st_transform(pe_lake_buffer_50, crs = "+proj=longlat +datum=WGS84 +no_defs")
pe_fp <- st_transform(pe_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(pe_lake_buffer_50)==st_crs(pe_fp)
qc_lake_buffer_50 <- st_transform(qc_lake_buffer_50, crs = "+proj=longlat +datum=WGS84 +no_defs")
qc_fp <- st_transform(qc_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(qc_lake_buffer_50)==st_crs(qc_fp)
yt_lake_buffer_50 <- st_transform(yt_lake_buffer_50, crs = "+proj=longlat +datum=WGS84 +no_defs")
yt_fp <- st_transform(yt_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(yt_lake_buffer_50)==st_crs(yt_fp)

# filter building footprint data to make sure single houses (remove other type of buildings)
# based on polygon area
# calculate polygon area
# 50m
sf_use_s2(FALSE)
ns_fp$area <- st_area(ns_fp) 
ns_fp$area<- as.numeric(ns_fp$area, units="m")
sk_fp$area <- st_area(sk_fp) 
sk_fp$area<- as.numeric(sk_fp$area, units="m")
ab_fp$area <- st_area(ab_fp) 
ab_fp$area<- as.numeric(ab_fp$area, units="m")
bc_fp$area <- st_area(bc_fp) 
bc_fp$area<- as.numeric(bc_fp$area, units="m")
mb_fp$area <- st_area(mb_fp) 
mb_fp$area<- as.numeric(mb_fp$area, units="m")
nl_fp$area <- st_area(nl_fp) 
nl_fp$area<- as.numeric(nl_fp$area, units="m")
nt_fp$area <- st_area(nt_fp) 
nt_fp$area<- as.numeric(nt_fp$area, units="m")
nu_fp$area <- st_area(nu_fp) 
nu_fp$area<- as.numeric(nu_fp$area, units="m")
on_fp$area <- st_area(on_fp) 
on_fp$area<- as.numeric(on_fp$area, units="m")
pe_fp$area <- st_area(pe_fp) 
pe_fp$area<- as.numeric(pe_fp$area, units="m")
qc_fp$area <- st_area(qc_fp) 
qc_fp$area<- as.numeric(qc_fp$area, units="m")
yt_fp$area <- st_area(yt_fp) 
yt_fp$area<- as.numeric(yt_fp$area, units="m")

# filter individual houses based on area
ns_fp <- ns_fp%>%
  filter(area < individual_houses_ub)
ns_fp <- ns_fp%>%
  filter(area > individual_houses_lb)
sk_fp <- sk_fp%>%
  filter(area < individual_houses_ub)
sk_fp <- sk_fp%>%
  filter(area > individual_houses_lb)
ab_fp <- ab_fp%>%
  filter(area < individual_houses_ub)
ab_fp <- ab_fp%>%
  filter(area > individual_houses_lb)
bc_fp <- bc_fp%>%
  filter(area < individual_houses_ub)
bc_fp <- bc_fp%>%
  filter(area > individual_houses_lb)
mb_fp <- mb_fp%>%
  filter(area < individual_houses_ub)
mb_fp <- mb_fp%>%
  filter(area > individual_houses_lb)
nl_fp <- nl_fp%>%
  filter(area < individual_houses_ub)
nl_fp <- nl_fp%>%
  filter(area > individual_houses_lb)
nt_fp <- nt_fp%>%
  filter(area < individual_houses_ub)
nt_fp <- nt_fp%>%
  filter(area > individual_houses_lb)
nu_fp <- nu_fp%>%
  filter(area < individual_houses_ub)
nu_fp <- nu_fp%>%
  filter(area > individual_houses_lb)
on_fp <- on_fp%>%
  filter(area < individual_houses_ub)
on_fp <- on_fp%>%
  filter(area > individual_houses_lb)
pe_fp <- pe_fp%>%
  filter(area < individual_houses_ub)
pe_fp <- pe_fp%>%
  filter(area > individual_houses_lb)
qc_fp <- qc_fp%>%
  filter(area < individual_houses_ub)
qc_fp <- qc_fp%>%
  filter(area > individual_houses_lb)
yt_fp <- yt_fp%>%
  filter(area < individual_houses_ub)
yt_fp <- yt_fp%>%
  filter(area > individual_houses_lb)

# clip - building foot prints within buffer - waterfront (50m)
ns_lake_buffer_50_epsg <- st_transform(ns_lake_buffer_50, crs = 3348)
sk_lake_buffer_50_epsg <- st_transform(sk_lake_buffer_50, crs = 3348)
ab_lake_buffer_50_epsg <- st_transform(ab_lake_buffer_50, crs = 3348)
bc_lake_buffer_50_epsg <- st_transform(bc_lake_buffer_50, crs = 3348)
mb_lake_buffer_50_epsg <- st_transform(mb_lake_buffer_50, crs = 3348)
nl_lake_buffer_50_epsg <- st_transform(nl_lake_buffer_50, crs = 3348)
nt_lake_buffer_50_epsg <- st_transform(nt_lake_buffer_50, crs = 3348)
nu_lake_buffer_50_epsg <- st_transform(nu_lake_buffer_50, crs = 3348)
on_lake_buffer_50_epsg <- st_transform(on_lake_buffer_50, crs = 3348)
pe_lake_buffer_50_epsg <- st_transform(pe_lake_buffer_50, crs = 3348)
qc_lake_buffer_50_epsg <- st_transform(qc_lake_buffer_50, crs = 3348)
yt_lake_buffer_50_epsg <- st_transform(yt_lake_buffer_50, crs = 3348)

# projection
ns_fp_epsg <- st_transform(ns_fp, crs = 3348)
sk_fp_epsg <- st_transform(sk_fp, crs = 3348)
ab_fp_epsg <- st_transform(ab_fp, crs = 3348)
bc_fp_epsg <- st_transform(bc_fp, crs = 3348)
mb_fp_epsg <- st_transform(mb_fp, crs = 3348)
nl_fp_epsg <- st_transform(nl_fp, crs = 3348)
nt_fp_epsg <- st_transform(nt_fp, crs = 3348)
nu_fp_epsg <- st_transform(nu_fp, crs = 3348)
on_fp_epsg <- st_transform(on_fp, crs = 3348)
pe_fp_epsg <- st_transform(pe_fp, crs = 3348)
qc_fp_epsg <- st_transform(qc_fp, crs = 3348)
yt_fp_epsg <- st_transform(yt_fp, crs = 3348)


# This data layer consist with lake id and adjacent
ns_lake_home_50 <- st_intersection(ns_lake_buffer_50_epsg, ns_fp_epsg)
sk_lake_home_50 <- st_intersection(sk_lake_buffer_50_epsg, sk_fp_epsg)
ab_lake_home_50 <- st_intersection(ab_lake_buffer_50_epsg, ab_fp_epsg)
bc_lake_home_50 <- st_intersection(bc_lake_buffer_50_epsg, bc_fp_epsg)
mb_lake_home_50 <- st_intersection(mb_lake_buffer_50_epsg, mb_fp_epsg)
nl_lake_home_50 <- st_intersection(nl_lake_buffer_50_epsg, nl_fp_epsg)
nt_lake_home_50 <- st_intersection(nt_lake_buffer_50_epsg, nt_fp_epsg)
nu_lake_home_50 <- st_intersection(nu_lake_buffer_50_epsg, nu_fp_epsg)
on_lake_home_50 <- st_intersection(on_lake_buffer_50_epsg, on_fp_epsg)
pe_lake_home_50 <- st_intersection(pe_lake_buffer_50_epsg, pe_fp_epsg)
qc_lake_home_50 <- st_intersection(qc_lake_buffer_50_epsg, qc_fp_epsg)
yt_lake_home_50 <- st_intersection(yt_lake_buffer_50_epsg, yt_fp_epsg)


# write shape file
st_write(ns_lake_home_50, "./check_arcGIS/NS/ns_lake_home_50.shp", delete_dsn = T)
st_write(sk_lake_home_50, "./check_arcGIS/SK/sk_lake_home_50.shp", delete_dsn = T)
st_write(ab_lake_home_50, "./check_arcGIS/AB/ab_lake_home_50.shp", delete_dsn = T)
st_write(bc_lake_home_50, "./check_arcGIS/BC/bc_lake_home_50.shp", delete_dsn = T)
st_write(mb_lake_home_50, "./check_arcGIS/MB/mb_lake_home_50.shp", delete_dsn = T)
st_write(nl_lake_home_50, "./check_arcGIS/NL/nl_lake_home_50.shp", delete_dsn = T)
st_write(nt_lake_home_50, "./check_arcGIS/NT/nt_lake_home_50.shp", delete_dsn = T)
st_write(nu_lake_home_50, "./check_arcGIS/NU/nu_lake_home_50.shp", delete_dsn = T)
st_write(on_lake_home_50, "./check_arcGIS/ON/on_lake_home_50.shp", delete_dsn = T)
st_write(pe_lake_home_50, "./check_arcGIS/PE/pe_lake_home_50.shp", delete_dsn = T)
st_write(qc_lake_home_50, "./check_arcGIS/QC/qc_lake_home_50.shp", delete_dsn = T)
st_write(yt_lake_home_50, "./check_arcGIS/YT/yt_lake_home_50.shp", delete_dsn = T)


# merge census DA data and building foot print data within 50 buffer
# projection 
ns_lake_home_50_espg <- st_transform(ns_lake_home_50, crs = 3348)
ns_census_da_espg <- st_transform(ns_census_da, crs = 3348)
ns_census_da_bfp_50 <- st_join(ns_census_da_espg,ns_lake_home_50_espg, all = TRUE )
sk_lake_home_50_espg <- st_transform(sk_lake_home_50, crs = 3348)
sk_census_da_espg <- st_transform(sk_census_da, crs = 3348)
sk_census_da_bfp_50 <- st_join(sk_census_da_espg,sk_lake_home_50_espg, all = TRUE )
ab_lake_home_50_espg <- st_transform(ab_lake_home_50, crs = 3348)
ab_census_da_espg <- st_transform(ab_census_da, crs = 3348)
ab_census_da_bfp_50 <- st_join(ab_census_da_espg,ab_lake_home_50_espg, all = TRUE )
bc_lake_home_50_espg <- st_transform(bc_lake_home_50, crs = 3348)
bc_census_da_espg <- st_transform(bc_census_da, crs = 3348)
bc_census_da_bfp_50 <- st_join(bc_census_da_espg,bc_lake_home_50_espg, all = TRUE )
mb_lake_home_50_espg <- st_transform(mb_lake_home_50, crs = 3348)
mb_census_da_espg <- st_transform(mb_census_da, crs = 3348)
mb_census_da_bfp_50 <- st_join(mb_census_da_espg,mb_lake_home_50_espg, all = TRUE )
nl_lake_home_50_espg <- st_transform(nl_lake_home_50, crs = 3348)
nl_census_da_espg <- st_transform(nl_census_da, crs = 3348)
nl_census_da_bfp_50 <- st_join(nl_census_da_espg,nl_lake_home_50_espg, all = TRUE )
nt_lake_home_50_espg <- st_transform(nt_lake_home_50, crs = 3348)
nt_census_da_espg <- st_transform(nt_census_da, crs = 3348)
nt_census_da_bfp_50 <- st_join(nt_census_da_espg,nt_lake_home_50_espg, all = TRUE )
nu_lake_home_50_espg <- st_transform(nu_lake_home_50, crs = 3348)
nu_census_da_espg <- st_transform(nu_census_da, crs = 3348)
nu_census_da_bfp_50 <- st_join(nu_census_da_espg,nu_lake_home_50_espg, all = TRUE )
on_lake_home_50_espg <- st_transform(on_lake_home_50, crs = 3348)
on_census_da_espg <- st_transform(on_census_da, crs = 3348)
on_census_da_bfp_50 <- st_join(on_census_da_espg,on_lake_home_50_espg, all = TRUE )
pe_lake_home_50_espg <- st_transform(pe_lake_home_50, crs = 3348)
pe_census_da_espg <- st_transform(pe_census_da, crs = 3348)
pe_census_da_bfp_50 <- st_join(pe_census_da_espg,pe_lake_home_50_espg, all = TRUE )
qc_lake_home_50_espg <- st_transform(qc_lake_home_50, crs = 3348)
qc_census_da_espg <- st_transform(qc_census_da, crs = 3348)
qc_census_da_bfp_50 <- st_join(qc_census_da_espg,qc_lake_home_50_espg, all = TRUE )
yt_lake_home_50_espg <- st_transform(yt_lake_home_50, crs = 3348)
yt_census_da_espg <- st_transform(yt_census_da, crs = 3348)
yt_census_da_bfp_50 <- st_join(yt_census_da_espg,yt_lake_home_50_espg, all = TRUE )

# write shape file
st_write(ns_census_da_bfp_50, "./check_arcGIS/NS/ns_census_da_bfp_50.shp", delete_dsn = T)
st_write(sk_census_da_bfp_50, "./check_arcGIS/SK/sk_census_da_bfp_50.shp", delete_dsn = T)
st_write(ab_census_da_bfp_50, "./check_arcGIS/AB/ab_census_da_bfp_50.shp", delete_dsn = T)
st_write(bc_census_da_bfp_50, "./check_arcGIS/BC/bc_census_da_bfp_50.shp", delete_dsn = T)
st_write(mb_census_da_bfp_50, "./check_arcGIS/MB/mb_census_da_bfp_50.shp", delete_dsn = T)
st_write(nl_census_da_bfp_50, "./check_arcGIS/NL/nl_census_da_bfp_50.shp", delete_dsn = T)
st_write(nt_census_da_bfp_50, "./check_arcGIS/NT/nt_census_da_bfp_50.shp", delete_dsn = T)
st_write(nu_census_da_bfp_50, "./check_arcGIS/NU/nu_census_da_bfp_50.shp", delete_dsn = T)
st_write(on_census_da_bfp_50, "./check_arcGIS/ON/on_census_da_bfp_50.shp", delete_dsn = T)
st_write(pe_census_da_bfp_50, "./check_arcGIS/PE/pe_census_da_bfp_50.shp", delete_dsn = T)
st_write(qc_census_da_bfp_50, "./check_arcGIS/QC/qc_census_da_bfp_50.shp", delete_dsn = T)
st_write(yt_census_da_bfp_50, "./check_arcGIS/YT/yt_census_da_bfp_50.shp", delete_dsn = T)


# remove GeoUID where FID = 0, to make sure that GeoUID with zero building foot print does not count

ns_census_da_bfp_50 <- ns_census_da_bfp_50%>%
  subset(FID > 0)
sk_census_da_bfp_50 <- sk_census_da_bfp_50%>%
  subset(FID > 0)
ab_census_da_bfp_50 <- ab_census_da_bfp_50%>%
  subset(FID > 0)
bc_census_da_bfp_50 <- bc_census_da_bfp_50%>%
  subset(FID > 0)
mb_census_da_bfp_50 <- mb_census_da_bfp_50%>%
  subset(FID > 0)
nl_census_da_bfp_50 <- nl_census_da_bfp_50%>%
  subset(FID > 0)
nt_census_da_bfp_50 <- nt_census_da_bfp_50%>%
  subset(FID > 0)
nu_census_da_bfp_50 <- nu_census_da_bfp_50%>%
  subset(FID > 0)
on_census_da_bfp_50 <- on_census_da_bfp_50%>%
  subset(FID > 0)
pe_census_da_bfp_50 <- pe_census_da_bfp_50%>%
  subset(FID > 0)
qc_census_da_bfp_50 <- qc_census_da_bfp_50%>%
  subset(FID > 0)
yt_census_da_bfp_50 <- yt_census_da_bfp_50%>%
  subset(FID > 0)

# count number of houses within buffer
ns_cen_da_bfp_count_50 <- ns_census_da_bfp_50%>%
  count(GeoUID)
sk_cen_da_bfp_count_50 <- sk_census_da_bfp_50%>%
  count(GeoUID)
ab_cen_da_bfp_count_50 <- ab_census_da_bfp_50%>%
  count(GeoUID)
bc_cen_da_bfp_count_50 <- bc_census_da_bfp_50%>%
  count(GeoUID)
mb_cen_da_bfp_count_50 <- mb_census_da_bfp_50%>%
  count(GeoUID)
nl_cen_da_bfp_count_50 <- nl_census_da_bfp_50%>%
  count(GeoUID)
nt_cen_da_bfp_count_50 <- nt_census_da_bfp_50%>%
  count(GeoUID)
nu_cen_da_bfp_count_50 <- nu_census_da_bfp_50%>%
  count(GeoUID)
on_cen_da_bfp_count_50 <- on_census_da_bfp_50%>%
  count(GeoUID)
pe_cen_da_bfp_count_50 <- pe_census_da_bfp_50%>%
  count(GeoUID)
qc_cen_da_bfp_count_50 <- qc_census_da_bfp_50%>%
  count(GeoUID)
yt_cen_da_bfp_count_50 <- yt_census_da_bfp_50%>%
  count(GeoUID)

# join census data
ns_cen_da_bfp_count_50 <-ns_cen_da_bfp_count_50%>%
  as.data.frame()%>%
  select(-geometry)
sk_cen_da_bfp_count_50 <-sk_cen_da_bfp_count_50%>%
  as.data.frame()%>%
  select(-geometry)
ab_cen_da_bfp_count_50 <-ab_cen_da_bfp_count_50%>%
  as.data.frame()%>%
  select(-geometry)
bc_cen_da_bfp_count_50 <-bc_cen_da_bfp_count_50%>%
  as.data.frame()%>%
  select(-geometry)
mb_cen_da_bfp_count_50 <-mb_cen_da_bfp_count_50%>%
  as.data.frame()%>%
  select(-geometry)
nl_cen_da_bfp_count_50 <-nl_cen_da_bfp_count_50%>%
  as.data.frame()%>%
  select(-geometry)
nt_cen_da_bfp_count_50 <-nt_cen_da_bfp_count_50%>%
  as.data.frame()%>%
  select(-geometry)
nu_cen_da_bfp_count_50 <-nu_cen_da_bfp_count_50%>%
  as.data.frame()%>%
  select(-geometry)
on_cen_da_bfp_count_50 <-on_cen_da_bfp_count_50%>%
  as.data.frame()%>%
  select(-geometry)
pe_cen_da_bfp_count_50 <-pe_cen_da_bfp_count_50%>%
  as.data.frame()%>%
  select(-geometry)
qc_cen_da_bfp_count_50 <-qc_cen_da_bfp_count_50%>%
  as.data.frame()%>%
  select(-geometry)
yt_cen_da_bfp_count_50 <-yt_cen_da_bfp_count_50%>%
  as.data.frame()%>%
  select(-geometry)

ns_cen_da_bfp_count_50 <- left_join(ns_census_da_espg,ns_cen_da_bfp_count_50,by = "GeoUID")
sk_cen_da_bfp_count_50 <- left_join(sk_census_da_espg,sk_cen_da_bfp_count_50,by = "GeoUID")
ab_cen_da_bfp_count_50 <- left_join(ab_census_da_espg,ab_cen_da_bfp_count_50,by = "GeoUID")
bc_cen_da_bfp_count_50 <- left_join(bc_census_da_espg,bc_cen_da_bfp_count_50,by = "GeoUID")
mb_cen_da_bfp_count_50 <- left_join(mb_census_da_espg,mb_cen_da_bfp_count_50,by = "GeoUID")
nl_cen_da_bfp_count_50 <- left_join(nl_census_da_espg,nl_cen_da_bfp_count_50,by = "GeoUID")
nt_cen_da_bfp_count_50 <- left_join(nt_census_da_espg,nt_cen_da_bfp_count_50,by = "GeoUID")
nu_cen_da_bfp_count_50 <- left_join(nu_census_da_espg,nu_cen_da_bfp_count_50,by = "GeoUID")
on_cen_da_bfp_count_50 <- left_join(on_census_da_espg,on_cen_da_bfp_count_50,by = "GeoUID")
pe_cen_da_bfp_count_50 <- left_join(pe_census_da_espg,pe_cen_da_bfp_count_50,by = "GeoUID")
qc_cen_da_bfp_count_50 <- left_join(qc_census_da_espg,qc_cen_da_bfp_count_50,by = "GeoUID")
yt_cen_da_bfp_count_50 <- left_join(yt_census_da_espg,yt_cen_da_bfp_count_50,by = "GeoUID")


# write file
st_write(ns_cen_da_bfp_count_50, "./check_arcGIS/NS/ns_census_da_bfp_count_50.shp", delete_dsn = T)
st_write(sk_cen_da_bfp_count_50, "./check_arcGIS/SK/sk_census_da_bfp_count_50.shp", delete_dsn = T)
st_write(ab_cen_da_bfp_count_50, "./check_arcGIS/AB/ab_census_da_bfp_count_50.shp", delete_dsn = T)
st_write(bc_cen_da_bfp_count_50, "./check_arcGIS/BC/bc_census_da_bfp_count_50.shp", delete_dsn = T)
st_write(mb_cen_da_bfp_count_50, "./check_arcGIS/MB/mb_census_da_bfp_count_50.shp", delete_dsn = T)
st_write(nl_cen_da_bfp_count_50, "./check_arcGIS/NL/nl_census_da_bfp_count_50.shp", delete_dsn = T)
st_write(nt_cen_da_bfp_count_50, "./check_arcGIS/NT/nt_census_da_bfp_count_50.shp", delete_dsn = T)
st_write(nu_cen_da_bfp_count_50, "./check_arcGIS/NU/nu_census_da_bfp_count_50.shp", delete_dsn = T)
st_write(on_cen_da_bfp_count_50, "./check_arcGIS/ON/on_census_da_bfp_count_50.shp", delete_dsn = T)
st_write(pe_cen_da_bfp_count_50, "./check_arcGIS/PE/pe_census_da_bfp_count_50.shp", delete_dsn = T)
st_write(qc_cen_da_bfp_count_50, "./check_arcGIS/QC/qc_census_da_bfp_count_50.shp", delete_dsn = T)
st_write(yt_cen_da_bfp_count_50, "./check_arcGIS/YT/yt_census_da_bfp_count_50.shp", delete_dsn = T)





# accounting for non-waterfront homes (homes located between the distance between 50m and 500m)

# creating buffer - meters (projection ensure this)
ns_lake_buffer_500 <- st_buffer(ns_lakes_epsg, dist = nonwater_front)
sk_lake_buffer_500 <- st_buffer(sk_lakes_epsg, dist = nonwater_front)
ab_lake_buffer_500 <- st_buffer(ab_lakes_epsg, dist = nonwater_front)
bc_lake_buffer_500 <- st_buffer(bc_lakes_epsg, dist = nonwater_front)
mb_lake_buffer_500 <- st_buffer(mb_lakes_epsg, dist = nonwater_front)
nl_lake_buffer_500 <- st_buffer(nl_lakes_epsg, dist = nonwater_front)
nt_lake_buffer_500 <- st_buffer(nt_lakes_epsg, dist = nonwater_front)
nu_lake_buffer_500 <- st_buffer(nu_lakes_epsg, dist = nonwater_front)
on_lake_buffer_500 <- st_buffer(on_lakes_epsg, dist = nonwater_front)
pe_lake_buffer_500 <- st_buffer(pe_lakes_epsg, dist = nonwater_front)
qc_lake_buffer_500 <- st_buffer(qc_lakes_epsg, dist = nonwater_front)
yt_lake_buffer_500 <- st_buffer(yt_lakes_epsg, dist = nonwater_front)


# write shape file
st_write(ns_lake_buffer_500, "./check_arcGIS/NS/ns_lake_buffer_500.shp", delete_dsn = T)
st_write(sk_lake_buffer_500, "./check_arcGIS/SK/sk_lake_buffer_500.shp", delete_dsn = T)
st_write(ab_lake_buffer_500, "./check_arcGIS/AB/ab_lake_buffer_500.shp", delete_dsn = T)
st_write(bc_lake_buffer_500, "./check_arcGIS/BC/bc_lake_buffer_500.shp", delete_dsn = T)
st_write(mb_lake_buffer_500, "./check_arcGIS/MB/mb_lake_buffer_500.shp", delete_dsn = T)
st_write(nl_lake_buffer_500, "./check_arcGIS/NL/nl_lake_buffer_500.shp", delete_dsn = T)
st_write(nt_lake_buffer_500, "./check_arcGIS/NT/nt_lake_buffer_500.shp", delete_dsn = T)
st_write(nu_lake_buffer_500, "./check_arcGIS/NU/nu_lake_buffer_500.shp", delete_dsn = T)
st_write(on_lake_buffer_500, "./check_arcGIS/ON/on_lake_buffer_500.shp", delete_dsn = T)
st_write(pe_lake_buffer_500, "./check_arcGIS/PE/pe_lake_buffer_500.shp", delete_dsn = T)
st_write(qc_lake_buffer_500, "./check_arcGIS/QC/qc_lake_buffer_500.shp", delete_dsn = T)
st_write(yt_lake_buffer_500, "./check_arcGIS/YT/yt_lake_buffer_500.shp", delete_dsn = T)


#projection - for map purpose
ns_lake_buffer_500 <- st_transform(ns_lake_buffer_500, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(ns_lake_buffer_500)==st_crs(ns_fp)
sk_lake_buffer_500 <- st_transform(sk_lake_buffer_500, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(sk_lake_buffer_500)==st_crs(sk_fp)
ab_lake_buffer_500 <- st_transform(ab_lake_buffer_500, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(ab_lake_buffer_500)==st_crs(ab_fp)
bc_lake_buffer_500 <- st_transform(bc_lake_buffer_500, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(bc_lake_buffer_500)==st_crs(bc_fp)
mb_lake_buffer_500 <- st_transform(mb_lake_buffer_500, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(mb_lake_buffer_500)==st_crs(mb_fp)
nl_lake_buffer_500 <- st_transform(nl_lake_buffer_500, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(nl_lake_buffer_500)==st_crs(nl_fp)
nt_lake_buffer_500 <- st_transform(nt_lake_buffer_500, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(nt_lake_buffer_500)==st_crs(nt_fp)
nu_lake_buffer_500 <- st_transform(nu_lake_buffer_500, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(nu_lake_buffer_500)==st_crs(nu_fp)
on_lake_buffer_500 <- st_transform(on_lake_buffer_500, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(on_lake_buffer_500)==st_crs(on_fp)
pe_lake_buffer_500 <- st_transform(pe_lake_buffer_500, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(pe_lake_buffer_500)==st_crs(pe_fp)
qc_lake_buffer_500 <- st_transform(qc_lake_buffer_500, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(qc_lake_buffer_500)==st_crs(qc_fp)
yt_lake_buffer_500 <- st_transform(yt_lake_buffer_500, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(yt_lake_buffer_500)==st_crs(yt_fp)

# clip - building foot prints within buffer - non-waterfront (500m)

ns_lake_buffer_500_epsg <- st_transform(ns_lake_buffer_500, crs = 3348)
sk_lake_buffer_500_epsg <- st_transform(sk_lake_buffer_500, crs = 3348)
ab_lake_buffer_500_epsg <- st_transform(ab_lake_buffer_500, crs = 3348)
bc_lake_buffer_500_epsg <- st_transform(bc_lake_buffer_500, crs = 3348)
mb_lake_buffer_500_epsg <- st_transform(mb_lake_buffer_500, crs = 3348)
nl_lake_buffer_500_epsg <- st_transform(nl_lake_buffer_500, crs = 3348)
nt_lake_buffer_500_epsg <- st_transform(nt_lake_buffer_500, crs = 3348)
nu_lake_buffer_500_epsg <- st_transform(nu_lake_buffer_500, crs = 3348)
on_lake_buffer_500_epsg <- st_transform(on_lake_buffer_500, crs = 3348)
pe_lake_buffer_500_epsg <- st_transform(pe_lake_buffer_500, crs = 3348)
qc_lake_buffer_500_epsg <- st_transform(qc_lake_buffer_500, crs = 3348)
yt_lake_buffer_500_epsg <- st_transform(yt_lake_buffer_500, crs = 3348)


# This data layer consist with lake id and adjacent
ns_lake_home_500 <- st_intersection(ns_lake_buffer_500_epsg, ns_fp_epsg)
sk_lake_home_500 <- st_intersection(sk_lake_buffer_500_epsg, sk_fp_epsg)
ab_lake_home_500 <- st_intersection(ab_lake_buffer_500_epsg, ab_fp_epsg)
bc_lake_home_500 <- st_intersection(bc_lake_buffer_500_epsg, bc_fp_epsg)
mb_lake_home_500 <- st_intersection(mb_lake_buffer_500_epsg, mb_fp_epsg)
nl_lake_home_500 <- st_intersection(nl_lake_buffer_500_epsg, nl_fp_epsg)
nt_lake_home_500 <- st_intersection(nt_lake_buffer_500_epsg, nt_fp_epsg)
nu_lake_home_500 <- st_intersection(nu_lake_buffer_500_epsg, nu_fp_epsg)
on_lake_home_500 <- st_intersection(on_lake_buffer_500_epsg, on_fp_epsg)
pe_lake_home_500 <- st_intersection(pe_lake_buffer_500_epsg, pe_fp_epsg)
qc_lake_home_500 <- st_intersection(qc_lake_buffer_500_epsg, qc_fp_epsg)
yt_lake_home_500 <- st_intersection(yt_lake_buffer_500_epsg, yt_fp_epsg)



# The issue with shape file of ns_lake_home_500 has the houses that are also within
# the buffer of ns_lake_home_250 shape file. Thus in order to avoid double counting 
# will detect the houses within 250m buffer and remove it from 500m buffer

ns_lake_home_500 <- ns_lake_home_500 %>%
  # make new dup_record column
  mutate(dup_record = case_when(
    # if record is in obs_keep data frame
    FID %in% ns_lake_home_50$FID ~ "Ignore", 
    # all else marked as "For analysis" for analysis purposes
    TRUE                            ~ "For analysis"))
sk_lake_home_500 <- sk_lake_home_500 %>%
  # make new dup_record column
  mutate(dup_record = case_when(
    # if record is in obs_keep data frame
    FID %in% sk_lake_home_50$FID ~ "Ignore", 
    # all else marked as "For analysis" for analysis purposes
    TRUE                            ~ "For analysis"))
ab_lake_home_500 <- ab_lake_home_500 %>%
  # make new dup_record column
  mutate(dup_record = case_when(
    # if record is in obs_keep data frame
    FID %in% ab_lake_home_50$FID ~ "Ignore", 
    # all else marked as "For analysis" for analysis purposes
    TRUE                            ~ "For analysis"))
bc_lake_home_500 <- bc_lake_home_500 %>%
  # make new dup_record column
  mutate(dup_record = case_when(
    # if record is in obs_keep data frame
    FID %in% bc_lake_home_50$FID ~ "Ignore", 
    # all else marked as "For analysis" for analysis purposes
    TRUE                            ~ "For analysis"))
mb_lake_home_500 <- mb_lake_home_500 %>%
  # make new dup_record column
  mutate(dup_record = case_when(
    # if record is in obs_keep data frame
    FID %in% mb_lake_home_50$FID ~ "Ignore", 
    # all else marked as "For analysis" for analysis purposes
    TRUE                            ~ "For analysis"))
nl_lake_home_500 <- nl_lake_home_500 %>%
  # make new dup_record column
  mutate(dup_record = case_when(
    # if record is in obs_keep data frame
    FID %in% nl_lake_home_50$FID ~ "Ignore", 
    # all else marked as "For analysis" for analysis purposes
    TRUE                            ~ "For analysis"))
nt_lake_home_500 <- nt_lake_home_500 %>%
  # make new dup_record column
  mutate(dup_record = case_when(
    # if record is in obs_keep data frame
    FID %in% nt_lake_home_50$FID ~ "Ignore", 
    # all else marked as "For analysis" for analysis purposes
    TRUE                            ~ "For analysis"))
nu_lake_home_500 <- nu_lake_home_500 %>%
  # make new dup_record column
  mutate(dup_record = case_when(
    # if record is in obs_keep data frame
    FID %in% nu_lake_home_50$FID ~ "Ignore", 
    # all else marked as "For analysis" for analysis purposes
    TRUE                            ~ "For analysis"))
on_lake_home_500 <- on_lake_home_500 %>%
  # make new dup_record column
  mutate(dup_record = case_when(
    # if record is in obs_keep data frame
    FID %in% on_lake_home_50$FID ~ "Ignore", 
    # all else marked as "For analysis" for analysis purposes
    TRUE                            ~ "For analysis"))
pe_lake_home_500 <- pe_lake_home_500 %>%
  # make new dup_record column
  mutate(dup_record = case_when(
    # if record is in obs_keep data frame
    FID %in% pe_lake_home_50$FID ~ "Ignore", 
    # all else marked as "For analysis" for analysis purposes
    TRUE                            ~ "For analysis"))
qc_lake_home_500 <- qc_lake_home_500 %>%
  # make new dup_record column
  mutate(dup_record = case_when(
    # if record is in obs_keep data frame
    FID %in% qc_lake_home_50$FID ~ "Ignore", 
    # all else marked as "For analysis" for analysis purposes
    TRUE                            ~ "For analysis"))
yt_lake_home_500 <- yt_lake_home_500 %>%
  # make new dup_record column
  mutate(dup_record = case_when(
    # if record is in obs_keep data frame
    FID %in% yt_lake_home_50$FID ~ "Ignore", 
    # all else marked as "For analysis" for analysis purposes
    TRUE                            ~ "For analysis"))


# select the subset that ensure houses only within 50m-500m distance

ns_lake_home_500 <- ns_lake_home_500%>%
  subset(dup_record == "For analysis")
sk_lake_home_500 <- sk_lake_home_500%>%
  subset(dup_record == "For analysis")
ab_lake_home_500 <- ab_lake_home_500%>%
  subset(dup_record == "For analysis")
bc_lake_home_500 <- bc_lake_home_500%>%
  subset(dup_record == "For analysis")
mb_lake_home_500 <- mb_lake_home_500%>%
  subset(dup_record == "For analysis")
nl_lake_home_500 <- nl_lake_home_500%>%
  subset(dup_record == "For analysis")
nt_lake_home_500 <- nt_lake_home_500%>%
  subset(dup_record == "For analysis")
nu_lake_home_500 <- nu_lake_home_500%>%
  subset(dup_record == "For analysis")
on_lake_home_500 <- on_lake_home_500%>%
  subset(dup_record == "For analysis")
pe_lake_home_500 <- pe_lake_home_500%>%
  subset(dup_record == "For analysis")
qc_lake_home_500 <- qc_lake_home_500%>%
  subset(dup_record == "For analysis")
yt_lake_home_500 <- yt_lake_home_500%>%
  subset(dup_record == "For analysis")



# write shape file
st_write(ns_lake_home_500, "./check_arcGIS/NS/ns_lake_home_500.shp", delete_dsn = T)
st_write(sk_lake_home_500, "./check_arcGIS/SK/sk_lake_home_500.shp", delete_dsn = T)
st_write(ab_lake_home_500, "./check_arcGIS/AB/ab_lake_home_500.shp", delete_dsn = T)
st_write(bc_lake_home_500, "./check_arcGIS/BC/bc_lake_home_500.shp", delete_dsn = T)
st_write(mb_lake_home_500, "./check_arcGIS/MB/mb_lake_home_500.shp", delete_dsn = T)
st_write(nl_lake_home_500, "./check_arcGIS/NL/nl_lake_home_500.shp", delete_dsn = T)
st_write(nt_lake_home_500, "./check_arcGIS/NT/nt_lake_home_500.shp", delete_dsn = T)
st_write(nu_lake_home_500, "./check_arcGIS/NU/nu_lake_home_500.shp", delete_dsn = T)
st_write(on_lake_home_500, "./check_arcGIS/ON/on_lake_home_500.shp", delete_dsn = T)
st_write(pe_lake_home_500, "./check_arcGIS/PE/pe_lake_home_500.shp", delete_dsn = T)
st_write(qc_lake_home_500, "./check_arcGIS/QC/qc_lake_home_500.shp", delete_dsn = T)
st_write(yt_lake_home_500, "./check_arcGIS/YT/yt_lake_home_500.shp", delete_dsn = T)




# merge census DA data and building foot print data within 500 buffer
# projection 
ns_lake_home_500_espg <- st_transform(ns_lake_home_500, crs = 3348)
ns_census_da_bfp_500 <- st_join(ns_census_da_espg,ns_lake_home_500_espg, all = TRUE )
sk_lake_home_500_espg <- st_transform(sk_lake_home_500, crs = 3348)
sk_census_da_bfp_500 <- st_join(sk_census_da_espg,sk_lake_home_500_espg, all = TRUE )
ab_lake_home_500_espg <- st_transform(ab_lake_home_500, crs = 3348)
ab_census_da_bfp_500 <- st_join(ab_census_da_espg,ab_lake_home_500_espg, all = TRUE )
bc_lake_home_500_espg <- st_transform(bc_lake_home_500, crs = 3348)
bc_census_da_bfp_500 <- st_join(bc_census_da_espg,bc_lake_home_500_espg, all = TRUE )
mb_lake_home_500_espg <- st_transform(mb_lake_home_500, crs = 3348)
mb_census_da_bfp_500 <- st_join(mb_census_da_espg,mb_lake_home_500_espg, all = TRUE )
nl_lake_home_500_espg <- st_transform(nl_lake_home_500, crs = 3348)
nl_census_da_bfp_500 <- st_join(nl_census_da_espg,nl_lake_home_500_espg, all = TRUE )
nt_lake_home_500_espg <- st_transform(nt_lake_home_500, crs = 3348)
nt_census_da_bfp_500 <- st_join(nt_census_da_espg,nt_lake_home_500_espg, all = TRUE )
nu_lake_home_500_espg <- st_transform(nu_lake_home_500, crs = 3348)
nu_census_da_bfp_500 <- st_join(nu_census_da_espg,nu_lake_home_500_espg, all = TRUE )
on_lake_home_500_espg <- st_transform(on_lake_home_500, crs = 3348)
on_census_da_bfp_500 <- st_join(on_census_da_espg,on_lake_home_500_espg, all = TRUE )
pe_lake_home_500_espg <- st_transform(pe_lake_home_500, crs = 3348)
pe_census_da_bfp_500 <- st_join(pe_census_da_espg,pe_lake_home_500_espg, all = TRUE )
qc_lake_home_500_espg <- st_transform(qc_lake_home_500, crs = 3348)
qc_census_da_bfp_500 <- st_join(qc_census_da_espg,qc_lake_home_500_espg, all = TRUE )
yt_lake_home_500_espg <- st_transform(yt_lake_home_500, crs = 3348)
yt_census_da_bfp_500 <- st_join(yt_census_da_espg,yt_lake_home_500_espg, all = TRUE )


# write shape file
st_write(ns_census_da_bfp_500, "./check_arcGIS/NS/ns_census_da_bfp_500.shp", delete_dsn = T)
st_write(sk_census_da_bfp_500, "./check_arcGIS/SK/sk_census_da_bfp_500.shp", delete_dsn = T)
st_write(ab_census_da_bfp_500, "./check_arcGIS/AB/ab_census_da_bfp_500.shp", delete_dsn = T)
st_write(bc_census_da_bfp_500, "./check_arcGIS/BC/bc_census_da_bfp_500.shp", delete_dsn = T)
st_write(mb_census_da_bfp_500, "./check_arcGIS/MB/mb_census_da_bfp_500.shp", delete_dsn = T)
st_write(nl_census_da_bfp_500, "./check_arcGIS/NL/nl_census_da_bfp_500.shp", delete_dsn = T)
st_write(nt_census_da_bfp_500, "./check_arcGIS/NT/nt_census_da_bfp_500.shp", delete_dsn = T)
st_write(nu_census_da_bfp_500, "./check_arcGIS/NU/nu_census_da_bfp_500.shp", delete_dsn = T)
st_write(on_census_da_bfp_500, "./check_arcGIS/ON/on_census_da_bfp_500.shp", delete_dsn = T)
st_write(pe_census_da_bfp_500, "./check_arcGIS/PE/pe_census_da_bfp_500.shp", delete_dsn = T)
st_write(qc_census_da_bfp_500, "./check_arcGIS/QC/qc_census_da_bfp_500.shp", delete_dsn = T)
st_write(yt_census_da_bfp_500, "./check_arcGIS/YT/yt_census_da_bfp_500.shp", delete_dsn = T)


# remove GeoUID where FID = 0, to make sure that GeoUID with zero building foot print does not count

ns_census_da_bfp_500 <- ns_census_da_bfp_500%>%
  subset(FID > 0)
sk_census_da_bfp_500 <- sk_census_da_bfp_500%>%
  subset(FID > 0)
ab_census_da_bfp_500 <- ab_census_da_bfp_500%>%
  subset(FID > 0)
bc_census_da_bfp_500 <- bc_census_da_bfp_500%>%
  subset(FID > 0)
mb_census_da_bfp_500 <- mb_census_da_bfp_500%>%
  subset(FID > 0)
nl_census_da_bfp_500 <- nl_census_da_bfp_500%>%
  subset(FID > 0)
nt_census_da_bfp_500 <- nt_census_da_bfp_500%>%
  subset(FID > 0)
nu_census_da_bfp_500 <- nu_census_da_bfp_500%>%
  subset(FID > 0)
on_census_da_bfp_500 <- on_census_da_bfp_500%>%
  subset(FID > 0)
pe_census_da_bfp_500 <- pe_census_da_bfp_500%>%
  subset(FID > 0)
qc_census_da_bfp_500 <- qc_census_da_bfp_500%>%
  subset(FID > 0)
yt_census_da_bfp_500 <- yt_census_da_bfp_500%>%
  subset(FID > 0)



# count number of houses within buffer
ns_cen_da_bfp_count_500 <- ns_census_da_bfp_500%>%
  count(GeoUID)
sk_cen_da_bfp_count_500 <- sk_census_da_bfp_500%>%
  count(GeoUID)
ab_cen_da_bfp_count_500 <- ab_census_da_bfp_500%>%
  count(GeoUID)
bc_cen_da_bfp_count_500 <- bc_census_da_bfp_500%>%
  count(GeoUID)
mb_cen_da_bfp_count_500 <- mb_census_da_bfp_500%>%
  count(GeoUID)
nl_cen_da_bfp_count_500 <- nl_census_da_bfp_500%>%
  count(GeoUID)
nt_cen_da_bfp_count_500 <- nt_census_da_bfp_500%>%
  count(GeoUID)
nu_cen_da_bfp_count_500 <- nu_census_da_bfp_500%>%
  count(GeoUID)
on_cen_da_bfp_count_500 <- on_census_da_bfp_500%>%
  count(GeoUID)
pe_cen_da_bfp_count_500 <- pe_census_da_bfp_500%>%
  count(GeoUID)
qc_cen_da_bfp_count_500 <- qc_census_da_bfp_500%>%
  count(GeoUID)
yt_cen_da_bfp_count_500 <- yt_census_da_bfp_500%>%
  count(GeoUID)


# join census data
ns_cen_da_bfp_count_500 <-ns_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(-geometry)
sk_cen_da_bfp_count_500 <-sk_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(-geometry)
ab_cen_da_bfp_count_500 <-ab_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(-geometry)
bc_cen_da_bfp_count_500 <-bc_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(-geometry)
mb_cen_da_bfp_count_500 <-mb_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(-geometry)
nl_cen_da_bfp_count_500 <-nl_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(-geometry)
nt_cen_da_bfp_count_500 <-nt_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(-geometry)
nu_cen_da_bfp_count_500 <-nu_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(-geometry)
on_cen_da_bfp_count_500 <-on_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(-geometry)
pe_cen_da_bfp_count_500 <-pe_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(-geometry)
qc_cen_da_bfp_count_500 <-qc_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(-geometry)
yt_cen_da_bfp_count_500 <-yt_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(-geometry)







ns_cen_da_bfp_count_500 <- left_join(ns_census_da_espg,ns_cen_da_bfp_count_500,by = "GeoUID")
sk_cen_da_bfp_count_500 <- left_join(sk_census_da_espg,sk_cen_da_bfp_count_500,by = "GeoUID")
ab_cen_da_bfp_count_500 <- left_join(ab_census_da_espg,ab_cen_da_bfp_count_500,by = "GeoUID")
bc_cen_da_bfp_count_500 <- left_join(bc_census_da_espg,bc_cen_da_bfp_count_500,by = "GeoUID")
mb_cen_da_bfp_count_500 <- left_join(mb_census_da_espg,mb_cen_da_bfp_count_500,by = "GeoUID")
nl_cen_da_bfp_count_500 <- left_join(nl_census_da_espg,nl_cen_da_bfp_count_500,by = "GeoUID")
nt_cen_da_bfp_count_500 <- left_join(nt_census_da_espg,nt_cen_da_bfp_count_500,by = "GeoUID")
nu_cen_da_bfp_count_500 <- left_join(nu_census_da_espg,nu_cen_da_bfp_count_500,by = "GeoUID")
on_cen_da_bfp_count_500 <- left_join(on_census_da_espg,on_cen_da_bfp_count_500,by = "GeoUID")
pe_cen_da_bfp_count_500 <- left_join(pe_census_da_espg,pe_cen_da_bfp_count_500,by = "GeoUID")
qc_cen_da_bfp_count_500 <- left_join(qc_census_da_espg,qc_cen_da_bfp_count_500,by = "GeoUID")
#yt_cen_da_bfp_count_500 <- left_join(yt_census_da_espg,ns_cen_da_bfp_count_500,by = "GeoUID")


# write file
st_write(ns_cen_da_bfp_count_500, "./check_arcGIS/NS/ns_census_da_bfp_count_500.shp", delete_dsn = T)
st_write(sk_cen_da_bfp_count_500, "./check_arcGIS/SK/sk_census_da_bfp_count_500.shp", delete_dsn = T)
st_write(ab_cen_da_bfp_count_500, "./check_arcGIS/AB/ab_census_da_bfp_count_500.shp", delete_dsn = T)
st_write(bc_cen_da_bfp_count_500, "./check_arcGIS/BC/bc_census_da_bfp_count_500.shp", delete_dsn = T)
st_write(mb_cen_da_bfp_count_500, "./check_arcGIS/MB/mb_census_da_bfp_count_500.shp", delete_dsn = T)
st_write(nl_cen_da_bfp_count_500, "./check_arcGIS/NL/nl_census_da_bfp_count_500.shp", delete_dsn = T)
st_write(nt_cen_da_bfp_count_500, "./check_arcGIS/NT/nt_census_da_bfp_count_500.shp", delete_dsn = T)
st_write(nu_cen_da_bfp_count_500, "./check_arcGIS/NU/nu_census_da_bfp_count_500.shp", delete_dsn = T)
st_write(on_cen_da_bfp_count_500, "./check_arcGIS/ON/on_census_da_bfp_count_500.shp", delete_dsn = T)
st_write(pe_cen_da_bfp_count_500, "./check_arcGIS/PE/pe_census_da_bfp_count_500.shp", delete_dsn = T)
st_write(qc_cen_da_bfp_count_500, "./check_arcGIS/QC/qc_census_da_bfp_count_500.shp", delete_dsn = T)
#st_write(yt_cen_da_bfp_count_500, "./check_arcGIS/YT/yt_census_da_bfp_count_500.shp", delete_dsn = T)


#####################################################################################################
# Additional distance buffer analysis - for sensitivity analysis
####################################################################################################
# creating buffer - meters (projection ensure this) - 250m
ns_lake_buffer_250 <- st_buffer(ns_lakes_epsg, dist = waterfront)
sk_lake_buffer_250 <- st_buffer(sk_lakes_epsg, dist = waterfront)
ab_lake_buffer_250 <- st_buffer(ab_lakes_epsg, dist = waterfront)
bc_lake_buffer_250 <- st_buffer(bc_lakes_epsg, dist = waterfront)
mb_lake_buffer_250 <- st_buffer(mb_lakes_epsg, dist = waterfront)
nl_lake_buffer_250 <- st_buffer(nl_lakes_epsg, dist = waterfront)
nt_lake_buffer_250 <- st_buffer(nt_lakes_epsg, dist = waterfront)
nu_lake_buffer_250 <- st_buffer(nu_lakes_epsg, dist = waterfront)
on_lake_buffer_250 <- st_buffer(on_lakes_epsg, dist = waterfront)
pe_lake_buffer_250 <- st_buffer(pe_lakes_epsg, dist = waterfront)
qc_lake_buffer_250 <- st_buffer(qc_lakes_epsg, dist = waterfront)
yt_lake_buffer_250 <- st_buffer(yt_lakes_epsg, dist = waterfront)

#write shape file - 250m
st_write(ns_lake_buffer_250, "./check_arcGIS/NS/ns_lake_buffer_250.shp", delete_dsn = T)
st_write(sk_lake_buffer_250, "./check_arcGIS/SK/sk_lake_buffer_250.shp", delete_dsn = T)
st_write(ab_lake_buffer_250, "./check_arcGIS/AB/ab_lake_buffer_250.shp", delete_dsn = T)
st_write(bc_lake_buffer_250, "./check_arcGIS/BC/bc_lake_buffer_250.shp", delete_dsn = T)
st_write(mb_lake_buffer_250, "./check_arcGIS/MB/mb_lake_buffer_250.shp", delete_dsn = T)
st_write(nl_lake_buffer_250, "./check_arcGIS/NL/nl_lake_buffer_250.shp", delete_dsn = T)
st_write(nt_lake_buffer_250, "./check_arcGIS/NT/nt_lake_buffer_250.shp", delete_dsn = T)
st_write(nu_lake_buffer_250, "./check_arcGIS/NU/nu_lake_buffer_250.shp", delete_dsn = T)
st_write(on_lake_buffer_250, "./check_arcGIS/ON/on_lake_buffer_250.shp", delete_dsn = T)
st_write(pe_lake_buffer_250, "./check_arcGIS/PE/pe_lake_buffer_250.shp", delete_dsn = T)
st_write(qc_lake_buffer_250, "./check_arcGIS/QC/qc_lake_buffer_250.shp", delete_dsn = T)
st_write(yt_lake_buffer_250, "./check_arcGIS/YT/yt_lake_buffer_250.shp", delete_dsn = T)

#projection - for map purpose
ns_lake_buffer_250 <- st_transform(ns_lake_buffer_250, crs = "+proj=longlat +datum=WGS84 +no_defs")
ns_fp <- st_transform(ns_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(ns_lake_buffer_250)==st_crs(ns_fp)
sk_lake_buffer_250 <- st_transform(sk_lake_buffer_250, crs = "+proj=longlat +datum=WGS84 +no_defs")
sk_fp <- st_transform(sk_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(sk_lake_buffer_250)==st_crs(sk_fp)
ab_lake_buffer_250 <- st_transform(ab_lake_buffer_250, crs = "+proj=longlat +datum=WGS84 +no_defs")
ab_fp <- st_transform(ab_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(ab_lake_buffer_250)==st_crs(ab_fp)
bc_lake_buffer_250 <- st_transform(bc_lake_buffer_250, crs = "+proj=longlat +datum=WGS84 +no_defs")
bc_fp <- st_transform(bc_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(bc_lake_buffer_250)==st_crs(bc_fp)
mb_lake_buffer_250 <- st_transform(mb_lake_buffer_250, crs = "+proj=longlat +datum=WGS84 +no_defs")
mb_fp <- st_transform(mb_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(mb_lake_buffer_250)==st_crs(mb_fp)
nl_lake_buffer_250 <- st_transform(nl_lake_buffer_250, crs = "+proj=longlat +datum=WGS84 +no_defs")
nl_fp <- st_transform(nl_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(nl_lake_buffer_250)==st_crs(nl_fp)
nt_lake_buffer_250 <- st_transform(nt_lake_buffer_250, crs = "+proj=longlat +datum=WGS84 +no_defs")
nt_fp <- st_transform(nt_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(nt_lake_buffer_250)==st_crs(nt_fp)
nu_lake_buffer_250 <- st_transform(nu_lake_buffer_250, crs = "+proj=longlat +datum=WGS84 +no_defs")
nu_fp <- st_transform(nu_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(nu_lake_buffer_250)==st_crs(nu_fp)
on_lake_buffer_250 <- st_transform(on_lake_buffer_250, crs = "+proj=longlat +datum=WGS84 +no_defs")
on_fp <- st_transform(on_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(on_lake_buffer_250)==st_crs(on_fp)
pe_lake_buffer_250 <- st_transform(pe_lake_buffer_250, crs = "+proj=longlat +datum=WGS84 +no_defs")
pe_fp <- st_transform(pe_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(pe_lake_buffer_250)==st_crs(pe_fp)
qc_lake_buffer_250 <- st_transform(qc_lake_buffer_250, crs = "+proj=longlat +datum=WGS84 +no_defs")
qc_fp <- st_transform(qc_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(qc_lake_buffer_250)==st_crs(qc_fp)
yt_lake_buffer_250 <- st_transform(yt_lake_buffer_250, crs = "+proj=longlat +datum=WGS84 +no_defs")
yt_fp <- st_transform(yt_fp, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(yt_lake_buffer_250)==st_crs(yt_fp)


# filter building footprint data to make sure single houses (remove other type of buildings)
# based on polygon area
# calculate polygon area
sf_use_s2(FALSE)
ns_fp$area <- st_area(ns_fp) 
ns_fp$area<- as.numeric(ns_fp$area, units="m")
sk_fp$area <- st_area(sk_fp) 
sk_fp$area<- as.numeric(sk_fp$area, units="m")
ab_fp$area <- st_area(ab_fp) 
ab_fp$area<- as.numeric(ab_fp$area, units="m")
bc_fp$area <- st_area(bc_fp) 
bc_fp$area<- as.numeric(bc_fp$area, units="m")
mb_fp$area <- st_area(mb_fp) 
mb_fp$area<- as.numeric(mb_fp$area, units="m")
nl_fp$area <- st_area(nl_fp) 
nl_fp$area<- as.numeric(nl_fp$area, units="m")
nt_fp$area <- st_area(nt_fp) 
nt_fp$area<- as.numeric(nt_fp$area, units="m")
nu_fp$area <- st_area(nu_fp) 
nu_fp$area<- as.numeric(nu_fp$area, units="m")
on_fp$area <- st_area(on_fp) 
on_fp$area<- as.numeric(on_fp$area, units="m")
pe_fp$area <- st_area(pe_fp) 
pe_fp$area<- as.numeric(pe_fp$area, units="m")
qc_fp$area <- st_area(qc_fp) 
qc_fp$area<- as.numeric(qc_fp$area, units="m")
yt_fp$area <- st_area(yt_fp) 
yt_fp$area<- as.numeric(yt_fp$area, units="m")


# filter individual houses based on area
ns_fp <- ns_fp%>%
  filter(area < individual_houses)
sk_fp <- sk_fp%>%
  filter(area < individual_houses)
ab_fp <- ab_fp%>%
  filter(area < individual_houses)
bc_fp <- bc_fp%>%
  filter(area < individual_houses)
mb_fp <- mb_fp%>%
  filter(area < individual_houses)
nl_fp <- nl_fp%>%
  filter(area < individual_houses)
nt_fp <- nt_fp%>%
  filter(area < individual_houses)
nu_fp <- nu_fp%>%
  filter(area < individual_houses)
on_fp <- on_fp%>%
  filter(area < individual_houses)
pe_fp <- pe_fp%>%
  filter(area < individual_houses)
qc_fp <- qc_fp%>%
  filter(area < individual_houses)
yt_fp <- yt_fp%>%
  filter(area < individual_houses)

# clip - building foot prints within buffer - waterfront (250m)
ns_lake_buffer_250_epsg <- st_transform(ns_lake_buffer_250, crs = 3348)
sk_lake_buffer_250_epsg <- st_transform(sk_lake_buffer_250, crs = 3348)
ab_lake_buffer_250_epsg <- st_transform(ab_lake_buffer_250, crs = 3348)
bc_lake_buffer_250_epsg <- st_transform(bc_lake_buffer_250, crs = 3348)
mb_lake_buffer_250_epsg <- st_transform(mb_lake_buffer_250, crs = 3348)
nl_lake_buffer_250_epsg <- st_transform(nl_lake_buffer_250, crs = 3348)
nt_lake_buffer_250_epsg <- st_transform(nt_lake_buffer_250, crs = 3348)
nu_lake_buffer_250_epsg <- st_transform(nu_lake_buffer_250, crs = 3348)
on_lake_buffer_250_epsg <- st_transform(on_lake_buffer_250, crs = 3348)
pe_lake_buffer_250_epsg <- st_transform(pe_lake_buffer_250, crs = 3348)
qc_lake_buffer_250_epsg <- st_transform(qc_lake_buffer_250, crs = 3348)
yt_lake_buffer_250_epsg <- st_transform(yt_lake_buffer_250, crs = 3348)

# projection
ns_fp_epsg <- st_transform(ns_fp, crs = 3348)
sk_fp_epsg <- st_transform(sk_fp, crs = 3348)
ab_fp_epsg <- st_transform(ab_fp, crs = 3348)
bc_fp_epsg <- st_transform(bc_fp, crs = 3348)
mb_fp_epsg <- st_transform(mb_fp, crs = 3348)
nl_fp_epsg <- st_transform(nl_fp, crs = 3348)
nt_fp_epsg <- st_transform(nt_fp, crs = 3348)
nu_fp_epsg <- st_transform(nu_fp, crs = 3348)
on_fp_epsg <- st_transform(on_fp, crs = 3348)
pe_fp_epsg <- st_transform(pe_fp, crs = 3348)
qc_fp_epsg <- st_transform(qc_fp, crs = 3348)
yt_fp_epsg <- st_transform(yt_fp, crs = 3348)


# This data layer consist with lake id and adjacent
ns_lake_home_250 <- st_intersection(ns_lake_buffer_250_epsg, ns_fp_epsg)
sk_lake_home_250 <- st_intersection(sk_lake_buffer_250_epsg, sk_fp_epsg)
ab_lake_home_250 <- st_intersection(ab_lake_buffer_250_epsg, ab_fp_epsg)
bc_lake_home_250 <- st_intersection(bc_lake_buffer_250_epsg, bc_fp_epsg)
mb_lake_home_250 <- st_intersection(mb_lake_buffer_250_epsg, mb_fp_epsg)
nl_lake_home_250 <- st_intersection(nl_lake_buffer_250_epsg, nl_fp_epsg)
nt_lake_home_250 <- st_intersection(nt_lake_buffer_250_epsg, nt_fp_epsg)
nu_lake_home_250 <- st_intersection(nu_lake_buffer_250_epsg, nu_fp_epsg)
on_lake_home_250 <- st_intersection(on_lake_buffer_250_epsg, on_fp_epsg)
pe_lake_home_250 <- st_intersection(pe_lake_buffer_250_epsg, pe_fp_epsg)
qc_lake_home_250 <- st_intersection(qc_lake_buffer_250_epsg, qc_fp_epsg)
yt_lake_home_250 <- st_intersection(yt_lake_buffer_250_epsg, yt_fp_epsg)


# write shape file
st_write(ns_lake_home_250, "./check_arcGIS/NS/ns_lake_home_250.shp", delete_dsn = T)
st_write(sk_lake_home_250, "./check_arcGIS/SK/sk_lake_home_250.shp", delete_dsn = T)
st_write(ab_lake_home_250, "./check_arcGIS/AB/ab_lake_home_250.shp", delete_dsn = T)
st_write(bc_lake_home_250, "./check_arcGIS/BC/bc_lake_home_250.shp", delete_dsn = T)
st_write(mb_lake_home_250, "./check_arcGIS/MB/mb_lake_home_250.shp", delete_dsn = T)
st_write(nl_lake_home_250, "./check_arcGIS/NL/nl_lake_home_250.shp", delete_dsn = T)
st_write(nt_lake_home_250, "./check_arcGIS/NT/nt_lake_home_250.shp", delete_dsn = T)
st_write(nu_lake_home_250, "./check_arcGIS/NU/nu_lake_home_250.shp", delete_dsn = T)
st_write(on_lake_home_250, "./check_arcGIS/ON/on_lake_home_250.shp", delete_dsn = T)
st_write(pe_lake_home_250, "./check_arcGIS/PE/pe_lake_home_250.shp", delete_dsn = T)
st_write(qc_lake_home_250, "./check_arcGIS/QC/qc_lake_home_250.shp", delete_dsn = T)
st_write(yt_lake_home_250, "./check_arcGIS/YT/yt_lake_home_250.shp", delete_dsn = T)


# merge census DA data and building foot print data within 250 buffer
# projection 
ns_lake_home_250_espg <- st_transform(ns_lake_home_250, crs = 3348)
ns_census_da_espg <- st_transform(ns_census_da, crs = 3348)
ns_census_da_bfp_250 <- st_join(ns_census_da_espg,ns_lake_home_250_espg, all = TRUE )
sk_lake_home_250_espg <- st_transform(sk_lake_home_250, crs = 3348)
sk_census_da_espg <- st_transform(sk_census_da, crs = 3348)
sk_census_da_bfp_250 <- st_join(sk_census_da_espg,sk_lake_home_250_espg, all = TRUE )
ab_lake_home_250_espg <- st_transform(ab_lake_home_250, crs = 3348)
ab_census_da_espg <- st_transform(ab_census_da, crs = 3348)
ab_census_da_bfp_250 <- st_join(ab_census_da_espg,ab_lake_home_250_espg, all = TRUE )
bc_lake_home_250_espg <- st_transform(bc_lake_home_250, crs = 3348)
bc_census_da_espg <- st_transform(bc_census_da, crs = 3348)
bc_census_da_bfp_250 <- st_join(bc_census_da_espg,bc_lake_home_250_espg, all = TRUE )
mb_lake_home_250_espg <- st_transform(mb_lake_home_250, crs = 3348)
mb_census_da_espg <- st_transform(mb_census_da, crs = 3348)
mb_census_da_bfp_250 <- st_join(mb_census_da_espg,mb_lake_home_250_espg, all = TRUE )
nl_lake_home_250_espg <- st_transform(nl_lake_home_250, crs = 3348)
nl_census_da_espg <- st_transform(nl_census_da, crs = 3348)
nl_census_da_bfp_250 <- st_join(nl_census_da_espg,nl_lake_home_250_espg, all = TRUE )
nt_lake_home_250_espg <- st_transform(nt_lake_home_250, crs = 3348)
nt_census_da_espg <- st_transform(nt_census_da, crs = 3348)
nt_census_da_bfp_250 <- st_join(nt_census_da_espg,nt_lake_home_250_espg, all = TRUE )
nu_lake_home_250_espg <- st_transform(nu_lake_home_250, crs = 3348)
nu_census_da_espg <- st_transform(nu_census_da, crs = 3348)
nu_census_da_bfp_250 <- st_join(nu_census_da_espg,nu_lake_home_250_espg, all = TRUE )
on_lake_home_250_espg <- st_transform(on_lake_home_250, crs = 3348)
on_census_da_espg <- st_transform(on_census_da, crs = 3348)
on_census_da_bfp_250 <- st_join(on_census_da_espg,on_lake_home_250_espg, all = TRUE )
pe_lake_home_250_espg <- st_transform(pe_lake_home_250, crs = 3348)
pe_census_da_espg <- st_transform(pe_census_da, crs = 3348)
pe_census_da_bfp_250 <- st_join(pe_census_da_espg,pe_lake_home_250_espg, all = TRUE )
qc_lake_home_250_espg <- st_transform(qc_lake_home_250, crs = 3348)
qc_census_da_espg <- st_transform(qc_census_da, crs = 3348)
qc_census_da_bfp_250 <- st_join(qc_census_da_espg,qc_lake_home_250_espg, all = TRUE )
yt_lake_home_250_espg <- st_transform(yt_lake_home_250, crs = 3348)
yt_census_da_espg <- st_transform(yt_census_da, crs = 3348)
yt_census_da_bfp_250 <- st_join(yt_census_da_espg,yt_lake_home_250_espg, all = TRUE )

# write shape file
st_write(ns_census_da_bfp_250, "./check_arcGIS/NS/ns_census_da_bfp_250.shp", delete_dsn = T)
st_write(sk_census_da_bfp_250, "./check_arcGIS/SK/sk_census_da_bfp_250.shp", delete_dsn = T)
st_write(ab_census_da_bfp_250, "./check_arcGIS/AB/ab_census_da_bfp_250.shp", delete_dsn = T)
st_write(bc_census_da_bfp_250, "./check_arcGIS/BC/bc_census_da_bfp_250.shp", delete_dsn = T)
st_write(mb_census_da_bfp_250, "./check_arcGIS/MB/mb_census_da_bfp_250.shp", delete_dsn = T)
st_write(nl_census_da_bfp_250, "./check_arcGIS/NL/nl_census_da_bfp_250.shp", delete_dsn = T)
st_write(nt_census_da_bfp_250, "./check_arcGIS/NT/nt_census_da_bfp_250.shp", delete_dsn = T)
st_write(nu_census_da_bfp_250, "./check_arcGIS/NU/nu_census_da_bfp_250.shp", delete_dsn = T)
st_write(on_census_da_bfp_250, "./check_arcGIS/ON/on_census_da_bfp_250.shp", delete_dsn = T)
st_write(pe_census_da_bfp_250, "./check_arcGIS/PE/pe_census_da_bfp_250.shp", delete_dsn = T)
st_write(qc_census_da_bfp_250, "./check_arcGIS/QC/qc_census_da_bfp_250.shp", delete_dsn = T)
st_write(yt_census_da_bfp_250, "./check_arcGIS/YT/yt_census_da_bfp_250.shp", delete_dsn = T)


# remove GeoUID where FID = 0, to make sure that GeoUID with zero building foot print does not count

ns_census_da_bfp_250 <- ns_census_da_bfp_250%>%
  subset(FID > 0)
sk_census_da_bfp_250 <- sk_census_da_bfp_250%>%
  subset(FID > 0)
ab_census_da_bfp_250 <- ab_census_da_bfp_250%>%
  subset(FID > 0)
bc_census_da_bfp_250 <- bc_census_da_bfp_250%>%
  subset(FID > 0)
mb_census_da_bfp_250 <- mb_census_da_bfp_250%>%
  subset(FID > 0)
nl_census_da_bfp_250 <- nl_census_da_bfp_250%>%
  subset(FID > 0)
nt_census_da_bfp_250 <- nt_census_da_bfp_250%>%
  subset(FID > 0)
nu_census_da_bfp_250 <- nu_census_da_bfp_250%>%
  subset(FID > 0)
on_census_da_bfp_250 <- on_census_da_bfp_250%>%
  subset(FID > 0)
pe_census_da_bfp_250 <- pe_census_da_bfp_250%>%
  subset(FID > 0)
qc_census_da_bfp_250 <- qc_census_da_bfp_250%>%
  subset(FID > 0)
yt_census_da_bfp_250 <- yt_census_da_bfp_250%>%
  subset(FID > 0)

# count number of houses within buffer
ns_cen_da_bfp_count_250 <- ns_census_da_bfp_250%>%
  count(GeoUID)
sk_cen_da_bfp_count_250 <- sk_census_da_bfp_250%>%
  count(GeoUID)
ab_cen_da_bfp_count_250 <- ab_census_da_bfp_250%>%
  count(GeoUID)
bc_cen_da_bfp_count_250 <- bc_census_da_bfp_250%>%
  count(GeoUID)
mb_cen_da_bfp_count_250 <- mb_census_da_bfp_250%>%
  count(GeoUID)
nl_cen_da_bfp_count_250 <- nl_census_da_bfp_250%>%
  count(GeoUID)
nt_cen_da_bfp_count_250 <- nt_census_da_bfp_250%>%
  count(GeoUID)
nu_cen_da_bfp_count_250 <- nu_census_da_bfp_250%>%
  count(GeoUID)
on_cen_da_bfp_count_250 <- on_census_da_bfp_250%>%
  count(GeoUID)
pe_cen_da_bfp_count_250 <- pe_census_da_bfp_250%>%
  count(GeoUID)
qc_cen_da_bfp_count_250 <- qc_census_da_bfp_250%>%
  count(GeoUID)
yt_cen_da_bfp_count_250 <- yt_census_da_bfp_250%>%
  count(GeoUID)

# join census data
ns_cen_da_bfp_count_250 <-ns_cen_da_bfp_count_250%>%
  as.data.frame()%>%
  select(-geometry)
sk_cen_da_bfp_count_250 <-sk_cen_da_bfp_count_250%>%
  as.data.frame()%>%
  select(-geometry)
ab_cen_da_bfp_count_250 <-ab_cen_da_bfp_count_250%>%
  as.data.frame()%>%
  select(-geometry)
bc_cen_da_bfp_count_250 <-bc_cen_da_bfp_count_250%>%
  as.data.frame()%>%
  select(-geometry)
mb_cen_da_bfp_count_250 <-mb_cen_da_bfp_count_250%>%
  as.data.frame()%>%
  select(-geometry)
nl_cen_da_bfp_count_250 <-nl_cen_da_bfp_count_250%>%
  as.data.frame()%>%
  select(-geometry)
nt_cen_da_bfp_count_250 <-nt_cen_da_bfp_count_250%>%
  as.data.frame()%>%
  select(-geometry)
nu_cen_da_bfp_count_250 <-nu_cen_da_bfp_count_250%>%
  as.data.frame()%>%
  select(-geometry)
on_cen_da_bfp_count_250 <-on_cen_da_bfp_count_250%>%
  as.data.frame()%>%
  select(-geometry)
pe_cen_da_bfp_count_250 <-pe_cen_da_bfp_count_250%>%
  as.data.frame()%>%
  select(-geometry)
qc_cen_da_bfp_count_250 <-qc_cen_da_bfp_count_250%>%
  as.data.frame()%>%
  select(-geometry)
yt_cen_da_bfp_count_250 <-yt_cen_da_bfp_count_250%>%
  as.data.frame()%>%
  select(-geometry)

ns_cen_da_bfp_count_250 <- left_join(ns_census_da_espg,ns_cen_da_bfp_count_250,by = "GeoUID")
sk_cen_da_bfp_count_250 <- left_join(sk_census_da_espg,sk_cen_da_bfp_count_250,by = "GeoUID")
ab_cen_da_bfp_count_250 <- left_join(ab_census_da_espg,ab_cen_da_bfp_count_250,by = "GeoUID")
bc_cen_da_bfp_count_250 <- left_join(bc_census_da_espg,bc_cen_da_bfp_count_250,by = "GeoUID")
mb_cen_da_bfp_count_250 <- left_join(mb_census_da_espg,mb_cen_da_bfp_count_250,by = "GeoUID")
nl_cen_da_bfp_count_250 <- left_join(nl_census_da_espg,nl_cen_da_bfp_count_250,by = "GeoUID")
nt_cen_da_bfp_count_250 <- left_join(nt_census_da_espg,nt_cen_da_bfp_count_250,by = "GeoUID")
nu_cen_da_bfp_count_250 <- left_join(nu_census_da_espg,nu_cen_da_bfp_count_250,by = "GeoUID")
on_cen_da_bfp_count_250 <- left_join(on_census_da_espg,on_cen_da_bfp_count_250,by = "GeoUID")
pe_cen_da_bfp_count_250 <- left_join(pe_census_da_espg,pe_cen_da_bfp_count_250,by = "GeoUID")
qc_cen_da_bfp_count_250 <- left_join(qc_census_da_espg,qc_cen_da_bfp_count_250,by = "GeoUID")
yt_cen_da_bfp_count_250 <- left_join(yt_census_da_espg,yt_cen_da_bfp_count_250,by = "GeoUID")


# write file
st_write(ns_cen_da_bfp_count_250, "./check_arcGIS/NS/ns_census_da_bfp_count_250.shp", delete_dsn = T)
st_write(sk_cen_da_bfp_count_250, "./check_arcGIS/SK/sk_census_da_bfp_count_250.shp", delete_dsn = T)
st_write(ab_cen_da_bfp_count_250, "./check_arcGIS/AB/ab_census_da_bfp_count_250.shp", delete_dsn = T)
st_write(bc_cen_da_bfp_count_250, "./check_arcGIS/BC/bc_census_da_bfp_count_250.shp", delete_dsn = T)
st_write(mb_cen_da_bfp_count_250, "./check_arcGIS/MB/mb_census_da_bfp_count_250.shp", delete_dsn = T)
st_write(nl_cen_da_bfp_count_250, "./check_arcGIS/NL/nl_census_da_bfp_count_250.shp", delete_dsn = T)
st_write(nt_cen_da_bfp_count_250, "./check_arcGIS/NT/nt_census_da_bfp_count_250.shp", delete_dsn = T)
st_write(nu_cen_da_bfp_count_250, "./check_arcGIS/NU/nu_census_da_bfp_count_250.shp", delete_dsn = T)
st_write(on_cen_da_bfp_count_250, "./check_arcGIS/ON/on_census_da_bfp_count_250.shp", delete_dsn = T)
st_write(pe_cen_da_bfp_count_250, "./check_arcGIS/PE/pe_census_da_bfp_count_250.shp", delete_dsn = T)
st_write(qc_cen_da_bfp_count_250, "./check_arcGIS/QC/qc_census_da_bfp_count_250.shp", delete_dsn = T)
st_write(yt_cen_da_bfp_count_250, "./check_arcGIS/YT/yt_census_da_bfp_count_250.shp", delete_dsn = T)


# calculate the cost or benefit based on predefined % change
ns_cen_da_bfp_count_250 <- ns_cen_da_bfp_count_250%>%
  mutate(value_wf = elast_est_wf*v_Avod.*percentate_chane*n)



# calculate the cost or benefit based on predefined % change
ns_cen_da_bfp_count_500 <- ns_cen_da_bfp_count_500%>%
  mutate(valuen_wf = elast_est_nwf*v_Avod.*percentate_chane*n)

#st_write(ns_cen_da_bfp_count_500, "./check_arcGIS/ns_census_da_bfp_count_500.shp")

# obtaining total value by combing value in waterfront and non-waterfront homes

colnames(ns_cen_da_bfp_count_250)[which(names(ns_cen_da_bfp_count_250) == "n")] <- "wf_house"
colnames(ns_cen_da_bfp_count_500)[which(names(ns_cen_da_bfp_count_500) == "n")] <- "nwf_house"

df <-ns_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(nwf_house,valuen_wf,GeoUID)

ns_cen_da_bfp_count_tot <- left_join(ns_cen_da_bfp_count_250,df, by =c("GeoUID"))

ns_cen_da_bfp_count_tot <- ns_cen_da_bfp_count_tot%>%
  mutate(total_value = value_wf+valuen_wf)

#st_write(ns_cen_da_bfp_count_tot, "./check_arcGIS/ns_census_db_bfp_count_tot.shp")

