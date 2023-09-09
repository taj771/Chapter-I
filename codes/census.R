# clean memory
rm(list = ls())
# load library 
library(cancensus)
library(geojsonsf)
library(rcanvec)
library(cancensus)
library(sf)
library(sp)
library(rgdal)
library(sp)
library(tidyverse)
library(spatialEco)
library(FIESTA)
library(geojsonsf)
library(geojsonio)
library(ggplot2)



options(cancensus.api_key = "CensusMapper_810d26a92ab56b8a9125d6da244403ab")
options(cancensus.cache_path = "./census_cache")

# meta data
cen_data <- list_census_datasets()
cen_region <- list_census_regions("CA21")
cen_vec21 <- list_census_vectors("CA21")
cen_vec16 <- list_census_vectors("CA16")

# since most of the layers are not available at CA_21 layer will depend on CA_16
# for now and later will update as they become public available
# Dissemination Area level - prefer to get Dissemination Block level, but most of the information
# are not available DB - future need to done a sensitivity analysis

# vectors of interest

vectors = c("v_CA16_404","v_CA16_4896","v_CA16_401")

# Alberta
ab_census_da <- get_census(dataset='CA16', regions=list(PR="48"),
                          vectors=paste0(vectors),
                          level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
# Saskatchewan
sk_census_da <- get_census(dataset='CA16', regions=list(PR="47"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
# Newfoundland Labrador
nl_census_da <- get_census(dataset='CA16', regions=list(PR="10"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
# Nunavut
nu_census_da <- get_census(dataset='CA16', regions=list(PR="62"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
# Northwest Territories
nt_census_da <- get_census(dataset='CA16', regions=list(PR="61"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
# Nova Scotia
ns_census_da <- get_census(dataset='CA16', regions=list(PR="12"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
# Yukon
yu_census_da <- get_census(dataset='CA16', regions=list(PR="60"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
# Quebec
qc_census_da <- get_census(dataset='CA16', regions=list(PR="24"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
# Prince Edwards
pe_census_da <- get_census(dataset='CA16', regions=list(PR="11"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
# New Brunswick
nb_census_da <- get_census(dataset='CA16', regions=list(PR="13"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
# Manitoba
mb_census_da <- get_census(dataset='CA16', regions=list(PR="46"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
# British Colombia
bc_census_da <- get_census(dataset='CA16', regions=list(PR="59"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
# Ontario
on_census_da <- get_census(dataset='CA16', regions=list(PR="35"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
# write shape file

st_write(ab_census_da, "./Census/AB/census_ab_da.shp")
st_write(sk_census_da, "./Census/SK/census_sk_da.shp")
st_write(nl_census_da, "./Census/NL/census_nl_da.shp")
st_write(nu_census_da, "./Census/NU/census_nu_da.shp")
st_write(nt_census_da, "./Census/NT/census_nt_da.shp")
st_write(ns_census_da, "./Census/NS/census_ns_da.shp")
st_write(yu_census_da, "./Census/YU/census_yu_da.shp")
st_write(qc_census_da, "./Census/QC/census_qc_da.shp")
st_write(pe_census_da, "./Census/PE/census_pe_da.shp")
st_write(nb_census_da, "./Census/NB/census_nb_da.shp")
st_write(mb_census_da, "./Census/MB/census_mb_da.shp")
st_write(bc_census_da, "./Census/BC/census_bc_da.shp")
st_write(on_census_da, "./Census/ON/census_on_da.shp")



ab_census_da <- get_census(dataset='CA16', regions=list(PR="48"),
                        vectors=c("v_CA16_404","v_CA16_4896","v_CA16_401"),
                        level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)

st_write(ab_census_da, "./Census/AB/census_ab_da.shp")

ab_census <- st_transform(ab_census, crs = "+proj=longlat +datum=WGS84 +no_defs")

# Alberta

ns_census_db <- get_census(dataset='CA16', regions=list(PR="12"),
                           vectors=c("v_CA16_404","v_CA16_4896","v_CA16_401"),
                           level='DB', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)

st_write(ns_census_db, "./Census/NS/census_ns_db.shp")


ns_census_da <- get_census(dataset='CA16', regions=list(PR="12"),
                           vectors=c("v_CA16_404","v_CA16_4896","v_CA16_401"),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)

st_write(ns_census_da, "./Census/NS/census_ns_da.shp")

ns_census <- st_transform(ns_census, crs = "+proj=longlat +datum=WGS84 +no_defs")

###############################################################################

ns <- st_read("./gpr_000b11a_e/gpr_000b11a_e.shp")
ns <- subset(ns, PRUID == "11")
pe_CRS <- st_transform(pe, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(pe_CRS)==st_crs(pe_census)


# creating buffer around water body. Need to define buffer zones as
# our objectives. For now I defined 500m buffer zone in Lake Winnnipeg.

pe_lake <- st_read("./data/processed/pe_lakes.shp")

pe_lakes_epsg <- st_transform(pe_lake, crs = 3348)


pe_lake_buffer <- st_buffer(pe_lakes_epsg, dist = 500)

pe_lake_buffer <- st_transform(pe_lake_buffer, crs = "+proj=longlat +datum=WGS84 +no_defs")


clip <- st_intersection(pe_lake_buffer, pe_census)

df <- distinct(pe_census$GeoUID, keep_all = TRUE)


bc_fp <- geojson_sf("./Building_footprint/BC/BritishColumbia.geojson")

spdf <- geojson_read("./Building_footprint/BC/BritishColumbia.geojson",  what = "sp")


writeOGR(spdf, dsn = "Building_footprint","bc", driver = "ESRI Shapefile")

df <- st_read("./Building_footprint/pe.shp")

bc_lake <- st_read("./data/processed/bc_lakes.shp")
Williston_Lake <- subset(bc_lake,namelk1en == "Williston Lake")

ggplot()+
  geom_sf(data = Williston_Lake, col = "red")+
  coord_sf()





ggplot()+
  geom_sf(data = pe_CRS)+
  geom_sf(data = pe_census, col = "red")+
  coord_sf()