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
list_census_regions("CA16")

# since most of the layers are not available at CA_21 layer will depend on CA_16
# for now and later will update as they become public available
# Dissemination Area level - prefer to get Dissemination Block level, but most of the information
# are not available DB - future need to done a sensitivity analysis

# vectors of interest

#2020
vectors = c("v_CA16_404","v_CA16_4896","v_CA16_401","v_CA16_4895")
#2021
vectors = c("v_CA21_4312","v_CA21_4311","v_CA21_4","v_CA21_8",
            "v_CA21_452","v_CA21_983","v_CA21_5847")


# Canada


# Alberta
ab_census_da <- get_census(dataset='CA21', regions=list(PR="48"),
                          vectors=paste0(vectors),
                          level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)%>%
  rename("AvgDwelval" = "v_CA21_4312: Average value of dwellings ($) (60)",
         "MedDwelval" = "v_CA21_4311: Median value of dwellings ($) (60)",
         "TotDweling" = "v_CA21_4: Total private dwellings",
         #"Population" = "v_CA21_1: Population, 2021",
         "Age" = "v_CA21_8: Total - Age",
         "AvgHousHold" = "v_CA21_452: Average household size",
         "MedianIncom" = "v_CA21_983: Median total income in 2020 ($)",
         "EduDegre" = "v_CA21_5847: Bachelor's degree or higher")%>%
  mutate(prov = "AB")

# Saskatchewan
sk_census_da <- get_census(dataset='CA21', regions=list(PR="47"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)%>%
  rename("AvgDwelval" = "v_CA21_4312: Average value of dwellings ($) (60)",
         "MedDwelval" = "v_CA21_4311: Median value of dwellings ($) (60)",
         "TotDweling" = "v_CA21_4: Total private dwellings",
         #"Population" = "v_CA21_1: Population, 2021",
         "Age" = "v_CA21_8: Total - Age",
         "AvgHousHold" = "v_CA21_452: Average household size",
         "MedianIncom" = "v_CA21_983: Median total income in 2020 ($)",
         "EduDegre" = "v_CA21_5847: Bachelor's degree or higher")%>%
  mutate(prov = "SK")


# Newfoundland Labrador
nl_census_da <- get_census(dataset='CA21', regions=list(PR="10"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)%>%
  rename("AvgDwelval" = "v_CA21_4312: Average value of dwellings ($) (60)",
         "MedDwelval" = "v_CA21_4311: Median value of dwellings ($) (60)",
         "TotDweling" = "v_CA21_4: Total private dwellings",
         #"Population" = "v_CA21_1: Population, 2021",
         "Age" = "v_CA21_8: Total - Age",
         "AvgHousHold" = "v_CA21_452: Average household size",
         "MedianIncom" = "v_CA21_983: Median total income in 2020 ($)",
         "EduDegre" = "v_CA21_5847: Bachelor's degree or higher")%>%
  mutate(prov = "NL")


# Nova Scotia
ns_census_da <- get_census(dataset='CA21', regions=list(PR="12"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)%>%
  rename("AvgDwelval" = "v_CA21_4312: Average value of dwellings ($) (60)",
         "MedDwelval" = "v_CA21_4311: Median value of dwellings ($) (60)",
         "TotDweling" = "v_CA21_4: Total private dwellings",
         #"Population" = "v_CA21_1: Population, 2021",
         "Age" = "v_CA21_8: Total - Age",
         "AvgHousHold" = "v_CA21_452: Average household size",
         "MedianIncom" = "v_CA21_983: Median total income in 2020 ($)",
         "EduDegre" = "v_CA21_5847: Bachelor's degree or higher")%>%
  mutate(prov = "NS")


# Quebec
qc_census_da <- get_census(dataset='CA21', regions=list(PR="24"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)%>%
  rename("AvgDwelval" = "v_CA21_4312: Average value of dwellings ($) (60)",
         "MedDwelval" = "v_CA21_4311: Median value of dwellings ($) (60)",
         "TotDweling" = "v_CA21_4: Total private dwellings",
         #"Population" = "v_CA21_1: Population, 2021",
         "Age" = "v_CA21_8: Total - Age",
         "AvgHousHold" = "v_CA21_452: Average household size",
         "MedianIncom" = "v_CA21_983: Median total income in 2020 ($)",
         "EduDegre" = "v_CA21_5847: Bachelor's degree or higher")%>%
  mutate(prov = "QC")


# Prince Edwards
pe_census_da <- get_census(dataset='CA21', regions=list(PR="11"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)%>%
  rename("AvgDwelval" = "v_CA21_4312: Average value of dwellings ($) (60)",
         "MedDwelval" = "v_CA21_4311: Median value of dwellings ($) (60)",
         "TotDweling" = "v_CA21_4: Total private dwellings",
         #"Population" = "v_CA21_1: Population, 2021",
         "Age" = "v_CA21_8: Total - Age",
         "AvgHousHold" = "v_CA21_452: Average household size",
         "MedianIncom" = "v_CA21_983: Median total income in 2020 ($)",
         "EduDegre" = "v_CA21_5847: Bachelor's degree or higher")%>%
  mutate(prov = "PE")


# New Brunswick
nb_census_da <- get_census(dataset='CA21', regions=list(PR="13"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)%>%
  rename("AvgDwelval" = "v_CA21_4312: Average value of dwellings ($) (60)",
         "MedDwelval" = "v_CA21_4311: Median value of dwellings ($) (60)",
         "TotDweling" = "v_CA21_4: Total private dwellings",
         #"Population" = "v_CA21_1: Population, 2021",
         "Age" = "v_CA21_8: Total - Age",
         "AvgHousHold" = "v_CA21_452: Average household size",
         "MedianIncom" = "v_CA21_983: Median total income in 2020 ($)",
         "EduDegre" = "v_CA21_5847: Bachelor's degree or higher")%>%
  mutate(prov = "NB")

# Manitoba
mb_census_da <- get_census(dataset='CA21', regions=list(PR="46"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)%>%
  rename("AvgDwelval" = "v_CA21_4312: Average value of dwellings ($) (60)",
         "MedDwelval" = "v_CA21_4311: Median value of dwellings ($) (60)",
         "TotDweling" = "v_CA21_4: Total private dwellings",
         #"Population" = "v_CA21_1: Population, 2021",
         "Age" = "v_CA21_8: Total - Age",
         "AvgHousHold" = "v_CA21_452: Average household size",
         "MedianIncom" = "v_CA21_983: Median total income in 2020 ($)",
         "EduDegre" = "v_CA21_5847: Bachelor's degree or higher")%>%
  mutate(prov = "MB")


# British Colombia
bc_census_da <- get_census(dataset='CA21', regions=list(PR="59"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)%>%
  rename("AvgDwelval" = "v_CA21_4312: Average value of dwellings ($) (60)",
         "MedDwelval" = "v_CA21_4311: Median value of dwellings ($) (60)",
         "TotDweling" = "v_CA21_4: Total private dwellings",
         #"Population" = "v_CA21_1: Population, 2021",
         "Age" = "v_CA21_8: Total - Age",
         "AvgHousHold" = "v_CA21_452: Average household size",
         "MedianIncom" = "v_CA21_983: Median total income in 2020 ($)",
         "EduDegre" = "v_CA21_5847: Bachelor's degree or higher")%>%
  mutate(prov = "BC")

# Ontario
on_census_da <- get_census(dataset='CA21', regions=list(PR="35"),
                           vectors=paste0(vectors),
                           level='DA', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)%>%
  rename("AvgDwelval" = "v_CA21_4312: Average value of dwellings ($) (60)",
         "MedDwelval" = "v_CA21_4311: Median value of dwellings ($) (60)",
         "TotDweling" = "v_CA21_4: Total private dwellings",
         #"Population" = "v_CA21_1: Population, 2021",
         "Age" = "v_CA21_8: Total - Age",
         "AvgHousHold" = "v_CA21_452: Average household size",
         "MedianIncom" = "v_CA21_983: Median total income in 2020 ($)",
         "EduDegre" = "v_CA21_5847: Bachelor's degree or higher")%>%
  mutate(prov = "ON")

# write shape file

st_write(ab_census_da, "./Census/AB/census_ab_da.shp",delete_layer = T)
st_write(sk_census_da, "./Census/SK/census_sk_da.shp",delete_layer = T)
st_write(nl_census_da, "./Census/NL/census_nl_da.shp",delete_layer = T)
st_write(ns_census_da, "./Census/NS/census_ns_da.shp",delete_layer = T)
st_write(qc_census_da, "./Census/QC/census_qc_da.shp",delete_layer = T)
st_write(pe_census_da, "./Census/PE/census_pe_da.shp",delete_layer = T)
st_write(nb_census_da, "./Census/NB/census_nb_da.shp",delete_layer = T)
st_write(mb_census_da, "./Census/MB/census_mb_da.shp",delete_layer = T)
st_write(bc_census_da, "./Census/BC/census_bc_da.shp",delete_layer = T)
st_write(on_census_da, "./Census/ON/census_on_da.shp",delete_layer = T)

