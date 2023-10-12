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

# Prince Edward Island
pe_fp <- geojson_read("./Building_footprint/PE/PrinceEdwardIsland.geojson",  what = "sp")
writeOGR(pe_sp, dsn = "./Building_footprint/PE","pe", driver = "ESRI Shapefile")

# British Columbia
bc_fp <- geojson_read("./Building_footprint/PE/BritishColumbia.geojson",  what = "sp")
writeOGR(bc_fp, dsn = "./Building_footprint/BC","pe", driver = "ESRI Shapefile")

# Nova Scotia
ns_fp <- geojson_read("./Building_footprint/NS/NovaScotia.geojson",  what = "sp")
writeOGR(ns_fp, dsn = "./Building_footprint/NS","pe", driver = "ESRI Shapefile")

# Alberta
ab_fp <- geojson_read("./Building_footprint/AB/Alberta.geojson",  what = "sp")
writeOGR(ab_fp, dsn = "./Building_footprint/AB","ab", driver = "ESRI Shapefile")

# Saskatchewan
sk_fp <- geojson_read("./Building_footprint/SK/Saskatchewan.geojson",  what = "sp")
writeOGR(sk_fp, dsn = "./Building_footprint/SK","sk", driver = "ESRI Shapefile")

# Manitoba
mb_fp <- geojson_read("./Building_footprint/MB/Manitoba.geojson",  what = "sp")
writeOGR(mb_fp, dsn = "./Building_footprint/MB","mb", driver = "ESRI Shapefile")

# New Brunswick
nb_fp <- geojson_read("./Building_footprint/NB/NewBrunswick.geojson",  what = "sp")
writeOGR(nb_fp, dsn = "./Building_footprint/NB","nb", driver = "ESRI Shapefile")

# New Foundland & Labrador
nl_fp <- geojson_read("./Building_footprint/NL/NewfoundlandAndLabrador.geojson",  what = "sp")
writeOGR(nl_fp, dsn = "./Building_footprint/NL","nl", driver = "ESRI Shapefile")

# Quebec
qc_fp <- geojson_read("./Building_footprint/QC/Quebec.geojson",  what = "sp")
writeOGR(qc_fp, dsn = "./Building_footprint/QC","qc", driver = "ESRI Shapefile")

# Northwest Territories 
nt_fp <- geojson_read("./Building_footprint/NT/NorthwestTerritories.geojson",  what = "sp")
writeOGR(nt_fp, dsn = "./Building_footprint/NT","nt", driver = "ESRI Shapefile")

# Nunavut
nu_fp <- geojson_read("./Building_footprint/NU/Nunavut.geojson",  what = "sp")
writeOGR(nu_fp, dsn = "./Building_footprint/NU","nu", driver = "ESRI Shapefile")

# Yukon
yu_fp <- geojson_read("./Building_footprint/YU/YukonTerritory.geojson",  what = "sp")
writeOGR(yu_fp, dsn = "./Building_footprint/YU","yu", driver = "ESRI Shapefile")

# Ontario
on_fp <- geojson_read("./Building_footprint/ON/Ontario.geojson",  what = "sp")
writeOGR(on_fp, dsn = "./Building_footprint/ON","on", driver = "ESRI Shapefile")









