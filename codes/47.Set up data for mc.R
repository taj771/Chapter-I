# clear memory
rm(list = ls())


## Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  tidyverse,
  sf
)

setwd("~/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I/MetaHedonicProject")               # Tim's working directory (Mac)
setwd("C:/Users/taj771/OneDrive - University of Saskatchewan/Chapter I/MetaHedonicProject") 


# Ratio calclulation 
# bilding foof print area definitions
# lower bound
lb_lb <- 50
lb_ub <- 100

ub <- 2000

# census
df_cen <- st_read("./Census/CAN/can.shp")%>%
  st_transform(df_cen, crs = 3348)

#lower bound

# 250 - bfp within 250m boundary
df_bf_250 <- st_read("./Building_footprint/CAN/can_250.shp")%>%
  subset(areasqm > lb_lb  & areasqm < lb_ub )%>%
  st_transform(df_bf_250, crs = 3348)

bf_cen_da_250 <- st_join(df_bf_250,df_cen, all = TRUE)

bf_cen_da_250_lb <- bf_cen_da_250%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_250_lb" = "n")

# 500 - bfp within 500m boundary

df_bf_500 <- st_read("./Building_footprint/CAN/can_500.shp")%>%
  subset(areasqm > lb_lb  & areasqm < lb_ub )%>%
  st_transform(df_bf_500, crs = 3348)

bf_cen_da_500 <- st_join(df_bf_500,df_cen, all = TRUE)

bf_cen_da_500_lb <- bf_cen_da_500%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_500_lb" = "n")

# upper bound

# 250 - bfp within 250m boundary
df_bf_250 <- st_read("./Building_footprint/CAN/can_250.shp")%>%
  subset(areasqm < ub )%>%
  st_transform(df_bf_250, crs = 3348)

bf_cen_da_250 <- st_join(df_bf_250,df_cen, all = TRUE)

bf_cen_da_250_ub <- bf_cen_da_250%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_250_ub" = "n")


# 500 - bfp within 500m boundary

df_bf_500 <- st_read("./Building_footprint/CAN/can_500.shp")%>%
  subset(areasqm < ub )%>%
  st_transform(df_bf_500, crs = 3348)

bf_cen_da_500 <- st_join(df_bf_500,df_cen, all = TRUE)

bf_cen_da_500_ub <- bf_cen_da_500%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_500_ub" = "n")


df_bf_mode <- st_read("./results/value_map/CAN/can.shp")%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(GeoUID,modbfp_250,modbfp_500,ratio)

df_bf_all <- df_cen%>%
  select(GeoUID,v_Avod_,Popultn,province)%>%
  left_join(bf_cen_da_250_lb)%>%
  left_join(bf_cen_da_250_ub)%>%
  left_join(bf_cen_da_500_lb)%>%
  left_join(bf_cen_da_500_ub)%>%
  left_join(df_bf_mode)%>%
  mutate(lbbfp_250 = ratio*bfp_250_lb)%>%
  mutate(lbbfp_500 = ratio*bfp_500_lb)%>%
  mutate(ubbfp_250 = ratio*bfp_250_ub)%>%
  mutate(ubbfp_500 = ratio*bfp_500_ub)%>%
  select(GeoUID,lbbfp_250,modbfp_250,ubbfp_250,lbbfp_500,modbfp_500,ubbfp_500,Popultn,v_Avod_,province)%>%
  filter(!if_all(c(lbbfp_250,modbfp_250,ubbfp_250,lbbfp_500,modbfp_500,ubbfp_500), is.na))%>%
  mutate_if(is.numeric, round)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))%>%
  filter(lbbfp_250 <= modbfp_250 & modbfp_250 <= ubbfp_250)%>%
  filter(lbbfp_500 <= modbfp_500 & modbfp_500 <= ubbfp_500)
  
  
st_write(df_bf_all, "./Building_footprint/CAN/bfp_for_mc_can.shp", delete_layer = T)





