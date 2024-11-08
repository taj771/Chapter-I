# In this code we set up the lower bound, upper bound and mode of building foot print based on pre defined polygon areas
# Taking time to run

# clear memory
rm(list = ls())


## Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  tidyverse,
  sf
)

#setwd("~/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I/MetaHedonicProject")               # Tim's working directory (Mac)
#setwd("C:/Users/taj771/OneDrive - University of Saskatchewan/Chapter I/MetaHedonicProject") 


# Ratio calclulation 
# bilding foof print area definitions
# lower bound
lb_lb <- 50
lb_ub <- 100

#upper bound
ub <- 2000

#mode bounds
mod_lb <- 40 
mod_ub <- 2000


# census
df_cen <- st_read("./Census/CAN/can.shp")%>%
  st_transform(df_cen, crs = 3348)

###########################################################
### DONOT RUN EVERT TIME - TAKES TIME
#bfp census_da - take the ratio between bfp and census data
# here we do not apply any filtetr to bfp

bfp_can <- st_read("./Building_footprint/CAN/can_all.shp")

bfp_can <- st_transform(bfp_can, crs = 3348)

bf_cen_da_all <- st_join(bfp_can,df_cen, all = TRUE)


bf_cen_da_all_count <- bf_cen_da_all%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_all" = "n")


df_cen <- df_cen%>%
  left_join(bf_cen_da_all_count)%>%
  mutate(ratio = Dwllngs/bfp_all)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(GeoUID,ratio)

write.csv(df_cen, "./Census/CAN/can_cen_bfp_ratio.csv", row.names = F)
################################################################################

bfp_cen_da <- read_csv("./Census/CAN/can_cen_bfp_ratio.csv")%>%
  mutate(GeoUID = as.character(GeoUID))

# census
df_cen <- st_read("./Census/CAN/can.shp")%>%
  st_transform(df_cen, crs = 3348)

# 500 - bfp within 500m boundary
df_bf_500 <- st_read("./Building_footprint/CAN/can_all_004_500m.shp")%>%
  st_transform(df_bf_500, crs = 3348)
bf_cen_da_500 <- st_join(df_bf_500,df_cen, all = TRUE)

# 1000 - bfp within 1000m boundary
df_bf_1000 <- st_read("./Building_footprint/CAN/can_all_004_1000m.shp")%>%
  st_transform(df_bf_1000, crs = 3348)
bf_cen_da_1000 <- st_join(df_bf_1000,df_cen, all = TRUE)


#lower bound##lower bound##lower bound##lower bound##lower bound#

bf_cen_da_500_lb <- bf_cen_da_500%>%
  subset(areasqm > lb_lb  & areasqm < lb_ub )%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_500_lb" = "n")


bf_cen_da_1000_lb <- bf_cen_da_1000%>%
  subset(areasqm > lb_lb  & areasqm < lb_ub )%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_1000_lb" = "n")

#upperbound#upperbound#upperbound#upperbound


bf_cen_da_500_ub <- bf_cen_da_500%>%
  subset(areasqm < ub )%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_500_ub" = "n")


bf_cen_da_1000_ub <- bf_cen_da_1000%>%
  subset(areasqm < ub )%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_1000_ub" = "n")


# mode bfp count 

bf_cen_da_500_mod <- bf_cen_da_500%>%
  subset(areasqm > mod_lb  & areasqm < mod_ub )%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("modbfp_500" = "n")


bf_cen_da_1000_mod <- bf_cen_da_1000%>%
  subset(areasqm > mod_lb  & areasqm < mod_ub)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("modbfp_1000" = "n")




#df_cen$GeoUID <- as.character(df_cen$GeoUID)

df_bf_all <- df_cen%>%
  select(GeoUID,AvgDwlv,Popultn,prov)%>%
  left_join(bf_cen_da_500_lb)%>%
  left_join(bf_cen_da_500_ub)%>%
  left_join(bf_cen_da_1000_lb)%>%
  left_join(bf_cen_da_1000_ub)%>%
  left_join(bf_cen_da_500_mod)%>%
  left_join(bf_cen_da_1000_mod)%>%
  left_join(bfp_cen_da)%>%
  mutate(bfp_500_1000_lb = bfp_1000_lb-bfp_500_lb)%>%
  mutate(bfp_500_1000_ub = bfp_1000_ub-bfp_500_ub)%>%
  mutate(modbfp_500_1000 = modbfp_1000 - modbfp_500)%>%
  mutate(lbbfp_500 = ratio*bfp_500_lb)%>%
  mutate(lbbfp_1000 = ratio*bfp_500_1000_lb)%>%
  mutate(ubbfp_500 = ratio*bfp_500_ub)%>%
  mutate(ubbfp_1000 = ratio*bfp_500_1000_ub)%>%
  mutate(modbfp_500 = ratio*modbfp_500)%>%
  mutate(modbfp_1000 = ratio*modbfp_500_1000)%>%
  select(GeoUID,lbbfp_500,modbfp_500,ubbfp_500,lbbfp_1000,modbfp_1000,ubbfp_1000,Popultn,AvgDwlv,prov)%>%
  filter(!if_all(c(lbbfp_500,modbfp_500,ubbfp_500,lbbfp_1000,modbfp_1000,ubbfp_1000), is.na))%>%
  #mutate_if(is.numeric, round)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

st_write(df_bf_all, "./Building_footprint/CAN/bfp_for_mc_can.shp", delete_layer = T)





