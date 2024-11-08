# clear memory
rm(list = ls())


## Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  tidyverse,
  sf
)

# bilding foof print area definitions
 ub <- 1000
 lb <- 50
 
# elasticity estimations - RE

e_250m <- 0.216
e_500m <- 0.062

# water qualiy change

wq_change <- 10/100
 
# working directory 



setwd("~/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I/MetaHedonicProject")               # Tim's working directory (Mac)
setwd("C:/Users/taj771/OneDrive - University of Saskatchewan/Chapter I/MetaHedonicProject") 

################################################################################
# Alberta
###############################################################################
# calculate the adjustment ratio using bfp data and census data
 
bf <- st_read("./Building_footprint/AB/ab.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf_250, crs = 3348)


cen_da <- st_read("./Census/AB/census_ab_da.shp")%>%
  st_transform(bf_250, crs = 3348)


bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_count" = "n")

cen_da_ratio <- cen_da%>%
  as.data.frame()%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod.,-geometry, Dwllngs)%>%
  left_join(bf_cen_da)%>%
  mutate(ratio = Dwllngs/bfp_count)%>%
  as.data.frame()%>%
  select(GeoUID,ratio,-geometry)

# 250
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/AB/ab_all_004_250m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)
  
cen_da <- st_read("./Census/AB/census_ab_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_250" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_250 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_250 = ratio*row_bfp_count_250)%>%
  drop_na()%>%
  select(GeoUID,adj_bfp_count_250,v_Avod.)%>%
  mutate_if(is.numeric, round)%>%
  as.data.frame()%>%
  select(-geometry)

# 500
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/AB/ab_all_004_500m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/AB/census_ab_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_500" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,Popultn,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_500 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_500 = ratio*row_bfp_count_500)%>%
  drop_na()%>%
  select(GeoUID,Popultn,v_Avod.,adj_bfp_count_500)%>%
  mutate_if(is.numeric, round)

tot_value <- value_500%>%
  left_join(value_250)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))%>%
  mutate(adj_bfp_count_250_500 = adj_bfp_count_500 - adj_bfp_count_250)%>%
  mutate(e_250 = e_250m/100)%>%
  mutate(e_500 = e_500m/100)%>%
  mutate(val_250 = e_250*v_Avod.*adj_bfp_count_250)%>%
  mutate(val_500 = e_500*v_Avod.*adj_bfp_count_250_500)%>%
  mutate(val_tot = val_250 + val_500)%>%
  mutate(val_tot_pp = val_tot/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)

ab_250_bfp <- sum(tot_value$adj_bfp_count_250, na.rm = T)
ab_250_val <- sum(tot_value$val_250, na.rm = T)
ab_500_bfp <- sum(tot_value$adj_bfp_count_250_500, na.rm = T)
ab_500_val <- sum(tot_value$val_500, na.rm = T)

cen_da <- st_read("./Census/AB/census_ab_da.shp")%>%
  select(GeoUID)%>%
  left_join(tot_value)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))


#csd
st_write(cen_da, "./results/value_map/AB/ab_value_map.shp", delete_layer = T )

# save as csv for MC

tot_value <- tot_value%>%
  select(GeoUID,v_Avod.,adj_bfp_count_250,adj_bfp_count_250_500)
  
write_csv(tot_value, "./results/value_map/AB/mc_design.csv")
################################################################################
# British Colombia
################################################################################

bf <- st_read("./Building_footprint/BC/bc.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf_250, crs = 3348)


cen_da <- st_read("./Census/BC/census_bc_da.shp")%>%
  st_transform(bf_250, crs = 3348)


bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_count" = "n")

cen_da_ratio <- cen_da%>%
  as.data.frame()%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod.,-geometry, Dwllngs)%>%
  left_join(bf_cen_da)%>%
  mutate(ratio = Dwllngs/bfp_count)%>%
  as.data.frame()%>%
  select(GeoUID,ratio,-geometry)

# 250
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/BC/bc_all_004_250m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/BC/census_bc_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_250" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_250 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_250 = ratio*row_bfp_count_250)%>%
  drop_na()%>%
  select(GeoUID,adj_bfp_count_250,v_Avod.)%>%
  mutate_if(is.numeric, round)%>%
  as.data.frame()%>%
  select(-geometry)

# 500
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/BC/bc_all_004_500m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/BC/census_bc_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_500" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,Popultn,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_500 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_500 = ratio*row_bfp_count_500)%>%
  drop_na()%>%
  select(GeoUID,Popultn,v_Avod.,adj_bfp_count_500)%>%
  mutate_if(is.numeric, round)

tot_value <- value_500%>%
  left_join(value_250)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))%>%
  mutate(adj_bfp_count_250_500 = adj_bfp_count_500 - adj_bfp_count_250)%>%
  mutate(e_250 = e_250m/100)%>%
  mutate(e_500 = e_500m/100)%>%
  mutate(val_250 = e_250*v_Avod.*adj_bfp_count_250)%>%
  mutate(val_500 = e_500*v_Avod.*adj_bfp_count_250_500)%>%
  mutate(val_tot = val_250 + val_500)%>%
  mutate(val_tot_pp = val_tot/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)

bc_250_bfp <- sum(tot_value$adj_bfp_count_250, na.rm = T)
bc_250_val <- sum(tot_value$val_250, na.rm = T)
bc_500_bfp <- sum(tot_value$adj_bfp_count_250_500, na.rm = T)
bc_500_val <- sum(tot_value$val_500, na.rm = T)

cen_da <- st_read("./Census/BC/census_bc_da.shp")%>%
  select(GeoUID)%>%
  left_join(tot_value)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

#csd
st_write(cen_da, "./results/value_map/BC/bc_value_map.shp", delete_layer = T )

# save as csv for MC

tot_value <- tot_value%>%
  select(GeoUID,v_Avod.,adj_bfp_count_250,adj_bfp_count_250_500)

write_csv(tot_value, "./results/value_map/BC/mc_design.csv")

################################################################################
# Manitoba
################################################################################

bf <- st_read("./Building_footprint/MB/mb.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf_250, crs = 3348)


cen_da <- st_read("./Census/MB/census_mb_da.shp")%>%
  st_transform(bf_250, crs = 3348)


bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_count" = "n")

cen_da_ratio <- cen_da%>%
  as.data.frame()%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod.,-geometry, Dwllngs)%>%
  left_join(bf_cen_da)%>%
  mutate(ratio = Dwllngs/bfp_count)%>%
  as.data.frame()%>%
  select(GeoUID,ratio,-geometry)

# 250
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/MB/mb_all_004_250m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/MB/census_mb_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_250" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_250 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_250 = ratio*row_bfp_count_250)%>%
  drop_na()%>%
  select(GeoUID,adj_bfp_count_250,v_Avod.)%>%
  mutate_if(is.numeric, round)%>%
  as.data.frame()%>%
  select(-geometry)

# 500
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/MB/mb_all_004_500m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/MB/census_mb_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_500" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,Popultn,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_500 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_500 = ratio*row_bfp_count_500)%>%
  drop_na()%>%
  select(GeoUID,Popultn,v_Avod.,adj_bfp_count_500)%>%
  mutate_if(is.numeric, round)

tot_value <- value_500%>%
  left_join(value_250)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))%>%
  mutate(adj_bfp_count_250_500 = adj_bfp_count_500 - adj_bfp_count_250)%>%
  mutate(e_250 = e_250m/100)%>%
  mutate(e_500 = e_500m/100)%>%
  mutate(val_250 = e_250*v_Avod.*adj_bfp_count_250)%>%
  mutate(val_500 = e_500*v_Avod.*adj_bfp_count_250_500)%>%
  mutate(val_tot = val_250 + val_500)%>%
  mutate(val_tot_pp = val_tot/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)

mb_250_bfp <- sum(tot_value$adj_bfp_count_250, na.rm = T)
mb_250_val <- sum(tot_value$val_250, na.rm = T)
mb_500_bfp <- sum(tot_value$adj_bfp_count_250_500, na.rm = T)
mb_500_val <- sum(tot_value$val_500, na.rm = T)

cen_da <- st_read("./Census/MB/census_mb_da.shp")%>%
  select(GeoUID)%>%
  left_join(tot_value)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

#csd
st_write(cen_da, "./results/value_map/MB/mb_value_map.shp", delete_layer = T )

# save as csv for MC

tot_value <- tot_value%>%
  select(GeoUID,v_Avod.,adj_bfp_count_250,adj_bfp_count_250_500)

write_csv(tot_value, "./results/value_map/MB/mc_design.csv")

################################################################################
# New Brunswick
################################################################################

bf <- st_read("./Building_footprint/NB/nb.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf_250, crs = 3348)


cen_da <- st_read("./Census/NB/census_nb_da.shp")%>%
  st_transform(bf_250, crs = 3348)


bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_count" = "n")

cen_da_ratio <- cen_da%>%
  as.data.frame()%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod.,-geometry, Dwllngs)%>%
  left_join(bf_cen_da)%>%
  mutate(ratio = Dwllngs/bfp_count)%>%
  as.data.frame()%>%
  select(GeoUID,ratio,-geometry)

# 250
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/NB/nb_all_004_250m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/NB/census_nb_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_250" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_250 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_250 = ratio*row_bfp_count_250)%>%
  drop_na()%>%
  select(GeoUID,adj_bfp_count_250,v_Avod.)%>%
  mutate_if(is.numeric, round)%>%
  as.data.frame()%>%
  select(-geometry)

# 500
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/NB/nb_all_004_500m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/NB/census_nb_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_500" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,Popultn,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_500 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_500 = ratio*row_bfp_count_500)%>%
  drop_na()%>%
  select(GeoUID,Popultn,v_Avod.,adj_bfp_count_500)%>%
  mutate_if(is.numeric, round)

tot_value <- value_500%>%
  left_join(value_250)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))%>%
  mutate(adj_bfp_count_250_500 = adj_bfp_count_500 - adj_bfp_count_250)%>%
  mutate(e_250 = e_250m/100)%>%
  mutate(e_500 = e_500m/100)%>%
  mutate(val_250 = e_250*v_Avod.*adj_bfp_count_250)%>%
  mutate(val_500 = e_500*v_Avod.*adj_bfp_count_250_500)%>%
  mutate(val_tot = val_250 + val_500)%>%
  mutate(val_tot_pp = val_tot/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)

nb_250_bfp <- sum(tot_value$adj_bfp_count_250, na.rm = T)
nb_250_val <- sum(tot_value$val_250, na.rm = T)
nb_500_bfp <- sum(tot_value$adj_bfp_count_250_500, na.rm = T)
nb_500_val <- sum(tot_value$val_500, na.rm = T)

cen_da <- st_read("./Census/NB/census_nb_da.shp")%>%
  select(GeoUID)%>%
  left_join(tot_value)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

#csd
st_write(cen_da, "./results/value_map/NB/nb_value_map.shp", delete_layer = T )

# save as csv for MC

tot_value <- tot_value%>%
  select(GeoUID,v_Avod.,adj_bfp_count_250,adj_bfp_count_250_500)

write_csv(tot_value, "./results/value_map/NB/mc_design.csv")

################################################################################
# Newfoundland Labrador 
################################################################################

bf <- st_read("./Building_footprint/NL/nl.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf_250, crs = 3348)


cen_da <- st_read("./Census/NL/census_nl_da.shp")%>%
  st_transform(bf_250, crs = 3348)


bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_count" = "n")

cen_da_ratio <- cen_da%>%
  as.data.frame()%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod.,-geometry, Dwllngs)%>%
  left_join(bf_cen_da)%>%
  mutate(ratio = Dwllngs/bfp_count)%>%
  as.data.frame()%>%
  select(GeoUID,ratio,-geometry)

# 250
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/NL/nl_all_004_250m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/NL/census_nl_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_250" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_250 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_250 = ratio*row_bfp_count_250)%>%
  drop_na()%>%
  select(GeoUID,adj_bfp_count_250,v_Avod.)%>%
  mutate_if(is.numeric, round)%>%
  as.data.frame()%>%
  select(-geometry)

# 500
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/NL/nl_all_004_500m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/NL/census_nl_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_500" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,Popultn,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_500 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_500 = ratio*row_bfp_count_500)%>%
  drop_na()%>%
  select(GeoUID,Popultn,v_Avod.,adj_bfp_count_500)%>%
  mutate_if(is.numeric, round)

tot_value <- value_500%>%
  left_join(value_250)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))%>%
  mutate(adj_bfp_count_250_500 = adj_bfp_count_500 - adj_bfp_count_250)%>%
  mutate(e_250 = e_250m/100)%>%
  mutate(e_500 = e_500m/100)%>%
  mutate(val_250 = e_250*v_Avod.*adj_bfp_count_250)%>%
  mutate(val_500 = e_500*v_Avod.*adj_bfp_count_250_500)%>%
  mutate(val_tot = val_250 + val_500)%>%
  mutate(val_tot_pp = val_tot/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)

nl_250_bfp <- sum(tot_value$adj_bfp_count_250, na.rm = T)
nl_250_val <- sum(tot_value$val_250, na.rm = T)
nl_500_bfp <- sum(tot_value$adj_bfp_count_250_500, na.rm = T)
nl_500_val <- sum(tot_value$val_500, na.rm = T)

cen_da <- st_read("./Census/NL/census_nl_da.shp")%>%
  select(GeoUID)%>%
  left_join(tot_value)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

#csd
st_write(cen_da, "./results/value_map/NL/nl_value_map.shp", delete_layer = T )

# save as csv for MC

tot_value <- tot_value%>%
  select(GeoUID,v_Avod.,adj_bfp_count_250,adj_bfp_count_250_500)

write_csv(tot_value, "./results/value_map/NL/nl_design.csv")

################################################################################
# Nova Scotchia 
################################################################################

bf <- st_read("./Building_footprint/NS/ns.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf_250, crs = 3348)


cen_da <- st_read("./Census/NS/census_ns_da.shp")%>%
  st_transform(bf_250, crs = 3348)


bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_count" = "n")

cen_da_ratio <- cen_da%>%
  as.data.frame()%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod.,-geometry, Dwllngs)%>%
  left_join(bf_cen_da)%>%
  mutate(ratio = Dwllngs/bfp_count)%>%
  as.data.frame()%>%
  select(GeoUID,ratio,-geometry)

# 250
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/NS/ns_all_004_250m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/NS/census_ns_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_250" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_250 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_250 = ratio*row_bfp_count_250)%>%
  drop_na()%>%
  select(GeoUID,adj_bfp_count_250,v_Avod.)%>%
  mutate_if(is.numeric, round)%>%
  as.data.frame()%>%
  select(-geometry)

# 500
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/NS/ns_all_004_500m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/NS/census_ns_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_500" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,Popultn,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_500 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_500 = ratio*row_bfp_count_500)%>%
  drop_na()%>%
  select(GeoUID,Popultn,v_Avod.,adj_bfp_count_500)%>%
  mutate_if(is.numeric, round)

tot_value <- value_500%>%
  left_join(value_250)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))%>%
  mutate(adj_bfp_count_250_500 = adj_bfp_count_500 - adj_bfp_count_250)%>%
  mutate(e_250 = e_250m/100)%>%
  mutate(e_500 = e_500m/100)%>%
  mutate(val_250 = e_250*v_Avod.*adj_bfp_count_250)%>%
  mutate(val_500 = e_500*v_Avod.*adj_bfp_count_250_500)%>%
  mutate(val_tot = val_250 + val_500)%>%
  mutate(val_tot_pp = val_tot/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)

ns_250_bfp <- sum(tot_value$adj_bfp_count_250, na.rm = T)
ns_250_val <- sum(tot_value$val_250, na.rm = T)
ns_500_bfp <- sum(tot_value$adj_bfp_count_250_500, na.rm = T)
ns_500_val <- sum(tot_value$val_500, na.rm = T)

cen_da <- st_read("./Census/NS/census_ns_da.shp")%>%
  select(GeoUID)%>%
  left_join(tot_value)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

#csd
st_write(cen_da, "./results/value_map/NS/ns_value_map.shp", delete_layer = T )

# save as csv for MC

tot_value <- tot_value%>%
  select(GeoUID,v_Avod.,adj_bfp_count_250,adj_bfp_count_250_500)

write_csv(tot_value, "./results/value_map/NS/mc_design.csv")


################################################################################
# Ontario
################################################################################

bf <- st_read("./Building_footprint/ON/on.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf_250, crs = 3348)


cen_da <- st_read("./Census/ON/census_on_da.shp")%>%
  st_transform(bf_250, crs = 3348)


bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  st_buffer(0)%>%
  count(GeoUID)%>%
  rename("bfp_count" = "n")

cen_da_ratio <- cen_da%>%
  as.data.frame()%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod.,-geometry, Dwllngs)%>%
  left_join(bf_cen_da)%>%
  mutate(ratio = Dwllngs/bfp_count)%>%
  as.data.frame()%>%
  select(GeoUID,ratio,-geometry)

# 250
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/ON/on_all_004_250m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/ON/census_on_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_250" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_250 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_250 = ratio*row_bfp_count_250)%>%
  drop_na()%>%
  select(GeoUID,adj_bfp_count_250,v_Avod.)%>%
  mutate_if(is.numeric, round)%>%
  as.data.frame()%>%
  select(-geometry)

# 500
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/ON/on_all_004_500m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/ON/census_on_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_500" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,Popultn,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_500 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_500 = ratio*row_bfp_count_500)%>%
  drop_na()%>%
  select(GeoUID,Popultn,v_Avod.,adj_bfp_count_500)%>%
  mutate_if(is.numeric, round)

tot_value <- value_500%>%
  left_join(value_250)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))%>%
  mutate(adj_bfp_count_250_500 = adj_bfp_count_500 - adj_bfp_count_250)%>%
  mutate(e_250 = e_250m/100)%>%
  mutate(e_500 = e_500m/100)%>%
  mutate(val_250 = e_250*v_Avod.*adj_bfp_count_250)%>%
  mutate(val_500 = e_500*v_Avod.*adj_bfp_count_250_500)%>%
  mutate(val_tot = val_250 + val_500)%>%
  mutate(val_tot_pp = val_tot/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)

on_250_bfp <- sum(tot_value$adj_bfp_count_250, na.rm = T)
on_250_val <- sum(tot_value$val_250, na.rm = T)
on_500_bfp <- sum(tot_value$adj_bfp_count_250_500, na.rm = T)
on_500_val <- sum(tot_value$val_500, na.rm = T)

cen_da <- st_read("./Census/ON/census_on_da.shp")%>%
  select(GeoUID)%>%
  left_join(tot_value)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

#csd
st_write(cen_da, "./results/value_map/ON/on_value_map.shp", delete_layer = T )

# save as csv for MC

tot_value <- tot_value%>%
  select(GeoUID,v_Avod.,adj_bfp_count_250,adj_bfp_count_250_500)

write_csv(tot_value, "./results/value_map/ON/mc_design.csv")

################################################################################
# Prince Edwards
################################################################################

bf <- st_read("./Building_footprint/PE/pe.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf_250, crs = 3348)


cen_da <- st_read("./Census/PE/census_pe_da.shp")%>%
  st_transform(bf_250, crs = 3348)


bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_count" = "n")

cen_da_ratio <- cen_da%>%
  as.data.frame()%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod.,-geometry, Dwllngs)%>%
  left_join(bf_cen_da)%>%
  mutate(ratio = Dwllngs/bfp_count)%>%
  as.data.frame()%>%
  select(GeoUID,ratio,-geometry)

# 250
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/PE/pe_all_004_250m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/PE/census_pe_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_250" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_250 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_250 = ratio*row_bfp_count_250)%>%
  drop_na()%>%
  select(GeoUID,adj_bfp_count_250,v_Avod.)%>%
  mutate_if(is.numeric, round)%>%
  as.data.frame()%>%
  select(-geometry)

# 500
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/PE/pe_all_004_500m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/PE/census_pe_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_500" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,Popultn,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_500 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_500 = ratio*row_bfp_count_500)%>%
  drop_na()%>%
  select(GeoUID,Popultn,v_Avod.,adj_bfp_count_500)%>%
  mutate_if(is.numeric, round)

tot_value <- value_500%>%
  left_join(value_250)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))%>%
  mutate(adj_bfp_count_250_500 = adj_bfp_count_500 - adj_bfp_count_250)%>%
  mutate(e_250 = e_250m/100)%>%
  mutate(e_500 = e_500m/100)%>%
  mutate(val_250 = e_250*v_Avod.*adj_bfp_count_250)%>%
  mutate(val_500 = e_500*v_Avod.*adj_bfp_count_250_500)%>%
  mutate(val_tot = val_250 + val_500)%>%
  mutate(val_tot_pp = val_tot/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)

pe_250_bfp <- sum(tot_value$adj_bfp_count_250, na.rm = T)
pe_250_val <- sum(tot_value$val_250, na.rm = T)
pe_500_bfp <- sum(tot_value$adj_bfp_count_250_500, na.rm = T)
pe_500_val <- sum(tot_value$val_500, na.rm = T)

cen_da <- st_read("./Census/PE/census_pe_da.shp")%>%
  select(GeoUID)%>%
  left_join(tot_value)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

#csd
st_write(cen_da, "./results/value_map/PE/pe_value_map.shp", delete_layer = T )

# save as csv for MC

tot_value <- tot_value%>%
  select(GeoUID,v_Avod.,adj_bfp_count_250,adj_bfp_count_250_500)

write_csv(tot_value, "./results/value_map/PE/mc_design.csv")

################################################################################
# Quebec
################################################################################

bf <- st_read("./Building_footprint/QC/qc.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf_250, crs = 3348)


cen_da <- st_read("./Census/QC/census_qc_da.shp")%>%
  st_transform(bf_250, crs = 3348)


bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_count" = "n")

cen_da_ratio <- cen_da%>%
  as.data.frame()%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod.,-geometry, Dwllngs)%>%
  left_join(bf_cen_da)%>%
  mutate(ratio = Dwllngs/bfp_count)%>%
  as.data.frame()%>%
  select(GeoUID,ratio,-geometry)

# 250
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/QC/qc_all_004_250m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/QC/census_qc_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_250" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_250 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_250 = ratio*row_bfp_count_250)%>%
  drop_na()%>%
  select(GeoUID,adj_bfp_count_250,v_Avod.)%>%
  mutate_if(is.numeric, round)%>%
  as.data.frame()%>%
  select(-geometry)

# 500
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/QC/qc_all_004_500m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/QC/census_qc_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_500" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,Popultn,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_500 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_500 = ratio*row_bfp_count_500)%>%
  drop_na()%>%
  select(GeoUID,Popultn,v_Avod.,adj_bfp_count_500)%>%
  mutate_if(is.numeric, round)

tot_value <- value_500%>%
  left_join(value_250)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))%>%
  mutate(adj_bfp_count_250_500 = adj_bfp_count_500 - adj_bfp_count_250)%>%
  mutate(e_250 = e_250m/100)%>%
  mutate(e_500 = e_500m/100)%>%
  mutate(val_250 = e_250*v_Avod.*adj_bfp_count_250)%>%
  mutate(val_500 = e_500*v_Avod.*adj_bfp_count_250_500)%>%
  mutate(val_tot = val_250 + val_500)%>%
  mutate(val_tot_pp = val_tot/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)

qc_250_bfp <- sum(tot_value$adj_bfp_count_250, na.rm = T)
qc_250_val <- sum(tot_value$val_250, na.rm = T)
qc_500_bfp <- sum(tot_value$adj_bfp_count_250_500, na.rm = T)
qc_500_val <- sum(tot_value$val_500, na.rm = T)

cen_da <- st_read("./Census/QC/census_qc_da.shp")%>%
  select(GeoUID)%>%
  left_join(tot_value)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

#csd
st_write(cen_da, "./results/value_map/QC/qc_value_map.shp", delete_layer = T )

# save as csv for MC

tot_value <- tot_value%>%
  select(GeoUID,v_Avod.,adj_bfp_count_250,adj_bfp_count_250_500)

write_csv(tot_value, "./results/value_map/QC/qc_design.csv")

################################################################################
# Saskatchewan
################################################################################

bf <- st_read("./Building_footprint/SK/sk.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf_250, crs = 3348)


cen_da <- st_read("./Census/SK/census_sk_da.shp")%>%
  st_transform(bf_250, crs = 3348)


bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_count" = "n")

cen_da_ratio <- cen_da%>%
  as.data.frame()%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod.,-geometry, Dwllngs)%>%
  left_join(bf_cen_da)%>%
  mutate(ratio = Dwllngs/bfp_count)%>%
  as.data.frame()%>%
  select(GeoUID,ratio,-geometry)

# 250
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/SK/sk_all_004_250m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/SK/census_sk_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_250" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_250 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_250 = ratio*row_bfp_count_250)%>%
  drop_na()%>%
  select(GeoUID,adj_bfp_count_250,v_Avod.)%>%
  mutate_if(is.numeric, round)%>%
  as.data.frame()%>%
  select(-geometry)

# 500
# load file with all bulding footprint within given buffer 
bf <- st_read("./Building_footprint/SK/sk_all_004_500m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(bf, crs = 3348)

cen_da <- st_read("./Census/SK/census_sk_da.shp")%>%
  st_transform(cen_da, crs = 3348)

bf_cen_da <- st_join(bf,cen_da, all = TRUE )

bf_cen_da <- bf_cen_da%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("row_bfp_count_500" = "n")%>%
  as.data.frame()%>%
  select(-geometry)

cen_da <- cen_da%>%
  select(GeoUID, CSD_UID, CD_UID,Popultn,v_Avod., Dwllngs)%>%
  subset(v_Avod. > 0)

value_500 <- cen_da%>%
  left_join(bf_cen_da)%>%
  left_join(cen_da_ratio)%>%
  mutate(adj_bfp_count_500 = ratio*row_bfp_count_500)%>%
  drop_na()%>%
  select(GeoUID,Popultn,v_Avod.,adj_bfp_count_500)%>%
  mutate_if(is.numeric, round)

tot_value <- value_500%>%
  left_join(value_250)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))%>%
  mutate(adj_bfp_count_250_500 = adj_bfp_count_500 - adj_bfp_count_250)%>%
  mutate(e_250 = e_250m/100)%>%
  mutate(e_500 = e_500m/100)%>%
  mutate(val_250 = e_250*v_Avod.*adj_bfp_count_250)%>%
  mutate(val_500 = e_500*v_Avod.*adj_bfp_count_250_500)%>%
  mutate(val_tot = val_250 + val_500)%>%
  mutate(val_tot_pp = val_tot/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)

sk_250_bfp <- sum(tot_value$adj_bfp_count_250, na.rm = T)
sk_250_val <- sum(tot_value$val_250, na.rm = T)
sk_500_bfp <- sum(tot_value$adj_bfp_count_250_500, na.rm = T)
sk_500_val <- sum(tot_value$val_500, na.rm = T)

cen_da <- st_read("./Census/SK/census_sk_da.shp")%>%
  select(GeoUID)%>%
  left_join(tot_value)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

#csd
st_write(cen_da, "./results/value_map/SK/sk_value_map.shp", delete_layer = T )

# save as csv for MC

tot_value <- tot_value%>%
  select(GeoUID,v_Avod.,adj_bfp_count_250,adj_bfp_count_250_500)

write_csv(tot_value, "./results/value_map/SK/mc_design.csv")


################################################################################
# Benefit table to mancuscript
# create a list 
list1 <- list("Province" = c("Saskatchewan", "Alberta", "British Columbia", "Manitoba","New Brunswick",
                             "Nova Scotia", "Ontario", "Quebec", "Prince Edwards"),
              "NUmbber of affected house within 250m" = c(sk_250_bfp, ab_250_bfp, bc_250_bfp, mb_250_bfp,
                                                          nb_250_bfp, ns_250_bfp, on_250_bfp,qc_250_bfp, 
                                                          pe_250_bfp),
              "NUmbber of affected house within 500m" = c(sk_500_bfp, ab_500_bfp, bc_500_bfp, mb_500_bfp,
                                                          nb_500_bfp, ns_500_bfp, on_500_bfp, qc_500_bfp,
                                                          pe_500_bfp),
              "Capitalized value within 250m" =c(sk_250_val, ab_250_val, bc_250_val, mb_250_val,
                                                 nb_250_val, ns_250_val, on_250_val,qc_250_val,
                                                 pe_250_val),
              "Capitalized value within 500m" =c(sk_500_val, ab_500_val, bc_500_val, mb_500_val,
                                                 nb_500_val, ns_500_val, on_500_val,qc_500_val,
                                                 pe_500_val)
)
result <- as.data.frame(list1)%>%
  mutate_if(is.numeric, round)



library(xtable)
print(xtable(result, type = "latex"), file = "./results/Tables/Benefits_table.tex")







