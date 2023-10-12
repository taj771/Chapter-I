# clear memory
rm(list = ls())

## Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  tidyverse,
  sf
)

# Parameters
# elasticity 
e_250m <- 0.216
e_500m <- 0.062
# bilding foof print area definitions
ub <- 1000
lb <- 50

# water qualiy change
wq_change <- 1/100


# building foot print
df_bf <- st_read("./Building_footprint/CAN/can.shp")%>%
  st_transform(df_bf, crs = 3348)

# census
df_cen <- st_read("./Census/CAN/can.shp")%>%
  st_transform(df_cen, crs = 3348)


# filter polygonns only that predefined area size
df_bf_filter <- df_bf%>%
  subset(areasqm > lb & areasqm < ub)
# spatial join filterbfp and census DA
bf_cen_da <- st_join(df_bf_filter,df_cen, all = TRUE )

# count bfp at each DA
bf_cen_da_count <- bf_cen_da%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_count" = "n")

write.csv(bf_cen_da_count, "././Census/CAN/bf_count_da.csv")


# 250 - bfp within 250m boundary
df_bf_250 <- st_read("./Building_footprint/CAN/can_250.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(df_bf_250, crs = 3348)

bf_cen_da_250 <- st_join(df_bf_250,df_cen, all = TRUE)

bf_cen_da_count_250 <- bf_cen_da_250%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_250" = "n")

write.csv(bf_cen_da_count_250,"./Census/CAN/can_250_bf_count.csv", row.names=FALSE)


# 500 - bfp within 500m boundary
df_bf_500 <- st_read("./Building_footprint/CAN/can_500.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(df_bf_500, crs = 3348)

bf_cen_da_500 <- st_join(df_bf_500,df_cen, all = TRUE )

bf_cen_da_count_500 <- bf_cen_da_500%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_500" = "n")

write.csv(bf_cen_da_count_500,"./Census/CAN/can_500_bf_count.csv",row.names=FALSE)

###############################################################################

# census
df_cen <- st_read("./Census/CAN/can.shp")%>%
  st_transform(df_cen, crs = 3348)
df_cen$GeoUID <- as.integer(df_cen$GeoUID)

bf_cen_da_count_250 <- read.csv("./Census/CAN/can_250_bf_count.csv")
bf_cen_da_count_500 <- read_csv("./Census/CAN/can_500_bf_count.csv")
bf_cen_da_count <- read_csv("./Census/CAN/bf_count_da.csv")


df_cen_value <- df_cen%>%
  left_join(bf_cen_da_count_250)%>%
  left_join(bf_cen_da_count_500)%>%
  filter(!if_all(c(bfp_250, bfp_500), is.na))%>%
  drop_na(v_Avod_)%>%
  filter(v_Avod_!=0)%>%
  select(GeoUID,Type,CD_UID,Dwllngs,Popultn,CSD_UID,v_Avod_,province,bfp_250,bfp_500)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))%>%
  mutate(bfp_250_500 = bfp_500-bfp_250)%>%
  relocate(bfp_250_500, .before=geometry)%>%
  left_join(bf_cen_da_count)%>%
  mutate(ratio = Dwllngs/bfp_count)%>%
  relocate(ratio, .before=geometry)%>%
  mutate(adj_bfp_250 = bfp_250*ratio)%>%
  mutate(adj_bfp_250_500 = bfp_250_500*ratio)%>%
  mutate_at(vars(adj_bfp_250, adj_bfp_250_500), list(~ round(., 0)))%>%
  mutate(e_250 = e_250m)%>%
  mutate(e_500 = e_500m)%>%
  mutate(d_wq = wq_change )%>%
  mutate(val_wf = e_250*adj_bfp_250*v_Avod_*d_wq)%>%
  mutate(val_nwf = e_500*adj_bfp_250_500*v_Avod_*d_wq)%>%
  mutate(val_tot = val_wf+val_nwf)%>%
  mutate(val_wf_pp = val_wf/Popultn)%>%
  mutate(val_nwf_pp = val_nwf/Popultn)%>%
  mutate(val_tot_pp = val_wf_pp + val_nwf_pp )%>%
  select(GeoUID,val_wf,val_nwf,val_tot,val_wf_pp,val_nwf_pp,val_tot_pp,province,ratio,adj_bfp_250,adj_bfp_250_500)%>%
  rename("modbfp_250" = "adj_bfp_250" )%>%
  rename("modbfp_500" = "adj_bfp_250_500" )
  
  
df_cen_value$GeoUID <- as.character(df_cen_value$GeoUID)


st_write(df_cen_value, "./results/value_map/CAN/can.shp", delete_layer = T)


#AB
df <- df_cen_value%>%
  subset(province == "AB")%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- st_read("./Census/AB/census_ab_da.shp")%>%
  select(GeoUID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/AB/ab_value_map.shp", delete_layer = T)


#BC
df <- df_cen_value%>%
  subset(province == "BC")%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- st_read("./Census/BC/census_bc_da.shp")%>%
  select(GeoUID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/BC/bc_value_map.shp", delete_layer = T)

#MB
df <- df_cen_value%>%
  subset(province == "MB")%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- st_read("./Census/MB/census_mb_da.shp")%>%
  select(GeoUID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/MB/mb_value_map.shp", delete_layer = T)

#NB
df <- df_cen_value%>%
  subset(province == "NB")%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- st_read("./Census/NB/census_nb_da.shp")%>%
  select(GeoUID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/NB/nb_value_map.shp", delete_layer = T)

#NL
df <- df_cen_value%>%
  subset(province == "NL")%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- st_read("./Census/NL/census_nl_da.shp")%>%
  select(GeoUID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/NL/nl_value_map.shp", delete_layer = T)

#NS
df <- df_cen_value%>%
  subset(province == "NS")%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- st_read("./Census/NS/census_ns_da.shp")%>%
  select(GeoUID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/NS/ns_value_map.shp", delete_layer = T)

#ON
df <- df_cen_value%>%
  subset(province == "ON")%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- st_read("./Census/ON/census_on_da.shp")%>%
  select(GeoUID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/ON/on_value_map.shp", delete_layer = T)

#PE
df <- df_cen_value%>%
  subset(province == "PE")%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- st_read("./Census/PE/census_pe_da.shp")%>%
  select(GeoUID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/PE/pe_value_map.shp", delete_layer = T)

#QC
df <- df_cen_value%>%
  subset(province == "QC")%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- st_read("./Census/QC/census_qc_da.shp")%>%
  select(GeoUID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/QC/qc_value_map.shp", delete_layer = T)

#SK
df <- df_cen_value%>%
  subset(province == "SK")%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- st_read("./Census/SK/census_sk_da.shp")%>%
  select(GeoUID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/SK/sk_value_map.shp", delete_layer = T)







