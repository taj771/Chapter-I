# clear memory
rm(list = ls())

## Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  tidyverse,
  sf,
  hrbrthemes,
  viridis,
  ggpubr
)

# Parameters
# elasticity 
e_250m <- 0.235/100
e_750m <- 0.040/100
# bilding foof print area definitions
ub <- 2000
lb <- 40

# water qualiy change
#wq_change <- 1/100


# building foot print - all (not only within buffers - this will use to calculate the ratio between bfp count and census count)
df_bf <- st_read("./Building_footprint/CAN/can_all.shp")%>%
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

write.csv(bf_cen_da_count, "./Census/CAN/bf_count_da.csv", row.names = F)




# 200 - bfp within 200m boundary
df_bf_200 <- st_read("./Building_footprint/CAN/can_all_004_200m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(df_bf_200, crs = 3348)

bf_cen_da_200 <- st_join(df_bf_200,df_cen, all = TRUE)

bf_cen_da_count_200 <- bf_cen_da_200%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_200" = "n")

write.csv(bf_cen_da_count_200,"./Census/CAN/can_200_bf_count.csv", row.names=FALSE)


# 600 - bfp within 600m boundary
df_bf_600 <- st_read("./Building_footprint/CAN/can_all_004_600m.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(df_bf_600, crs = 3348)

bf_cen_da_600 <- st_join(df_bf_600,df_cen, all = TRUE )

bf_cen_da_count_600 <- bf_cen_da_600%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_600" = "n")

write.csv(bf_cen_da_count_600,"./Census/CAN/can_600_bf_count.csv",row.names=FALSE)

###############################################################################

# census
df_cen <- st_read("./Census/CAN/can.shp")%>%
  st_transform(df_cen, crs = 3348)
df_cen$GeoUID <- as.integer(df_cen$GeoUID)

bf_cen_da_count_200 <- read.csv("./Census/CAN/can_200_bf_count.csv")
bf_cen_da_count_600 <- read_csv("./Census/CAN/can_600_bf_count.csv")
bf_cen_da_count <- read_csv("./Census/CAN/bf_count_da.csv")
cen_can <- st_read("./Census/CEN/CAN/lcsd000b21a_e.shp")%>%
  rename("CSD_UID" = "CSDUID")


df_cen_value <- df_cen%>%
  left_join(bf_cen_da_count_200)%>%
  left_join(bf_cen_da_count_600)%>%
  filter(!if_all(c(bfp_200, bfp_600), is.na))%>%
  drop_na(AvgDwlv)%>%
  filter(AvgDwlv!=0)%>%
  select(GeoUID,Type,CD_UID,Dwllngs,Popultn,CSD_UID,AvgDwlv,prov,bfp_200,bfp_600)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))%>%
  mutate(bfp_200_600 = bfp_600-bfp_200)%>%
  relocate(bfp_200_600, .before=geometry)%>%
  left_join(bf_cen_da_count)%>%
  mutate(ratio = Dwllngs/bfp_count)%>%
  relocate(ratio, .before=geometry)%>%
  mutate(adj_bfp_200 = bfp_200*ratio)%>%
  mutate(adj_bfp_200_600 = bfp_200_600*ratio)%>%
  mutate_at(vars(adj_bfp_200, adj_bfp_200_600), list(~ round(., 0)))%>%
  mutate(e_200 = e_200m)%>%
  mutate(e_600 = e_600m)%>%
  mutate(val_wf = e_200*adj_bfp_200*AvgDwlv)%>%
  mutate(val_nwf = e_600*adj_bfp_200_600*AvgDwlv)%>%
  mutate(val_tot = val_wf+val_nwf)%>%
  mutate(val_wf_pp = val_wf/Popultn)%>%
  mutate(val_nwf_pp = val_nwf/Popultn)%>%
  mutate(val_tot_pp = val_wf_pp + val_nwf_pp )%>%
  select(GeoUID,val_wf,val_nwf,val_tot,val_wf_pp,val_nwf_pp,val_tot_pp,prov,ratio,adj_bfp_200,adj_bfp_200_600,CSD_UID)%>%
  rename("modbfp_200" = "adj_bfp_200" )%>%
  rename("modbfp_600" = "adj_bfp_200_600" )
  
  
df_cen_value$GeoUID <- as.character(df_cen_value$GeoUID)


st_write(df_cen_value, "./results/value_map/CAN/can.shp", delete_layer = T)


#AB
df <- df_cen_value%>%
  subset(prov == "AB")
ab_200_bfp <- sum(df$modbfp_200)
ab_200_val <- sum(df$val_wf/1000000)
ab_600_bfp <- sum(df$modbfp_600)
ab_600_val <- sum(df$val_nwf/1000000)
ab_tot_pp <- mean(df$val_tot_pp)
ab_tot <- sum(df$val_tot/1000000)


df1 <- df%>%
  subset(val_wf > 0)
df2 <- df%>%
  subset(val_nwf > 0)
df3 <- df%>%
  subset(val_tot_pp > 0)

ab_200_val_lb <- min(df1$val_wf/1000000)
ab_200_val_ub <- max(df1$val_wf/1000000)

ab_600_val_lb <- min(df2$val_nwf/1000000)
ab_600_val_ub <- max(df2$val_nwf/1000000)

ab_tot_pp_lb <- min(df3$val_tot_pp)
ab_tot_pp_ub <- max(df3$val_tot_pp)

max(df$val_tot)/1000000



df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_200 = sum(modbfp_200),
            modbfp_600 = sum(modbfp_600))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 48)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/AB/ab_value_map.shp", delete_layer = T)

#BC
df <- df_cen_value%>%
  subset(prov == "BC")
bc_200_bfp <- sum(df$modbfp_200)
bc_200_val <- sum(df$val_wf/1000000)
bc_600_bfp <- sum(df$modbfp_600)
bc_600_val <- sum(df$val_nwf/1000000)
bc_tot_pp <- mean(df$val_tot_pp)
bc_tot <- sum(df$val_tot/1000000)

df1 <- df%>%
  subset(val_wf > 0)
df2 <- df%>%
  subset(val_nwf > 0)
df3 <- df%>%
  subset(val_tot_pp > 0)

bc_200_val_lb <- min(df1$val_wf/1000000)
bc_200_val_ub <- max(df1$val_wf/1000000)

bc_600_val_lb <- min(df2$val_nwf/1000000)
bc_600_val_ub <- max(df2$val_nwf/1000000)

bc_tot_pp_lb <- min(df3$val_tot_pp)
bc_tot_pp_ub <- max(df3$val_tot_pp)

max(df$val_tot)/1000000



df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_200 = sum(modbfp_200),
            modbfp_600 = sum(modbfp_600))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 59)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

st_write(cen_da, "./results/value_map/BC/bc_value_map.shp", delete_layer = T)

#MB
df <- df_cen_value%>%
  subset(prov == "MB")
mb_200_bfp <- sum(df$modbfp_200)
mb_200_val <- sum(df$val_wf/1000000)%>%
  round(., digits = 0)
mb_600_bfp <- sum(df$modbfp_600)
mb_600_val <- sum(df$val_nwf/1000000)%>%
  round(., digits = 0)
mb_tot_pp <- mean(df$val_tot_pp)%>%
  round(., digits = 0)
mb_tot <- sum(df$val_tot/1000000)%>%
  round(., digits = 0)

df1 <- df%>%
  subset(val_wf > 0)
df2 <- df%>%
  subset(val_nwf > 0)
df3 <- df%>%
  subset(val_tot_pp > 0)

mb_200_val_lb <- min(df1$val_wf/1000000)
mb_200_val_ub <- max(df1$val_wf/1000000)

mb_600_val_lb <- min(df2$val_nwf/1000000)
mb_600_val_ub <- max(df2$val_nwf/1000000)

mb_tot_pp_lb <- min(df3$val_tot_pp)
mb_tot_pp_ub <- max(df3$val_tot_pp)


df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_200 = sum(modbfp_200),
            modbfp_600 = sum(modbfp_600))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 46)%>%
  select(CSD_UID)%>% # get CSD level
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/MB/mb_value_map.shp", delete_layer = T)



#NB
df <- df_cen_value%>%
  subset(prov == "NB")
nb_200_bfp <- sum(df$modbfp_200)
nb_200_val <- sum(df$val_wf/1000000)%>%
  round(., digits = 0)
nb_600_bfp <- sum(df$modbfp_600)
nb_600_val <- sum(df$val_nwf/1000000)%>%
  round(., digits = 0)
nb_tot_pp <- mean(df$val_tot_pp)%>%
  round(., digits = 0)
nb_tot <- sum(df$val_tot/1000000)%>%
  round(., digits = 0)

df1 <- df%>%
  subset(val_wf > 0)
df2 <- df%>%
  subset(val_nwf > 0)
df3 <- df%>%
  subset(val_tot_pp > 0)

nb_200_val_lb <- min(df1$val_wf/1000000)
nb_200_val_ub <- max(df1$val_wf/1000000)

nb_600_val_lb <- min(df2$val_nwf/1000000)
nb_600_val_ub <- max(df2$val_nwf/1000000)

nb_tot_pp_lb <- min(df3$val_tot_pp)
nb_tot_pp_ub <- max(df3$val_tot_pp)



df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_200 = sum(modbfp_200),
            modbfp_600 = sum(modbfp_600))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 13)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

st_write(cen_da, "./results/value_map/NB/nb_value_map.shp", delete_layer = T)


#NL
df <- df_cen_value%>%
  subset(prov == "NL")
nl_200_bfp <- sum(df$modbfp_200)
nl_200_val <- sum(df$val_wf/1000000)
nl_600_bfp <- sum(df$modbfp_600)
nl_600_val <- sum(df$val_nwf/1000000)
nl_tot_pp <- mean(df$val_tot_pp)
nl_tot <- sum(df$val_tot/1000000)

df1 <- df%>%
  subset(val_wf > 0)
df2 <- df%>%
  subset(val_nwf > 0)
df3 <- df%>%
  subset(val_tot_pp > 0)

nl_200_val_lb <- min(df1$val_wf/1000000)
nl_200_val_ub <- max(df1$val_wf/1000000)

nl_600_val_lb <- min(df2$val_nwf/1000000)
nl_600_val_ub <- max(df2$val_nwf/1000000)

nl_tot_pp_lb <- min(df3$val_tot_pp)
nl_tot_pp_ub <- max(df3$val_tot_pp)




df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_200 = sum(modbfp_200),
            modbfp_600 = sum(modbfp_600))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 10)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/NL/nl_value_map.shp", delete_layer = T)

#NS
df <- df_cen_value%>%
  subset(prov == "NS")
ns_200_bfp <- sum(df$modbfp_200)
ns_200_val <- sum(df$val_wf/1000000)
ns_600_bfp <- sum(df$modbfp_600)
ns_600_val <- sum(df$val_nwf/1000000)
ns_tot_pp <- mean(df$val_tot_pp)
ns_tot <- sum(df$val_tot/1000000)


df1 <- df%>%
  subset(val_wf > 0)
df2 <- df%>%
  subset(val_nwf > 0)
df3 <- df%>%
  subset(val_tot_pp > 0)

ns_200_val_lb <- min(df1$val_wf/1000000)
ns_200_val_ub <- max(df1$val_wf/1000000)

ns_600_val_lb <- min(df2$val_nwf/1000000)
ns_600_val_ub <- max(df2$val_nwf/1000000)

ns_tot_pp_lb <- min(df3$val_tot_pp)
ns_tot_pp_ub <- max(df3$val_tot_pp)


df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_200 = sum(modbfp_200),
            modbfp_600 = sum(modbfp_600))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 12)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/NS/ns_value_map.shp", delete_layer = T)



#ON
df <- df_cen_value%>%
  subset(prov == "ON")
on_200_bfp <- sum(df$modbfp_200)
on_200_val <- sum(df$val_wf/1000000)
on_600_bfp <- sum(df$modbfp_600)
on_600_val <- sum(df$val_nwf/1000000)
on_tot_pp <- mean(df$val_tot_pp)
on_tot <- sum(df$val_tot/1000000)

df1 <- df%>%
  subset(val_wf > 0)
df2 <- df%>%
  subset(val_nwf > 0)
df3 <- df%>%
  subset(val_tot_pp > 0)

on_200_val_lb <- min(df1$val_wf/1000000)
on_200_val_ub <- max(df1$val_wf/1000000)

on_600_val_lb <- min(df2$val_nwf/1000000)
on_600_val_ub <- max(df2$val_nwf/1000000)

on_tot_pp_lb <- min(df3$val_tot_pp)
on_tot_pp_ub <- max(df3$val_tot_pp)

max(df$val_tot)/1000000

df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_200 = sum(modbfp_200),
            modbfp_600 = sum(modbfp_600))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 35)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

st_write(cen_da, "./results/value_map/ON/on_value_map.shp", delete_layer = T)


#PE
df <- df_cen_value%>%
  subset(prov == "PE")
pe_200_bfp <- sum(df$modbfp_200)
pe_200_val <- sum(df$val_wf/1000000)
pe_600_bfp <- sum(df$modbfp_600)
pe_600_val <- sum(df$val_nwf/1000000)
pe_tot_pp <- mean(df$val_tot_pp)
pe_tot <- sum(df$val_tot/1000000)

df1 <- df%>%
  subset(val_wf > 0)
df2 <- df%>%
  subset(val_nwf > 0)
df3 <- df%>%
  subset(val_tot_pp > 0)

pe_200_val_lb <- min(df1$val_wf/1000000)
pe_200_val_ub <- max(df1$val_wf/1000000)

pe_600_val_lb <- min(df2$val_nwf/1000000)
pe_600_val_ub <- max(df2$val_nwf/1000000)

pe_tot_pp_lb <- min(df3$val_tot_pp)
pe_tot_pp_ub <- max(df3$val_tot_pp)




df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_200 = sum(modbfp_200),
            modbfp_600 = sum(modbfp_600))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 11)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/PE/pe_value_map.shp", delete_layer = T)



#QC
df <- df_cen_value%>%
  subset(prov == "QC")
qc_200_bfp <- sum(df$modbfp_200)
qc_200_val <- sum(df$val_wf/1000000)
qc_600_bfp <- sum(df$modbfp_600)
qc_600_val <- sum(df$val_nwf/1000000)
qc_tot_pp <- mean(df$val_tot_pp)
qc_tot <- sum(df$val_tot/1000000)

df1 <- df%>%
  subset(val_wf > 0)
df2 <- df%>%
  subset(val_nwf > 0)
df3 <- df%>%
  subset(val_tot_pp > 0)

qc_200_val_lb <- min(df1$val_wf/1000000)
qc_200_val_ub <- max(df1$val_wf/1000000)

qc_600_val_lb <- min(df2$val_nwf/1000000)
qc_600_val_ub <- max(df2$val_nwf/1000000)

qc_tot_pp_lb <- min(df3$val_tot_pp)
qc_tot_pp_ub <- max(df3$val_tot_pp)





df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_200 = sum(modbfp_200),
            modbfp_600 = sum(modbfp_600))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 24)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

st_write(cen_da, "./results/value_map/QC/qc_value_map.shp", delete_layer = T)



#SK
df <- df_cen_value%>%
  subset(prov == "SK")
sk_200_bfp <- sum(df$modbfp_200)
sk_200_val <- sum(df$val_wf/1000000)
sk_600_bfp <- sum(df$modbfp_600)
sk_600_val <- sum(df$val_nwf/1000000)
sk_tot_pp <- mean(df$val_tot_pp)
sk_tot <- sum(df$val_tot/1000000)

df1 <- df%>%
  subset(val_wf > 0)
df2 <- df%>%
  subset(val_nwf > 0)
df3 <- df%>%
  subset(val_tot_pp > 0)

sk_200_val_lb <- min(df1$val_wf/1000000)
sk_200_val_ub <- max(df1$val_wf/1000000)

sk_600_val_lb <- min(df2$val_nwf/1000000)
sk_600_val_ub <- max(df2$val_nwf/1000000)

sk_tot_pp_lb <- min(df3$val_tot_pp)
sk_tot_pp_ub <- max(df3$val_tot_pp)



df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_200 = sum(modbfp_200),
            modbfp_600 = sum(modbfp_600))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 47)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

st_write(cen_da, "./results/value_map/SK/sk_value_map.shp", delete_layer = T)


###########################################################################
# value summary table


list1 <- list("Province" = c("Saskatchewan","Alberta", "British Columbia", "Manitoba","New Brunswick",
                             "Nova Scotia", "Ontario", "Quebec", "Prince Edwards"),
              "Total affected houses" = c(sk_200_bfp+sk_600_bfp, ab_200_bfp+ab_600_bfp, bc_200_bfp+bc_600_bfp,
                                          mb_200_bfp+mb_600_bfp,nb_200_bfp+nb_600_bfp, ns_200_bfp+ns_600_bfp,
                                          on_200_bfp+on_600_bfp, qc_200_bfp+qc_600_bfp, pe_200_bfp+pe_600_bfp),
              "Capitalized value within 200m" =c(sk_200_val, ab_200_val, bc_200_val, mb_200_val,
                                                 nb_200_val, ns_200_val, on_200_val,qc_200_val,
                                                 pe_200_val),
              "Capitalized value within 600m" =c(sk_600_val, ab_600_val, bc_600_val, mb_600_val,
                                                 nb_600_val, ns_600_val, on_600_val,qc_600_val,
                                                 pe_600_val),
              "Cumilative Total" = c(sk_200_val+sk_600_val, ab_200_val+ab_600_val, bc_200_val+bc_600_val, mb_200_val+mb_600_val,
                                    nb_200_val+nb_600_val, ns_200_val+ns_600_val, on_200_val+on_600_val,qc_200_val+qc_600_val,
                                    pe_200_val),
              "Value per household" =c(sk_tot_pp, ab_tot_pp, bc_tot_pp, mb_tot_pp,
                                                 nb_tot_pp, ns_tot_pp, on_tot_pp,qc_tot_pp,
                                                 pe_tot_pp)
)
result <- as.data.frame(list1)



library(xtable)
print(xtable(result, type = "latex"), file = "./results/Tables/Benefits_table.tex")

####################################################################
# graph
# all in one - violine plot - total value
p <- df_cen_value%>%
  ggplot( aes(x=prov, y=val_tot, fill=prov)) +
  #geom_violin(width=0.9) +
  geom_boxplot(width=0.25, color="grey", alpha=0.9) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ylim(-10,500)+
  scale_y_continuous(limits = c(1000, 100000))+
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ Total value CSD")

p

# all in one - violine plot - total value pp
p <- df_cen_value%>%
  ggplot( aes(x=prov, y=val_tot_pp, fill=prov)) +
  #geom_violin(width=1.1,alpha=0.3) +
  geom_boxplot(width=0.2, color="grey",alpha=0.9) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ylim(0,250)+
  #scale_y_continuous(limits = c(100, 100000))+
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ Tota value per-household CSD")

p


