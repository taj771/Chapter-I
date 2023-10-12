# clear memory
rm(list = ls())


# load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  sf,
  dplyr,
  xtable
)


# Random effect model unit estimations

e_250m <- 0.216
e_500m <- 0.062

################################################################################
# Saskatchewan
bfp_0_250m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/SK/bfp_count_mode_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  select(count_250,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)

bfp_250_500m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/SK/bfp_count_98_4454sqm_500m.shp")%>%
  left_join(bfp_0_250m, by = c('GeoUID'))%>%
  rename("count_500" = "Join_Count")%>%
  mutate(count_250_500 = count_500 - count_250)


st_write(bfp_250_500m, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/SK/bfp_count_98_4454sqm_250m_500m.shp", delete_layer = T)


# layer with count of all bfp data within each census DA, (98-4454sqm)
bfp_at_cen_da <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/SK/bfp_count_98_4454_at_each_cen_da.shp")%>%
  mutate(ratio_da_bfp = Dwllngs/Join_Count)%>%
  select(ratio_da_bfp,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)


bfp_0_250m_sk <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/SK/bfp_count_98_4454sqm_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_250 = ratio_da_bfp*count_250)%>%
  #mutate(wf_premium = 0.0480511*v_Avod_)%>%
  #mutate(v_Avod_adjusted = v_Avod_ + wf_premium)%>%
  mutate(elas_250m = e_250m/100)%>%
  mutate(value_250m = elas_250m*bfp_count_250*v_Avod_)

st_write(bfp_0_250m_sk, "./results/value_map/SK/bfp_count_cen_da_250m_value.shp",delete_layer = T)


sk_250_bfp <- sum(bfp_0_250m_sk$bfp_count_250, na.rm = T)
sk_250_val <- sum(bfp_0_250m_sk$value_250m, na.rm = T)

bfp_250_500m_sk <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/SK/bfp_count_98_4454sqm_250m_500m.shp")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_500 = ratio_da_bfp*c_250_5)%>%
  mutate(elas_250m_500m = e_500m/100)%>%
  mutate(value_500m = elas_250m_500m*bfp_count_500*v_Avod_)


st_write(bfp_250_500m_sk, "./results/value_map/SK/bfp_count_cen_da_250m_500m_value.shp",delete_layer = T)


sk_500_bfp <- sum(bfp_250_500m_sk$bfp_count_500, na.rm = T)
sk_500_val <- sum(bfp_250_500m_sk$value_500m, na.rm = T)

# csd
bfp_0_250m_csd <- bfp_0_250m_sk%>%
  select(CSD_UID,value_250m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd
bfp_0_250m_cd <- bfp_0_250m_sk%>%
  select(CD_UID,value_250m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd
bfp_250_500m_csd <- bfp_250_500m_sk%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CSD_UID,value_500m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd

bfp_250_500m_cd <- bfp_250_500m_sk%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CD_UID,value_500m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd map
df_value_map <- bfp_0_250m_csd%>%
  left_join(bfp_250_500m_csd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CSD_UID, sum_pop)

# cd map
df_value_map_cd <- bfp_0_250m_cd%>%
  left_join(bfp_250_500m_cd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CD_UID, sum_pop)


sum(df_value_map_cd$Total_value)
sum(df_value_map$Total_value)

#csd
st_write(df_value_map, "./results/value_map/SK/sk_value_map.shp", delete_layer = T )
#cd
st_write(df_value_map_cd, "./results/value_map/SK/sk_value_map_cd.shp", delete_layer = T )


##############################################################################

# Alberta
bfp_0_250m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_mode_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  select(count_250,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)

bfp_250_500m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_mode_500m.shp")%>%
  left_join(bfp_0_250m, by = c('GeoUID'))%>%
  rename("count_500" = "Join_Count")%>%
  mutate(count_250_500 = count_500 - count_250)


st_write(bfp_250_500m, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_mode_250m_500m.shp", delete_layer = T)


# layer with count of all bfp data within each census DA, (50-1000sqm)
bfp_at_cen_da <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_50_1000sqm_at_each_cen_da.shp")%>%
  mutate(ratio_da_bfp = Dwllngs/Join_Count)%>%
  select(ratio_da_bfp,GeoUID,Dwllngs,Join_Count)%>%
  as.data.frame()%>%
  select(-geometry)

bfp_0_250m_ab <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_mode_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_250 = ratio_da_bfp*count_250)%>%
  #mutate(wf_premium = 0.0480511*v_Avod_)%>%
  #mutate(v_Avod_adjusted = v_Avod_ + wf_premium)%>%
  mutate(elas_250m = e_250m/100)%>%
  mutate(value_250m = elas_250m*bfp_count_250*v_Avod_)


st_write(bfp_0_250m_ab, "./results/value_map/AB/bfp_count_cen_da_250m_value.shp",delete_layer = T)


ab_250_bfp <- sum(bfp_0_250m_ab$bfp_count_250, na.rm = T)
ab_250_val <- sum(bfp_0_250m_ab$value_250m, na.rm = T)

bfp_250_500m_ab <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_mode_250m_500m.shp")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_500 = ratio_da_bfp*c_250_5)%>%
  mutate(elas_250m_500m = e_500m/100)%>%
  mutate(value_500m = elas_250m_500m*bfp_count_500*v_Avod_)


st_write(bfp_250_500m_ab, "./results/value_map/AB/bfp_count_cen_da_250m_500m_value.shp",delete_layer = T)


ab_500_bfp <- sum(bfp_250_500m_ab$bfp_count_500, na.rm = T)
ab_500_val <- sum(bfp_250_500m_ab$value_500m, na.rm = T)

# csd
bfp_0_250m_csd <- bfp_0_250m_ab%>%
  select(CSD_UID,value_250m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd
bfp_0_250m_cd <- bfp_0_250m_ab%>%
  select(CD_UID,value_250m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd
bfp_250_500m_csd <- bfp_250_500m_ab%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CSD_UID,value_500m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd

bfp_250_500m_cd <- bfp_250_500m_ab%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CD_UID,value_500m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd map
df_value_map <- bfp_0_250m_csd%>%
  left_join(bfp_250_500m_csd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CSD_UID, sum_pop)

# cd map
df_value_map_cd <- bfp_0_250m_cd%>%
  left_join(bfp_250_500m_cd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CD_UID, sum_pop)


sum(df_value_map_cd$Total_value)
sum(df_value_map$Total_value)

#csd
st_write(df_value_map, "./results/value_map/AB/ab_value_map.shp", delete_layer = T )
#cd
st_write(df_value_map_cd, "./results/value_map/AB/ab_value_map_cd.shp", delete_layer = T )

################################################################################

# British Colombia
bfp_0_250m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/BC/bfp_count_98_4454sqm_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  select(count_250,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)

bfp_250_500m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/BC/bfp_count_98_4454sqm_500m.shp")%>%
  left_join(bfp_0_250m, by = c('GeoUID'))%>%
  rename("count_500" = "Join_Count")%>%
  mutate(count_250_500 = count_500 - count_250)


st_write(bfp_250_500m, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/BC/bfp_count_98_4454sqm_250m_500m.shp", delete_layer = T)


# layer with count of all bfp data within each census DA, (98-4454sqm)
bfp_at_cen_da <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/BC/bfp_count_98_4454_at_each_cen_da.shp")%>%
  mutate(ratio_da_bfp = Dwllngs/Join_Count)%>%
  select(ratio_da_bfp,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)

bfp_0_250m_bc <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/BC/bfp_count_98_4454sqm_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_250 = ratio_da_bfp*count_250)%>%
  #mutate(wf_premium = 0.0480511*v_Avod_)%>%
  #mutate(v_Avod_adjusted = v_Avod_ + wf_premium)%>%
  mutate(elas_250m = e_250m/100)%>%
  mutate(value_250m = elas_250m*bfp_count_250*v_Avod_)


st_write(bfp_0_250m_bc, "./results/value_map/BC/bfp_count_cen_da_250m_value.shp",delete_layer = T)


bc_250_bfp <- sum(bfp_0_250m_bc$bfp_count_250, na.rm = T)
bc_250_val <- sum(bfp_0_250m_bc$value_250m, na.rm = T)

bfp_250_500m_bc <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/BC/bfp_count_98_4454sqm_250m_500m.shp")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_500 = ratio_da_bfp*c_250_5)%>%
  mutate(elas_250m_500m = e_500m/100)%>%
  mutate(value_500m = elas_250m_500m*bfp_count_500*v_Avod_)


st_write(bfp_250_500m_bc, "./results/value_map/BC/bfp_count_cen_da_250m_500m_value.shp",delete_layer = T)


bc_500_bfp <- sum(bfp_250_500m_bc$bfp_count_500, na.rm = T)
bc_500_val <- sum(bfp_250_500m_bc$value_500m, na.rm = T)

# csd
bfp_0_250m_csd <- bfp_0_250m_bc%>%
  select(CSD_UID,value_250m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd
bfp_0_250m_cd <- bfp_0_250m_bc%>%
  select(CD_UID,value_250m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd
bfp_250_500m_csd <- bfp_250_500m_bc%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CSD_UID,value_500m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd

bfp_250_500m_cd <- bfp_250_500m_bc%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CD_UID,value_500m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd map
df_value_map <- bfp_0_250m_csd%>%
  left_join(bfp_250_500m_csd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CSD_UID, sum_pop)

# cd map
df_value_map_cd <- bfp_0_250m_cd%>%
  left_join(bfp_250_500m_cd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CD_UID, sum_pop)


sum(df_value_map_cd$Total_value)
sum(df_value_map$Total_value)

#csd
st_write(df_value_map, "./results/value_map/BC/bc_value_map.shp", delete_layer = T )
#cd
st_write(df_value_map_cd, "./results/value_map/BC/bc_value_map_cd.shp", delete_layer = T )

################################################################################

# Manitoba
bfp_0_250m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/MB/bfp_count_98_4454sqm_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  select(count_250,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)

bfp_250_500m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/MB/bfp_count_98_4454sqm_500m.shp")%>%
  left_join(bfp_0_250m, by = c('GeoUID'))%>%
  rename("count_500" = "Join_Count")%>%
  mutate(count_250_500 = count_500 - count_250)


st_write(bfp_250_500m, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/MB/bfp_count_98_4454sqm_250m_500m.shp", delete_layer = T)


# layer with count of all bfp data within each census DA, (98-4454sqm)
bfp_at_cen_da <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/MB/bfp_count_98_4454_at_each_cen_da.shp")%>%
  mutate(ratio_da_bfp = Dwllngs/Join_Count)%>%
  select(ratio_da_bfp,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)

bfp_0_250m_mb <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/MB/bfp_count_98_4454sqm_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_250 = ratio_da_bfp*count_250)%>%
  #mutate(wf_premium = 0.0480511*v_Avod_)%>%
  #mutate(v_Avod_adjusted = v_Avod_ + wf_premium)%>%
  mutate(elas_250m = e_250m/100)%>%
  mutate(value_250m = elas_250m*bfp_count_250*v_Avod_)


st_write(bfp_0_250m_mb, "./results/value_map/MB/bfp_count_cen_da_250m_value.shp",delete_layer = T)


mb_250_bfp <- sum(bfp_0_250m_mb$bfp_count_250, na.rm = T)
mb_250_val <- sum(bfp_0_250m_mb$value_250m, na.rm = T)

bfp_250_500m_mb <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/MB/bfp_count_98_4454sqm_250m_500m.shp")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_500 = ratio_da_bfp*c_250_5)%>%
  mutate(elas_250m_500m = e_500m/100)%>%
  mutate(value_500m = elas_250m_500m*bfp_count_500*v_Avod_)


st_write(bfp_250_500m_mb, "./results/value_map/MB/bfp_count_cen_da_250m_500m_value.shp",delete_layer = T)


mb_500_bfp <- sum(bfp_250_500m_mb$bfp_count_500, na.rm = T)
mb_500_val <- sum(bfp_250_500m_mb$value_500m, na.rm = T)

# csd
bfp_0_250m_csd <- bfp_0_250m_mb%>%
  select(CSD_UID,value_250m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd
bfp_0_250m_cd <- bfp_0_250m_mb%>%
  select(CD_UID,value_250m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd
bfp_250_500m_csd <- bfp_250_500m_mb%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CSD_UID,value_500m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd

bfp_250_500m_cd <- bfp_250_500m_mb%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CD_UID,value_500m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd map
df_value_map <- bfp_0_250m_csd%>%
  left_join(bfp_250_500m_csd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CSD_UID, sum_pop)

# cd map
df_value_map_cd <- bfp_0_250m_cd%>%
  left_join(bfp_250_500m_cd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CD_UID, sum_pop)


sum(df_value_map_cd$Total_value)
sum(df_value_map$Total_value)

#csd
st_write(df_value_map, "./results/value_map/MB/mb_value_map.shp", delete_layer = T )
#cd
st_write(df_value_map_cd, "./results/value_map/MB/mb_value_map_cd.shp", delete_layer = T )

################################################################################

# New Brinswick
bfp_0_250m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/NB/bfp_count_98_4454sqm_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  select(count_250,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)

bfp_250_500m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/NB/bfp_count_98_4454sqm_500m.shp")%>%
  left_join(bfp_0_250m, by = c('GeoUID'))%>%
  rename("count_500" = "Join_Count")%>%
  mutate(count_250_500 = count_500 - count_250)


st_write(bfp_250_500m, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/NB/bfp_count_98_4454sqm_250m_500m.shp", delete_layer = T)


# layer with count of all bfp data within each census DA, (98-4454sqm)
bfp_at_cen_da <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/NB/bfp_count_98_4454_at_each_cen_da.shp")%>%
  mutate(ratio_da_bfp = Dwllngs/Join_Count)%>%
  select(ratio_da_bfp,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)

bfp_0_250m_nb <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/NB/bfp_count_98_4454sqm_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_250 = ratio_da_bfp*count_250)%>%
  #mutate(wf_premium = 0.0480511*v_Avod_)%>%
  #mutate(v_Avod_adjusted = v_Avod_ + wf_premium)%>%
  mutate(elas_250m = e_250m/100)%>%
  mutate(value_250m = elas_250m*bfp_count_250*v_Avod_)


st_write(bfp_0_250m_nb, "./results/value_map/NB/bfp_count_cen_da_250m_value.shp",delete_layer = T)


nb_250_bfp <- sum(bfp_0_250m_nb$bfp_count_250, na.rm = T)
nb_250_val <- sum(bfp_0_250m_nb$value_250m, na.rm = T)

bfp_250_500m_nb <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/NB/bfp_count_98_4454sqm_250m_500m.shp")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_500 = ratio_da_bfp*c_250_5)%>%
  mutate(elas_250m_500m = e_500m/100)%>%
  mutate(value_500m = elas_250m_500m*bfp_count_500*v_Avod_)


st_write(bfp_250_500m_nb, "./results/value_map/NB/bfp_count_cen_da_250m_500m_value.shp",delete_layer = T)


nb_500_bfp <- sum(bfp_250_500m_nb$bfp_count_500, na.rm = T)
nb_500_val <- sum(bfp_250_500m_nb$value_500m, na.rm = T)

# csd
bfp_0_250m_csd <- bfp_0_250m_nb%>%
  select(CSD_UID,value_250m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd
bfp_0_250m_cd <- bfp_0_250m_nb%>%
  select(CD_UID,value_250m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd
bfp_250_500m_csd <- bfp_250_500m_nb%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CSD_UID,value_500m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd

bfp_250_500m_cd <- bfp_250_500m_nb%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CD_UID,value_500m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd map
df_value_map <- bfp_0_250m_csd%>%
  left_join(bfp_250_500m_csd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CSD_UID, sum_pop)

# cd map
df_value_map_cd <- bfp_0_250m_cd%>%
  left_join(bfp_250_500m_cd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CD_UID, sum_pop)


sum(df_value_map_cd$Total_value)
sum(df_value_map$Total_value)

#csd
st_write(df_value_map, "./results/value_map/NB/nb_value_map.shp", delete_layer = T )
#cd
st_write(df_value_map_cd, "./results/value_map/NB/nb_value_map_cd.shp", delete_layer = T )

################################################################################

# Nova Scotia
bfp_0_250m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/NS/bfp_count_98_4454sqm_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  select(count_250,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)

bfp_250_500m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/NS/bfp_count_98_4454sqm_500m.shp")%>%
  left_join(bfp_0_250m, by = c('GeoUID'))%>%
  rename("count_500" = "Join_Count")%>%
  mutate(count_250_500 = count_500 - count_250)


st_write(bfp_250_500m, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/NS/bfp_count_98_4454sqm_250m_500m.shp", delete_layer = T)


# layer with count of all bfp data within each census DA, (98-4454sqm)
bfp_at_cen_da <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/NS/bfp_count_98_4454_at_each_cen_da.shp")%>%
  mutate(ratio_da_bfp = Dwllngs/Join_Count)%>%
  select(ratio_da_bfp,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)

bfp_0_250m_ns <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/NS/bfp_count_98_4454sqm_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_250 = ratio_da_bfp*count_250)%>%
  #mutate(wf_premium = 0.0480511*v_Avod_)%>%
  #mutate(v_Avod_adjusted = v_Avod_ + wf_premium)%>%
  mutate(elas_250m = e_250m/100)%>%
  mutate(value_250m = elas_250m*bfp_count_250*v_Avod_)


st_write(bfp_0_250m_ns, "./results/value_map/NS/bfp_count_cen_da_250m_value.shp",delete_layer = T)


ns_250_bfp <- sum(bfp_0_250m_ns$bfp_count_250, na.rm = T)
ns_250_val <- sum(bfp_0_250m_ns$value_250m, na.rm = T)

bfp_250_500m_ns <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/NS/bfp_count_98_4454sqm_250m_500m.shp")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_500 = ratio_da_bfp*c_250_5)%>%
  mutate(elas_250m_500m = e_500m/100)%>%
  mutate(value_500m = elas_250m_500m*bfp_count_500*v_Avod_)


st_write(bfp_250_500m_ns, "./results/value_map/NS/bfp_count_cen_da_250m_500m_value.shp",delete_layer = T)


ns_500_bfp <- sum(bfp_250_500m_ns$bfp_count_500, na.rm = T)
ns_500_val <- sum(bfp_250_500m_ns$value_500m, na.rm = T)

# csd
bfp_0_250m_csd <- bfp_0_250m_ns%>%
  select(CSD_UID,value_250m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd
bfp_0_250m_cd <- bfp_0_250m_ns%>%
  select(CD_UID,value_250m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd
bfp_250_500m_csd <- bfp_250_500m_ns%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CSD_UID,value_500m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd

bfp_250_500m_cd <- bfp_250_500m_ns%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CD_UID,value_500m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd map
df_value_map <- bfp_0_250m_csd%>%
  left_join(bfp_250_500m_csd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CSD_UID, sum_pop)

# cd map
df_value_map_cd <- bfp_0_250m_cd%>%
  left_join(bfp_250_500m_cd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CD_UID, sum_pop)


sum(df_value_map_cd$Total_value)
sum(df_value_map$Total_value)

#csd
st_write(df_value_map, "./results/value_map/NS/ns_value_map.shp", delete_layer = T )
#cd
st_write(df_value_map_cd, "./results/value_map/NS/ns_value_map_cd.shp", delete_layer = T )

################################################################################

# Ontario
bfp_0_250m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/ON/bfp_count_98_4454sqm_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  select(count_250,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)

bfp_250_500m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/ON/bfp_count_98_4454sqm_500m.shp")%>%
  left_join(bfp_0_250m, by = c('GeoUID'))%>%
  rename("count_500" = "Join_Count")%>%
  mutate(count_250_500 = count_500 - count_250)


st_write(bfp_250_500m, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/ON/bfp_count_98_4454sqm_250m_500m.shp", delete_layer = T)


# layer with count of all bfp data within each census DA, (98-4454sqm)
bfp_at_cen_da <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/ON/bfp_count_98_4454_at_each_cen_da.shp")%>%
  mutate(ratio_da_bfp = Dwllngs/Join_Count)%>%
  select(ratio_da_bfp,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)

bfp_0_250m_on <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/ON/bfp_count_98_4454sqm_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_250 = ratio_da_bfp*count_250)%>%
  #mutate(wf_premium = 0.0480511*v_Avod_)%>%
  #mutate(v_Avod_adjusted = v_Avod_ + wf_premium)%>%
  mutate(elas_250m = e_250m/100)%>%
  mutate(value_250m = elas_250m*bfp_count_250*v_Avod_)


st_write(bfp_0_250m_on, "./results/value_map/ON/bfp_count_cen_da_250m_value.shp",delete_layer = T)


on_250_bfp <- sum(bfp_0_250m_on$bfp_count_250, na.rm = T)
on_250_val <- sum(bfp_0_250m_on$value_250m, na.rm = T)

bfp_250_500m_on <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/ON/bfp_count_98_4454sqm_250m_500m.shp")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_500 = ratio_da_bfp*c_250_5)%>%
  mutate(elas_250m_500m = e_500m/100)%>%
  mutate(value_500m = elas_250m_500m*bfp_count_500*v_Avod_)


st_write(bfp_250_500m_on, "./results/value_map/ON/bfp_count_cen_da_250m_500m_value.shp",delete_layer = T)


on_500_bfp <- sum(bfp_250_500m_on$bfp_count_500, na.rm = T)
on_500_val <- sum(bfp_250_500m_on$value_500m, na.rm = T)

# csd
bfp_0_250m_csd <- bfp_0_250m_on%>%
  select(CSD_UID,value_250m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd
bfp_0_250m_cd <- bfp_0_250m_on%>%
  select(CD_UID,value_250m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd
bfp_250_500m_csd <- bfp_250_500m_on%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CSD_UID,value_500m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd

bfp_250_500m_cd <- bfp_250_500m_on%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CD_UID,value_500m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd map
df_value_map <- bfp_0_250m_csd%>%
  left_join(bfp_250_500m_csd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CSD_UID, sum_pop)

# cd map
df_value_map_cd <- bfp_0_250m_cd%>%
  left_join(bfp_250_500m_cd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CD_UID, sum_pop)


sum(df_value_map_cd$Total_value)
sum(df_value_map$Total_value)

#csd
st_write(df_value_map, "./results/value_map/ON/on_value_map.shp", delete_layer = T )
#cd
st_write(df_value_map_cd, "./results/value_map/ON/on_value_map_cd.shp", delete_layer = T )

################################################################################

# Qubec
bfp_0_250m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/QC/bfp_count_qc_98_4454sqm_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  select(count_250,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)

bfp_250_500m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/QC/bfp_count_qc_98_4454_500m.shp")%>%
  left_join(bfp_0_250m, by = c('GeoUID'))%>%
  rename("count_500" = "Join_Count")%>%
  mutate(count_250_500 = count_500 - count_250)


st_write(bfp_250_500m, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/QC/bfp_count_qc_98_4454sqm_250m_500m.shp", delete_layer = T)


# layer with count of all bfp data within each census DA, (98-4454sqm)
bfp_at_cen_da <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/QC/bfp_count_qc_98_4454sqm_at_each_cen_da.shp")%>%
  mutate(ratio_da_bfp = Dwllngs/Join_Count)%>%
  select(ratio_da_bfp,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)

bfp_0_250m_qc <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/QC/bfp_count_qc_98_4454sqm_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_250 = ratio_da_bfp*count_250)%>%
  #mutate(wf_premium = 0.0480511*v_Avod_)%>%
  #mutate(v_Avod_adjusted = v_Avod_ + wf_premium)%>%
  mutate(elas_250m = e_250m/100)%>%
  mutate(value_250m = elas_250m*bfp_count_250*v_Avod_)


st_write(bfp_0_250m_qc, "./results/value_map/QC/bfp_count_cen_da_250m_value.shp",delete_layer = T)


qc_250_bfp <- sum(bfp_0_250m_qc$bfp_count_250, na.rm = T)
qc_250_val <- sum(bfp_0_250m_qc$value_250m, na.rm = T)

bfp_250_500m_qc <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/QC/bfp_count_qc_98_4454sqm_250m_500m.shp")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_500 = ratio_da_bfp*c_250_5)%>%
  mutate(elas_250m_500m = e_500m/100)%>%
  mutate(value_500m = elas_250m_500m*bfp_count_500*v_Avod_)


st_write(bfp_250_500m_qc, "./results/value_map/QC/bfp_count_cen_da_250m_500m_value.shp",delete_layer = T)


qc_500_bfp <- sum(bfp_250_500m_qc$bfp_count_500, na.rm = T)
qc_500_val <- sum(bfp_250_500m_qc$value_500m, na.rm = T)

# csd
bfp_0_250m_csd <- bfp_0_250m_qc%>%
  select(CSD_UID,value_250m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd
bfp_0_250m_cd <- bfp_0_250m_qc%>%
  select(CD_UID,value_250m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd
bfp_250_500m_csd <- bfp_250_500m_qc%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CSD_UID,value_500m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd

bfp_250_500m_cd <- bfp_250_500m_qc%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CD_UID,value_500m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd map
df_value_map <- bfp_0_250m_csd%>%
  left_join(bfp_250_500m_csd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CSD_UID, sum_pop)

# cd map
df_value_map_cd <- bfp_0_250m_cd%>%
  left_join(bfp_250_500m_cd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CD_UID, sum_pop)


sum(df_value_map_cd$Total_value)
sum(df_value_map$Total_value)

#csd
st_write(df_value_map, "./results/value_map/QC/qc_value_map.shp", delete_layer = T )
#cd
st_write(df_value_map_cd, "./results/value_map/QC/qc_value_map_cd.shp", delete_layer = T )

################################################################################

# Pricne Edwards
bfp_0_250m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/PE/bfp_count_98_4454sqm_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  select(count_250,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)

bfp_250_500m <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/PE/bfp_count_98_4454sqm_500m.shp")%>%
  left_join(bfp_0_250m, by = c('GeoUID'))%>%
  rename("count_500" = "Join_Count")%>%
  mutate(count_250_500 = count_500 - count_250)


st_write(bfp_250_500m, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/PE/bfp_count_pe_98_4454sqm_250m_500m.shp", delete_layer = T)


# layer with count of all bfp data within each census DA, (98-4454sqm)
bfp_at_cen_da <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/PE/bfp_count_98_4454_at_each_cen_da.shp")%>%
  mutate(ratio_da_bfp = Dwllngs/Join_Count)%>%
  select(ratio_da_bfp,GeoUID)%>%
  as.data.frame()%>%
  select(-geometry)

bfp_0_250m_pe <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/PE/bfp_count_98_4454sqm_250m.shp")%>%
  rename("count_250" = "Join_Count")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_250 = ratio_da_bfp*count_250)%>%
  #mutate(wf_premium = 0.0480511*v_Avod_)%>%
  #mutate(v_Avod_adjusted = v_Avod_ + wf_premium)%>%
  mutate(elas_250m = e_250m/100)%>%
  mutate(value_250m = elas_250m*bfp_count_250*v_Avod_)


st_write(bfp_0_250m_pe, "./results/value_map/PE/bfp_count_cen_da_250m_value.shp",delete_layer = T)


pe_250_bfp <- sum(bfp_0_250m_pe$bfp_count_250, na.rm = T)
pe_250_val <- sum(bfp_0_250m_pe$value_250m, na.rm = T)

bfp_250_500m_pe <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/PE/bfp_count_pe_98_4454sqm_250m_500m.shp")%>%
  left_join(bfp_at_cen_da)%>%
  mutate(bfp_count_500 = ratio_da_bfp*c_250_5)%>%
  mutate(elas_250m_500m = e_500m/100)%>%
  mutate(value_500m = elas_250m_500m*bfp_count_500*v_Avod_)


st_write(bfp_250_500m_pe, "./results/value_map/PE/bfp_count_cen_da_250m_500m_value.shp",delete_layer = T)


pe_500_bfp <- sum(bfp_250_500m_pe$bfp_count_500, na.rm = T)
pe_500_val <- sum(bfp_250_500m_pe$value_500m, na.rm = T)

# csd
bfp_0_250m_csd <- bfp_0_250m_pe%>%
  select(CSD_UID,value_250m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd
bfp_0_250m_cd <- bfp_0_250m_pe%>%
  select(CD_UID,value_250m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_250m = sum(value_250m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd
bfp_250_500m_csd <- bfp_250_500m_pe%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CSD_UID,value_500m,Popultn)%>%
  group_by(CSD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

#cd

bfp_250_500m_cd <- bfp_250_500m_pe%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(CD_UID,value_500m,Popultn)%>%
  group_by(CD_UID)%>%
  summarise(sum_value_500m = sum(value_500m, na.rm = T),
            sum_pop = sum(Popultn, na.rm = T))

# csd map
df_value_map <- bfp_0_250m_csd%>%
  left_join(bfp_250_500m_csd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CSD_UID, sum_pop)

# cd map
df_value_map_cd <- bfp_0_250m_cd%>%
  left_join(bfp_250_500m_cd)%>%
  mutate(Total_value = sum_value_250m + sum_value_500m)%>%
  mutate(Total_value_pp =Total_value/sum_pop)%>%
  select(Total_value,Total_value_pp, CD_UID, sum_pop)


sum(df_value_map_cd$Total_value)
sum(df_value_map$Total_value)

#csd
st_write(df_value_map, "./results/value_map/PE/pe_value_map.shp", delete_layer = T )
#cd
st_write(df_value_map_cd, "./results/value_map/PE/pe_value_map_cd.shp", delete_layer = T )

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




print(xtable(result, type = "latex"), file = "./results/Tables/Benefits_table.tex")




