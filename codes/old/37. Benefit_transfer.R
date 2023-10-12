# clear memories 
rm(list = ls())

# load libraries
library(pacman)
pacman::p_load(sf,sp,rgdal,tidyverse,spatialEco,FIESTA,lwgeom,data.table,janitor,cancensus) 


# parameters
elast_est_wf <- 0.349 # RE model size waterfront estimations
elast_est_nwf <- 0.603 # RE model size non-waterfront estimations
percentate_chane <- 0.01 # 10%
vectors = c("v_CA16_404","v_CA16_4896","v_CA16_401")

# Read files - waterfront

ns_cen_da_bfp_count_50 <- st_read( "./check_arcGIS/NS/ns_census_da_bfp_count_50.shp")
sk_cen_da_bfp_count_50 <- st_read( "./check_arcGIS/SK/sk_census_da_bfp_count_50.shp")
ab_cen_da_bfp_count_50 <- st_read( "./check_arcGIS/AB/ab_census_da_bfp_count_50.shp")
bc_cen_da_bfp_count_50 <- st_read( "./check_arcGIS/BC/bc_census_da_bfp_count_50.shp")
mb_cen_da_bfp_count_50 <- st_read( "./check_arcGIS/MB/mb_census_da_bfp_count_50.shp")
nl_cen_da_bfp_count_50 <- st_read("./check_arcGIS/NL/nl_census_da_bfp_count_50.shp")
nt_cen_da_bfp_count_50 <- st_read( "./check_arcGIS/NT/nt_census_da_bfp_count_50.shp")
nu_cen_da_bfp_count_50 <- st_read("./check_arcGIS/NU/nu_census_da_bfp_count_50.shp")
on_cen_da_bfp_count_50 <- st_read("./check_arcGIS/ON/on_census_da_bfp_count_50.shp")
pe_cen_da_bfp_count_50 <- st_read( "./check_arcGIS/PE/pe_census_da_bfp_count_50.shp")
qc_cen_da_bfp_count_50 <- st_read( "./check_arcGIS/QC/qc_census_da_bfp_count_50.shp")
yt_cen_da_bfp_count_50 <- st_read("./check_arcGIS/YT/yt_census_da_bfp_count_50.shp")



# calculate the cost or benefit based on predefined % change
ns_cen_da_bfp_count_50 <- ns_cen_da_bfp_count_50%>%
  mutate(value_wf = elast_est_wf*v_Avod_*percentate_chane*n)
sk_cen_da_bfp_count_50 <- sk_cen_da_bfp_count_50%>%
  mutate(value_wf = elast_est_wf*v_Avod_*percentate_chane*n)
ab_cen_da_bfp_count_50 <- ab_cen_da_bfp_count_50%>%
  mutate(value_wf = elast_est_wf*v_Avod_*percentate_chane*n)
bc_cen_da_bfp_count_50 <- bc_cen_da_bfp_count_50%>%
  mutate(value_wf = elast_est_wf*v_Avod_*percentate_chane*n)
mb_cen_da_bfp_count_50 <- mb_cen_da_bfp_count_50%>%
  mutate(value_wf = elast_est_wf*v_Avod_*percentate_chane*n)
nl_cen_da_bfp_count_50 <- nl_cen_da_bfp_count_50%>%
  mutate(value_wf = elast_est_wf*v_Avod_*percentate_chane*n)
nt_cen_da_bfp_count_50 <- nt_cen_da_bfp_count_50%>%
  mutate(value_wf = elast_est_wf*v_Avod_*percentate_chane*n)
nu_cen_da_bfp_count_50 <- nu_cen_da_bfp_count_50%>%
  mutate(value_wf = elast_est_wf*v_Avod_*percentate_chane*n)
on_cen_da_bfp_count_50 <- on_cen_da_bfp_count_50%>%
  mutate(value_wf = elast_est_wf*v_Avod_*percentate_chane*n)
pe_cen_da_bfp_count_50 <- pe_cen_da_bfp_count_50%>%
  mutate(value_wf = elast_est_wf*v_Avod_*percentate_chane*n)
qc_cen_da_bfp_count_50 <- qc_cen_da_bfp_count_50%>%
  mutate(value_wf = elast_est_wf*v_Avod_*percentate_chane*n)
yt_cen_da_bfp_count_50 <- yt_cen_da_bfp_count_50%>%
  mutate(value_wf = elast_est_wf*v_Avod_*percentate_chane*n)

# write file
st_write(ns_cen_da_bfp_count_50, "./check_arcGIS/NS/ns_census_da_value_50.shp", delete_dsn = T)
st_write(sk_cen_da_bfp_count_50, "./check_arcGIS/SK/sk_census_da_value_50.shp", delete_dsn = T)
st_write(ab_cen_da_bfp_count_50, "./check_arcGIS/AB/ab_census_da_value_50.shp", delete_dsn = T)
st_write(bc_cen_da_bfp_count_50, "./check_arcGIS/BC/bc_census_da_value_50.shp", delete_dsn = T)
st_write(mb_cen_da_bfp_count_50, "./check_arcGIS/MB/mb_census_da_value_50.shp", delete_dsn = T)
st_write(nl_cen_da_bfp_count_50, "./check_arcGIS/NL/nl_census_da_value_50.shp", delete_dsn = T)
st_write(nt_cen_da_bfp_count_50, "./check_arcGIS/NT/nt_census_da_value_50.shp", delete_dsn = T)
st_write(nu_cen_da_bfp_count_50, "./check_arcGIS/NU/nu_census_da_value_50.shp", delete_dsn = T)
st_write(on_cen_da_bfp_count_50, "./check_arcGIS/ON/on_census_da_value_50.shp", delete_dsn = T)
st_write(pe_cen_da_bfp_count_50, "./check_arcGIS/PE/pe_census_da_value_50.shp", delete_dsn = T)
st_write(qc_cen_da_bfp_count_50, "./check_arcGIS/QC/qc_census_da_value_50.shp", delete_dsn = T)
st_write(yt_cen_da_bfp_count_50, "./check_arcGIS/YT/yt_census_da_value_50.shp", delete_dsn = T)

# Read files - non-waterfront

ns_cen_da_bfp_count_500 <- st_read( "./check_arcGIS/NS/ns_census_da_bfp_count_500.shp")
sk_cen_da_bfp_count_500 <- st_read( "./check_arcGIS/SK/sk_census_da_bfp_count_500.shp")
ab_cen_da_bfp_count_500 <- st_read( "./check_arcGIS/AB/ab_census_da_bfp_count_500.shp")
bc_cen_da_bfp_count_500 <- st_read( "./check_arcGIS/BC/bc_census_da_bfp_count_500.shp")
mb_cen_da_bfp_count_500 <- st_read( "./check_arcGIS/MB/mb_census_da_bfp_count_500.shp")
nl_cen_da_bfp_count_500 <- st_read("./check_arcGIS/NL/nl_census_da_bfp_count_500.shp")
nt_cen_da_bfp_count_500 <- st_read( "./check_arcGIS/NT/nt_census_da_bfp_count_500.shp")
nu_cen_da_bfp_count_500 <- st_read("./check_arcGIS/NU/nu_census_da_bfp_count_500.shp")
on_cen_da_bfp_count_500 <- st_read("./check_arcGIS/ON/on_census_da_bfp_count_500.shp")
pe_cen_da_bfp_count_500 <- st_read( "./check_arcGIS/PE/pe_census_da_bfp_count_500.shp")
qc_cen_da_bfp_count_500 <- st_read( "./check_arcGIS/QC/qc_census_da_bfp_count_500.shp")
#yt_cen_da_bfp_count_500 <- st_read("./check_arcGIS/YT/yt_census_da_bfp_count_500.shp")

# calculate the cost or benefit based on predefined % change
ns_cen_da_bfp_count_500 <- ns_cen_da_bfp_count_500%>%
  mutate(value_nwf = elast_est_nwf*v_Avod_*percentate_chane*n)
sk_cen_da_bfp_count_500 <- sk_cen_da_bfp_count_500%>%
  mutate(value_nwf = elast_est_nwf*v_Avod_*percentate_chane*n)
ab_cen_da_bfp_count_500 <- ab_cen_da_bfp_count_500%>%
  mutate(value_nwf = elast_est_nwf*v_Avod_*percentate_chane*n)
bc_cen_da_bfp_count_500 <- bc_cen_da_bfp_count_500%>%
  mutate(value_nwf = elast_est_nwf*v_Avod_*percentate_chane*n)
mb_cen_da_bfp_count_500 <- mb_cen_da_bfp_count_500%>%
  mutate(value_nwf = elast_est_nwf*v_Avod_*percentate_chane*n)
nl_cen_da_bfp_count_500 <- nl_cen_da_bfp_count_500%>%
  mutate(value_nwf = elast_est_nwf*v_Avod_*percentate_chane*n)
nt_cen_da_bfp_count_500 <- nt_cen_da_bfp_count_500%>%
  mutate(value_nwf = elast_est_nwf*v_Avod_*percentate_chane*n)
nu_cen_da_bfp_count_500 <- nu_cen_da_bfp_count_500%>%
  mutate(value_nwf = elast_est_nwf*v_Avod_*percentate_chane*n)
on_cen_da_bfp_count_500 <- on_cen_da_bfp_count_500%>%
  mutate(value_nwf = elast_est_nwf*v_Avod_*percentate_chane*n)
pe_cen_da_bfp_count_500 <- pe_cen_da_bfp_count_500%>%
  mutate(value_nwf = elast_est_nwf*v_Avod_*percentate_chane*n)
qc_cen_da_bfp_count_500 <- qc_cen_da_bfp_count_500%>%
  mutate(value_nwf = elast_est_nwf*v_Avod_*percentate_chane*n)
#yt_cen_da_bfp_count_500 <- yt_cen_da_bfp_count_500%>%
  #mutate(value_wf = elast_est_wf*v_Avod_*percentate_chane*n)

# write file
st_write(ns_cen_da_bfp_count_500, "./check_arcGIS/NS/ns_census_da_value_500.shp", delete_dsn = T)
st_write(sk_cen_da_bfp_count_500, "./check_arcGIS/SK/sk_census_da_value_500.shp", delete_dsn = T)
st_write(ab_cen_da_bfp_count_500, "./check_arcGIS/AB/ab_census_da_value_500.shp", delete_dsn = T)
st_write(bc_cen_da_bfp_count_500, "./check_arcGIS/BC/bc_census_da_value_500.shp", delete_dsn = T)
st_write(mb_cen_da_bfp_count_500, "./check_arcGIS/MB/mb_census_da_value_500.shp", delete_dsn = T)
st_write(nl_cen_da_bfp_count_500, "./check_arcGIS/NL/nl_census_da_value_500.shp", delete_dsn = T)
st_write(nt_cen_da_bfp_count_500, "./check_arcGIS/NT/nt_census_da_value_500.shp", delete_dsn = T)
st_write(nu_cen_da_bfp_count_500, "./check_arcGIS/NU/nu_census_da_value_500.shp", delete_dsn = T)
st_write(on_cen_da_bfp_count_500, "./check_arcGIS/ON/on_census_da_value_500.shp", delete_dsn = T)
st_write(pe_cen_da_bfp_count_500, "./check_arcGIS/PE/pe_census_da_value_500.shp", delete_dsn = T)
st_write(qc_cen_da_bfp_count_500, "./check_arcGIS/QC/qc_census_da_value_500.shp", delete_dsn = T)
#st_write(yt_cen_da_bfp_count_500, "./check_arcGIS/YT/yt_census_da_value_500.shp", delete_dsn = T)

# total value
# obtaining total value by combing value in waterfront and non-waterfront homes

colnames(ns_cen_da_bfp_count_50)[which(names(ns_cen_da_bfp_count_50) == "n")] <- "wf_house"
colnames(sk_cen_da_bfp_count_50)[which(names(sk_cen_da_bfp_count_50) == "n")] <- "wf_house"
colnames(ab_cen_da_bfp_count_50)[which(names(ab_cen_da_bfp_count_50) == "n")] <- "wf_house"
colnames(bc_cen_da_bfp_count_50)[which(names(bc_cen_da_bfp_count_50) == "n")] <- "wf_house"
colnames(mb_cen_da_bfp_count_50)[which(names(mb_cen_da_bfp_count_50) == "n")] <- "wf_house"
colnames(nl_cen_da_bfp_count_50)[which(names(nl_cen_da_bfp_count_50) == "n")] <- "wf_house"
colnames(nt_cen_da_bfp_count_50)[which(names(nt_cen_da_bfp_count_50) == "n")] <- "wf_house"
colnames(nu_cen_da_bfp_count_50)[which(names(nu_cen_da_bfp_count_50) == "n")] <- "wf_house"
colnames(on_cen_da_bfp_count_50)[which(names(on_cen_da_bfp_count_50) == "n")] <- "wf_house"
colnames(pe_cen_da_bfp_count_50)[which(names(pe_cen_da_bfp_count_50) == "n")] <- "wf_house"
colnames(qc_cen_da_bfp_count_50)[which(names(qc_cen_da_bfp_count_50) == "n")] <- "wf_house"


colnames(ns_cen_da_bfp_count_500)[which(names(ns_cen_da_bfp_count_500) == "n")] <- "nwf_house"
colnames(sk_cen_da_bfp_count_500)[which(names(sk_cen_da_bfp_count_500) == "n")] <- "nwf_house"
colnames(ab_cen_da_bfp_count_500)[which(names(ab_cen_da_bfp_count_500) == "n")] <- "nwf_house"
colnames(bc_cen_da_bfp_count_500)[which(names(bc_cen_da_bfp_count_500) == "n")] <- "nwf_house"
colnames(mb_cen_da_bfp_count_500)[which(names(mb_cen_da_bfp_count_500) == "n")] <- "nwf_house"
colnames(nl_cen_da_bfp_count_500)[which(names(nl_cen_da_bfp_count_500) == "n")] <- "nwf_house"
colnames(nt_cen_da_bfp_count_500)[which(names(nt_cen_da_bfp_count_500) == "n")] <- "nwf_house"
colnames(nu_cen_da_bfp_count_500)[which(names(nu_cen_da_bfp_count_500) == "n")] <- "nwf_house"
colnames(on_cen_da_bfp_count_500)[which(names(on_cen_da_bfp_count_500) == "n")] <- "nwf_house"
colnames(pe_cen_da_bfp_count_500)[which(names(pe_cen_da_bfp_count_500) == "n")] <- "nwf_house"
colnames(qc_cen_da_bfp_count_500)[which(names(qc_cen_da_bfp_count_500) == "n")] <- "nwf_house"



ns <-ns_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(nwf_house,value_nwf,GeoUID)
sk <-sk_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(nwf_house,value_nwf,GeoUID)
ab <-ab_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(nwf_house,value_nwf,GeoUID)
bc <-bc_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(nwf_house,value_nwf,GeoUID)
mb <-mb_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(nwf_house,value_nwf,GeoUID)
nl <-nl_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(nwf_house,value_nwf,GeoUID)
nt <-nt_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(nwf_house,value_nwf,GeoUID)
nu <-nu_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(nwf_house,value_nwf,GeoUID)
on <-on_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(nwf_house,value_nwf,GeoUID)
pe <-pe_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(nwf_house,value_nwf,GeoUID)
qc <-qc_cen_da_bfp_count_500%>%
  as.data.frame()%>%
  select(nwf_house,value_nwf,GeoUID)



ns_cen_da_bfp_count_tot <- left_join(ns_cen_da_bfp_count_50,ns, by =c("GeoUID"))
sk_cen_da_bfp_count_tot <- left_join(sk_cen_da_bfp_count_50,sk, by =c("GeoUID"))
ab_cen_da_bfp_count_tot <- left_join(ab_cen_da_bfp_count_50,ab, by =c("GeoUID"))
bc_cen_da_bfp_count_tot <- left_join(bc_cen_da_bfp_count_50,bc, by =c("GeoUID"))
mb_cen_da_bfp_count_tot <- left_join(mb_cen_da_bfp_count_50,mb, by =c("GeoUID"))
nl_cen_da_bfp_count_tot <- left_join(nl_cen_da_bfp_count_50,nl, by =c("GeoUID"))
nt_cen_da_bfp_count_tot <- left_join(nt_cen_da_bfp_count_50,nt, by =c("GeoUID"))
nu_cen_da_bfp_count_tot <- left_join(nu_cen_da_bfp_count_50,nu, by =c("GeoUID"))
on_cen_da_bfp_count_tot <- left_join(on_cen_da_bfp_count_50,on, by =c("GeoUID"))
pe_cen_da_bfp_count_tot <- left_join(pe_cen_da_bfp_count_50,pe, by =c("GeoUID"))
qc_cen_da_bfp_count_tot <- left_join(qc_cen_da_bfp_count_50,qc, by =c("GeoUID"))



ns_cen_da_bfp_count_tot <- ns_cen_da_bfp_count_tot%>%
  rowwise()%>%
  mutate(total_value = sum(value_wf,value_nwf, na.rm = T))
sk_cen_da_bfp_count_tot <- sk_cen_da_bfp_count_tot%>%
  rowwise()%>%
  mutate(total_value = sum(value_wf,value_nwf, na.rm = T))
ab_cen_da_bfp_count_tot <- ab_cen_da_bfp_count_tot%>%
  rowwise()%>%
  mutate(total_value = sum(value_wf,value_nwf, na.rm = T))
bc_cen_da_bfp_count_tot <- bc_cen_da_bfp_count_tot%>%
  rowwise()%>%
  mutate(total_value = sum(value_wf,value_nwf, na.rm = T))
mb_cen_da_bfp_count_tot <- mb_cen_da_bfp_count_tot%>%
  rowwise()%>%
  mutate(total_value = sum(value_wf,value_nwf, na.rm = T))
nl_cen_da_bfp_count_tot <- nl_cen_da_bfp_count_tot%>%
  rowwise()%>%
  mutate(total_value = sum(value_wf,value_nwf, na.rm = T))
nt_cen_da_bfp_count_tot <- nt_cen_da_bfp_count_tot%>%
  rowwise()%>%
  mutate(total_value = sum(value_wf,value_nwf, na.rm = T))
nu_cen_da_bfp_count_tot <- nu_cen_da_bfp_count_tot%>%
  rowwise()%>%
  mutate(total_value = sum(value_wf,value_nwf, na.rm = T))
on_cen_da_bfp_count_tot <- on_cen_da_bfp_count_tot%>%
  rowwise()%>%
  mutate(total_value = sum(value_wf,value_nwf, na.rm = T))
pe_cen_da_bfp_count_tot <- pe_cen_da_bfp_count_tot%>%
  rowwise()%>%
  mutate(total_value = sum(value_wf,value_nwf, na.rm = T))
qc_cen_da_bfp_count_tot <- qc_cen_da_bfp_count_tot%>%
  rowwise()%>%
  mutate(total_value = sum(value_wf,value_nwf, na.rm = T))

# write file
st_write(ns_cen_da_bfp_count_tot, "./check_arcGIS/NS/ns_census_db_bfp_count_tot.shp", delete_dsn = T)
st_write(sk_cen_da_bfp_count_tot, "./check_arcGIS/SK/sk_census_db_bfp_count_tot.shp", delete_dsn = T)
st_write(ab_cen_da_bfp_count_tot, "./check_arcGIS/AB/ab_census_db_bfp_count_tot.shp", delete_dsn = T)
st_write(bc_cen_da_bfp_count_tot, "./check_arcGIS/BC/bc_census_db_bfp_count_tot.shp", delete_dsn = T)
st_write(mb_cen_da_bfp_count_tot, "./check_arcGIS/MB/mb_census_db_bfp_count_tot.shp", delete_dsn = T)
st_write(nl_cen_da_bfp_count_tot, "./check_arcGIS/NL/nl_census_db_bfp_count_tot.shp", delete_dsn = T)
st_write(nt_cen_da_bfp_count_tot, "./check_arcGIS/NT/nt_census_db_bfp_count_tot.shp", delete_dsn = T)
st_write(nu_cen_da_bfp_count_tot, "./check_arcGIS/NU/nu_census_db_bfp_count_tot.shp", delete_dsn = T)
st_write(on_cen_da_bfp_count_tot, "./check_arcGIS/ON/on_census_db_bfp_count_tot.shp", delete_dsn = T)
st_write(pe_cen_da_bfp_count_tot, "./check_arcGIS/PE/pe_census_db_bfp_count_tot.shp", delete_dsn = T)
st_write(qc_cen_da_bfp_count_tot, "./check_arcGIS/QC/qc_census_db_bfp_count_tot.shp", delete_dsn = T)



# summary table

df_ns <- ns_cen_da_bfp_count_tot%>%
  select(wf_house,nwf_house,value_wf,value_nwf,total_value)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  pivot_longer(cols = everything(), names_to = "cat", values_to = "val")%>%
  group_by(cat)%>%
  summarise(Total=sum(val, na.rm = T))%>%
  transpose()%>%
  row_to_names(row_number = 1)%>%
  mutate(province = "Nova Scotia")

df_sk <- sk_cen_da_bfp_count_tot%>%
  select(wf_house,nwf_house,value_wf,value_nwf,total_value)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  pivot_longer(cols = everything(), names_to = "cat", values_to = "val")%>%
  group_by(cat)%>%
  summarise(Total=sum(val, na.rm = T))%>%
  transpose()%>%
  row_to_names(row_number = 1)%>%
  mutate(province = "Saskatchewan")

df_ab <- ab_cen_da_bfp_count_tot%>%
  select(wf_house,nwf_house,value_wf,value_nwf,total_value)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  pivot_longer(cols = everything(), names_to = "cat", values_to = "val")%>%
  group_by(cat)%>%
  summarise(Total=sum(val, na.rm = T))%>%
  transpose()%>%
  row_to_names(row_number = 1)%>%
  mutate(province = "Alberta")

df_bc <- bc_cen_da_bfp_count_tot%>%
  select(wf_house,nwf_house,value_wf,value_nwf,total_value)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  pivot_longer(cols = everything(), names_to = "cat", values_to = "val")%>%
  group_by(cat)%>%
  summarise(Total=sum(val, na.rm = T))%>%
  transpose()%>%
  row_to_names(row_number = 1)%>%
  mutate(province = "British Columbia")

df_mb <- mb_cen_da_bfp_count_tot%>%
  select(wf_house,nwf_house,value_wf,value_nwf,total_value)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  pivot_longer(cols = everything(), names_to = "cat", values_to = "val")%>%
  group_by(cat)%>%
  summarise(Total=sum(val, na.rm = T))%>%
  transpose()%>%
  row_to_names(row_number = 1)%>%
  mutate(province = "Manitoba")

df_nl <- nl_cen_da_bfp_count_tot%>%
  select(wf_house,nwf_house,value_wf,value_nwf,total_value)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  pivot_longer(cols = everything(), names_to = "cat", values_to = "val")%>%
  group_by(cat)%>%
  summarise(Total=sum(val, na.rm = T))%>%
  transpose()%>%
  row_to_names(row_number = 1)%>%
  mutate(province = "Newfoundland and Labrador")

df_nt <- nt_cen_da_bfp_count_tot%>%
  select(wf_house,nwf_house,value_wf,value_nwf,total_value)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  pivot_longer(cols = everything(), names_to = "cat", values_to = "val")%>%
  group_by(cat)%>%
  summarise(Total=sum(val, na.rm = T))%>%
  transpose()%>%
  row_to_names(row_number = 1)%>%
  mutate(province = "Northwest Territories")

df_nu <- nu_cen_da_bfp_count_tot%>%
  select(wf_house,nwf_house,value_wf,value_nwf,total_value)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  pivot_longer(cols = everything(), names_to = "cat", values_to = "val")%>%
  group_by(cat)%>%
  summarise(Total=sum(val, na.rm = T))%>%
  transpose()%>%
  row_to_names(row_number = 1)%>%
  mutate(province = "Nunavut")


df_on <- on_cen_da_bfp_count_tot%>%
  select(wf_house,nwf_house,value_wf,value_nwf,total_value)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  pivot_longer(cols = everything(), names_to = "cat", values_to = "val")%>%
  group_by(cat)%>%
  summarise(Total=sum(val, na.rm = T))%>%
  transpose()%>%
  row_to_names(row_number = 1)%>%
  mutate(province = "Ontario")

df_pe <- pe_cen_da_bfp_count_tot%>%
  select(wf_house,nwf_house,value_wf,value_nwf,total_value)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  pivot_longer(cols = everything(), names_to = "cat", values_to = "val")%>%
  group_by(cat)%>%
  summarise(Total=sum(val, na.rm = T))%>%
  transpose()%>%
  row_to_names(row_number = 1)%>%
  mutate(province = " Prince Edward Island")

df_qc <- qc_cen_da_bfp_count_tot%>%
  select(wf_house,nwf_house,value_wf,value_nwf,total_value)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  pivot_longer(cols = everything(), names_to = "cat", values_to = "val")%>%
  group_by(cat)%>%
  summarise(Total=sum(val, na.rm = T))%>%
  transpose()%>%
  row_to_names(row_number = 1)%>%
  mutate(province = "Quebec")

df <- rbind(df_ns,df_ab,df_bc,df_mb,df_sk,df_nl,df_nt,df_nu,df_on,df_pe,df_qc)

df <- df[, c("province", "wf_house", "nwf_house","value_wf","value_nwf","total_value")]


library(xtable)
print.xtable(xtable(df), file = "./results/benefitvalue.tex")

sum(ns_cen_da_bfp_count_500$value_nwf, na.rm = T)

sum(ns_cen_da_bfp_count_tot$total_value, na.rm = T)
  

sum(ns_cen_da_bfp_count_50$value_wf,ns_cen_da_bfp_count_tot$value_nwf, na.rm = T)

################################################################################
#maps
# NS
ns_cen_da_bfp_count_tot <- st_read( "./check_arcGIS/NS/ns_census_db_bfp_count_tot.shp")
ns_cen_da_bfp_count_tot_csd <- ns_cen_da_bfp_count_tot%>%
  group_by(CSD_UID)%>%
  mutate(val_wf_csd = sum(valu_wf, na.rm = T))%>%
  mutate(val_nwf_csd = sum(val_nwf, na.rm = T))%>%
  ungroup()
ns_census_csd <- get_census(dataset='CA16', regions=list(PR="12"),
                           vectors=paste0(vectors),
                           level='CSD', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
ns_cen_da_bfp_count_tot_csd <- ns_cen_da_bfp_count_tot_csd %>%
  select(val_wf_csd,val_nwf_csd, CSD_UID)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  rename(GeoUID = CSD_UID)
ns <- distinct(ns_cen_da_bfp_count_tot_csd, GeoUID, .keep_all = T)
ns_csd <- left_join(ns_census_csd,ns, by= "GeoUID")
ns_csd_espg <- st_transform(ns_csd, crs = 3348)
st_write(ns_csd_espg, "./check_arcGIS/NS/ns_csd.shp", delete_dsn = T)

# AB
ab_cen_da_bfp_count_tot <- st_read( "./check_arcGIS/AB/ab_census_db_bfp_count_tot.shp")
ab_cen_da_bfp_count_tot_csd <- ab_cen_da_bfp_count_tot%>%
  group_by(CSD_UID)%>%
  mutate(val_wf_csd = sum(valu_wf, na.rm = T))%>%
  mutate(val_nwf_csd = sum(val_nwf, na.rm = T))%>%
  ungroup()
ab_census_csd <- get_census(dataset='CA16', regions=list(PR="48"),
                            vectors=paste0(vectors),
                            level='CSD', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
ab_cen_da_bfp_count_tot_csd <- ab_cen_da_bfp_count_tot_csd %>%
  select(val_wf_csd,val_nwf_csd, CSD_UID)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  rename(GeoUID = CSD_UID)
ab <- distinct(ab_cen_da_bfp_count_tot_csd, GeoUID, .keep_all = T)
ab_csd <- left_join(ab_census_csd,ab, by= "GeoUID")
ab_csd_espg <- st_transform(ab_csd, crs = 3348)
st_write(ab_csd_espg, "./check_arcGIS/AB/ab_csd.shp", delete_dsn = T)

# BC
bc_cen_da_bfp_count_tot <- st_read( "./check_arcGIS/BC/bc_census_db_bfp_count_tot.shp")
bc_cen_da_bfp_count_tot_csd <- bc_cen_da_bfp_count_tot%>%
  group_by(CSD_UID)%>%
  mutate(val_wf_csd = sum(valu_wf, na.rm = T))%>%
  mutate(val_nwf_csd = sum(val_nwf, na.rm = T))%>%
  ungroup()
bc_census_csd <- get_census(dataset='CA16', regions=list(PR="59"),
                            vectors=paste0(vectors),
                            level='CSD', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
bc_cen_da_bfp_count_tot_csd <- bc_cen_da_bfp_count_tot_csd %>%
  select(val_wf_csd,val_nwf_csd, CSD_UID)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  rename(GeoUID = CSD_UID)
bc <- distinct(bc_cen_da_bfp_count_tot_csd, GeoUID, .keep_all = T)
bc_csd <- left_join(bc_census_csd,bc, by= "GeoUID")
bc_csd_espg <- st_transform(bc_csd, crs = 3348)
st_write(bc_csd_espg, "./check_arcGIS/BC/bc_csd.shp", delete_dsn = T)


# MB
mb_cen_da_bfp_count_tot <- st_read( "./check_arcGIS/MB/mb_census_db_bfp_count_tot.shp")
mb_cen_da_bfp_count_tot_csd <- mb_cen_da_bfp_count_tot%>%
  group_by(CSD_UID)%>%
  mutate(val_wf_csd = sum(valu_wf, na.rm = T))%>%
  mutate(val_nwf_csd = sum(val_nwf, na.rm = T))%>%
  ungroup()
mb_census_csd <- get_census(dataset='CA16', regions=list(PR="46"),
                            vectors=paste0(vectors),
                            level='CSD', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
mb_cen_da_bfp_count_tot_csd <- mb_cen_da_bfp_count_tot_csd %>%
  select(val_wf_csd,val_nwf_csd, CSD_UID)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  rename(GeoUID = CSD_UID)
mb <- distinct(mb_cen_da_bfp_count_tot_csd, GeoUID, .keep_all = T)
mb_csd <- left_join(mb_census_csd,mb, by= "GeoUID")
mb_csd_espg <- st_transform(mb_csd, crs = 3348)
st_write(mb_csd_espg, "./check_arcGIS/MB/mb_csd.shp", delete_dsn = T)

# NL
nl_cen_da_bfp_count_tot <- st_read( "./check_arcGIS/NL/nl_census_db_bfp_count_tot.shp")
nl_cen_da_bfp_count_tot_csd <- nl_cen_da_bfp_count_tot%>%
  group_by(CSD_UID)%>%
  mutate(val_wf_csd = sum(valu_wf, na.rm = T))%>%
  mutate(val_nwf_csd = sum(val_nwf, na.rm = T))%>%
  ungroup()
nl_census_csd <- get_census(dataset='CA16', regions=list(PR="10"),
                            vectors=paste0(vectors),
                            level='CSD', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
nl_cen_da_bfp_count_tot_csd <- nl_cen_da_bfp_count_tot_csd %>%
  select(val_wf_csd,val_nwf_csd, CSD_UID)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  rename(GeoUID = CSD_UID)
nl <- distinct(nl_cen_da_bfp_count_tot_csd, GeoUID, .keep_all = T)
nl_csd <- left_join(nl_census_csd,nl, by= "GeoUID")
nl_csd_espg <- st_transform(nl_csd, crs = 3348)
st_write(nl_csd_espg, "./check_arcGIS/NL/nl_csd.shp", delete_dsn = T)

# SK
sk_cen_da_bfp_count_tot <- st_read( "./check_arcGIS/SK/sk_census_db_bfp_count_tot.shp")
sk_cen_da_bfp_count_tot_csd <- sk_cen_da_bfp_count_tot%>%
  group_by(CSD_UID)%>%
  mutate(val_wf_csd = sum(valu_wf, na.rm = T))%>%
  mutate(val_nwf_csd = sum(val_nwf, na.rm = T))%>%
  ungroup()
sk_census_csd <- get_census(dataset='CA16', regions=list(PR="47"),
                            vectors=paste0(vectors),
                            level='CSD', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
sk_cen_da_bfp_count_tot_csd <- sk_cen_da_bfp_count_tot_csd %>%
  select(val_wf_csd,val_nwf_csd, CSD_UID)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  rename(GeoUID = CSD_UID)
sk <- distinct(sk_cen_da_bfp_count_tot_csd, GeoUID, .keep_all = T)
sk_csd <- left_join(sk_census_csd,sk, by= "GeoUID")
sk_csd_espg <- st_transform(sk_csd, crs = 3348)
st_write(sk_csd_espg, "./check_arcGIS/SK/sk_csd.shp", delete_dsn = T)

# ON
on_cen_da_bfp_count_tot <- st_read( "./check_arcGIS/ON/on_census_db_bfp_count_tot.shp")
on_cen_da_bfp_count_tot_csd <- on_cen_da_bfp_count_tot%>%
  group_by(CSD_UID)%>%
  mutate(val_wf_csd = sum(valu_wf, na.rm = T))%>%
  mutate(val_nwf_csd = sum(val_nwf, na.rm = T))%>%
  ungroup()
on_census_csd <- get_census(dataset='CA16', regions=list(PR="35"),
                            vectors=paste0(vectors),
                            level='CSD', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
on_cen_da_bfp_count_tot_csd <- on_cen_da_bfp_count_tot_csd %>%
  select(val_wf_csd,val_nwf_csd, CSD_UID)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  rename(GeoUID = CSD_UID)
on <- distinct(on_cen_da_bfp_count_tot_csd, GeoUID, .keep_all = T)
on_csd <- left_join(on_census_csd,on, by= "GeoUID")
on_csd_espg <- st_transform(on_csd, crs = 3348)
st_write(on_csd_espg, "./check_arcGIS/ON/on_csd.shp", delete_dsn = T)

#QC
qc_cen_da_bfp_count_tot <- st_read( "./check_arcGIS/QC/qc_census_db_bfp_count_tot.shp")
qc_cen_da_bfp_count_tot_csd <- qc_cen_da_bfp_count_tot%>%
  group_by(CSD_UID)%>%
  mutate(val_wf_csd = sum(valu_wf, na.rm = T))%>%
  mutate(val_nwf_csd = sum(val_nwf, na.rm = T))%>%
  ungroup()
qc_census_csd <- get_census(dataset='CA16', regions=list(PR="24"),
                            vectors=paste0(vectors),
                            level='CSD', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
qc_cen_da_bfp_count_tot_csd <- qc_cen_da_bfp_count_tot_csd %>%
  select(val_wf_csd,val_nwf_csd, CSD_UID)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  rename(GeoUID = CSD_UID)
qc <- distinct(qc_cen_da_bfp_count_tot_csd, GeoUID, .keep_all = T)
qc_csd <- left_join(qc_census_csd,qc, by= "GeoUID")
qc_csd_espg <- st_transform(qc_csd, crs = 3348)
st_write(qc_csd_espg, "./check_arcGIS/QC/qc_csd.shp", delete_dsn = T)

#PE
pe_cen_da_bfp_count_tot <- st_read( "./check_arcGIS/PE/pe_census_db_bfp_count_tot.shp")
pe_cen_da_bfp_count_tot_csd <- pe_cen_da_bfp_count_tot%>%
  group_by(CSD_UID)%>%
  mutate(val_wf_csd = sum(valu_wf, na.rm = T))%>%
  mutate(val_nwf_csd = sum(val_nwf, na.rm = T))%>%
  ungroup()
pe_census_csd <- get_census(dataset='CA16', regions=list(PR="11"),
                            vectors=paste0(vectors),
                            level='CSD', use_cache = FALSE, geo_format = 'sf', quiet = TRUE)
pe_cen_da_bfp_count_tot_csd <- pe_cen_da_bfp_count_tot_csd %>%
  select(val_wf_csd,val_nwf_csd, CSD_UID)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  rename(GeoUID = CSD_UID)
pe <- distinct(pe_cen_da_bfp_count_tot_csd, GeoUID, .keep_all = T)
pe_csd <- left_join(pe_census_csd,pe, by= "GeoUID")
pe_csd_espg <- st_transform(pe_csd, crs = 3348)
st_write(pe_csd_espg, "./check_arcGIS/PE/pe_csd.shp", delete_dsn = T)








