# This code use the lake buffers created at 500m and 100m with Canadian building foot print data to dtermine 
# the buillding foot print within each buffer. Please note here we do not discriminate the spatial polygons 
# based on ares. Insted we extarct all building foot print within buffer which will give us some flexibility
# to give some treshholds at different level in later codes

# building foot print data at state level extarct from https://github.com/microsoft/CanadianBuildingFootprints

# clear memory
rm(list = ls())
sf_use_s2(FALSE)

#AB
buf_500 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_AB_Hydro/ab_all_004_500m.shp")%>%
  mutate(OBJECTID = row_number())
bf <- st_read("./Building_footprint/AB/ab.shp")

buf_500 <- st_transform(buf_500, 4269)%>%
  subset(definit==83)%>% #filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names

bf <- st_transform(bf, 4269)

bf_buf_500 <- st_join(bf,buf_500)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary


buf_1000 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_AB_Hydro/ab_all_004_1000m.shp")%>%
  mutate(OBJECTID = row_number())

buf_1000 <- st_transform(buf_1000, 4269)%>%
  subset(definit==83)%>% #filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names


bf_buf_1000 <- st_join(bf,buf_1000)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary

st_write(bf_buf_500, "./Building_footprint/AB/ab_all_004_500m.shp", delete_dsn = T)
st_write(bf_buf_1000, "./Building_footprint/AB/ab_all_004_1000m.shp", delete_dsn = T)


#BC
buf_500 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_BC_Hydro/bc_all_004_500m.shp")%>%
  mutate(OBJECTID = row_number())
bf <- st_read("./Building_footprint/BC/bc.shp")

buf_500 <- st_transform(buf_500, 4269)%>%
  subset(definit==83)%>% #filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names


bf <- st_transform(bf, 4269)

bf_buf_500 <- st_join(bf,buf_500)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary


buf_1000 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_BC_Hydro/bc_all_004_1000m.shp")%>%
  mutate(OBJECTID = row_number())

buf_1000 <- st_transform(buf_1000, 4269)%>%
  subset(definit==83)%>% #filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names


bf_buf_1000 <- st_join(bf,buf_1000)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary

st_write(bf_buf_500, "./Building_footprint/BC/bc_all_004_500m.shp", delete_dsn = T)
st_write(bf_buf_1000, "./Building_footprint/BC/bc_all_004_1000m.shp", delete_dsn = T)


#MB
buf_500 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_MB_Hydro/mb_all_004_500m.shp")%>%
  mutate(OBJECTID = row_number())
bf <- st_read("./Building_footprint/MB/mb.shp")

buf_500 <- st_transform(buf_500, 4269)%>%
  subset(definit==83)%>% #filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names


bf <- st_transform(bf, 4269)

bf_buf_500 <- st_join(bf,buf_500)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary


buf_1000 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_MB_Hydro/mb_all_004_1000m.shp")%>%
  mutate(OBJECTID = row_number())

buf_1000 <- st_transform(buf_1000, 4269)%>%
  subset(definit==83)%>% #filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names


bf_buf_1000 <- st_join(bf,buf_1000)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary

st_write(bf_buf_500, "./Building_footprint/MB/mb_all_004_500m.shp", delete_dsn = T)
st_write(bf_buf_1000, "./Building_footprint/MB/mb_all_004_1000m.shp", delete_dsn = T)


#NB
buf_500 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_NB_Hydro/nb_all_004_500m.shp")%>%
  mutate(OBJECTID = row_number())
bf <- st_read("./Building_footprint/NB/nb.shp")

buf_500 <- st_transform(buf_500, 4269)%>%
  subset(definit==83)%>%#filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names


bf <- st_transform(bf, 4269)

bf_buf_500 <- st_join(bf,buf_500)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary

buf_1000 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_NB_Hydro/nb_all_004_1000m.shp")%>%
  mutate(OBJECTID = row_number())

buf_1000 <- st_transform(buf_1000, 4269)%>%
  subset(definit==83)%>%#filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names


bf_buf_1000 <- st_join(bf,buf_1000)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary

st_write(bf_buf_500, "./Building_footprint/NB/nb_all_004_500m.shp", delete_dsn = T)
st_write(bf_buf_1000, "./Building_footprint/NB/nb_all_004_1000m.shp", delete_dsn = T)


#NL
buf_500 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_NL_Hydro/nl_all_004_500m.shp")%>%
  mutate(OBJECTID = row_number())
bf <- st_read("./Building_footprint/NL/nl.shp")

buf_500 <- st_transform(buf_500, 4269)%>%
  subset(definit==83)%>%#filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names


bf <- st_transform(bf, 4269)

bf_buf_500 <- st_join(bf,buf_500)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary

buf_1000 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_NL_Hydro/nl_all_004_1000m.shp")%>%
  mutate(OBJECTID = row_number())

buf_1000 <- st_transform(buf_1000, 4269)%>%
  subset(definit==83)%>%#filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names


bf_buf_1000 <- st_join(bf,buf_1000)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary

st_write(bf_buf_500, "./Building_footprint/NL/nl_all_004_500m.shp", delete_dsn = T)
st_write(bf_buf_1000, "./Building_footprint/NL/nl_all_004_1000m.shp", delete_dsn = T)


#NS
buf_500 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_NS_Hydro/ns_all_004_500m.shp")%>%
  mutate(OBJECTID = row_number())
bf <- st_read("./Building_footprint/NS/ns.shp")

buf_500 <- st_transform(buf_500, 4269)%>%
  subset(definit==83)%>%#filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names

bf <- st_transform(bf, 4269)

bf_buf_500 <- st_join(bf,buf_500)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary

buf_1000 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_NS_Hydro/ns_all_004_1000m.shp")%>%
  mutate(OBJECTID = row_number())

buf_1000 <- st_transform(buf_1000, 4269)%>%
  subset(definit==83)%>%#filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names


bf_buf_1000 <- st_join(bf,buf_1000)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary

st_write(bf_buf_500, "./Building_footprint/NS/ns_all_004_500m.shp", delete_dsn = T)
st_write(bf_buf_1000, "./Building_footprint/NS/ns_all_004_1000m.shp", delete_dsn = T)


#ON
buf_500 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_ON_Hydro/on_all_004_500m.shp")%>%
  mutate(OBJECTID = row_number())
bf <- st_read("./Building_footprint/ON/on.shp")

buf_500 <- st_transform(buf_500, 4269)%>%
  subset(definit==83)%>%#filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names

bf <- st_transform(bf, 4269)

bf_buf_500 <- st_join(bf,buf_500)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary


buf_1000 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_ON_Hydro/on_all_004_1000m.shp")%>%
  mutate(OBJECTID = row_number())

buf_1000 <- st_transform(buf_1000, 4269)%>%
  subset(definit==83)%>%#filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names


bf_buf_1000 <- st_join(bf,buf_1000)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary

st_write(bf_buf_500, "./Building_footprint/ON/on_all_004_500m.shp", delete_dsn = T)
st_write(bf_buf_1000, "./Building_footprint/ON/on_all_004_1000m.shp", delete_dsn = T)


#PE
buf_500 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_PE_Hydro/pe_all_004_500m.shp")%>%
  mutate(OBJECTID = row_number())
bf <- st_read("./Building_footprint/PE/pe.shp")

buf_500 <- st_transform(buf_500, 4269)%>%
  subset(definit==83)%>%#filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names

bf <- st_transform(bf, 4269)

bf_buf_500 <- st_join(bf,buf_500)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary


buf_1000 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_PE_Hydro/pe_all_004_1000m.shp")%>%
  mutate(OBJECTID = row_number())

buf_1000 <- st_transform(buf_1000, 4269)%>%
  subset(definit==83)%>%#filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names


bf <- st_transform(bf, 4269)

bf_buf_1000 <- st_join(bf,buf_1000)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary

st_write(bf_buf_500, "./Building_footprint/PE/pe_all_004_500m.shp", delete_dsn = T)
st_write(bf_buf_1000, "./Building_footprint/PE/pe_all_004_1000m.shp", delete_dsn = T)


#QC
buf_500 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_QC_Hydro/qc_all_004_500m.shp")%>%
  mutate(OBJECTID = row_number())
bf <- st_read("./Building_footprint/QC/qc.shp")

buf_500 <- st_transform(buf_500, 4269)%>%
  subset(definit==83)%>%#filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names

bf <- st_transform(bf, 4269)

bf_buf_500 <- st_join(bf,buf_500)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary


buf_1000 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_QC_Hydro/qc_all_004_1000m.shp")%>%
  mutate(OBJECTID = row_number())

buf_1000 <- st_transform(buf_1000, 4269)%>%
  subset(definit==83)%>%#filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names


bf_buf_1000 <- st_join(bf,buf_1000)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary

st_write(bf_buf_500, "./Building_footprint/QC/qc_all_004_500m.shp", delete_dsn = T)
st_write(bf_buf_1000, "./Building_footprint/QC/qc_all_004_1000m.shp", delete_dsn = T)


#SK
buf_500 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_SK_Hydro/sk_all_004_500m.shp")%>%
  mutate(OBJECTID = row_number())
bf <- st_read("./Building_footprint/SK/sk.shp")

buf_500 <- st_transform(buf_500, 4269)%>%
  subset(definit==83)%>%#filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names

bf <- st_transform(bf, 4269)

bf_buf_500 <- st_join(bf,buf_500)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary

buf_1000 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_SK_Hydro/sk_all_004_1000m.shp")%>%
  mutate(OBJECTID = row_number())

buf_1000 <- st_transform(buf_1000, 4269)%>%
  subset(definit==83)%>%#filter lakes
  subset(!is.na(namelk1en)|perm==59)# take only permanent lakes or lakes with given names


bf_buf_1000 <- st_join(bf,buf_1000)%>%
  drop_na(OBJECTID) # drop lakes without houses within boundary

st_write(bf_buf_500, "./Building_footprint/SK/sk_all_004_500m.shp", delete_dsn = T)
st_write(bf_buf_1000, "./Building_footprint/SK/sk_all_004_1000m.shp", delete_dsn = T)

####################################################################################

# Aggregate each province data into single file to represent the whole Canada.

#500
AB_500 <- st_read("./Building_footprint/AB/ab_all_004_500m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqkm)%>%
  rename("lakeareasqkm" = "areasqkm")%>%
  mutate(prov = "AB")
BC_500 <- st_read("./Building_footprint/BC/bc_all_004_500m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqkm)%>%
  rename("lakeareasqkm" = "areasqkm")%>%
  mutate(prov = "BC")
MB_500 <- st_read("./Building_footprint/MB/mb_all_004_500m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqkm)%>%
  rename("lakeareasqkm" = "areasqkm")%>%
  mutate(prov = "MB")
NB_500 <- st_read("./Building_footprint/NB/nb_all_004_500m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqkm)%>%
  rename("lakeareasqkm" = "areasqkm")%>%
  mutate(prov = "NB")
NL_500 <- st_read("./Building_footprint/NL/nl_all_004_500m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqkm)%>%
  rename("lakeareasqkm" = "areasqkm")%>%
  mutate(prov = "NL")
NS_500 <- st_read("./Building_footprint/NS/ns_all_004_500m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqkm)%>%
  rename("lakeareasqkm" = "areasqkm")%>%
  mutate(prov = "NS")
ON_500 <- st_read("./Building_footprint/ON/on_all_004_500m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,Shape_Area)%>%
  rename("lakeareasqkm" = "Shape_Area")%>%
  mutate(prov = "ON")
PE_500 <- st_read("./Building_footprint/PE/pe_all_004_500m.shp")%>%
  select(FID_1,areasqm_x,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqm_y)%>%
  rename("lakeareasqkm" = "areasqm_y",
         "areasqm" = "areasqm_x")%>%
  mutate(prov = "PE")
QC_500 <- st_read("./Building_footprint/QC/qc_all_004_500m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqkm)%>%
  rename("lakeareasqkm" = "areasqkm")%>%
  mutate(prov = "QC")
SK_500 <- st_read("./Building_footprint/SK/sk_all_004_500m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqkm)%>%
  rename("lakeareasqkm" = "areasqkm")%>%
  mutate(prov = "SK")

can_500 <- rbind(AB_500,BC_500,MB_500,NB_500,NL_500,NS_500,ON_500,PE_500,QC_500,SK_500)

#1000
AB_1000 <- st_read("./Building_footprint/AB/ab_all_004_1000m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqkm)%>%
  rename("lakeareasqkm" = "areasqkm")%>%
  mutate(prov = "AB")
BC_1000 <- st_read("./Building_footprint/BC/bc_all_004_1000m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqkm)%>%
  rename("lakeareasqkm" = "areasqkm")%>%
  mutate(prov = "BC")
MB_1000 <- st_read("./Building_footprint/MB/mb_all_004_1000m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqkm)%>%
  rename("lakeareasqkm" = "areasqkm")%>%
  mutate(prov = "MB")
NB_1000 <- st_read("./Building_footprint/NB/nb_all_004_1000m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqkm)%>%
  rename("lakeareasqkm" = "areasqkm")%>%
  mutate(prov = "NB")
NL_1000 <- st_read("./Building_footprint/NL/nl_all_004_1000m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqkm)%>%
  rename("lakeareasqkm" = "areasqkm")%>%
  mutate(prov = "NL")
NS_1000 <- st_read("./Building_footprint/NS/ns_all_004_1000m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqkm)%>%
  rename("lakeareasqkm" = "areasqkm")%>%
  mutate(prov = "NS")
ON_1000 <- st_read("./Building_footprint/ON/on_all_004_1000m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,Shape_Area)%>%
  rename("lakeareasqkm" = "Shape_Area")%>%
  mutate(prov = "ON")
PE_1000 <- st_read("./Building_footprint/PE/pe_all_004_1000m.shp")%>%
  select(FID_1,areasqm_x,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqm_y)%>%
  rename("lakeareasqkm" = "areasqm_y",
         "areasqm" = "areasqm_x")%>%
  mutate(prov = "PE")
QC_1000 <- st_read("./Building_footprint/QC/qc_all_004_1000m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqkm)%>%
  rename("lakeareasqkm" = "areasqkm")%>%
  mutate(prov = "QC")
SK_1000 <- st_read("./Building_footprint/SK/sk_all_004_1000m.shp")%>%
  select(FID_1,areasqm,OBJECTID,feature_id,perm_en,perm,shorlev,definit_en,definit,name_id,areasqkm)%>%
  rename("lakeareasqkm" = "areasqkm")%>%
  mutate(prov = "SK")

can_1000 <- rbind(AB_1000,BC_1000,MB_1000,NB_1000,NL_1000,NS_1000,ON_1000,PE_1000,QC_1000,SK_1000)


st_write(can_500, "./Building_footprint/CAN/can_all_004_500m.shp", delete_dsn = T)
st_write(can_1000, "./Building_footprint/CAN/can_all_004_1000m.shp", delete_dsn = T)



# Canada - building foot print

AB <- st_read("./Building_footprint/AB/ab.shp")
BC <- st_read("./Building_footprint/BC/bc.shp")
MB <- st_read("./Building_footprint/MB/mb.shp")
NB <- st_read("./Building_footprint/NB/nb.shp")
NL <- st_read("./Building_footprint/NL/nl.shp")
NS <- st_read("./Building_footprint/NS/ns.shp")
ON <- st_read("./Building_footprint/ON/on.shp")
PE <- st_read("./Building_footprint/PE/pe.shp")
QC <- st_read("./Building_footprint/QC/qc.shp")
SK <- st_read("./Building_footprint/SK/sk.shp")

can <- rbind(AB,BC,MB,NB,NL,NS,ON,PE,QC,SK)


st_write(can, "./Building_footprint/CAN/can_all.shp", delete_dsn = T)









