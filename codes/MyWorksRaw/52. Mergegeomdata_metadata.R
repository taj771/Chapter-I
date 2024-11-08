# clear memory
rm(list = ls())



#load geomdata
df <- read.csv("./metadata/Meta_dataset_from_geomsmooth.csv")

# create unique name column
df$id <- paste(df$study_name, "-", df$model.name)

df_geom <- df%>%
  select(-study_name,-model.name)

#create studyid
t <- df_geom%>%
  select(id)%>%
  unique()%>%
  group_by(id)%>%
  mutate(studyid = cur_group_id())

#create unique geomid
df <- df%>%
  left_join(t)%>%
  ungroup()%>%
  mutate(geomid = row_number())


#load metadata csv
df1 <- read.csv("./metadata/Meta_dataset_water_clarity_TJ.csv")%>%
  select(studyname,geog,sampsize,specname,pubtype,wbtype,funcform,wbsize,avgwqvar,west, midwest, northeast, south, multireg,canada,state, region)%>%
  rename("model.name" = "specname",
         "study_name" = "studyname")%>%
  filter(study_name != "Steinnes 1992")

# create unique name column i similar with geomdata
df1$id <- paste(df1$study_name, "-", df1$model.name)

df1 <- df1%>%
  select(-study_name,-model.name)

#create studyid - similar with geomdata
tt <- df1%>%
  select(id)%>%
  unique()%>%
  filter(id != " - " )%>%
  group_by(id)%>%
  mutate(studyid = cur_group_id())

  
df1 <- df1%>%
  left_join(tt)

# join geomdata and meta csv data
df_final <- df%>%
  left_join(df1)

# keeo unique geomid
df_final <- df_final[!duplicated(df_final[,c('geomid')]),]

# cleate cluster id (study name + geog)
df_final <- df_final%>%
  group_by(study_name,geog)%>% # cluster definition
  mutate(cluster_id = cur_group_id())

df_final$dist <- as.numeric(df_final$dist)




###########################################################################
df <- subset(df_final, !is.na(sampsize))
df <- df%>%
  filter_all(all_vars(!is.infinite(.)))
df_1 <- df[!duplicated(df[,c('id')]),]

#df_1$sampsize <- as.numeric(as.character(df_1$sampsize))


df_2 <- df%>%
  left_join(df_1)

write.csv(df_2, "./metadata/meta_data_all_distance.csv")

wf_100 <- 100
wf_200 <- 200
wf_300 <- 300
wf_350 <- 360
wf_400 <- 400
wf_500 <- 520
wf_600 <- 600
wf_700 <- 700
wf_800 <- 800
wf_900 <- 900
wf_1000 <- 1000



df_wf_100 <- df_2%>%
  filter(dist==wf_100)%>%
  group_by(study_name,geog)%>%
  mutate(vi = 1/sampsize)%>%
  mutate(vi_log = 1/log(sampsize))%>%
  mutate(count = n())%>%
  mutate(w_1 = (1/count))%>%
  mutate(ss_within_clus = sum(sampsize))%>%
  mutate(w_2 = log(sampsize)/log(ss_within_clus))%>%
  #mutate(cw_elast = elast*w_1)%>%
  #mutate(w1 = sum(w_1))%>%
  ungroup()

df_wf_200 <- df_2%>%
  filter(dist==wf_200)%>%
  group_by(study_name,geog)%>%
  mutate(vi = 1/sampsize)%>%
  mutate(vi_log = 1/log(sampsize))%>%
  mutate(count = n())%>%
  mutate(w_1 = (1/count))%>%
  mutate(ss_within_clus = sum(sampsize))%>%
  mutate(w_2 = log(sampsize)/log(ss_within_clus))%>%
  #mutate(cw_elast = elast*w_1)%>%
  #mutate(w1 = sum(w_1))%>%
  ungroup()

df_wf_300 <- df_2%>%
  filter(dist==wf_300)%>%
  group_by(study_name,geog)%>%
  mutate(vi = 1/sampsize)%>%
  mutate(vi_log = 1/log(sampsize))%>%
  mutate(count = n())%>%
  mutate(w_1 = (1/count))%>%
  mutate(ss_within_clus = sum(sampsize))%>%
  mutate(w_2 = log(sampsize)/log(ss_within_clus))%>%
  #mutate(cw_elast = elast*w_1)%>%
  #mutate(w1 = sum(w_1))%>%
  ungroup()


df_wf_350 <- df_2%>%
  filter(dist==wf_350)%>%
  group_by(study_name,geog)%>%
  mutate(vi = 1/sampsize)%>%
  mutate(vi_log = 1/log(sampsize))%>%
  mutate(count = n())%>%
  mutate(w_1 = (1/count))%>%
  mutate(ss_within_clus = sum(sampsize))%>%
  mutate(w_2 = log(sampsize)/log(ss_within_clus))%>%
  #mutate(cw_elast = elast*w_1)%>%
  #mutate(w1 = sum(w_1))%>%
  ungroup()

df_wf_400 <- df_2%>%
  filter(dist==wf_400)%>%
  group_by(study_name,geog)%>%
  mutate(vi = 1/sampsize)%>%
  mutate(vi_log = 1/log(sampsize))%>%
  mutate(count = n())%>%
  mutate(w_1 = (1/count))%>%
  mutate(ss_within_clus = sum(sampsize))%>%
  mutate(w_2 = log(sampsize)/log(ss_within_clus))%>%
  #mutate(cw_elast = elast*w_1)%>%
  #mutate(w1 = sum(w_1))%>%
  ungroup()

df_wf_500 <- df_2%>%
  filter(dist==wf_500)%>%
  group_by(study_name,geog)%>%
  mutate(vi = 1/sampsize)%>%
  mutate(vi_log = 1/log(sampsize))%>%
  mutate(count = n())%>%
  mutate(w_1 = (1/count))%>%
  mutate(ss_within_clus = sum(sampsize))%>%
  mutate(w_2 = log(sampsize)/log(ss_within_clus))%>%
  #mutate(cw_elast = elast*w_1)%>%
  #mutate(w1 = sum(w_1))%>%
  ungroup()

df_wf_600 <- df_2%>%
  filter(dist==wf_600)%>%
  group_by(study_name,geog)%>%
  mutate(vi = 1/sampsize)%>%
  mutate(vi_log = 1/log(sampsize))%>%
  mutate(count = n())%>%
  mutate(w_1 = (1/count))%>%
  mutate(ss_within_clus = sum(sampsize))%>%
  mutate(w_2 = log(sampsize)/log(ss_within_clus))%>%
  #mutate(cw_elast = elast*w_1)%>%
  #mutate(w1 = sum(w_1))%>%
  ungroup()

df_wf_700 <- df_2%>%
  filter(dist==wf_700)%>%
  group_by(study_name,geog)%>%
  mutate(vi = 1/sampsize)%>%
  mutate(vi_log = 1/log(sampsize))%>%
  mutate(count = n())%>%
  mutate(w_1 = (1/count))%>%
  mutate(ss_within_clus = sum(sampsize))%>%
  mutate(w_2 = log(sampsize)/log(ss_within_clus))%>%
  #mutate(cw_elast = elast*w_1)%>%
  #mutate(w1 = sum(w_1))%>%
  ungroup()

df_wf_800 <- df_2%>%
  filter(dist==wf_800)%>%
  group_by(study_name,geog)%>%
  mutate(vi = 1/sampsize)%>%
  mutate(vi_log = 1/log(sampsize))%>%
  mutate(count = n())%>%
  mutate(w_1 = (1/count))%>%
  mutate(ss_within_clus = sum(sampsize))%>%
  mutate(w_2 = log(sampsize)/log(ss_within_clus))%>%
  #mutate(cw_elast = elast*w_1)%>%
  #mutate(w1 = sum(w_1))%>%
  ungroup()

df_wf_900 <- df_2%>%
  filter(dist==wf_900)%>%
  group_by(study_name,geog)%>%
  mutate(vi = 1/sampsize)%>%
  mutate(vi_log = 1/log(sampsize))%>%
  mutate(count = n())%>%
  mutate(w_1 = (1/count))%>%
  mutate(ss_within_clus = sum(sampsize))%>%
  mutate(w_2 = log(sampsize)/log(ss_within_clus))%>%
  #mutate(cw_elast = elast*w_1)%>%
  #mutate(w1 = sum(w_1))%>%
  ungroup()

df_wf_1000 <- df_2%>%
  filter(dist==wf_1000)%>%
  group_by(study_name,geog)%>%
  mutate(vi = 1/sampsize)%>%
  mutate(vi_log = 1/log(sampsize))%>%
  mutate(count = n())%>%
  mutate(w_1 = (1/count))%>%
  mutate(ss_within_clus = sum(sampsize))%>%
  mutate(w_2 = log(sampsize)/log(ss_within_clus))%>%
  #mutate(cw_elast = elast*w_1)%>%
  #mutate(w1 = sum(w_1))%>%
  ungroup()



write.csv(df_wf_100, "./metadata/Meta_data_waterfront_100.csv", row.names = F)
write.csv(df_wf_200, "./metadata/Meta_data_waterfront_200.csv", row.names = F)
write.csv(df_wf_300, "./metadata/Meta_data_waterfront_300.csv", row.names = F)
write.csv(df_wf_350, "./metadata/Meta_data_waterfront_350.csv", row.names = F)
write.csv(df_wf_400, "./metadata/Meta_data_waterfront_400.csv", row.names = F)
write.csv(df_wf_500, "./metadata/Meta_data_waterfront_500.csv", row.names = F)
write.csv(df_wf_600, "./metadata/Meta_data_waterfront_600.csv", row.names = F)
write.csv(df_wf_700, "./metadata/Meta_data_waterfront_700.csv", row.names = F)
write.csv(df_wf_800, "./metadata/Meta_data_waterfront_800.csv", row.names = F)
write.csv(df_wf_900, "./metadata/Meta_data_waterfront_900.csv", row.names = F)
write.csv(df_wf_1000, "./metadata/Meta_data_waterfront_1000.csv", row.names = F)


write_csv(df_2, "./metadata/Meta_dataset_from_geomsmooth_filter.csv")


#############################################################################



df_plot <- df_2%>%
  group_by(study_name,geog)%>%
  mutate(vi = 1/sampsize)%>%
  mutate(vi_log = 1/log(sampsize))%>%
  mutate(count = n())%>%
  mutate(w_1 = (1/count))

ggplot(df_plot, aes(x = dist, y = elast, colour = distance_spec)) + 
  geom_smooth(aes(weight = w_1),method ="gam",span = 0.07) + theme_bw() + 
  scale_x_continuous(breaks=seq(0, 2050, 250), expand=c(0, 0), limits=c(0, 2050))+
  scale_y_continuous(expand=c(0, 0), limits=c(0, 0.2))+
  scale_colour_discrete("")+
  labs(x = "Distance (Meters)", y = "Elasticity")+
  labs(color='Distance Specification')+
  theme(legend.position = "bottom")


ggplot(df_plot) +
  #geom_smooth(aes(dist, elast, weight = w_1, colour = "Cluster Weight"),
              #method = "gam", formula = y ~ s(x, bs = "cs")) +
  geom_smooth(aes(dist, cw_elast, weight = vi_log, colour = "Smooth conditional mean - Weighted"),
              method = "gam",formula = y ~ s(x, bs = "cs")) +
  geom_smooth(method = "gam",formula = y ~ s(x, bs = "cs"), aes(dist, elast,colour = "Smooth conditional mean - Unweight")) +
  theme_bw() + 
  scale_x_continuous(breaks=seq(0, 2050, 250), expand=c(0, 0), limits=c(0, 2050))+
  scale_y_continuous(expand=c(0, 0), limits=c(0, 0.15))+
  scale_colour_discrete("")+
  labs(x = "Distance (Meters)", y = "Elasticity")+
  labs(color='Distance Specification')+
  theme(legend.position = "bottom")+
  scale_colour_manual(values = c("red", "blue", "black"))+
  labs(col='')+
  theme(legend.position = "bottom")



library(ggplot2)
theme_set(theme_bw())
library(dplyr)
library(mgcv)
library(tidymv)

g1 <- gam(elast ~ s(dist, bs="cs"), data = df_2)

model_p <- predict_gam(g1, length_out =200)

model_p %>%
  ggplot(aes(dist, fit)) +
  geom_smooth()

# seperate each type

df_3 <- df_2%>%
  filter(distance_spec == "Discrete Distance")



ggplot(df_3, aes(dist, elast)) +
  geom_smooth(aes(weight = w_1, colour = "Cluster Weight"),
              method = "gam") +
  geom_smooth(aes(weight = cluster_ss_w, colour = "Cluster Sample Weight"),
              method = "gam") +
  geom_smooth(method = "gam", aes(colour = "No Weight")) +
  theme_bw() + 
  scale_x_continuous(breaks=seq(0, 2000, 250), expand=c(0, 0), limits=c(0, 2000))+
  scale_y_continuous(expand=c(0, 0), limits=c(0, 0.2))+
  scale_colour_discrete("")+
  labs(x = "Distance (Meters)", y = "Elasticity")+
  labs(color='Distance Specification')+
  theme(legend.position = "bottom")+
  scale_colour_manual(values = c("blue", "red", "black"))+
  labs(col='')+
  theme(legend.position = "bottom")


df_4 <- df_2%>%
  filter(distance_spec == "Continious Distance and Discrete Distance")

ggplot(df_4, aes(dist, elast)) +
  geom_smooth(aes(weight = w_1, colour = "Cluster Weight"),
              method = "gam") +
  geom_smooth(aes(weight = cluster_ss_w, colour = "Cluster Sample Weight"),
              method = "gam") +
  geom_smooth(method = "gam", aes(colour = "No Weight")) +
  theme_bw() + 
  scale_x_continuous(breaks=seq(0, 2000, 250), expand=c(0, 0), limits=c(0, 2000))+
  scale_y_continuous(expand=c(0, 0), limits=c(0, 0.2))+
  scale_colour_discrete("")+
  labs(x = "Distance (Meters)", y = "Elasticity")+
  labs(color='Distance Specification')+
  theme(legend.position = "bottom")+
  scale_colour_manual(values = c("blue", "red", "black"))+
  labs(col='')+
  theme(legend.position = "bottom")


df_5 <- df_2%>%
  filter(distance_spec == "Continious Distance Variable")

ggplot(df_5, aes(dist, elast)) +
  geom_smooth(aes(weight = w_1, colour = "Cluster Weight"),
              method = "gam") +
  geom_smooth(aes(weight = cluster_ss_w, colour = "Cluster Sample Weight"),
              method = "gam") +
  geom_smooth(method = "gam", aes(colour = "No Weight")) +
  theme_bw() + 
  scale_x_continuous(breaks=seq(0, 2000, 250), expand=c(0, 0), limits=c(0, 2000))+
  scale_y_continuous(expand=c(0, 0), limits=c(0, 0.2))+
  scale_colour_discrete("")+
  labs(x = "Distance (Meters)", y = "Elasticity")+
  labs(color='Distance Specification')+
  theme(legend.position = "bottom")+
  scale_colour_manual(values = c("blue", "red", "black"))+
  labs(col='')+
  theme(legend.position = "bottom")

df_6 <- df_2%>%
  filter(distance_spec == "No Distance Variable")

ggplot(df_6, aes(dist, elast)) +
  geom_smooth(aes(weight = w_1, colour = "Cluster Weight"),
              method = "gam") +
  geom_smooth(aes(weight = cluster_ss_w, colour = "Cluster Sample Weight"),
              method = "gam") +
  geom_smooth(method = "gam", aes(colour = "No Weight")) +
  theme_bw() + 
  scale_x_continuous(breaks=seq(0, 2000, 250), expand=c(0, 0), limits=c(0, 2000))+
  scale_y_continuous(expand=c(0, 0), limits=c(0, 0.2))+
  scale_colour_discrete("")+
  labs(x = "Distance (Meters)", y = "Elasticity")+
  labs(color='Distance Specification')+
  theme(legend.position = "bottom")+
  scale_colour_manual(values = c("blue", "red", "black"))+
  labs(col='')+
  theme(legend.position = "bottom")

# filter elasticity at predeined distance point

wf <- 260
nwf <- 500

df_wf <- df_2%>%
  filter(dist==wf)

df_nwf <- df_2%>%
  filter(dist==nwf)

write.csv(df_wf, "./metadata/Meta_data_waterfront.csv", row.names = F)

write.csv(df_nwf, "./metadata/Meta_data_nonwaterfront.csv", row.names = F)
