# This code identify the outliers in elasticity and remive them within 250m and 750m anc create data for our main analysis at 
# distance bins of 250m and 750m
# clear memory
rm(list = ls())

## Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  meta,
  metafor,
  tidyverse,
  robumeta,
  clubSandwich,
  pandoc,
  tinytex,
  modelsummary,
  kableExtra
)


df_2 <- read.csv("./metadata/Meta_data_at_50m_distcal_elast.csv")

df_wf <- df_2%>%
  filter(x==250)%>% # filter elasticitlt at appropriate distance
  #filter(study_name != "Wolf et al 2022")%>%
  ungroup()%>%
  mutate(obsid = row_number())%>%
  group_by(study_name,region)%>% # create cluster only based on unique study
  mutate(cluster_id = cur_group_id())%>%
  ungroup()%>%
  mutate(vi = 1/sampsize)%>% # define proxy of variance as inverse sample size
  mutate(vi_log = 1/log(sampsize))%>% # define proxy of variance as inverse log(sample size)
  group_by(cluster_id) %>%
  mutate(count = n())%>% # 
  mutate(w_1 = (1/count))%>% # define first weight - cluster weight based on the number of observations within cluster
  mutate(log_ss = log(sampsize))%>%
  mutate(log_ss_within_clus = sum(log_ss))%>% # for cluster sample size weight - sum sample size withi cluster
  mutate(w_2 = log(sampsize)/log_ss_within_clus)%>% # weight define as ratio between log(sample size)/log(sum of sample size within cluster)
  mutate(ave_ss_wt_clus = mean(sampsize))%>% #find average sample size within cluster
  ungroup()%>%
  mutate(log_ave_ss_wt_clus = log(ave_ss_wt_clus))%>%
  #mutate(ave_ss_acros_clus = sum(unique(ave_ss_wt_clus)))%>%# total sample size based on average sample size within a cluster
  mutate(w_3 = log_ave_ss_wt_clus/sum(log_ave_ss_wt_clus))


re_wf <- rma.mv(yi = y_value, vi_log, random = ~ 1 | cluster_id/obsid, data=df_wf)

rstud_wf <- rstudent.rma.mv(re_wf, cluster = df_wf$cluster_id )


df_wf <- df_wf%>%
  mutate(obsid = row_number())


rstud_wf <- rstud_wf$obs%>%
  as.data.frame()

rstud_wf <- rownames_to_column(rstud_wf, "obsid")


rstud_wf_out <- rstud_wf%>%
  filter(!between(resid,-2,2))



df_250 <- subset(df_wf, !(obsid%in%rstud_wf_out$obsid))

df_250 <- df_250[!is.na(df_250$y_value), ]

re_wf <- rma.mv(yi = y_value, vi, random = ~ 1 | cluster_id/obsid, data=df_250)



write.csv(df_250, "./metadata/meta_data_distance_250m.csv")


t <- subset(df_wf, (obsid%in%rstud_wf_out$obsid))

t <- t%>%
  select(obsid,y_value,study_name,sampsize)



df_wf <- df_2%>%
  filter(x==750)%>% # filter elasticitlt at appropriate distance
  #filter(study_name != "Wolf et al 2022")%>%
  ungroup()%>%
  mutate(obsid = row_number())%>%
  group_by(study_name,region)%>% # create cluster only based on unique study
  mutate(cluster_id = cur_group_id())%>%
  ungroup()%>%
  mutate(vi = 1/sampsize)%>% # define proxy of variance as inverse sample size
  mutate(vi_log = 1/log(sampsize))%>% # define proxy of variance as inverse log(sample size)
  group_by(cluster_id) %>%
  mutate(count = n())%>% # 
  mutate(w_1 = (1/count))%>% # define first weight - cluster weight based on the number of observations within cluster
  mutate(log_ss = log(sampsize))%>%
  mutate(log_ss_within_clus = sum(log_ss))%>% # for cluster sample size weight - sum sample size withi cluster
  mutate(w_2 = log(sampsize)/log_ss_within_clus)%>% # weight define as ratio between log(sample size)/log(sum of sample size within cluster)
  mutate(ave_ss_wt_clus = mean(sampsize))%>% #find average sample size within cluster
  ungroup()%>%
  mutate(log_ave_ss_wt_clus = log(ave_ss_wt_clus))%>%
  #mutate(ave_ss_acros_clus = sum(unique(ave_ss_wt_clus)))%>%# total sample size based on average sample size within a cluster
  mutate(w_3 = log_ave_ss_wt_clus/sum(log_ave_ss_wt_clus))


re_wf <- rma.mv(yi = y_value, vi_log, random = ~ 1 | cluster_id/obsid, data=df_wf)

rstud_wf <- rstudent.rma.mv(re_wf, cluster = df_wf$cluster_id )


df_wf <- df_wf%>%
  mutate(obsid = row_number())


rstud_wf <- rstud_wf$obs%>%
  as.data.frame()

rstud_wf <- rownames_to_column(rstud_wf, "obsid")


rstud_wf_out <- rstud_wf%>%
  filter(!between(resid,-2,2))


df_750 <- subset(df_wf, !(obsid%in%rstud_wf_out$obsid))

df_750 <- df_750[!is.na(df_750$y_value), ]

write.csv(df_750, "./metadata/meta_data_distance_750m.csv")
