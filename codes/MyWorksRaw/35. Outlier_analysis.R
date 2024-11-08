
###########################################################################
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


################################################################################

p <- ggplot(df_500, aes(x = elast)) +
  geom_histogram(alpha = 20, binwidth = 0.1) + 
  scale_x_continuous(limits = c(-5, 10))+
  theme_bw()+
  labs(x = "Elasticity", y = "Density")




rstud_wf%>%
  ggplot(aes(x=obsid,y=resid))+
  ylab("rstudent")+
  geom_line()+
  geom_point()+
  scale_x_continuous(limits = c(0, 270), breaks = seq(0, 270, 10))+
  geom_hline(yintercept = 2)+
  geom_hline(yintercept = -2)+
  theme_bw()

rstud_wf1 <- rstud_wf_100%>%
  filter(!between(resid,-2,2))

df_100 <- df_100%>%
  subset(df_100$obsid%in% rstud_wf1$obsid)

##########old#########old##########old#############
#load data
df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")

# plot graph to elicit outliers
p <- ggplot(df, aes(x = elast_sim)) +
  geom_histogram(alpha = 20, binwidth = 0.1) + 
  scale_x_continuous(limits = c(-5, 10))+
  theme_bw()+
  labs(x = "Elasticity", y = "Density")

p 


## Subset: Main models
# waterfront
df.wf <- df[which(df$distbuf == 1), ]

# non-waterfront
df.nwf <- df[which(df$distbuf == 2), ]
#df.nwf$sampsize <- as.numeric(as.character(df.nwf$sampsize))


df.wf.re <- df.wf%>%
  mutate(vi = 1/sampsize)%>% # use sample size instead of variance
  #filter(vi>0)%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()%>%
  rownames_to_column("id")

df.nwf.re <- df.nwf%>%
  mutate(vi = 1/sampsize)%>% # use sample size instead of variance
  #filter(vi>0)%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()%>%
  rownames_to_column("id")

#waterfront 
re.wf <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re)

rstud_wf <- rstudent.rma.mv(re.wf, cluster = df.wf.re$cluster )

rstud_wf <- rstud_wf$obs%>%
  as.data.frame()

rstud_wf <- rownames_to_column(rstud_wf, "obsid")

rstud_wf$obsid <- as.numeric(as.character(rstud_wf$obsid))

#use row id for the graph, obsid is different, that is equal to obs id in meta
# analysis
rstud_wf <- mutate(rstud_wf, id = row_number())


rstud_wf%>%
  ggplot(aes(x=obsid,y=resid))+
  ylab("rstudent")+
  geom_line()+
  geom_point()+
  scale_x_continuous(limits = c(0, 270), breaks = seq(0, 270, 10))+
  geom_hline(yintercept = 2)+
  geom_hline(yintercept = -2)+
  theme_bw()

# Cooks distance if want
cook_dis_wf <- cooks.distance(re.wf, cluster = df.wf.re$cluster)
plot(cooks.distance(re.wf), type="o", pch=19)

rstud_wf <- rstud_wf%>%
  filter(between(resid,-2,2))


df.wf.re <- df.wf.re%>%
  subset(df.wf.re$id%in% rstud_wf$obsid)

# write csv for future analysis

write.csv(df.wf.re, "./metadata/wo_outliers_wf.csv")


# non-waterfront

re.nwf <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.nwf.re)

rstud_nwf <- rstudent.rma.mv(re.nwf, cluster = df.nwf.re$cluster )

rstud_nwf <- rstud_nwf$obs%>%
  as.data.frame()

rstud_nwf <- rownames_to_column(rstud_nwf, "obsid")

rstud_nwf$obsid <- as.numeric(as.character(rstud_nwf$obsid))

#use row id for the graph, obsid is different, that is equal to obs id in meta
# analysis
rstud_nwf <- mutate(rstud_nwf, id = row_number())


rstud_nwf%>%
  ggplot(aes(x=id,y=resid))+
  ylab("rstudent")+
  geom_line()+
  geom_point()+
  scale_x_continuous(limits = c(0, 140), breaks = seq(0, 140, 10))+
  geom_hline(yintercept = 2)+
  geom_hline(yintercept = -2)+
  theme_bw()

rstud_nwf <- rstud_nwf%>%
  filter(between(resid,-2,2))

df.nwf.re <- mutate(df.nwf.re, id = row_number())


df.nwf.re <- df.nwf.re%>%
  subset(df.nwf.re$id%in% rstud_nwf$obsid)

# write csv for future analysis

write.csv(df.nwf.re, "./metadata/wo_outliers_nwf.csv")

