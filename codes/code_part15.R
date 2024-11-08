# Benefits transfer error calculation
# Unit valye transfer error 
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

# load data - wo outliers 
df_100 <- read.csv("./metadata/meta_data_distance_250m.csv")%>%
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
  mutate(w_3 = log_ave_ss_wt_clus/sum(log_ave_ss_wt_clus))%>%
  rename("elast"="y_value")


df_400 <- read.csv("./metadata/meta_data_distance_750m.csv")%>%
  mutate(obsid = row_number())%>%
  group_by(study_name,region)%>%  # create cluster only based on unique study
  mutate(cluster_id = cur_group_id())%>%
  ungroup()%>%
  mutate(vi = 1/sampsize)%>% # define proxy of variance as inverse sample size
  mutate(vi_log = 1/log(sampsize))%>% # define proxy of variance as inverse log(sample size)
  group_by(cluster_id) %>%
  mutate(count = n())%>%
  mutate(w_1 = (1/count))%>% # define first weight - cluster weight based on the number of observations within cluster
  mutate(log_ss = log(sampsize))%>%
  mutate(log_ss_within_clus = sum(log_ss))%>% # for cluster sample size weight - sum sample size withi cluster
  mutate(w_2 = log(sampsize)/log_ss_within_clus)%>% # weight define as ratio between log(sample size)/log(sum of sample size within cluster)
  mutate(ave_ss_wt_clus = mean(sampsize))%>% #find average sample size within cluster
  ungroup()%>%
  mutate(log_ave_ss_wt_clus = log(ave_ss_wt_clus))%>%
  #mutate(ave_ss_acros_clus = sum(unique(ave_ss_wt_clus)))%>%# total sample size based on average sample size within a cluster
  mutate(w_3 = log_ave_ss_wt_clus/sum(log_ave_ss_wt_clus))%>%
  rename("elast"="y_value")




##############################################################################
# Random efefct size modeling
##############################################################################
# random effect - 100m
#df_100 <- df_100%>%
#group_by(cluster_id)%>%
#mutate(k = n())%>%
#filter(k > 1)

result_3 <- list()
result_4 <- list()

#loop through obsid and extract betas
for(i in unique(df_100$obsid)){
  #construct linear model (metafor package)
  #elas_3 <- rma.mv(yi = elast, vi,random = ~ 1 | obsid ,data=subset(df_100, df_100$obsid == i),level = 95)
  elas_4 <- rma.mv(yi = elast, vi,random = ~ 1 | cluster_id/obsid,data=subset(df_100, df_100$obsid != i),control=list(rel.tol=1e-8),level = 95,parallel="multicore")
  
  #create data.frame containing intercept left out and coefficient
  #result.dt_3 <- data.frame(beta_1 = coef(elas_3),
  #cluster = i)
  result.dt_4 <- data.frame(beta_2 = coef(elas_4),
                            obsid = i)
  #bind to list
  #result_3[[i]] <- result.dt_3
  result_4[[i]] <- result.dt_4
}


#bind to data.frame
#result_3 <- do.call(rbind, result_3)
result_4 <- do.call(rbind, result_4)
#head(result_3)
head(result_4)


wf.re <- rma.mv(yi = elast, vi,random = ~ 1 | cluster_id/obsid ,data=df_100,level = 95)

re_weight <- weights(wf.re, type="rowsum")[1:669]%>%
  as.data.frame()%>%
  mutate(id = row_number())%>%
  rename(re_w = ".")%>%
  rename("obsid" = "id")

final_100 <- df_100%>%
  left_join(result_4)%>%
  left_join(re_weight)%>%
  mutate(re_w = re_w/100)%>%
  mutate(elast_1 = elast*re_w)%>%
  mutate(error = beta_2 - elast)%>%
  mutate(error = abs(error))%>%
  mutate(error = error/beta_2*100)

median(final_100$error)




#####################################################################
# random effect - 400m
#result_3 <- list()
result_4 <- list()


#loop through obsid and extract betas
for(i in unique(df_400$obsid)){
  #construct linear model (metafor package)
  #elas_3 <- rma.mv(yi = elast, vi,random = ~ 1 | cluster_id/obsid ,data=subset(df_400, df_400$cluster_id == i),level = 95)
  elas_4 <- rma.mv(yi = elast, vi,random = ~ 1 | cluster_id/obsid,data=subset(df_400, df_400$obsid != i) ,level = 95, parallel="multicore")
  
  #create data.frame containing intercept left out and coefficient
  #result.dt_3 <- data.frame(beta_1 = coef(elas_3),
  #cluster = i)
  result.dt_4 <- data.frame(beta_2 = coef(elas_4),
                            cluster_id = i)
  #bind to list
  #result_3[[i]] <- result.dt_3
  result_4[[i]] <- result.dt_4
}

#bind to data.frame
#result_3 <- do.call(rbind, result_3)
result_4 <- do.call(rbind, result_4)

#head(result_3)
head(result_4)


nwf.re <- rma.mv(yi = elast, vi,random = ~ 1 | cluster_id/obsid ,data=df_400,level = 95)

re_weight_nwf <- weights(nwf.re, type="rowsum")[1:225]%>%
  as.data.frame()%>%
  mutate(id = row_number())%>%
  rename(re_w = ".")%>%
  rename("obsid" = "id")



final_400 <- df_400%>%
  left_join(result_4)%>%
  left_join(re_weight_nwf)%>%
  mutate(re_w = re_w/100)%>%
  mutate(elast_1 = elast*re_w)%>%
  mutate(error = beta_2 - elast)%>%
  mutate(error = abs(error))%>%
  mutate(error = error/beta_2*100)


RE_100 <- median(final_100$error, na.rm = T)
RE_400 <- median(final_400$error, na.rm = T)