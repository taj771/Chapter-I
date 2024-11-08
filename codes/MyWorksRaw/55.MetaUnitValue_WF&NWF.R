# clear memory
rm(list = ls())


## Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  metafor,
  dplyr,
  tidyverse,
  modelsummary
)


#df_2 <- read.csv("./metadata/meta_data_all_distance.csv")

df_250 <- read.csv("./metadata/meta_data_distance_250m.csv")%>%
  #filter(study_name != "Wolf et al 2022")%>%
  ungroup()%>%
  mutate(obsid = row_number())%>%
  group_by(study_name,region)%>% # create cluster only based on unique study
  mutate(cluster_id = cur_group_id())%>%
  ungroup()%>%
  mutate(vi = 1/sampsize)%>% # define proxy of variance as inverse sample size
  mutate(inv_vi = 1/vi)%>%
  group_by(cluster_id)%>%
  mutate(sum_inv_vi = sum(inv_vi))%>%
  mutate(w2 = inv_vi/sum_inv_vi)%>%
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


df_750 <- read.csv("./metadata/meta_data_distance_750m.csv")%>%
  mutate(obsid = row_number())%>%
  group_by(study_name,region)%>%  # create cluster only based on unique study
  mutate(cluster_id = cur_group_id())%>%
  ungroup()%>%
  mutate(vi = 1/sampsize)%>% # define proxy of variance as inverse sample size
  mutate(inv_vi = 1/vi)%>%
  group_by(cluster_id)%>%
  mutate(sum_inv_vi = sum(inv_vi))%>%
  mutate(w2 = inv_vi/sum_inv_vi)%>%
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



panels <- list(
  "Waterfront distance-250m" = list(
    "Unweighted" = rma(yi = elast, vi=vi, data=df_250, method="EE", weighted=FALSE, level = 95),
    #"Cluster weighted " = rma(yi = elast, vi=vi, data=df_250, method="EE",weighted=T,weights = w_1, level = 95),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= rma(yi = cw_elast, vi, data=df_wf, method="EE",weighted=T, level = 95),
    #"Cluster sample size weight"= rma(yi = elast, vi=vi, data=df_250, method="EE",weighted=T,weights =w2, level = 95),
    #"Random effect (sample size variance)"= rma.mv(yi = cw_elast, vi, random = ~ 1 | cluster_id/obsid, data=df_wf),
    "Random effect (log(sample size variance))"= rma.mv(yi = elast, vi, random = ~ 1 | cluster_id/obsid, data=df_250)
    
  ),
  
  "Waterfront distance-750m" = list(
    "Unweighted" = rma(yi = elast, vi=vi, data=df_750, method="EE", weighted=FALSE, level = 95),
    #"Cluster weighted " = rma(yi = elast, vi=vi, data=df_750, method="EE",weighted=T,weights = w_1, level = 95),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= rma(yi = cw_elast, vi, data=df_nwf, method="EE",weighted=T, level = 95),
    #"Cluster sample size weight"= rma(yi = elast, vi=vi, data=df_750, method="EE",weighted=T,weights = w2, level = 95),
    #"Random effect (sample size variance)"= rma.mv(yi = cw_elast, vi, random = ~ 1 | cluster_id/obsid, data=df_nwf),
    "Random effect (log(sample size variance))"= rma.mv(yi = elast, vi, random = ~ 1 | cluster_id/obsid, data=df_750, control=list(rel.tol=1e-8))
  )
)


panels_robu <- list(
  "Waterfront distance-250m" = list(
    "Unweighted" = robust(panels$`Waterfront distance-250m`$`Unweighted`, cluster = obsid),
    #"Cluster weighted " = robust(panels$`Waterfront distance-250m`$`Cluster weighted `, cluster = cluster_id),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= robust(panels$Waterfront$`Fixed effect - Cluster weighted - Varance Adjusted (sample size)`, cluster = cluster_id),
    #"Cluster sample size weight"= robust(panels$`Waterfront distance-250m`$`Cluster sample size weight`, cluster = cluster_id),
    #"Random effect (sample size variance)"= robust(panels$Waterfront$`Random effect (sample size variance)`, cluster = cluster_id),
    "Random effect "= robust(panels$`Waterfront distance-250m`$`Random effect (log(sample size variance))`, cluster = cluster_id)
  ),
  "Waterfront distance-750m" = list(
    "Unweighted" =robust(panels$`Waterfront distance-750m`$`Unweighted`, cluster = obsid),
    #"Cluster weighted " = robust(panels$`Waterfront distance-750m`$`Cluster weighted `, cluster = cluster_id),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= robust(panels$`Non-waterfront`$`Fixed effect - Cluster weighted - Varance Adjusted (sample size)`, cluster = cluster_id),
    #"Cluster sample size weight"= robust(panels$`Waterfront distance-750m`$`Cluster sample size weight`, cluster = cluster_id),
    #"Random effect (sample size variance)"= robust(panels$`Non-waterfront`$`Random effect (sample size variance)`, cluster = cluster_id),
    "Random effect"= robust(panels$`Waterfront distance-750m`$`Random effect (log(sample size variance))`, cluster = cluster_id)
    
  )
)



f <- function(x) format(round(x, 3), big.mark=",")


gm <- list(
  list("raw" = "nobs", "clean" = "Num. of Obs.", "fmt" = f)
)


modelsummary(
  panels_robu,
  shape = "rbind",
  stars = TRUE, gof_map = gm)



modelsummary(
  panels_robu,
  shape = "rbind",
  stars = TRUE, gof_map = gm,
  coef_rename = c("overall" = "Estimated mean"), output = "./results/Tables/table2_distabuffer_elast.tex")



