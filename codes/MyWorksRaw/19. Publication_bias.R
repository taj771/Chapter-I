rm(list = ls())
library(dplyr)
library(meta)
library(metafor)
###############################################################################

# set up data set with required variables and necessary weighting schemes


# load data - with outliers 
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




res <- rma.mv(yi = elast, vi, random = ~ 1 | cluster_id/obsid, data=df_100)
funnel.rma(res, main="", pch = 20,back=NA, shade=NA,hlines="lightgray",ylab = "1/sample size", xlab = "Elasticity - Lakeshore within 500 meters")

res.nwf <- rma.mv(yi = elast, vi, random = ~ 1 | cluster_id/obsid, data=df_400)
funnel.rma(res.nwf, main="", pch = 20,back=NA, shade=NA,hlines="lightgray",ylab = "1/sample size", xlab = "Elasticity - Lakeshore within 500-1,000 meters")


res_100 <- rma(yi = elast, vi, data=df_100,level = 95)
res_400 <- rma(yi = elast, vi, data=df_400,level = 95)



p1 <- ggplot(df_100, aes(x=elast, y = vi))+ 
  geom_point()+
  geom_vline(xintercept = 0)+
  theme_bw()+
  xlab("Elasticity - Waterfront Bin")+
  ylab("Precision of the estimate (1/Sample Size)")+
  theme(text=element_text(size=8))

p4 <- ggplot(df_400, aes(x=elast, y = vi))+ 
  geom_point()+
  geom_vline(xintercept = 0)+
  theme_bw()+
  xlab("Elasticity-Near-waterfront Bin")+
  ylab("Precision of the estimate (1/Sample Size)")+
  theme(text=element_text(size=8))

library(patchwork)
p1+p4+plot_layout(ncol = 1)


regtest(res_100)
regtest(res_400)



regtest(res_100,predictor="ni", ni = sampsize, data=df_100, model="lm")
regtest(res_400,predictor="ni", ni = sampsize, data=df_400, model="lm")



#### bias correction

library(fixest)
library(modelsummary)

feols(wqelast ~ vi, data = df_wf)

feols(wqelast ~ vi, data = df_nwf)

feols(wqelast ~ vi + wf, data = df)

rma.mv(yi = wqelast, vi,mods = ~ vi, random = ~ 1 | cluster/obsid, data=df_wf,level = 95,test = "t")

res_wf  <- rma(yi = wqelast, vi,data=df_wf, method="EE",weights=cluster_ss_w,level = 95, test = "t")

taf <- trimfill(res_wf, estimator="R0")

res_wf <- rma.mv(yi = wqelast, vi,mods = ~vi, random = ~ 1 | cluster/obsid, data=df_wf, test = "t")
res_wf

# adjusting using sample sixe (proxy of SE) as a explanatory variable in meta regression

panels <- list(
  "Waterfront distance-100m" = list(
    "Fixed efffet - Unweighted" = rma(yi = elast, vi=vi,mods = ~ vi, data=df_100, method="EE", weighted=FALSE, level = 95),
    "Fixed effect - Cluster weighted " = rma(yi = elast, vi=vi,mods = ~ vi, data=df_100, method="EE",weighted=T,weights = w_1, level = 95),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= rma(yi = cw_elast, vi, data=df_wf, method="EE",weighted=T, level = 95),
    "Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))"= rma(yi = elast, vi=vi,mods = ~ vi, data=df_100, method="EE",weighted=T,weights =w_2, level = 95),
    #"Random effect (sample size variance)"= rma.mv(yi = cw_elast, vi, random = ~ 1 | cluster_id/obsid, data=df_wf),
    "Random effect (log(sample size variance))"= rma.mv(yi = elast, vi,mods = ~ vi, random = ~ 1 | cluster_id/obsid, data=df_100)
    
  ),
  "Waterfront distance-400m" = list(
    "Fixed efffet - Unweighted" = rma(yi = elast, vi=vi, mods = ~ vi, data=df_400, method="EE", weighted=FALSE, level = 95),
    "Fixed effect - Cluster weighted " = rma(yi = elast, vi=vi, mods = ~ vi,data=df_400, method="EE",weighted=T,weights = w_1, level = 95),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= rma(yi = cw_elast, vi, data=df_nwf, method="EE",weighted=T, level = 95),
    "Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))"= rma(yi = elast, vi=vi,mods = ~ vi, data=df_400, method="EE",weighted=T,weights = w_2, level = 95),
    #"Random effect (sample size variance)"= rma.mv(yi = cw_elast, vi, random = ~ 1 | cluster_id/obsid, data=df_nwf),
    "Random effect (log(sample size variance))"= rma.mv(yi = elast, vi, random = ~ 1 | cluster_id/obsid,mods = ~ vi, data=df_400, control=list(rel.tol=1e-8))
  )
)


panels_robu <- list(
  "Waterfront distance-100m" = list(
    "Fixed efffet - Unweighted" = robust(panels$`Waterfront distance-100m`$`Fixed efffet - Unweighted`, cluster = obsid),
    #"Fixed effect - Cluster weighted " = robust(panels$`Waterfront distance-100m`$`Fixed effect - Cluster weighted `, cluster = cluster_id),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= robust(panels$Waterfront$`Fixed effect - Cluster weighted - Varance Adjusted (sample size)`, cluster = cluster_id),
    #"Fixed effect - Cluster weighted - Varance Adjusted"= robust(panels$`Waterfront distance-100m`$`Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))`, cluster = cluster_id),
    #"Random effect (sample size variance)"= robust(panels$Waterfront$`Random effect (sample size variance)`, cluster = cluster_id),
    "Random effect "= robust(panels$`Waterfront distance-100m`$`Random effect (log(sample size variance))`, cluster = cluster_id)
  ),
    "Waterfront distance-400m" = list(
    "Fixed efffet - Unweighted" =robust(panels$`Waterfront distance-400m`$`Fixed efffet - Unweighted`, cluster = obsid),
    #"Fixed effect - Cluster weighted " = robust(panels$`Waterfront distance-400m`$`Fixed effect - Cluster weighted `, cluster = cluster_id),
    ##"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= robust(panels$`Non-waterfront`$`Fixed effect - Cluster weighted - Varance Adjusted (sample size)`, cluster = cluster_id),
    #"Fixed effect - Cluster weighted - Varance Adjusted"= robust(panels$`Waterfront distance-400m`$`Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))`, cluster = cluster_id),
    ##"Random effect (sample size variance)"= robust(panels$`Non-waterfront`$`Random effect (sample size variance)`, cluster = cluster_id),
    "Random effect"= robust(panels$`Waterfront distance-400m`$`Random effect (log(sample size variance))`, cluster = cluster_id)
    
    )
)

f <- function(x) format(round(x, 3), big.mark=",")


gm <- list(
  list("raw" = "nobs", "clean" = "Num. of Obs.", "fmt" = f)
)

cm <- c("intercept" = "Mean",
        "sqrt(vi)"="SE",
        "vi"="1/Sample Size")

modelsummary(
  panels_robu,
  coef_map = cm)


modelsummary(
  panels_robu,
  shape = "rbind",
  stars = TRUE, gof_map = gm,
  coef_map = cm,
  output = "./results/Tables/pubbias_correct.tex")

##############old#######old#######old#####old

df_100 <- read.csv("./metadata/Meta_data_waterfront_100.csv")%>%
  mutate(obsid = row_number())%>%
  filter(obsid != 168)%>% # outlier - 1:Swedberg el al 2020 - Multi-state model
  filter(obsid != 167)%>% # Swedberg el al 2020 - State level model - Maine
  filter(obsid != 155)%>%# Swedberg el al 2020 - Multi-state model
  filter(elast >=-0.5 & elast <= 1 )

df_400 <- read.csv("./metadata/Meta_data_waterfront_400.csv")%>%
  mutate(obsid = row_number())%>%
  filter(obsid != 168)%>% # outlier - 1:Swedberg el al 2020 - Multi-state model
  filter(obsid != 167)%>% # Swedberg el al 2020 - State level model - Maine
  filter(obsid != 155)%>%# Swedberg el al 2020 - Multi-state model
  filter(elast >=-0.2 & elast <= 0.2 )



res <- rma.mv(yi = elast, vi, random = ~ 1 | cluster_id/obsid, data=df_100)


#main figure
p1 <- funnel.rma(res, main="", pch = 20,back=NA, shade=NA,hlines="lightgray",ylab = "1/sample size", xlab = "Elasticity - Waterfront Bin")


res.nwf <- rma.mv(yi = elast, vi, random = ~ 1 | cluster_id/obsid, data=df_400)

p4 <- funnel.rma(res.nwf, main="", pch = 20,back=NA, shade=NA,hlines="lightgray",ylab = "1/sample size", xlab = "Elasticity - Near-Waterfront Bin")


library(patchwork)
p1+p4+plot_layout(ncol = 1)


#other options

funnel(res, yaxis="vi", main="Sampling Variance")
funnel(res, yaxis="seinv", main="Inverse Standard Error")
funnel(res, yaxis="vinv", main="Inverse Sampling Variance")


#classical Egger test
# waterfront
ranktest(wqelast, vi, data=df_wf, digits=3, exact=FALSE)
regtest(wqelast, vi, data=df_nwf, model="lm")

coef_test(res, vcov = "CR2")