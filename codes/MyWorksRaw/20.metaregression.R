rm(list = ls())
library(dplyr)
library(meta)
library(metafor)
library(tidyverse)
library(robumeta)
library(clubSandwich)
library(modelsummary)

# Reference : https://cran.r-project.org/web/packages/clubSandwich/vignettes/meta-analysis-with-CRVE.html
#           : https://www.jepusto.com/robust-meta-analysis-1/
#           : https://cran.r-project.org/web/packages/robumeta/vignettes/robumetaVignette.pdf
##############################################################################

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
  kableExtra,
  fixest,
  viridis,
  ipsum
)

# load data - with outliers 
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
  mutate(waterfront =1)%>%
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
  mutate(waterfront =0)%>%
  rename("elast"="y_value")


df <- rbind(df_250, df_750)%>%
  group_by(study_name, geog)%>%
  mutate(cluster_id = cur_group_id())%>%
  mutate(can = case_when(northeast=="1"~0,
                         midwest=="1"~0,
                         south=="1"~0,
                         west=="1"~0,
                         multireg=="1"~0,
                         canada=="1"~1),
         fun_loglin = ifelse(funcform =="log-lin",1,0),
         fun_linlog = ifelse(funcform == "lin-log",1,0),
         fun_doblog = ifelse(funcform == "double-log",1,0),
         fun_linear = ifelse(funcform == "linear", 1,0)
         )

reg1 <- rma.mv(yi=elast, vi_log, mods = ~ waterfront+avgwqvar, random = ~1| cluster_id/obsid,verbose=TRUE, data=df,control=list(rel.tol=1e-8), digits=3)


reg2 <- rma.mv(yi=elast, vi_log, mods = ~ waterfront+avgwqvar+fun_loglin+fun_linlog+fun_doblog, random = ~1| cluster_id/obsid,verbose=TRUE, data=df,control=list(rel.tol=1e-8), digits=3)

reg3 <- rma.mv(yi=elast, vi_log, mods = ~ waterfront+can+fun_loglin+fun_linlog+fun_doblog+avgwqvar, random = ~ 1 | cluster_id/obsid, verbose=TRUE,  data=df, digits=3)

reg4 <- rma.mv(yi=elast, vi_log, mods = ~ waterfront+northeast+midwest+south+west+multireg+fun_loglin+fun_linlog+fun_doblog+avgwqvar, 
               random = ~ 1|cluster_id/obsid,verbose=TRUE,data=df,control=list(rel.tol=1e-8) ,digits=3)






models <- list(
  "Model 1" =robust(reg1, cluster=cluster_id),
  "Model 2" = robust(reg2, cluster=cluster_id),
  "Model 3" = robust(reg3, cluster=cluster_id),
  "Model 4" =robust(reg4, cluster=cluster_id)
  
)


# Model summary - table 3


cm <- c('waterfront'='Waterfront',
        'avgwqvar'='Avg.Secchi Depth',
        'fun_loglin'='Functional form: log-lin',
        'fun_linlog' = 'Finctional form: lin-log',
        'fun_doblog' = "Functional form: log-log",
        'fun_linear' = 'Functional form: linear',
        'funcformlin-log' = 'Finctional form: lin-log',
        'funcformlinear' ='Functional form: linear',
        'funcformlog-lin' = 'Functional form: log-lin',
        "can" = "Canada",
        'west'='West',
        'midwest'='Midwest',
        'northeast' = 'North-East',
        'south'='South',
        'multireg'='Multi-regional',
        'canada'='Canada',
        'time'='Time trend',
        'vi'='Variance',
        'intercept'= 'Constant'
        )

modelsummary(
  models,
  stars = TRUE,
  coef_map = cm)


modelsummary(
  models,
  stars = TRUE,
  coef_map = cm, output = "./results/Tables/table3.tex" )




# predict elasticity for each observation based on estimated model

Model1 =robust(reg1, cluster=cluster_id)
Model2 = robust(reg2, cluster=cluster)
Model3 = robust(reg3, cluster=cluster)
Model4 =robust(reg4, cluster=cluster)



