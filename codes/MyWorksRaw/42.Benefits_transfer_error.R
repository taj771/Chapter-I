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
  mutate(w_3 = log_ave_ss_wt_clus/sum(log_ave_ss_wt_clus))


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
  mutate(w_3 = log_ave_ss_wt_clus/sum(log_ave_ss_wt_clus))


###############################################################################
# Cluster adjusted
##############################################################################
# cluster adjusted - fixed effect - 100m
result_1 <- list()
result_2 <- list()
#loop through obsid and extract betas
for(i in unique(df_100$obsid)){
  #construct linear model (metafor package)
  #elas_1 <- rma(yi = elast, vi, data=subset(df_100, df_100$obsid == i), method="EE",weighted = T,weights = w_1,level = 95)
  elas_2 <- rma(yi = elast, vi, data=subset(df_100, df_100$obsid != i), method="EE",weighted = T,weights = w_1,level = 95)
  
  #create data.frame containing intercept left out and coefficient
  #result.dt_1 <- data.frame(beta_1 = coef(elas_1),
                            #cluster = i)
  result.dt_2 <- data.frame(beta_2 = coef(elas_2),
                            cluster_id = i)
  #bind to list
 # result_1[[i]] <- result.dt_1
  result_2[[i]] <- result.dt_2
}

#bind to data.frame
#result_1 <- do.call(rbind, result_1)
result_2 <- do.call(rbind, result_2)
#head(result_1)
head(result_2)

final_100 <- df_100%>%
  left_join(result_2)%>%
  mutate(elast_1 = elast*w_1)%>%
  mutate(error = beta_2 - elast_1)%>%
  mutate(error = abs(error))%>%
  mutate(error = error/beta_2*100)

  
median(final_100$error)



# cluster adjusted - fixed effect - 400m
#result_3 <- list()
result_3 <- list()
#loop through obsid and extract betas
for(i in unique(df_400$obsid)){
  #construct linear model (metafor package)
  #elas_3 <- rma(yi = elast, vi, data=subset(df_400, df_400$obsid == i), method="EE",weighted = T,weights = w_1,level = 95)
  elas_4 <- rma(yi = elast, vi, data=subset(df_400, df_400$obsid != i), method="EE",weighted = T,weights = w_1,level = 95)
  #create data.frame containing intercept left out and coefficient
  #result.dt_3 <- data.frame(beta_1 = coef(elas_3),
                            #cluster = i)
  result.dt_3 <- data.frame(beta_2 = coef(elas_4),
                            cluster_id = i)
  #bind to list
  #result_3[[i]] <- result.dt_3
  result_3[[i]] <- result.dt_3
}

#bind to data.frame
#result_3 <- do.call(rbind, result_3)
result_3 <- do.call(rbind, result_3)
head(result_3)



final_400 <- df_400%>%
  left_join(result_3)%>%
  mutate(elast_1 = elast*w_1)%>%
  mutate(error = beta_2 - elast_1)%>%
  mutate(error = abs(error))%>%
  mutate(error = error/beta_2*100)


median(final_400$error)



cluster_adjuste_100 <- median(final_100$error)
cluster_adjuste_400 <- median(final_400$error)


###############################################################################
# cluster sample size adjust
##############################################################################
# cluster sample size adjusted - fixed effect - 100
#result_1 <- list()
result_2 <- list()
#loop through obsid and extract betas
for(i in unique(df_100$obsid)){
  #construct linear model (metafor package)
  #elas_1 <- rma(yi = elast, vi, data=subset(df_100, df_100$obsid == i), method="EE",weighted=T,weights =w_2,level = 95)
  elas_2 <- rma(yi = elast, vi, data=subset(df_100, df_100$obsid != i), method="EE",weighted=T,weights =w_2,level = 95)
  
  #create data.frame containing intercept left out and coefficient
  #result.dt_1 <- data.frame(beta_1 = coef(elas_1),
                            #cluster = i)
  result.dt_2 <- data.frame(beta_2 = coef(elas_2),
                            cluster_id = i)
  #bind to list
  #result_1[[i]] <- result.dt_1
  result_2[[i]] <- result.dt_2
}

#bind to data.frame
#result_1 <- do.call(rbind, result_1)
result_2 <- do.call(rbind, result_2)


final_100 <- df_100%>%
  left_join(result_2)%>%
  mutate(elast_1 = elast*w_2)%>%
  mutate(error = beta_2 - elast_1)%>%
  mutate(error = abs(error))%>%
  mutate(error = error/beta_2*100)

median(final_100$error)


#######################################################
# cluster sample size adjusted - fixed effect - 400
result_3 <- list()
#result_4 <- list()
#loop through obsid and extract betas
for(i in unique(df_400$obsid)){
  #construct linear model (metafor package)
  #elas_3 <- rma(yi = elast, vi, data=subset(df_400, df_400$obsid == i), method="EE",weighted=T,weights =w_2,level = 95)
  elas_4 <- rma(yi = elast, vi, data=subset(df_400, df_400$obsid != i), method="EE",weighted=T,weights =w_2,level = 95)
  #create data.frame containing intercept left out and coefficient
  #result.dt_3 <- data.frame(beta_1 = coef(elas_3),
                            #cluster = i)
  result.dt_3 <- data.frame(beta_2 = coef(elas_4),
                            cluster_id = i)
  #bind to list
  #result_3[[i]] <- result.dt_3
  result_3[[i]] <- result.dt_3
}

#bind to data.frame
#result_3 <- do.call(rbind, result_3)
result_3 <- do.call(rbind, result_3)
head(result_3)
#head(result_4)

final_400 <- df_400%>%
  left_join(result_3)%>%
  mutate(elast_1 = elast*w_2)%>%
  mutate(error = beta_2 - elast_1)%>%
  mutate(error = abs(error))%>%
  mutate(error = error/beta_2*100)


median(final_400$error)

cluster_ss_adjuste_100 <- median(final_100$error, na.rm = T)
cluster_ss_adjuste_400 <- median(final_400$error, na.rm = T)



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


#####################################################################

Ave.clus_adj = cluster_adjuste_100+cluster_adjuste_400/2
Ave.clus_ss_adj = cluster_ss_adjuste_100+cluster_ss_adjuste_400/2
Ave.RE = RE_100+RE_400/2



df <- data.frame(
weighting_Scheme = c("FE:Cluster weight", "FE:Cluster-Sample size weight", "Random Effect" ),
Distance_100m = c(cluster_adjuste_100, cluster_ss_adjuste_100, RE_100),
Distance_400m = c(cluster_adjuste_400, cluster_ss_adjuste_400, RE_400),
Average = c(Ave.clus_adj,Ave.clus_ss_adj,Ave.RE)
)

xtable(df)

write.table(df,"./results/Tables/benefit_trans_error.tex")




##############################################################################
#. meta regression
# 100m - Model 1

df <- df_100%>%
  group_by(cluster_id)%>%
  mutate(k = n())
df1 <- df%>%
  filter(k > 1)

# cluster adjusted - fixed effect - 100m
result_1 <- list()
result_2 <- list()

#loop through obsid and extract betas
for(i in unique(df1$cluster_id)){
  #construct linear model (metafor package)
  elas_1 <- rma.mv(yi = elast, vi_log,mods = ~ wf_100+wf_200+wf_300+wf_400, random = ~ 1 | cluster_id/obsid, verbose=TRUE ,data=subset(df1, df1$cluster_id == i))
  elas_2 <- rma.mv(yi = elast, vi_log,mods = ~ wf_100+wf_200+wf_300+wf_400, random = ~ 1 | cluster_id/obsid, verbose=TRUE,data=subset(df1, df1$cluster_id != i),level = 95)
  
  #create data.frame containing intercept left out and coefficient
  result.dt_1 <- data.frame(beta_1 = coef(elas_1),
                            cluster = i)
  result.dt_2 <- data.frame(beta_2 = coef(elas_2),
                            cluster = i)
  #bind to list
  result_1[[i]] <- result.dt_1
  result_2[[i]] <- result.dt_2
}

#bind to data.frame
result_1 <- do.call(rbind, result_1)
result_2 <- do.call(rbind, result_2)
head(result_1)
head(result_2)
final_100_model1 <- result_1%>%
  left_join(result_2)%>%
  mutate(error = abs((beta_2-beta_1)/beta_1)*100)

median(final_100_model1$error)


#############################################################################

# 100m - Model 2

df <- df_100%>%
  group_by(cluster_id)%>%
  mutate(k = n())
df1 <- df%>%
  filter(k > 1)

df1 <- df1%>%
  mutate(fun_dlog = ifelse(funcform == "double-log", 1,0))%>%
  mutate(fun_linlog = ifelse(funcform == "lin-log", 1,0))%>%
  mutate(fun_linear = ifelse(funcform == "linear", 1,0))%>%
  mutate(fun_loglin = ifelse(funcform == "log-lin", 1,0))
  
# cluster adjusted - fixed effect - 100m
result_1 <- list()
result_2 <- list()

#loop through obsid and extract betas
for(i in unique(df1$cluster_id)){
  #construct linear model (metafor package)
  elas_1 <- rma.mv(yi = elast, vi_log,mods = ~ wf_100+wf_200+wf_300+wf_400+fun_linlog+fun_linear+fun_loglin, random = ~ 1 | cluster_id/obsid, verbose=TRUE ,data=subset(df1, df1$cluster_id == i))
  elas_2 <- rma.mv(yi = elast, vi_log,mods = ~ wf_100+wf_200+wf_300+wf_400+fun_linlog+fun_linear+fun_loglin, random = ~ 1 | cluster_id/obsid, verbose=TRUE,data=subset(df1, df1$cluster_id != i))
  
  #create data.frame containing intercept left out and coefficient
  result.dt_1 <- data.frame(beta_1 = coef(elas_1),
                            cluster = i)
  result.dt_2 <- data.frame(beta_2 = coef(elas_2),
                            cluster = i)
  #bind to list
  result_1[[i]] <- result.dt_1
  result_2[[i]] <- result.dt_2
}

#bind to data.frame
result_1 <- do.call(rbind, result_1)
result_2 <- do.call(rbind, result_2)
head(result_1)
head(result_2)
final_100_model2 <- result_1%>%
  left_join(result_2)%>%
  mutate(error = abs((beta_2-beta_1)/beta_1)*100)

median(final_100_model2$error)
#############################################################################
# 100m - Model 3

df <- df_100%>%
  group_by(cluster_id)%>%
  mutate(k = n())
df1 <- df%>%
  filter(k > 1)

df1 <- df1%>%
  mutate(fun_dlog = ifelse(funcform == "double-log", 1,0))%>%
  mutate(fun_linlog = ifelse(funcform == "lin-log", 1,0))%>%
  mutate(fun_linear = ifelse(funcform == "linear", 1,0))%>%
  mutate(fun_loglin = ifelse(funcform == "log-lin", 1,0))

# cluster adjusted - fixed effect - 100m
result_1 <- list()
result_2 <- list()

#loop through obsid and extract betas
for(i in unique(df1$cluster_id)){
  #construct linear model (metafor package)
  elas_1 <- rma.mv(yi = elast, vi_log,mods = ~ wf_100+wf_200+wf_300+wf_400+fun_linlog+fun_linear+fun_loglin+canada+west+midwest+northeast+south, random = ~ 1 | cluster_id/obsid, verbose=TRUE ,data=subset(df1, df1$cluster_id == i))
  elas_2 <- rma.mv(yi = elast, vi_log,mods = ~ wf_100+wf_200+wf_300+wf_400+fun_linlog+fun_linear+fun_loglin+canada+west+midwest+northeast+south, random = ~ 1 | cluster_id/obsid, verbose=TRUE,data=subset(df1, df1$cluster_id != i),level = 95)
  
  #create data.frame containing intercept left out and coefficient
  result.dt_1 <- data.frame(beta_1 = coef(elas_1),
                            cluster = i)
  result.dt_2 <- data.frame(beta_2 = coef(elas_2),
                            cluster = i)
  #bind to list
  result_1[[i]] <- result.dt_1
  result_2[[i]] <- result.dt_2
}

#bind to data.frame
result_1 <- do.call(rbind, result_1)
result_2 <- do.call(rbind, result_2)
head(result_1)
head(result_2)
final_100_model3 <- result_1%>%
  left_join(result_2)%>%
  mutate(error = abs((beta_2-beta_1)/beta_1)*100)

median(final_100_model3$error)
#############################################################################
# 100m - Model 4

df <- df_100%>%
  group_by(cluster_id)%>%
  mutate(k = n())
df1 <- df%>%
  filter(k > 1)

df1 <- df1%>%
  mutate(fun_dlog = ifelse(funcform == "double-log", 1,0))%>%
  mutate(fun_linlog = ifelse(funcform == "lin-log", 1,0))%>%
  mutate(fun_linear = ifelse(funcform == "linear", 1,0))%>%
  mutate(fun_loglin = ifelse(funcform == "log-lin", 1,0))%>%
  mutate(can = case_when(northeast=="1"~0,
                         midwest=="1"~0,
                         south=="1"~0,
                         west=="1"~0,
                         multireg=="1"~0,
                         canada=="1"~1))

# cluster adjusted - fixed effect - 100m
result_1 <- list()
result_2 <- list()

#loop through obsid and extract betas
for(i in unique(df1$cluster_id)){
  #construct linear model (metafor package)
  elas_1 <- rma.mv(yi = elast, vi_log,mods = ~ wf_100+wf_200+wf_300+wf_400+fun_linlog+fun_linear+fun_loglin+can+avgwqvar, random = ~ 1 | cluster_id/obsid, verbose=TRUE ,data=subset(df1, df1$cluster_id == i))
  elas_2 <- rma.mv(yi = elast, vi_log,mods = ~ wf_100+wf_200+wf_300+wf_400+fun_linlog+fun_linear+fun_loglin+can+avgwqvar, random = ~ 1 | cluster_id/obsid, verbose=TRUE,data=subset(df1, df1$cluster_id != i),level = 95)
  
  #create data.frame containing intercept left out and coefficient
  result.dt_1 <- data.frame(beta_1 = coef(elas_1),
                            cluster = i)
  result.dt_2 <- data.frame(beta_2 = coef(elas_2),
                            cluster = i)
  #bind to list
  result_1[[i]] <- result.dt_1
  result_2[[i]] <- result.dt_2
}

#bind to data.frame
result_1 <- do.call(rbind, result_1)
result_2 <- do.call(rbind, result_2)
head(result_1)
head(result_2)
final_100_model4 <- result_1%>%
  left_join(result_2)%>%
  mutate(error = abs((beta_2-beta_1)/beta_1)*100)

median(final_100_model4$error)



##############################################################################
##############################################################################
# error model 1
Model_1 =robust(reg1, cluster=cluster_id)

t <- model.matrix(Model_1)%>%
  as.data.frame()%>%
  select(-intrcpt)


pred <- predict(Model_1, newmods=cbind(c(t$wf_100),c(t$wf_200), c(t$wf_300), c(t$wf_400)), addx=TRUE)%>%
  as.data.frame()


pred <- rownames_to_column(pred, var="obsid")
pred$obsid <- as.numeric(pred$obsid)
pred <- mutate(pred,e_pred1 = pred) # take the exp as it predicted log of elasticcity
pred <- select(pred,obsid,e_pred1)

df <- df%>%
  left_join(pred)



df.re <- df_100%>%
  mutate(error1 = abs((e_pred1 - elast)/elast)*100 )

median(df.re$error1, na.rm = T)



# error model 2

Model_2 = robust(reg2, cluster=cluster_id)

t <- model.matrix(Model_2)%>%
  as.data.frame()%>%
  select(-intrcpt)

pred <- predict(Model2,newdata = t)%>%
  as.data.frame()

pred <- rownames_to_column(pred, var="obsid")
pred$obsid <- as.numeric(pred$obsid)
pred <- mutate(pred,e_pred2 = pred) # take the exp as it predicted log of elasticcity
pred <- select(pred,obsid,e_pred2)

pred <- predict(Model_2, newmods=cbind(c(t$wf_100),c(t$wf_200), c(t$wf_300), c(t$wf_400),
                                       c(t$`funcformlin-log`), c(t$funcformlinear), c(t$`funcformlog-lin`)), addx=TRUE)%>%
  as.data.frame()

pred <- rownames_to_column(pred, var="obsid")
pred$obsid <- as.numeric(pred$obsid)
pred <- mutate(pred,e_pred2 = pred) # take the exp as it predicted log of elasticcity
pred <- select(pred,obsid,e_pred2)

df <- df%>%
  left_join(pred)

df <- df%>%
  mutate(error2 = abs((e_pred2 - elast)/elast)*100 )

median(df$error2, na.rm = T)


# error model 3
t <- model.matrix(Model3)%>%
  as.data.frame()%>%
  select(-intrcpt)

pred <- predict(Model3,newdata = t)%>%
  as.data.frame()

pred <- rownames_to_column(pred, var="obsid")
pred$obsid <- as.numeric(pred$obsid)
pred <- mutate(pred,e_pred3 = pred) # take the exp as it predicted log of elasticcity
pred <- select(pred,obsid,e_pred3)

df.re <- df.re%>%
  left_join(pred)

df.re <- df.re%>%
  mutate(error3 = abs((e_pred3 - elast_sim)/elast_sim)*100 )

median(df.re$error3, na.rm = T)



# error model 4
t <- model.matrix(Model4)%>%
  as.data.frame()%>%
  select(-intrcpt)

pred <- predict(Model4,newdata = t)%>%
  as.data.frame()

pred <- rownames_to_column(pred, var="obsid")
pred$obsid <- as.numeric(pred$obsid)
pred <- mutate(pred,e_pred4 = pred) # take the exp as it predicted log of elasticcity
pred <- select(pred,obsid,e_pred4)

df.re <- df.re%>%
  left_join(pred)

df.re <- df.re%>%
  mutate(error4 = abs((e_pred4 - elast_sim)/elast_sim)*100 )

median(df.re$error4, na.rm = T)

# error model 5
t <- model.matrix(Model5)%>%
  as.data.frame()%>%
  select(-intrcpt)

pred <- predict(Model5,newdata = t)%>%
  as.data.frame()

pred <- rownames_to_column(pred, var="obsid")
pred$obsid <- as.numeric(pred$obsid)
pred <- mutate(pred,e_pred5 = pred) # take the exp as it predicted log of elasticcity
pred <- select(pred,obsid,e_pred5)

df.re <- df.re%>%
  left_join(pred)

df.re <- df.re%>%
  mutate(error5 = abs((e_pred5 - elast_sim)/elast_sim)*100 )

median(df.re$error5, na.rm = T)


##############################################################################

