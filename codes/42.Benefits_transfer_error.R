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

# load data - with outliers 
df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")

re_w <- read.csv("./metadata/RE_weight_withoutdiscrim_wf_nwf.csv") # weights in RE model (just to compare weights)



# set up data set with required variables and necessary weighting schemes

# waterfront
df <- df%>%
  select(obsid,studyid,geog,wqelast,sampsize)%>%
  drop_na(sampsize)%>% # drop obs without sample size
  drop_na(wqelast)%>% # drop obs without wqelast
  mutate(vi = log(sampsize))%>%
  mutate(id = row_number())%>% # use to attach RE weights
  group_by(studyid, geog)%>%
  mutate(cluster = cur_group_id())%>%
  mutate(unweight_w = 1)%>% # un-weighted scheme
  group_by(cluster)%>%
  mutate(count = n())%>%
  mutate(cluster_w = 1/count)%>% # cluster adjusted weight
  select(-count)%>%
  mutate(cluster_ss_w = cluster_w*log(sampsize))%>%
  left_join(re_w)


###############################################################################
# Transfer  error Fixed effect unweighted - waterfront
# loop - drop each observation and calculate elasticity for each observation
result <- list()
#loop through obsid and extract betas
for(i in unique(df$id)){
  #construct linear model (metafor package)
  elas <- rma(yi = wqelast, vi, data = subset(df, df$id != i), method="EE", weights=unweight_w, level = 95)
  #create data.frame containing intercept left out and coefficient
  result.dt <- data.frame(beta = coef(elas),
                          cluster = i)
  #bind to list
  result[[i]] <- result.dt
}

#bind to data.frame
result <- do.call(rbind, result)
head(result)
# left_join - to calculate transfer error
transfer_error <- left_join(result,df , by = "cluster")%>%
  select(wqelast,beta,unweight_w)
error_wf_re <- mutate(transfer_error,T_error = abs((beta-wqelast*unweight_w)/beta)*100)

median(error_wf_re$T_error, na.rm = T)

###########################################################################
# Transfer  error Fixed effect fixed effect - cluster adjusted - waterfront
# loop - drop each observation and calculate elasticity for each observation

result <- list()
#loop through obsid and extract betas
for(i in unique(df$id)){
  #construct linear model (metafor package)
  elas <- rma(yi = wqelast, vi, data=subset(df, df$id != i), method="EE",weights=cluster_w,level = 95)
  #create data.frame containing intercept left out and coefficient
  result.dt <- data.frame(beta = coef(elas),
                          cluster = i)
  #bind to list
  result[[i]] <- result.dt
}

#bind to data.frame
result <- do.call(rbind, result)
head(result)
# left_join - to calculate transfer error
transfer_error <- left_join(result,df, by = "cluster")%>%
  select(wqelast,beta,cluster_w)
error_wf_re <- mutate(transfer_error,T_error = abs((beta - (wqelast*cluster_w))/beta)*100)

median(error_wf_re$T_error, na.rm = T)

###############################################################################
# Transfer  error Fixed effect fixed effect - variance cluster adjusted - waterfront
# loop - drop each observation and calculate elasticity for each observation

result <- list()
#loop through obsid and extract betas
for(i in unique(df$id)){
  #construct linear model (metafor package)
  elas <- rma(yi = wqelast, vi, data=subset(df, df$id != i), method="EE",weights=cluster_ss_w,level = 95)
  #create data.frame containing intercept left out and coefficient
  result.dt <- data.frame(beta = coef(elas),
                          cluster = i)
  #bind to list
  result[[i]] <- result.dt
}

#bind to data.frame
result <- do.call(rbind, result)
head(result)
# left_join - to calculate transfer error
transfer_error <- left_join(result,df, by = "cluster")%>%
  select(wqelast,beta,cluster_ss_w)
error_wf_re <- mutate(transfer_error,T_error = abs((beta-wqelast*cluster_ss_w)/beta)*100)

median(error_wf_re$T_error, na.rm = T)
#####################################################################

# Transfer  error Random effect
# loop - drop each observation and calculate elasticity for each observation

result <- list()
#loop through obsid and extract betas
for(i in unique(df$id)){
  #construct linear model (metafor package)
  elas <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=subset(df, df$id != i), level = 95)
  #create data.frame containing intercept left out and coefficient
  result.dt <- data.frame(beta = coef(elas),
                          cluster = i)
  #bind to list
  result[[i]] <- result.dt
}

#bind to data.frame
result <- do.call(rbind, result)
head(result)
# left_join - to calculate transfer error
transfer_error <- left_join(result,df , by = "cluster")%>%
  select(wqelast,beta,re_w)
error_wf_re <- mutate(transfer_error,T_error = abs((beta - wqelast*re_w)/beta )*100)

median(error_wf_re$T_error, na.rm = T)


##############################################################################

# run the 20. meta regression forst

# error model 1
t <- model.matrix(Model1)%>%
  as.data.frame()%>%
  select(-intrcpt)

pred <- predict(Model1,newdata = t)%>%
  as.data.frame()

pred <- rownames_to_column(pred, var="obsid")
pred$obsid <- as.numeric(pred$obsid)
pred <- mutate(pred,e_pred1 = pred) # take the exp as it predicted log of elasticcity
pred <- select(pred,obsid,e_pred1)

df.re <- df.re%>%
  left_join(pred)



df.re <- df.re%>%
  mutate(error1 = abs((e_pred1 - elast_sim)/elast_sim)*100 )

median(df.re$error1, na.rm = T)



# error model 2
t <- model.matrix(Model2)%>%
  as.data.frame()%>%
  select(-intrcpt)

pred <- predict(Model2,newdata = t)%>%
  as.data.frame()

pred <- rownames_to_column(pred, var="obsid")
pred$obsid <- as.numeric(pred$obsid)
pred <- mutate(pred,e_pred2 = pred) # take the exp as it predicted log of elasticcity
pred <- select(pred,obsid,e_pred2)

df.re <- df.re%>%
  left_join(pred)

df.re <- df.re%>%
  mutate(error2 = abs((e_pred2 - elast_sim)/elast_sim)*100 )

median(df.re$error2, na.rm = T)


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

