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

df <- df%>%
  select(obsid,studyid,geog,wqelast,sampsize)%>%
  drop_na(sampsize)%>% # drop obs without sample size
  drop_na(wqelast)%>% # drop obs without wqelast
  mutate(vi = log(sampsize))%>%
  group_by(studyid, geog)%>%
  mutate(cluster = cur_group_id())%>%
  mutate(unweight_w = 1)%>% # un-weighted scheme
  group_by(cluster)%>%
  mutate(count = n())%>%
  rename("k" = "count")%>%
  mutate(cluster_w = 1/k)%>% # cluster adjusted weight
  mutate(cluster_ss_w = cluster_w*log(sampsize))


###############################################################################
# Unweighted fixed effect
result_1 <- list()
result_2 <- list()

#loop through obsid and extract betas
for(i in unique(df$cluster)){
  #construct linear model (metafor package)
  elas_1 <- rma(yi = wqelast, vi, data = subset(df, df$cluster == i), method="EE", weights=unweight_w, level = 95)
  elas_2 <- rma(yi = wqelast, vi, data = subset(df, df$cluster != i), method="EE", weights=unweight_w, level = 95)
  
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

final <- result_1%>%
  left_join(result_2)%>%
  mutate(error = abs((beta_2-beta_1)/beta_1)*100)


fixed_efect <- median(final$error, na.rm = T)

##############################################################################
# cluster adjusted - fixed effect
result_1 <- list()
result_2 <- list()

#loop through obsid and extract betas
for(i in unique(df$cluster)){
  #construct linear model (metafor package)
  elas_1 <- rma(yi = wqelast, vi, data=subset(df, df$cluster == i), method="EE",weights=cluster_w,level = 95)
  elas_2 <- rma(yi = wqelast, vi, data=subset(df, df$cluster != i), method="EE",weights=cluster_w,level = 95)
  
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

final <- result_1%>%
  left_join(result_2)%>%
  mutate(error = abs((beta_2-beta_1)/beta_1)*100)


cluster_adjusted <- median(final$error, na.rm = T)

##############################################################################
# cluster sample size adjusted - fixed effect

result_1 <- list()
result_2 <- list()

#loop through obsid and extract betas
for(i in unique(df$cluster)){
  #construct linear model (metafor package)
  elas_1 <- rma(yi = wqelast, vi, data=subset(df, df$cluster == i), method="EE",weights=cluster_ss_w,level = 95)
  elas_2 <- rma(yi = wqelast, vi, data=subset(df, df$cluster != i), method="EE",weights=cluster_ss_w,level = 95)
  
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

final <- result_1%>%
  left_join(result_2)%>%
  mutate(error = abs((beta_2-beta_1)/beta_1)*100)


cluster_sample_size_adjusted <- median(final$error, na.rm = T)
##############################################################################

# Random Effect model

df_1 <- df%>%
  subset(k==1)

df_2 <- df%>%
  subset(k != 1)


# Transfer  error Random effect
# loop - drop each observation and calculate elasticity for each observation

result <- list()
#loop through obsid and extract betas
for(i in unique(df_1$cluster)){
  #construct linear model (metafor package)
  elas <- rma(yi = wqelast, vi, data=subset(df_1, df_1$cluster == i), level = 95)
  #create data.frame containing intercept left out and coefficient
  result.dt <- data.frame(beta = coef(elas),
                          cluster = i)
  #bind to list
  result[[i]] <- result.dt
}

#bind to data.frame
result_1 <- do.call(rbind, result)
head(result_1)

result <- list()
#loop through obsid and extract betas
for(i in unique(df_2$cluster)){
  #construct linear model (metafor package)
  elas <- rma.mv(yi = wqelast, vi, random = ~ 1 |cluster/ obsid, data=subset(df_2, df_2$cluster == i), level = 95)
  #create data.frame containing intercept left out and coefficient
  result.dt <- data.frame(beta = coef(elas),
                          cluster = i)
  #bind to list
  result[[i]] <- result.dt
}

#bind to data.frame
result_2 <- do.call(rbind, result)
head(result_2)

df_12 <- rbind(result_1,result_2)%>%
  rename(beta_1 = beta)

# Transfer  error Random effect
# loop - drop each observation and calculate elasticity for each observation

result <- list()
#loop through obsid and extract betas
for(i in unique(df$cluster)){
  #construct linear model (metafor package)
  elas <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=subset(df, df$cluster != i), level = 95)
  #create data.frame containing intercept left out and coefficient
  result.dt <- data.frame(beta = coef(elas),
                          cluster = i)
  #bind to list
  result[[i]] <- result.dt
}


#bind to data.frame
result <- do.call(rbind, result)
head(result)


final <-result%>%
  left_join(df_12)%>%
  mutate(error = abs((beta-beta_1)/beta_1)*100)


Random_effect<- median(final$error, na.rm = T)


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

