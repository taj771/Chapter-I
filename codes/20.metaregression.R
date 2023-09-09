rm(list = ls())
library(dplyr)
library(meta)
library(metafor)
library(tidyverse)
library(robumeta)
library(clubSandwich)

# Reference : https://cran.r-project.org/web/packages/clubSandwich/vignettes/meta-analysis-with-CRVE.html
#           : https://www.jepusto.com/robust-meta-analysis-1/
#           : https://cran.r-project.org/web/packages/robumeta/vignettes/robumetaVignette.pdf
##############################################################################

# laod data - with outliers 
df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")

# load data - without outliers 
#wf <- read.csv("./metadata/wo_outliers_wf.csv")
#nwf <- read.csv("./metadata/wo_outliers_nwf.csv")

#df <- rbind(wf,nwf)


# set parameters
 base_year <- min(df$pubyear)

## Subset: Main models - Random Effect
# waterfront
df.re<- df%>%
  mutate(vi = 1/sampsize)%>%
  mutate(invvariance = 1/vi)%>%
  filter(vi > 0)%>%
  group_by(studyid,geog)%>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()%>%
  mutate(wf = case_when(distbuf=="1"~1,
                        distbuf=="2"~0))%>%
  mutate(time = pubyear-base_year)
  #filter(elast_sim < 3 )



#df.re[c('invvariance')][sapply(df.re[c('invvariance')], is.infinite)] <- NA

reg1 <- rma.mv(yi=wqelast, vi, mods = ~ wf, random = ~1| cluster/obsid, struct="DIAG",verbose=TRUE, data=df.re, digits=3)
reg1

reg2 <- rma.mv(yi=wqelast, vi, mods = ~ wf+funcform, random = ~1| cluster/obsid, struct="DIAG",verbose=TRUE, data=df.re, digits=3)
reg2

reg3 <- rma.mv(yi=wqelast, vi, mods = ~ wf+funcform+time, random = ~1| cluster/obsid, struct="DIAG",verbose=TRUE, data=df.re, digits=3)
reg3

reg4 <- rma.mv(yi=wqelast, vi, mods = ~ wf+funcform+time+canada+west+midwest+northeast+multireg, random = ~ 1|cluster/obsid, struct="DIAG",verbose=TRUE, data=df.re, digits=3)
reg4




models <- list(
  "Model 1" =robust(reg1, cluster=cluster),
  "Model 2" = robust(reg2, cluster=cluster),
  "Model 3" = robust(reg3, cluster=cluster),
  "Model 4" =robust(reg4, cluster=cluster)
)


# Model summary - table 3


cm <- c('wf'='Waterfront',
        'funcformlin-log' = 'Finctional form: lin-log',
        'funcformlinear' ='Functional form: linear',
        'funcformlog-lin' = 'Functional form: log-lin',
        'west'='West',
        'midwest'='Midwest',
        'south'='South',
        'multireg'='Multi-regional',
        'canada'='Canada',
        'northeast' = 'North-East',
        'avgwqvar'='Avg.Secchi Depth',
        'time'='Time trend',
        'vi'='Variance',
        'intercept'= 'Constant')

modelsummary(
  models,
  stars = TRUE,
  coef_map = cm, output = "./results/table3.tex" )

modelsummary(
  models,
  stars = TRUE,
  coef_map = cm)



df.re <- df.re%>%
mutate(linlog = case_when(funcform=="lin-log"~1,
                      funcform=="linear"~ 0,
                      funcform=="log-lin"~ 0,
                      funcform=="double-log"~0))%>%
mutate(linear = case_when(funcform=="lin-log"~0,
                      funcform=="linear"~ 1,
                      funcform=="log-lin"~ 0,
                      funcform=="double-log"~0))%>%
mutate(loglin = case_when(funcform=="lin-log"~0,
                          funcform=="linear"~ 0,
                          funcform=="log-lin"~ 1,
                          funcform=="double-log"~0))


# predict elasticity for each observation based on estimated model

Model1 =robust(reg1, cluster=cluster)
Model2 = robust(reg2, cluster=cluster)
Model3 = robust(reg3, cluster=cluster)
Model4 =robust(reg4, cluster=cluster)


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

##############################################################################

