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

# laod data - with outliers 
df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")


# set parameters
base_year <- min(df$pubyear)

## Subset: Main models - Random Effect
# waterfront
df.re<- df%>%
  drop_na(sampsize)%>% # drop obs without sample size
  drop_na(wqelast)%>% # drop obs without wqelast
  mutate(vi = log(sampsize))%>%
  group_by(studyid, geog)%>%
  mutate(cluster = cur_group_id())%>%
  mutate(wf = case_when(distbuf=="1"~1,
                        distbuf=="2"~0))%>%
  mutate(time = pubyear-base_year)%>%
  mutate(can = case_when(northeast=="1"~0,
                        midwest=="1"~0,
                        south=="1"~0,
                        west=="1"~0,
                        multireg=="1"~0,
                        canada=="1"~1))




#df.re[c('invvariance')][sapply(df.re[c('invvariance')], is.infinite)] <- NA

reg1 <- rma.mv(yi=log(wqelast), vi, mods = ~ wf+avgwqvar, random = ~1| obsid/cluster,verbose=TRUE, data=df.re,control=list(rel.tol=1e-8), digits=3)

reg2 <- rma.mv(yi=log(wqelast), vi, mods = ~ wf+avgwqvar+funcform, random = ~1| cluster/obsid,verbose=TRUE, data=df.re,control=list(rel.tol=1e-8), digits=3)

reg3 <- rma.mv(yi=log(wqelast), vi, mods = ~ wf+avgwqvar+funcform+canada+west+midwest+northeast+multireg, 
               random = ~ 1|cluster/obsid,verbose=TRUE, data=df.re,control=list(rel.tol=1e-8),digits=3)

reg4 <- rma.mv(yi=log(wqelast), vi, mods = ~ wf+avgwqvar+funcform+can, 
               random = ~ 1|cluster/obsid,verbose=TRUE,data=df.re,control=list(rel.tol=1e-8) ,digits=3)




models <- list(
  "Model 1" =robust(reg1, cluster=studyid),
  "Model 2" = robust(reg2, cluster=obsid),
  "Model 3" = robust(reg3, cluster=obsid),
  "Model 4" =robust(reg4, cluster=obsid)
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
        'intercept'= 'Constant',
        "can" = "Canada")






modelsummary(
  models,
  stars = TRUE,
  coef_map = cm, output = "./results/Tables/table3.tex" )

modelsummary(
  models,
  stars = TRUE,
  coef_map = cm)




# predict elasticity for each observation based on estimated model

Model1 =robust(reg1, cluster=cluster)
Model2 = robust(reg2, cluster=cluster)
Model3 = robust(reg3, cluster=cluster)
Model4 =robust(reg4, cluster=cluster)



