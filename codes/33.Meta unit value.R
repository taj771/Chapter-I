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
  fixest
)

# load data - with outliers 
df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")

re_wf_w <- read.csv("./metadata/RE_weight.csv") # weights in RE model (just to compare weights)

re_nwf_w <- read.csv("./metadata/RE_weight_nwf.csv") # weights in RE model (just to compare weights)


# set up data set with required variables and necessary weighting schemes

# waterfront
df_wf <- df%>%
  subset(distbuf ==1)%>%
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
  left_join(re_wf_w)

# Nonwaterfront

df_nwf <- df%>%
  subset(distbuf ==2)%>%
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
  left_join(re_nwf_w)


panels <- list(
  "Waterfront" = list(
    "Fixed efffet - Unweighted" = rma(yi = wqelast, vi, data=df_wf, method="EE", weighted=FALSE, level = 95),
    "Fixed effect - Cluster weighted " = rma(yi = wqelast, vi, data=df_wf, method="EE",weights=cluster_w,level = 95),
    "Fixed effect - VAC"= rma(yi = wqelast, vi, data=df_wf, method="EE",weights=cluster_ss_w,level = 95),
    "Random effect"= rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df_wf)
  ),
  "Non-waterfront" = list(
    "Fixed efffet - Unweighted" = rma(yi = wqelast, vi, data=df_nwf, method="EE", weighted=FALSE,level = 95),
    "Fixed effect - Cluster weighted"= rma(yi = wqelast, vi, data=df_nwf, method="EE",weights=cluster_w,level = 95),
    "Fixed effect - VAC"= rma(yi = wqelast, vi, data=df_nwf, method="EE",weights=cluster_ss_w,level = 95),
    "Random effect"= rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df_nwf)
  )
)



panels_robu <- list(
  "Waterfront" = list(
    "Fixed efffet - Unweighted" = robust(panels$Waterfront$`Fixed efffet - Unweighted`, cluster = id),
    "Fixed effect - Cluster weighted " = robust(panels$Waterfront$`Fixed effect - Cluster weighted `, cluster = cluster),
    "Fixed effect - VAC"= robust(panels$Waterfront$`Fixed effect - VAC`, cluster = cluster),
    "Random effect"= robust(panels$Waterfront$`Random effect`, cluster = cluster)
  ),
  "Non-waterfront" = list(
    "Fixed efffet - Unweighted" =robust(panels$`Non-waterfront`$`Fixed efffet - Unweighted`, cluster = id),
    "Fixed effect - Cluster weighted " = robust(panels$`Non-waterfront`$`Fixed effect - Cluster weighted`, cluster = cluster),
    "Fixed effect - VAC"= robust(panels$`Non-waterfront`$`Fixed effect - VAC`, cluster = cluster),
    "Random effect"= robust(panels$`Non-waterfront`$`Random effect`, cluster = cluster)
  )
)

f <- function(x) format(round(x, 3), big.mark=",")


gm <- list(
  list("raw" = "nobs", "clean" = "Num. of Obs.", "fmt" = f)
)

modelsummary(
  panels_robu,
  shape = "rbind",
  stars = TRUE, gof_map = gm,
  coef_rename = c("overall" = "Estimated mean"), output = "./results/Tables/table2.tex")




##############################################################################
# some quality check works

# test corelation of weights


cor_test <- df_wf%>%
  select(cluster_w,cluster_ss_w,re_w)

library(corrplot)

corrplot(cor(cor_test),
         method = "number",
         type = "upper" # show only upper side
)



# un-weight
# feols
feols(wqelast ~ 1, data = df_wf, weights = ~ unweight_w, vcov = ~ cluster)
# metafor
res <- rma(yi = wqelast, vi, data=df_wf, method="EE", weighted=FALSE, level = 95)
robust(res, cluster=id)
# cluster weight
# feols
feols(wqelast ~ 1, data = df_wf, weights = ~ cluster_w, vcov = ~ cluster)
# metafor
res <- rma(yi = wqelast, vi, data=df_wf, method="EE", weights=cluster_w, level = 95)
robust(res, cluster=cluster)
# cluster and sample size  weight
# feols
feols(wqelast ~ 1, data = df_wf, weights = ~ cluster_ss_w, vcov = ~ cluster)
# metafor
res <- rma(yi = wqelast, vi, data=df_wf, method="EE", weights=cluster_ss_w, level = 95)
robust(res, cluster=cluster)





