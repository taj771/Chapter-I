#clear memory
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
  stargazer,
  xtable,
  texreg,
  modelsummary
)


# Reference : https://cran.r-project.org/web/packages/clubSandwich/vignettes/meta-analysis-with-CRVE.html
#           : https://www.jepusto.com/robust-meta-analysis-1/
#           : https://cran.r-project.org/web/packages/robumeta/vignettes/robumetaVignette.pdf
###############################################################################
# load data
df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")

## Subset: Main models
# waterfront
df.wf <- df[which(df$distbuf == 1), ]
# non-waterfront
df.nwf <- df[which(df$distbuf == 2), ]

################################################################################
#                               un-weighted mean                               #
################################################################################

# Fixed effect model

#define weights
df.wf.fe.u <- df.wf%>%
  mutate(var = 1, weight = 1) #do not consider variance into the accuracy of the estimation
df.nwf.fe.u <- df.nwf%>%
  mutate(var = 1, weight = 1) #do not consider variance into the accuracy of the estimation

#using metafor function
fe.wf.u <- rma(yi = wqelast, vi = var, data=df.wf.fe.u, method="EE",level = 95)
fe.nwf.u <- rma(yi = wqelast, vi = var, data=df.nwf.fe.u, method="EE",level = 95)

################################################################################
#                           Cluster weighted mean                              #
################################################################################

# define weights

# waterfront

df.wf.fe.c <- df.wf%>%
  group_by(studyid,geog,wqvar)%>%
  mutate(e_bar = mean(wqelast))%>% # change wqelasr_simu if you need
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()%>%
  distinct(studyid,geog,wqvar, .keep_all = TRUE)%>%
  mutate(var = 1, weight = 1)


# non-waterfront 

df.nwf.fe.c <- df.nwf%>%
  group_by(studyid,geog,wqvar)%>%
  mutate(e_bar = mean(wqelast))%>% # change wqelasr_simu if you need
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()%>%
  distinct(studyid,geog,wqvar, .keep_all = TRUE)%>%
  mutate(var = 1, weight = 1)


fe.wf.c <- rma(yi = e_bar, vi = var, data=df.wf.fe.c, method="EE",level = 95)
fe.nwf.c <- rma(yi = e_bar, vi = var, data=df.nwf.fe.c, method="EE",level = 95)

models <- list(fe.wf.c, fe.nwf.c)



modelsummary(
  models,
  fmt = 1,
  estimate  = c("estimate",
                "{estimate}{stars}",
                "{estimate} ({std.error})",
                "{estimate} ({std.error}){stars}",
                "{estimate} [{conf.low}, {conf.high}]"),
  statistic = NULL,
  coef_omit = "Intercept")

modelsummary(models)


waterfront <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)%>%
  filter(distbuf == 1)%>%
  group_by(studyid, geog, wqvar) %>%
  mutate(e_bar = mean(wqelast)) %>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()%>%
  distinct(studyid,geog,wqvar, .keep_all = TRUE)%>%
  mutate(var = 1, weight = 1)

#loop for all wq variables
data_list <- split(waterfront, waterfront$wqvar)
model_list_wf <- lapply(data_list, function(sub_df) {
  wq_cluster <- robu(formula = e_bar ~1, 
                     data = sub_df, studynum = obsid, var.eff.size = var, userweights = weight)
})



modelsum


# Table & Figures

# fixed effect - unweighted
sink("./meta_results/fe_wf.tex")
texreg(list(fe.wf.u,fe.nwf.u),
  file = NULL,
  single.row = FALSE,
  stars = c(0.001, 0.01, 0.05),
  custom.header = NULL,
  custom.model.names = c("Waterfront", "Non-waterfront"),
  custom.coef.names = c("Unweighted mean"),
  custom.coef.map = NULL,
  custom.gof.names = NULL,
  custom.gof.rows = NULL,
  custom.note = NULL,
  digits = 4,
  leading.zero = TRUE,
  symbol = "\\cdot",
  override.coef = 0,
  override.se = 0,
  override.pvalues = 0,
  override.ci.low = 0,
  override.ci.up = 0,
  omit.coef = NULL,
  reorder.coef = NULL,
  reorder.gof = NULL,
  ci.force = FALSE,
  ci.force.level = 0.95,
  ci.test = 0,
  groups = NULL,
  custom.columns = NULL,
  custom.col.pos = NULL,
  bold = 0,
  center = TRUE,
  caption = "Fixed effect - Unweighted Mean",
  caption.above = FALSE,
  label = "table:coefficients",
  booktabs = FALSE,
  dcolumn = FALSE,
  siunitx = FALSE,
  lyx = FALSE,
  sideways = FALSE,
  longtable = FALSE,
  threeparttable = FALSE,
  use.packages = TRUE,
  table = TRUE,
  tabular = TRUE,
  no.margin = FALSE,
  fontsize = NULL,
  scalebox = NULL)
sink()

 

################################################################################
#                           Cluster weighted mean                              #
################################################################################

#waterfront houses
waterfront <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)%>%
  filter(distbuf == 1)%>%
  group_by(studyid, geog, wqvar) %>%
  mutate(e_bar = mean(wqelast)) %>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()%>%
  distinct(studyid,geog,wqvar, .keep_all = TRUE)%>%
  mutate(var = 1, weight = 1)

#loop for all wq variables
data_list <- split(waterfront, waterfront$wqvar)
model_list_wf <- lapply(data_list, function(sub_df) {
  wq_cluster <- robu(formula = e_bar ~1, 
                     data = sub_df, studynum = obsid, var.eff.size = var, userweights = weight)
})


#non-waterfront houses
nonwaterfront <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)%>%
  filter(distbuf == 2)%>%
  group_by(studyid, geog, wqvar) %>%
  mutate(e_bar = mean(wqelast)) %>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()%>%
  distinct(studyid,geog,wqvar, .keep_all = TRUE)%>%
  mutate(var = 1, weight = 1)

#loop for all wq variables
data_list <- split(nonwaterfront, nonwaterfront$wqvar)
model_list_nwf <- lapply(data_list, function(sub_df) {
  wq_cluster <- robu(formula = e_bar ~1, 
                     data = sub_df, studynum = obsid, var.eff.size = var, userweights = weight)
})


################################################################################
#              Variance adjusted Cluster weighted mean                         #
################################################################################
#waterfront houses
waterfront <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)%>%
  filter(distbuf == 1)%>%
  mutate(var = 1/elast_sim_se^2)%>%
  group_by(studyid, geog, wqvar) %>%
  mutate(V_bar = sum(var)) %>%
  ungroup()%>%
  mutate(weight = var/V_bar)%>%
  mutate(varc = 1)

data_list <- split(waterfront, waterfront$wqvar)
model_list_wf <- lapply(data_list, function(sub_df) {
  wq_cluster <- robu(formula = wqelast ~1, 
                     data = sub_df, studynum = obsid, var.eff.size = varc, userweights = weight)
})

# non-waterfront homes
nonwaterfront <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)%>%
  filter(distbuf == 2)%>%
  mutate(var = 1/elast_sim_se^2)%>%
  group_by(studyid, geog, wqvar) %>%
  mutate(V_bar = sum(var)) %>%
  ungroup()%>%
  mutate(weight = var/V_bar)%>%
  mutate(varc = 1)

data_list <- split(nonwaterfront, nonwaterfront$wqvar)
model_list_nwf <- lapply(data_list, function(sub_df) {
  wq_cluster <- robu(formula = wqelast ~1, 
                     data = sub_df, studynum = obsid, var.eff.size = varc, userweights = weight)
})


################################################################################
#              Random effect size cluster adjusted weighted mean               #
################################################################################

# multiple random effects and possibly correlated sampling errors
# The model now considers three sources of variability: 
# Between study heterogeneity (^σ1), 
# within cluster heterogeneity (^σ2),
# and sampling variability (vi)
# weight (w) i=1/(^σ1+^σ2+vi)

waterfront <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)%>%
  filter(distbuf == 1, wqvar == "Water Clarity (Secchi depth)")%>%
  mutate(vi = elast_sim_se^2)%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()

# using metafor package
res.re <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=waterfront)
res.re


# robu package
re <- robu(wqelast ~  1,
                  data = waterfront, studynum = cluster, var.eff.size = elast_sim_se^2, 
                  modelweights = "HIER")

# calculating weights


wi <- 1 / (sum(res.re$sigma2) + waterfront$vi)
sum(wi * waterfront$wqelast) / sum(wi)

coef(res.re)

round(vcov(res.re, type="obs")[1:8,1:8], 3)

round(weights(res.re, type="matrix")[1:8,1:8], 3)

W <- weights(res.re, type="matrix")
X <- model.matrix(res.re)
y <- cbind(waterfront$wqelast)
solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% y

W <- weights(res.re, type="matrix")
sum(rowSums(W) * waterfront$wqelast) / sum(W)

W[1,1:5]

weights(res.re, type="rowsum")[1:10]

wi <- weights(res.re, type="rowsum")
sum(wi * waterfront$wqelast) / sum(wi)

data.frame(k = c(table(waterfront$cluster)),
           weight = tapply(wi, waterfront$cluster, sum))


###############################################################################
#              Benefit transfer error                                         #
###############################################################################

waterfront <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)%>%
  filter(wqvar == "Water Clarity (Secchi depth)" )%>%
  mutate(var = 1/elast_sim_se^2)%>%
  group_by(studyid, geog) %>%
  mutate(V_bar = sum(var)) %>%
  ungroup()%>%
  mutate(weight = var/V_bar)%>%
  mutate(varc = 1)%>%
  group_by(studyid, geog, distbuf) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()%>%
  mutate(w_elas = wqelast*weight)%>%
  group_by(cluster)%>%
  mutate(vac_elas = mean(w_elas))%>%
  mutate(weight_rob = 1)%>%
  distinct(cluster, .keep_all = TRUE)
  

# loop - drop each observation and calculate elasticity for each observation

result <- list()

#loop through obsid and extract betas
for(i in unique(waterfront$cluster)){
  
  #construct linear model (robu package)
  vac_elast <- robu(formula = vac_elas ~1, 
                data = subset(waterfront, waterfront$cluster != i),
                studynum = obsid, var.eff.size = varc, userweights = weight_rob)
  
  #create data.frame containing intercept left out and coefficient
  result.dt <- data.frame(beta = vac_elast$reg_table[[2]],
                          cluster = i)
  
  #bind to list
  result[[i]] <- result.dt
}

#bind to data.frame
result <- do.call(rbind, result)

head(result)

# left_join - to calculate transfer error

vac_transfer_error <- left_join(waterfront, result, by = "cluster")

vac_T_error <- mutate(vac_transfer_error,T_error = abs((beta - wqelast)/wqelast)*100)

median(vac_T_error$T_error)







