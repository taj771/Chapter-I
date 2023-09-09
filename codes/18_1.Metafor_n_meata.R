library(pacman)
pacman::p_load(data.table,dplyr,meta,metafor,tidyverse,fabricatr,clubSandwich,parameters) 


#un-weighted mean
#Do not use inverse variance (vi = 1)
#read file full data set
setwd("E:/Thara_Academic/Chapter I/Canada Lakes/Can_Lakes")
df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)
# mutate variance =1 as un-weighted do not correct based on the variance of the estimate
df <- mutate(df, var = 1)

# select different water quality measures and estimate the fixed effect size
# based on the assumption of un-weighted mean
##############################################################################
# "Water Clarity (Secchi depth)"
# waterfront
wf <- filter(df, distbuf == 1, wqvar == "Water Clarity (Secchi depth)")
#define clusters
wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package:
# reference : https://www.metafor-project.org/doku.php/tips:weights_in_rma.mv_models
res.ee <- rma(yi = elast_sim, vi = var, data=wf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
# nonwaterfront
nwf <- filter(df, distbuf == 2, wqvar == "Water Clarity (Secchi depth)")
#define clusters
nwf <- 
  nwf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data=nwf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
###############################################################################
# "Fecal coliform"
# waterfront
wf <- filter(df, distbuf == 1, wqvar == "Fecal coliform")
#define clusters
wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= wf, method="EE",level = 95)
summary(res.ee) 
# robu meta package : 
# reference : https://cran.r-project.org/web/packages/robumeta/vignettes/robumetaVignette.pdf
reg_1 <- robu(elast_sim ~ 1,
              data = wf, studynum = cluster, var.eff.size = elast_sim_se^2, 
              userweights = var, rho = .8)
print(reg_1)
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
# nonwaterfront
nwf <- filter(df, distbuf == 2, wqvar == "Fecal coliform")
nwf <- 
  nwf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data=nwf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
###############################################################################
# sediments
# waterfront
wf <- filter(df, distbuf == 1, wqvar == "Sediment")
wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= wf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
# nonwaterfront
nwf <- filter(df, distbuf == 2, wqvar == "Sediment")
nwf <- 
  nwf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= nwf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
##############################################################################

# Nitrogen
# waterfront
wf <- filter(df, distbuf == 1, wqvar == "Nitrogen")
wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= wf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
# nonwaterfront
nwf <- filter(df, distbuf == 2, wqvar == "Nitrogen")
nwf <- 
  nwf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= nwf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
###############################################################################
# Phosphorus
# waterfront
wf <- filter(df, distbuf == 1, wqvar == "Phosphorus")
wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= wf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
# nonwaterfront
nwf <- filter(df, distbuf == 2, wqvar == "Phosphorus")
nwf <- 
  nwf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= nwf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
###############################################################################
# Total Suspended Solids
# waterfront
wf <- filter(df, distbuf == 1, wqvar == "Total Suspended Solids")
wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= wf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
# nonwaterfront
nwf <- filter(df, distbuf == 2, wqvar == "Total Suspended Solids")
nwf <- 
  nwf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= nwf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
###############################################################################
# Dissolved oxygen
# waterfront
wf <- filter(df, distbuf == 1, wqvar == "Dissolved oxygen")
wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= wf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
# nonwaterfront
nwf <- filter(df, distbuf == 2, wqvar == "Dissolved oxygen")
nwf <- 
  nwf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= nwf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
###############################################################################
# pH
# waterfront
wf <- filter(df, distbuf == 1, wqvar == "pH")
wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= wf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
# nonwaterfront
nwf <- filter(df, distbuf == 2, wqvar == "pH")
nwf <- 
  nwf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= nwf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
###############################################################################
# Temperature
# waterfront
wf <- filter(df, distbuf == 1, wqvar == "Temperature")
wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= wf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5) 
# nonwaterfront
nwf <- filter(df, distbuf == 2, wqvar == "Temperature")
nwf <- 
  nwf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= nwf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
################################################################################
# E-coli
# waterfront
wf <- filter(df, distbuf == 1, wqvar == "E-coli")
wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= wf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
# nonwaterfront
nwf <- filter(df, distbuf == 2, wqvar == "E-coli")
wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= nwf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5) 
###############################################################################
# Turbidity
# waterfront
wf <- filter(df, distbuf == 1, wqvar == "Turbidity")
wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= wf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
# nonwaterfront
nwf <- filter(df, distbuf == 2, wqvar == "Turbidity")
nwf <- 
  nwf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= nwf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
###############################################################################
# Lake trophic state 
# waterfront
wf <- filter(df, distbuf == 1, wqvar == "Lake trophic state ")
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data=wf, method="EE",level = 95)
summary(res.ee) 
# nonwaterfront
nwf <- filter(df, distbuf == 2, wqvar == "Lake trophic state ")
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data=nwf, method="EE",level = 95)
summary(res.ee) 
################################################################################
# Chlorophyll a
# waterfront
wf <- filter(df, distbuf == 1, wqvar == "Chlorophyll a")
wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= wf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
# nonwaterfront
nwf <- filter(df, distbuf == 2, wqvar == "Chlorophyll a")
nwf <- 
  nwf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data= nwf, method="EE",level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
################################################################################
# trophic state index
# waterfront
wf <- filter(df, distbuf == 1, wqvar == "trophic state index")
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data=wf, method="EE",level = 95)
summary(res.ee) 
# nonwaterfront
nwf <- filter(df, distbuf == 2, wqvar == "trophic state index")
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data=nwf, method="EE",level = 95)
summary(res.ee) 
################################################################################
# Percent Water Visibility
# waterfront
wf <- filter(df, distbuf == 1, wqvar == "Percent Water Visibility")
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data=wf, method="EE",level = 95)
summary(res.ee) 
# nonwaterfront
nwf <- filter(df, distbuf == 2, wqvar == "Percent Water Visibility")
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data=nwf, method="EE",level = 95)
summary(res.ee) 
################################################################################
# Sedimentation Rate
# waterfront
wf <- filter(df, distbuf == 1, wqvar == "Sedimentation Rate")
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data=wf, method="EE",level = 95)
summary(res.ee) 
# nonwaterfront
nwf <- filter(df, distbuf == 2, wqvar == "Sedimentation Rate")
# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data=nwf, method="EE",level = 95)
summary(res.ee) 
###############################################################################
###############################################################################
# cluster weighted mean
# P.S. cluster is unique study + housing market (results in multiple 
# observation from a single study)
# Therefore average effect size within cluster - to cancel out the unequal 
# from a single study
# still variance does not play a role (accuracy of the estimate, vi =1)
# read file full data set
df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)
# select different water quality measures and estimate the fixed effect size
# based on the assumption of cluster adjested weight
################################################################################
#Water Clarity (Secchi depth)
# water front
wf <- filter(df, distbuf == 1, wqvar == "Water Clarity (Secchi depth)")
# calculate average effect size within cluster
wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(e_bar = mean(wqelast)) %>%
  ungroup()
# variance is still = 1
wf <- mutate(wf, var = 1)
# keep the average
wf <- distinct(wf, studyid,census_fips, .keep_all = TRUE)
# cluster
wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = e_bar, vi = var, data=wf, method="EE", level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
# weights
w <- weights(res.ee, type="matrix")
w
# using meta package
m.gen <- metagen(TE = e_bar,
                 seTE = var,
                 studlab = census_fips,
                 data = wf,
                 sm = "SMD",
                 fixed = TRUE,
                 random = FALSE,
                 method.tau = "DL",
                 hakn = TRUE,
                 title = "Chlorophyll a")

summary(m.gen)

# non-water front
nwf <- filter(df, distbuf == 2, wqvar == "Water Clarity (Secchi depth)")
# calculate average effect size within cluster
nwf <- 
  nwf %>%
  group_by(studyid, census_fips) %>%
  mutate(e_bar = mean(wqelast)) %>%
  ungroup()
# variance is still = 1
nwf <- mutate(nwf, var = 1)
# keep the average
nwf <- distinct(nwf, studyid,census_fips, .keep_all = TRUE)
# cluster
nwf <- 
  nwf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
# using metafor package
res.ee <- rma(yi = e_bar, vi = var, data=nwf, method="EE", level = 95)
summary(res.ee) 
# robust standard errors
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)
# weights
w <- weights(res.ee, type="matrix")
w
# using meta package
m.gen <- metagen(TE = e_bar,
                 seTE = var,
                 studlab = census_fips,
                 data = nwf,
                 sm = "SMD",
                 fixed = TRUE,
                 random = FALSE,
                 method.tau = "DL",
                 hakn = TRUE,
                 title = "Chlorophyll a")

summary(m.gen)
################################################################################
################################################################################
# VAC (variance adjusted cluster weights)
df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)

# water front data + wqvar = "Water Clarity (Secchi depth)"

wf <- filter(df, distbuf == 1, wqvar == "Water Clarity (Secchi depth)")

# calculate variance
wf$var <- 1/wf$elast_sim_se^2

# calculate weights based on variance within cluster
# still heterogeneity between study did not consider

wf <- 
  wf %>%
  group_by(studyid, geog) %>%
  mutate(V_bar = sum(var)) %>%
  ungroup()

wf <- wf%>%
  mutate(weig = var/V_bar)

wf <- wf%>%
  mutate(w_elas = wqelast*weig)

wf <- wf %>%
  group_by(studyid, geog)%>%
  summarise_at(vars(w_elas,elast_sim_se), sum)

wf <- mutate(wf, var = 1)

# cluster
wf <- 
  wf %>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()

# metafor package
res.ee <- rma(yi = w_elas, vi = var, data=wf, method="EE", level = 95)
summary(res.ee) 

# robust standard error
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)

# robu
wf <- mutate(wf, weig = 1)


reg_1 <- robu(w_elas ~ 1 ,
              data = wf, studynum = cluster, var.eff.size = var, 
              userweights = weig)
print(reg_1)


# non-water front data + wqvar = "Water Clarity (Secchi depth)"

nwf <- filter(df, distbuf == 2, wqvar == "Water Clarity (Secchi depth)")

# calculate variance
nwf$var <- 1/nwf$elast_sim_se^2

# calculate weights based on variance within cluster
# still heterogeneity between study did not consider

nwf <- 
  nwf %>%
  group_by(studyid, census_fips) %>%
  mutate(V_bar = sum(var)) %>%
  ungroup()

nwf <- nwf%>%
  mutate(weig = var/V_bar)

nwf <- nwf%>%
  mutate(w_elas = wqelast*weig)

nwf <- nwf %>%
  group_by(studyid, census_fips)%>%
  summarise_at(vars(w_elas), sum)

nwf <- mutate(nwf, var = 1)

# cluster
nwf <- 
  nwf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()

# metafor package
res.ee <- rma(yi = w_elas, vi = var, data=nwf, method="EE", level = 95)
summary(res.ee) 
# robust standard error
robust.rma.uni(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)


# RESCA (random effect size)
# multiple random effects and possibly correlated sampling errors
# The model now considers three sources of variability: 
# Between study heterogeneity (^σ1), 
# within cluster heterogeneity (^σ2),
# and sampling variability (vi)
# weight (w) i=1/(^σ1+^σ2+vi)

df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)

# water front data + wqvar = Water Clarity (Secchi depth)

wf <- filter(df, distbuf == 1, wqvar == "Water Clarity (Secchi depth)")

# calculate variance
wf <- mutate(wf, vi = wf$elast_sim_se^2)

# placing cluster ID
wf <- 
  wf %>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()

# using metafor package
res.re <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=wf)
res.re

#
# calculate weights 
wi <- 1 / (sum(res.re$sigma2) + wf$vi)
sum(wi * wf$wqelast) / sum(wi)

coef(res.re)

round(vcov(res.re, type="obs")[1:8,1:8], 3)

round(weights(res.re, type="matrix")[1:8,1:8], 3)


W <- weights(res.re, type="matrix")
X <- model.matrix(res.re)
y <- cbind(wf$wqelast)
solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% y

W <- weights(res.re, type="matrix")
sum(rowSums(W) * wf$wqelast) / sum(W)

W[1,1:5]

weights(res.re, type="rowsum")[1:10]

wi <- weights(res.re, type="rowsum")
sum(wi * wf$wqelast) / sum(wi)

data.frame(k = c(table(wf$cluster)),
           weight = tapply(wi, wf$cluster, sum))

################################################################################

