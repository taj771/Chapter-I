rm(list = ls())
library(dplyr)
library(meta)
library(metafor)
library(tidyverse)
###############################################################################

# un-weighted mean
# Do not use inverse variance (vi = 1)

# read file full data set

df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)
# water front data
wf <- filter(df, distbuf == 1, wqvar == "Water Clarity (Secchi depth)")

wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()

wf <- mutate(wf, var = 1)

# using metafor package
res.ee <- rma(yi = elast_sim, vi = var, data=wf, method="EE",level = 95)
summary(res.ee) 


robust(res.ee, cluster = cluster, clubSandwich = TRUE, digits = 5)

# to check the weights 
w <- weights(res.ee, type="matrix")
w

# forest plot
forest(wf$elast_sim, wf$elast_sim_se^2, header=TRUE)


# using general meta package
m.gen <- metagen(TE = wqelast,
                 seTE = var,
                 studlab = studyid,
                 data = wf,
                 sm = "MD",
                 fixed = TRUE,
                 random = FALSE,
                 method.tau = "DL",
                 hakn = TRUE,
                 title = "Chlorophyll a")

summary(m.gen)


# both package results are similar confidence interval need to be address/
# not consistent with Guignet

# here I used roubust standard error 

model <- lm(elast_sim ~ 1, data = wf, digits = 5)


mp <- model_parameters(
  model,
  vcov = "CL", # type of covariance matrix
  vcov_args = list(type = "HC1") # type of robust estimation
)

mp


# cluster weighted mean
# P.S. cluster is unique study + housing market (results in multiple 
# observation from a single study)
# Therefore average effect size within cluster - to cancel out the unequal 
# from a single study
# still variance does not play a role (accuracy of the estimate, vi =1)

# read file full data set
df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)

# select water front data and wqpar = chlorophyll a
wf <- filter(df, distbuf == 1, wqvar == "Chlorophyll a")
wf <- select(wf, obsid, studyid, wqelast, elast_sim_se, census_fips)
# calculate average effect size within cluster
wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(e_bar = mean(wqelast)) %>%
  ungroup()

wf <- mutate(wf, var = 1)

wf <- distinct(wf, studyid,census_fips, .keep_all = TRUE)

# using metafor package
res.ee <- rma(yi = e_bar, vi = var, data=wf, method="EE", level = 95)
summary(res.ee) 

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


# VAC (variance adjusted cluster weights)

df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)

# water front data + wqvar = "Chlorophyll a"

wf <- filter(df, distbuf == 1, wqvar == "Chlorophyll a")

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
  summarise_at(vars(w_elas), sum)

wf <- mutate(wf, var = 1)

# metafor package
res.ee <- rma(yi = w_elas, vi = var, data=wf, method="EE", level = 95)
summary(res.ee) 

#general meta package
m.gen <- metagen(TE = w_elas,
                 seTE = var,
                 studlab = census_fips,
                 data = wf,
                 sm = "SMD",
                 fixed = TRUE,
                 random = FALSE,
                 method.tau = "DL",
                 hakn = TRUE,
                 title = "Third Wave Psychotherapies")

summary(m.gen)


# RESCA (random effect size)
# multiple random effects and possibly correlated sampling errors
# The model now considers three sources of variability: 
# Between study heterogeneity (^σ1), 
# within cluster heterogeneity (^σ2),
# and sampling variability (vi)
# weight (w) i=1/(^σ1+^σ2+vi)

df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)

# water front data + wqvar = chlorophyl a

wf <- filter(df, distbuf == 1, wqvar == "Chlorophyll a")

wf <- select(wf, obsid, studyid, wqelast, elast_sim_se, census_fips)

# calculate variance
wf <- mutate(wf, vi = wf$elast_sim_se^2)

# placing cluster ID
wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()

# using metafor package
res.re <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=wf)
res.re

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

rm(list = ls())


df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)
# water front data

wf <- filter(df, distbuf == 1, wqvar == "Chlorophyll a")

wf <- select(wf, obsid, studyid, wqelast, elast_sim_se, census_fips)

res.re <- rma(yi = wqelast, vi = elast_sim_se, data=wf, level = 95)
summary(res.re) 


wf <- mutate(wf, wi = 1 / (res.re$tau2 + wf$elast_sim_se^2))

wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(w_bar = mean(wi)) %>%
  ungroup()

wf <- 
  wf %>%
  group_by(studyid, census_fips) %>%
  mutate(w_bar_sum = sum(w_bar)) %>%
  ungroup()

wf <- mutate(wf, weight = wf$w_bar/wf$w_bar_sum)
wf <- mutate(wf, w_elas = wf$weight*wf$wqelast)


wf <- mutate(wf, var = 1)

wf <- wf %>%
  group_by(studyid, census_fips)%>%
  summarise_at(vars(w_elas), mean)


res.ee <- rma(yi = w_elas, vi = var, data=wf, method="EE", level = 95)
summary(res.ee) 




res.re <- rma(yi = w_elas, vi = var, data=wf, level = 95)
summary(res.re) 




m.gen <- metagen(TE = w_elas,
                 seTE = var,
                 studlab = census_fips,
                 data = wf,
                 sm = "SMD",
                 fixed = TRUE,
                 random = TRUE,
                 method.tau = "DL",
                 hakn = TRUE,
                 title = "Third Wave Psychotherapies")

summary(m.gen)

