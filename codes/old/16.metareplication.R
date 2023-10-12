# clean memory
rm(list = ls())
# load library 
library(tidyverse)
library(dplyr)
library(meta)
library(fixest)

################################################################################
# un-weighted mean
################################################################################
# read file full data set
df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)

# water front data

wf <- filter(df, distbuf == 1)
unweighted_mean_wf <- lm(wqelast ~ 0 + wqvar, data = wf)
summary(unweighted_mean_wf)
confint(unweighted_mean_wf)

# non-waterfront data

non_wf <- filter(df, distbuf == 2)
unweighted_mean_nwf <- lm(wqelast ~ 0 + wqvar, data = non_wf)
summary(unweighted_mean_nwf)
confint(unweighted_mean_nwf)

###############################################################################
# cluster weighted mean
###############################################################################

# read file full data set
df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)
# water front data
wf <- filter(df, distbuf == 1)

wf <- wf%>%
  group_by(studyid, wqvar, census_fips)%>%
  summarise_at(vars(wqelast), mean)

clus_wei_mean_wf <- lm(wqelast ~ 0 + wqvar, data = wf)

summary(clus_wei_mean_wf)
confint(clus_wei_mean_wf)

# non-water front data
non_wf <- filter(df, distbuf == 2)

non_wf <- non_wf%>%
  group_by(studyid, wqvar, census_fips)%>%
  summarise_at(vars(wqelast), mean)

clus_wei_mean_non_wf <- lm(wqelast ~ 0 + wqvar, data = non_wf)

summary(clus_wei_mean_non_wf)
confint(clus_wei_mean_non_wf)


###############################################################################
# VAC weights
###############################################################################
# read file full data set
df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)

# water front

wf <- filter(df, distbuf == 1)

wf$var <- 1/wf$elast_sim_se^2


wf1 <- wf%>%
  group_by(studyid, wqvar, census_fips)%>%
  summarise_at(vars(var), sum)%>%
  select(var,census_fips,wqvar)

colnames(wf1)[2]<- "weight"

wf2 <- left_join(wf1, wf, by = c("census_fips", "wqvar","studyid"))

wf2 <- wf2%>%
  mutate(weig = var/weight)

wf2 <- wf2%>%
  mutate(w_elas = wqelast*weig)

wf2 <- wf2 %>%
  group_by(studyid, wqvar, census_fips)%>%
  summarise_at(vars(w_elas), sum)


VAC_wf <- lm(w_elas ~ 0 + wqvar, data = wf2)
summary(VAC_wf)
confint(VAC_wf)


# non-waterfront

non_wf <- filter(df, distbuf == 2)

non_wf$var <- 1/non_wf$elast_sim_se^2


non_wf1 <- non_wf%>%
  group_by(studyid, wqvar, census_fips)%>%
  summarise_at(vars(var), sum)%>%
  select(var,census_fips,wqvar)

colnames(non_wf1)[2]<- "weight"

non_wf2 <- left_join(non_wf1, non_wf, by = c("census_fips", "wqvar","studyid"))

non_wf2 <- non_wf2%>%
  mutate(weig = var/weight)

non_wf2 <- non_wf2%>%
  mutate(w_elas = wqelast*weig)

non_wf2 <- non_wf2 %>%
  group_by(studyid, wqvar, census_fips)%>%
  summarise_at(vars(w_elas), sum)


VAC_non_wf <- lm(w_elas ~ 0 + wqvar, data = non_wf2)
summary(VAC_non_wf)
confint(VAC_non_wf)


predict(unweighted_mean_wf,interval ="confidence" )

df1 <- subset(df, wqvar == "Chlorophyll a")
write.csv(df1, "./other/data.csv")

# RESCA

# read file full data set
df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)

# water front

wf <- filter(df, distbuf == 1 & wqvar == "Chlorophyll a")

wf$w_fe <- 1/wf$elast_sim_se^2

# calculating T2

  # calculating q

    # calculating standard fixed effect

wf$var_sum <- sum(wf$w_fe)
wf$weight <- wf$w_fe/wf$var_sum
wf$fe_e <- wf$weight*wf$wqelast
wf$q <- (wf$wqelast - wf$fe_e)^2

wf$nume <- wf$fe_e - (wf$fe_e^2/sum(wf$fe_e))

wf$T2 <- wf$q-17/wf$nume

wf$w_res <- 1/wf$w_fe+wf$T2

wf$sum_wf_res <- sum(wf$w_res)

wf$wf_weight_res <- wf$w_res/wf$sum_wf_res

wf$res_e <- wf$wqelast*wf$wf_weight_res


RESCA_wf <- lm(res_e ~ 1, data = wf)
summary(RESCA_wf)
