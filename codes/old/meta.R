# clean memory
rm(list = ls())
# load library 
library(tidyverse)
library(dplyr)
library(meta)
library(fixest)
library(dmetar)

data(ThirdWave)
glimpse(ThirdWave)

m.gen <- metagen(TE = TE,
                 seTE = seTE,
                 studlab = Author,
                 data = ThirdWave,
                 sm = "SMD",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 hakn = TRUE,
                 title = "Third Wave Psychotherapies")

summary(m.gen)

# VAC
# read file full data set
df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)
# water front data
#Chlorophyll a
wf <- filter(df, distbuf == 1 & wqvar == "Chlorophyll a")

wf <- wf%>%
  group_by(studyid, wqvar, census_fips)%>%
  summarise_at(vars(wqelast, elast_sim_se), mean)


m.gen <- metagen(TE = wqelast,
                 seTE = elast_sim_se,
                 studlab = studyname,
                 data = wf,
                 sm = "SM",
                 fixed = TRUE,
                 random = FALSE,
                 method.tau = "REML",
                 hakn = TRUE,
                 title = "Third Wave Psychotherapies")

summary(m.gen)

wf <- filter(df, distbuf == 1 & wqvar == "Chlorophyll a")

write.csv(wf, "./other/wf.csv")
wf <- read.csv("./other/wf.csv")
non_wf <- read.csv("./other/non_wf.csv")


wf <- wf%>%
  group_by(studyid, wqvar, census_fips)%>%
  summarise_at(vars(wqelast, elast_sim_se), mean)

wf <- filter(wf2, distbuf == 1 & wqvar == "Dissolved oxygen")


m.gen <- metagen(TE = w_elas,
                 seTE = elast_sim_se,
                 studlab = census_fips,
                 data = wf,
                 sm = "SM",
                 fixed = TRUE,
                 random = TRUE,
                 method.tau = "REML",
                 hakn = TRUE,
                 title = "Third Wave Psychotherapies")

summary(m.gen)


library(dmetar)
library(tidyverse)
library(meta)
data(BdiScores)

# We only need the first four columns
glimpse(BdiScores[,1:4])

wf <- read.csv("./other/test.csv")


m.mean <- metamean(n = n,
                   mean = elas,
                   sd = elas_se,
                   studlab = cluster,
                   data = wf,
                   sm = "MRAW",
                   fixed = TRUE,
                   random = FALSE,
                   method.tau = "REML",
                   hakn = TRUE,
                   title = "BDI-II Scores")
summary(m.mean)













df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)
# water front data
#Chlorophyll a
wf <- filter(df, distbuf == 1 & wqvar == "Dissolved oxygen")


# water front


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



m.gen <- metagen(TE = w_elas,
                 seTE = elast_sim_se,
                 studlab = census_fips,
                 data = wf2,
                 sm = "SMD",
                 fixed = TRUE,
                 random = FALSE,
                 method.tau = "DL",
                 hakn = TRUE,
                 title = "Third Wave Psychotherapies")

summary(m.gen)





# RESCA
# read file full data set
df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")%>%
  drop_na(wqelast)
# water front data
wf <- filter(df, distbuf == 1 & wqvar == "Chlorophyll a")

wf <- wf%>%
  group_by(studyid, wqvar, census_fips)%>%
  summarise_at(vars(wqelast, elast_sim_se), mean)

m.gen <- metagen(TE = wqelast,
                 seTE = elast_sim_se,
                 studlab = census_fips,
                 data = wf,
                 sm = "SMD",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "DL",
                 hakn = TRUE,
                 title = "Third Wave Psychotherapies")


summary(m.gen)

update(m.gen, pooledvar = TRUE)

