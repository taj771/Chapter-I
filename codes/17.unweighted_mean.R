################################################################################
# Model 1 - Mean elasticity estimates weighing 
# Date 04/22/22
# Description - Replication of Guignet's meta analysis results
################################################################################

# clean memory
rm(list = ls())
# load library 
library(tidyverse)
library(dplyr)
library(car)

# read file full data set
df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")

################################################################################
# un-weighted mean elasticity measures calculation
################################################################################
# filter waterfront homes vs non-waterfront homes 
# waterfront homes = 1
# non-waterfront homes = 2
###############################################################################

df1 <- filter(df, distbuf == 1)
  
# water quality parameter
# Chlorophyll a
# Dissolved oxygen
# E-coli
# Fecal coliform
# Lake trophic state index
# Light attenuation
# Nitrogen
# Percent Water Visibility
# Phosphorous
# Salinity
# Sediment
# Sedimentation Rate
# Temperature
# Total Suspended Solids
# Turbidity
# pH
# Trophic state index
# Water clarity


# chlorophyll a - waterfront

cha_wf <- filter(df1, wqvar == "Chlorophyll a")%>%
  select(studyid, wqelast, elast_sim, elast_sim_se, distbuf, studyname)
# determination of number of clusters
# Total number of clusters in the meta data set for distance bin d is kd
mean(cha_wf$elast_sim)
t.test(cha_wf$wqelast, conf.level = 0.95) 
# Delta method
deltaMethod(lm(wqelast~1,cha_wf),"Intercept" )


# chlorophyll a - non-waterfront

df1 <- filter(df, distbuf == 2)


cha_nwf <- filter(df1, wqvar == "Chlorophyll a")%>%
  select(studyid, wqelast, elast_sim, elast_sim_se, distbuf, studyname)
# determination of number of clusters
# Total number of clusters in the meta data set for distance bin d is kd
mean(cha_nwf$elast_sim)
t.test(cha_nwf$wqelast, conf.level = 0.95) 
# Delta method
deltaMethod(lm(wqelast~1,cha_nwf),"Intercept" )


model <- feols(wqelast~1 , data = wf, split = df1$wqvar)
model
confint(model, level = 0.95, vcov, se, cluster)


df3 <- distinct(df2, studyid , .keep_all = TRUE)
nrow(df3)
head(df3$studyid)
#Total number of cluster = 3 (kd)
mean(df2$wqelast)





smd.rma<-rma(wqelast,elast_sim_se,method="FE",data=df2)

smd.rma


#t test 95% boundaries not match with paper's numbers, need to discuss that they 
# done a simple t test or else


################################################################################
# cluster weighted mean elasticity calculation
################################################################################

df4 <- subset(df2, studyid == "28")
P1 <- mean(df4$wqelast)
df5 <- subset(df2, studyid == "32")
P2 <- mean(df5$wqelast) 
df6 <- subset(df2, studyid == "35")
P3 <- mean(df6$wqelast) 
clus_chl_wf <- (P1+P2+P3)/3

clus_chl_wf


################################################################################
# Variance adjusted cluster (VAC) weight mean
################################################################################


df2 <- filter(df1, wqvar == "Chlorophyll a")%>%
  select(studyid, wqelast, elast_sim, elast_sim_se, distbuf, studyname)


df3 <- distinct(df2, studyid , .keep_all = TRUE)
nrow(df3)
head(df3$studyid)



df4 <- subset(df2, studyid == "28")
V1 <- df4$wqelast
V2 <- 1/(df4$elast_sim_se)^2 #1/vidj
V3 <- sum(V2)
V4 <- V2/V3
V5 <- V4*V1
V6 <- sum(V5)

  
df5 <- subset(df2, studyid == "32")
V7 <- df5$wqelast
V8 <- 1/(df5$elast_sim_se)^2 #1/vidj
V9 <- sum(V8)
V10 <- V8/V9
V11 <- V10*V7
V12 <- sum(V11)


df6 <- subset(df2, studyid == "35")
V13 <- df6$wqelast
V14 <- 1/(df6$elast_sim_se)^2 #1/vidj
V15 <- sum(V14)
V16 <- V14/V15
V17 <- V16*V13
V18 <- sum(V17)
V19 <- (V6+V12+V18)/3

v19


###############################################################################
# RES
###############################################################################

df7 <- filter(df1, wqvar == "Chlorophyll a")%>%
  select(studyid, wqelast, elast_sim, elast_sim_se)
df7 <- mutate(df7, w = 1/elast_sim_se)%>%
       mutate(df7, wy = w*wqelast)%>%
       mutate(df7, wy2 = w*wqelast^2)%>%
       mutate(df7, w2 = w^2)

#sum of columns
sum_w <- sum(df7$w)
sum_wy <- sum(df7$wy)
sum_wy2 <- sum(df7$wy2)
sum_w2 <- sum(df7$w2)

# fixed effect
m <- sum_wy/sum_w

# calculation of Q
Q <- sum_wy2 - (sum_wy^2/sum_w)

# df
df <- 17

# calculation of c
c <- sum_w- (sum_w2/sum_w)

# calculation of T
T2 <- (Q-df)/c

df8 <- filter(df1, wqvar == "Chlorophyll a")%>%
  select(studyid, wqelast, elast_sim, elast_sim_se)
df8 <- mutate(df8, T2 = T2)%>%
       mutate(df8, v_total = elast_sim_se+T2)

df8_28 <- subset(df8, studyid == 28)
df8_28 <- mutate(df8_28, w = (1/ v_total)/3)
df8_32 <- subset(df8, studyid == 32)
df8_32 <- mutate(df8_32, w = (1/ v_total)/13)
df8_35 <- subset(df8, studyid == 35)
df8_35 <- mutate(df8_35, w = (1/ v_total)/2)

df8 <- rbind(df8_28,df8_32,df8_35)%>%
       mutate(df8, wy = w*wqelast)%>%
       mutate(df8, wy2 = w*wqelast^2)%>%
       mutate(df8, w2 = w^2)

sum_w <- sum(df8$w)
sum_wy <- sum(df8$wy)
sum_wy2 <- sum(df8$wy2)
sum_w2 <- sum(df8$w2)

m <- sum_wy/sum_w

m
###############################################################################
# Non-waterfront homes
################################################################################
df1 <- filter(df, distbuf == 2)

# water quality parameter
# Chlorophyll a
# Dissolved oxygen
# E-coli
# Fecal coliform
# Lake trophic state index
# Light attenuation
# Nitrogen
# Percent Water Visibility
# Phosphorous
# Salinity
# Sediment
# Sedimentation Rate
# Temperature
# Total Suspended Solids
# Turbidity
# pH
# Trophic state index
# Water clarity
df2 <- filter(df1, wqvar == "Chlorophyll a")%>%
  select(studyid, wqelast, elast_sim, elast_sim_se)
mean(df2$wqelast)
t.test(df2$wqelast, conf.level = 0.95) 
#t test 95% boundaries not match with paper's numbers, need to discuss that they 
# done a simple t test or else
################################################################################
# cluster weighted mean elasticity calculation
################################################################################
# Total number of clusters in the meta data set for distance bin d is kd
df3 <- distinct(df2, studyid , .keep_all = TRUE)
nrow(df3)
head(df3$studyid)
#Total number of cluster = 3 (kd)
df4 <- subset(df2, studyid == "28")
P1 <- mean(df4$wqelast)
df5 <- subset(df2, studyid == "32")
P2 <- mean(df5$wqelast) 
df6 <- subset(df2, studyid == "35")
P3 <- mean(df6$wqelast) 
clus_chl_wf <- (P1+P2+P3)/3

################################################################################
# Variance adjusted cluster (VAC) weight mean
################################################################################

df3 <- distinct(df2, studyid , .keep_all = TRUE)
nrow(df3)
head(df3$studyid)

df4 <- subset(df2, studyid == "28")
V1 <- df4$wqelast
V2 <- 1/df4$elast_sim_se #1/vidj
V3 <- sum(V2)
V4 <- V2/V3
V5 <- V4*V1
V6 <- sum(V5)


df5 <- subset(df2, studyid == "32")
V7 <- df5$wqelast
V8 <- 1/df5$elast_sim_se #1/vidj
V9 <- sum(V8)
V10 <- V8/V9
V11 <- V10*V7
V12 <- sum(V11)


df6 <- subset(df2, studyid == "35")
V13 <- df6$wqelast
V14 <- 1/df6$elast_sim_se #1/vidj
V15 <- sum(V14)
V16 <- V14/V15
V17 <- V16*V13
V18 <- sum(V17)
V19 <- (V6+V12+V18)/3


###############################################################################
# RES
###############################################################################

df7 <- filter(df1, wqvar == "Chlorophyll a")%>%
  select(studyid, wqelast, elast_sim, elast_sim_se, distbuf)
df7 <- mutate(df7, w = 1/elast_sim_se)%>%
  mutate(df7, wy = w*elast_sim)%>%
  mutate(df7, wy2 = w*elast_sim^2)%>%
  mutate(df7, w2 = w^2)

#sum of columns
sum_w <- sum(df7$w)
sum_wy <- sum(df7$wy)
sum_wy2 <- sum(df7$wy2)
sum_w2 <- sum(df7$w2)

# fixed effect
m <- sum_wy/sum_w

# calculation of Q
Q <- sum_wy2 - (sum_wy^2/sum_w)

# df
df <- 17

# calculation of c
c <- sum_w- (sum_w2/sum_w)

# calculation of T
T2 <- (Q-df)/c

df8 <- filter(df1, wqvar == "Chlorophyll a")%>%
  select(studyid, wqelast, elast_sim, elast_sim_se)
df8 <- mutate(df8, T2 = T2)%>%
  mutate(df8, v_total = elast_sim_se+T2)

df8_28 <- subset(df8, studyid == 28)
df8_28 <- mutate(df8_28, w = (1/ v_total)/3)
df8_32 <- subset(df8, studyid == 32)
df8_32 <- mutate(df8_32, w = (1/ v_total)/13)
df8_35 <- subset(df8, studyid == 35)
df8_35 <- mutate(df8_35, w = (1/ v_total)/2)

df8 <- rbind(df8_28,df8_32,df8_35)%>%
  mutate(df8, wy = w*wqelast)%>%
  mutate(df8, wy2 = w*wqelast^2)%>%
  mutate(df8, w2 = w^2)

sum_w <- sum(df8$w)
sum_wy <- sum(df8$wy)
sum_wy2 <- sum(df8$wy2)
sum_w2 <- sum(df8$w2)

m <- sum_wy/sum_w





