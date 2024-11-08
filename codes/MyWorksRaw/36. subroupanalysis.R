# clear memories
rm(list = ls())
library(dplyr)
library(meta)
library(metafor)
library(tidyverse)
library(robumeta)
library(clubSandwich)
library(Hmisc)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(ggpubr)
library(SimDesign)
library(ggExtra)

# load data - without outliers 
#wf <- read.csv("./metadata/wo_outliers_wf.csv")
#nwf <- read.csv("./metadata/wo_outliers_nwf.csv")

df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")


df.wf.re <- df[which(df$distbuf == 1), ]%>%
  mutate(region = case_when(west==1~"west",
                            midwest==1~"midwest",
                            south==1~"south",
                            northeast==1~"northeast",
                            canada==1~"canada",
                            multireg==1~"multireg"))%>%
  mutate(vi = log(sampsize))%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()

# full sample

re.wf <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re)
re_wf_robu <- robust(re.wf,cluster = cluster)

# seperate meta analysis - sub groups

res11 <- list(#rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=west==1),
             rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=midwest==1),
             rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=south==1),
             rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=northeast==1),
             rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=multireg==1),
             rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=canada==1))

modelsummary(res11)

#west <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=west==1)
midwest <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=midwest==1)
midwest_robu <- robust(midwest, cluster = cluster)
south <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=south==1)
south_robu <- robust(south, cluster = cluster)
northeast <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=northeast==1)
northeast_robu <- robust(northeast, cluster = cluster)
multireg <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=multireg==1)
multireg_robu <- robust(multireg, cluster = cluster)
canada <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=canada==1)
canada_robu <- robust(canada, cluster = cluster)

#### Step 1 --- Define your conditions under study and parameters

N <- 1000

Design <- createDesign(sample_size = c(10000), 
                       distribution = c("midwest","south",'northeast','multireg','canada','full'))
Design


#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL) {
  N <- condition$sample_size
  dist <- condition$distribution
  if(dist == 'midwest'){
    dat <- rnorm(N, midwest_robu$beta[1], midwest_robu$se)
  } 
  else if(dist == 'south'){
    dat <- rnorm(N, mean = south_robu$beta[1], south_robu$se)
  }
  else if(dist == 'northeast'){
    dat <- rnorm(N, mean = northeast_robu$beta[1], northeast_robu$se)
  }
  else if(dist == 'multireg'){
    dat <- rnorm(N, mean = multireg_robu$beta[1], multireg_robu$se)
  }
  else if(dist == 'canada'){
    dat <- rnorm(N, mean = canada_robu$beta[1], canada_robu$se)
  }
  else if(dist == 'full'){
    dat <- rnorm(N, mean = canada_robu$beta[1], canada_robu$se)
  }
  dat
}

# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(mu=mean(results)) # mean and SD summary of the sample means
  ret
}


#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final <- runSimulation(design=Design, replications=1000,
                       generate=Generate, analyse=Analyse, summarise=Summarise, save_results = TRUE)




df_final <- SimResults(Final)



df1 <- df_final[[1]]$results%>%
  as.data.frame()%>%
  rename("simu_elast" = ".")%>%
  mutate(region = "midwest")

df2 <- df_final[[2]]$results%>%
  as.data.frame()%>%
  rename("simu_elast" = ".")%>%
  mutate(region = "south")


df3 <- df_final[[3]]$results%>%
  as.data.frame()%>%
  rename("simu_elast" = ".")%>%
  mutate(region = "northeast")


df4 <- df_final[[4]]$results%>%
  as.data.frame()%>%
  rename("simu_elast" = ".")%>%
  mutate(region = "multireg")


df5 <- df_final[[5]]$results%>%
  as.data.frame()%>%
  rename("simu_elast" = ".")%>%
  mutate(region = "canada")

df6 <- df_final[[6]]$results%>%
  as.data.frame()%>%
  rename("simu_elast" = ".")%>%
  mutate(region = "full")

df <- rbind(df1,df2,df3,df4,df5,df6)


# all in one
p <- df%>%
  ggplot( aes(x=region, y=simu_elast, fill=region)) +
  geom_violin(width=2) +
  geom_boxplot(width=0.25, color="grey", alpha=0.9) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  scale_y_continuous(limits = c(0.05, 0.4))+
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ Total value CSD")

p

################################################################################

# Non waterfront

df.wf.re <- df[which(df$distbuf == 2), ]%>%
  mutate(region = case_when(west==1~"west",
                            midwest==1~"midwest",
                            south==1~"south",
                            northeast==1~"northeast",
                            canada==1~"canada",
                            multireg==1~"multireg"))%>%
  mutate(vi = log(sampsize))%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()

# full sample

re.wf <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re)
re_wf_robu <- robust(re.wf,cluster = cluster)

# seperate meta analysis - sub groups

res11 <- list(#rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=west==1),
  rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=midwest==1),
  rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=south==1),
  rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=northeast==1),
  rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=multireg==1),
  rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=canada==1))

modelsummary(res11)

#west <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=west==1)
midwest <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=midwest==1)
midwest_robu <- robust(midwest, cluster = cluster)
south <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=south==1)
south_robu <- robust(south, cluster = cluster)
northeast <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=northeast==1)
northeast_robu <- robust(northeast, cluster = obsid)
multireg <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=multireg==1)
multireg_robu <- robust(multireg, cluster = obsid)
canada <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=canada==1)
canada_robu <- robust(canada, cluster = cluster)

#### Step 1 --- Define your conditions under study and parameters

N <- 1000

Design <- createDesign(sample_size = c(10000), 
                       distribution = c("midwest","south",'northeast','multireg','canada','full'))
Design


#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL) {
  N <- condition$sample_size
  dist <- condition$distribution
  if(dist == 'midwest'){
    dat <- rnorm(N, midwest_robu$beta[1], midwest_robu$se)
  } 
  else if(dist == 'south'){
    dat <- rnorm(N, mean = south_robu$beta[1], south_robu$se)
  }
  else if(dist == 'northeast'){
    dat <- rnorm(N, mean = northeast_robu$beta[1], northeast_robu$se)
  }
  else if(dist == 'multireg'){
    dat <- rnorm(N, mean = multireg_robu$beta[1], multireg_robu$se)
  }
  else if(dist == 'canada'){
    dat <- rnorm(N, mean = canada_robu$beta[1], canada_robu$se)
  }
  else if(dist == 'full'){
    dat <- rnorm(N, mean = canada_robu$beta[1], canada_robu$se)
  }
  dat
}

# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(mu=mean(results)) # mean and SD summary of the sample means
  ret
}


#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final <- runSimulation(design=Design, replications=1000,
                       generate=Generate, analyse=Analyse, summarise=Summarise, save_results = TRUE)




df_final <- SimResults(Final)



df1 <- df_final[[1]]$results%>%
  as.data.frame()%>%
  rename("simu_elast" = ".")%>%
  mutate(region = "midwest")

df2 <- df_final[[2]]$results%>%
  as.data.frame()%>%
  rename("simu_elast" = ".")%>%
  mutate(region = "south")


df3 <- df_final[[3]]$results%>%
  as.data.frame()%>%
  rename("simu_elast" = ".")%>%
  mutate(region = "northeast")


df4 <- df_final[[4]]$results%>%
  as.data.frame()%>%
  rename("simu_elast" = ".")%>%
  mutate(region = "multireg")


df5 <- df_final[[5]]$results%>%
  as.data.frame()%>%
  rename("simu_elast" = ".")%>%
  mutate(region = "canada")

df6 <- df_final[[6]]$results%>%
  as.data.frame()%>%
  rename("simu_elast" = ".")%>%
  mutate(region = "full")

df <- rbind(df1,df2,df3,df4,df5,df6)


# all in one
p <- df%>%
  ggplot( aes(x=region, y=simu_elast, fill=region)) +
  geom_violin(width=2) +
  geom_boxplot(width=0.25, color="grey", alpha=0.9) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  scale_y_continuous(limits = c(0.05, 0.4))+
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ Total value CSD")

p

