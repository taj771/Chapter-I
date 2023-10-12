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
  kableExtra
)

# Simulation elasticity measures
# option 1

# Simulate rma objects for benefit transfer

# load data - with outliers 
df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")

# Random effect models
df.wf.re <- df[which(df$distbuf == 1), ]%>%
  mutate(vi = log(sampsize))%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
df.nwf.re <- df[which(df$distbuf == 2), ]%>%
  mutate(vi = log(sampsize))%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()


#waterfront
re.wf <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re)

simu_wf <- simulate.rma(re.wf, nsim = 100, seed = 1234)

simu_wf <- data.frame(simu_wf=unlist(simu_wf))

dat.hdi_simu_e_250m <-hBayesDM::HDIofMCMC(simu_wf$simu_wf)

lb_simu_e_250m <- dat.hdi_simu_e_250m[1]
ub_simu_e_250m <- dat.hdi_simu_e_250m[2]

mean_data <- mean(simu_wf$simu_wf)

# Plot
# 250m
ggplot(simu_wf, aes(x=simu_wf)) +
  geom_histogram(binwidth=0.0005)

p <- ggplot(simu_wf, aes(x=simu_wf)) +
  geom_histogram(binwidth=0.0005)+
  geom_segment(x=dat.hdi_simu_e_250m[1],xend=dat.hdi_simu_e_250m[2],y=0,yend=0,color="blue",size=2,lineend="round")+
  geom_vline(xintercept = mean_data,
             color = "red", linetype = "dashed")+
  geom_vline(xintercept = dat.hdi_simu_e_250m,
             color = "red", linetype = "dashed")+
  theme_bw()


p + scale_x_continuous(breaks=seq(-15, 15, 1))

# nonwaterfront
re.nwf <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.nwf.re)

simu_nwf <- simulate.rma(re.nwf, nsim = 10000, seed = 1234)

simu_nwf <- data.frame(simu_nwf=unlist(simu_nwf))

dat.hdi_simu_e_500m <-hBayesDM::HDIofMCMC(simu_nwf$simu_nwf)

lb_simu_e_500m <- dat.hdi_simu_e_500m[1]
ub_simu_e_500m <- dat.hdi_simu_e_500m[2]

mean_data_500 <- mean(simu_nwf$simu_nwf)

# Plot
# 250m
ggplot(simu_nwf, aes(x=simu_nwf)) +
  geom_histogram(binwidth=0.0005)

p <- ggplot(simu_nwf, aes(x=simu_nwf)) +
  geom_histogram(binwidth=0.0005)+
  geom_segment(x=dat.hdi_simu_e_500m[1],xend=dat.hdi_simu_e_500m[2],y=0,yend=0,color="blue",size=2,lineend="round")+
  geom_vline(xintercept = mean_data_500,
             color = "red", linetype = "dashed")+
  geom_vline(xintercept = dat.hdi_simu_e_500m,
             color = "red", linetype = "dashed")+
  theme_bw()


p + scale_x_continuous(breaks=seq(-15, 15, 1))


##############################################################################
# option 2
# simulation mean elasticity with sd 
# Clean memory 
rm(list = ls())
# Load library 

library(SimDesign)
library(dplyr)
library(sf)
#library(Rmisc)

set.seed(1234)

# option 1 : simulating elasticity based on the distribution can caluclate the
# value based on 100000

#### Step 1 --- Define your conditions under study and parameters

N <- 1000
elast_250m <-  0.216
se_elast_250m <- 0.137
elast_500m <- 0.062
se_elast_500m <- 0.208 

#~~~~~~~~~~~~~~~~~~~~~~~~

Design <- createDesign(sample_size = c(10000), 
                       distribution = c("elast_250m","elast_500m"))
Design

#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL) {
  N <- condition$sample_size
  dist <- condition$distribution
  if(dist == 'elast_250m'){
    dat <- rnorm(N, elast_250m, se_elast_250m)
  } 
  else if(dist == 'elast_500m'){
    dat <- rnorm(N, mean = elast_500m, se_elast_500m)
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
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final <- runSimulation(design=Design, replications=100000,
                          generate=Generate, analyse=Analyse, summarise=Summarise, save_results = TRUE)




df_final <- SimResults(Final)
  

df1 <- df_final[[1]]$results%>%
  as.data.frame()%>%
  rename("simu_elast_250m" = ".")

df2 <- df_final[[2]]$results%>%
  as.data.frame()%>%
  rename("simu_elast_500m" = ".")



df_final <- cbind(df1,df2)


dat.hdi_simu_e_250m <-hBayesDM::HDIofMCMC(df_final$simu_elast_250m)
dat.hdi_simu_e_500m <-hBayesDM::HDIofMCMC(df_final$simu_elast_500m)


lb_simu_e_250m <- dat.hdi_simu_e_250m[1]
ub_simu_e_250m <- dat.hdi_simu_e_250m[2]

lb_simu_e_500m <- dat.hdi_simu_e_500m[1]
ub_simu_e_500m <- dat.hdi_simu_e_500m[2]

mean_data <- mean(df_final$simu_elast_250m)

mean_data_500 <- mean(df_final$simu_elast_500m)



# Plot
# 250m
ggplot(df_final, aes(x=simu_elast_250m)) +
  geom_histogram(binwidth=0.000005)

ggplot(df_final, aes(x=simu_elast_250m)) +
  geom_histogram(binwidth=0.000005)+
  geom_segment(x=dat.hdi_simu_e_250m[1],xend=dat.hdi_simu_e_250m[2],y=0,yend=0,color="blue",size=2,lineend="round")+
  geom_vline(xintercept = mean_data,
             color = "red", linetype = "dashed")+
  geom_vline(xintercept = dat.hdi_simu_e_250m,
             color = "red", linetype = "dashed")+
  theme_bw()

#500m

ggplot(df_final, aes(x=simu_elast_500m)) +
  geom_histogram(binwidth=0.000005)

ggplot(df_final, aes(x=simu_elast_500m)) +
  geom_histogram(binwidth=0.000005)+
  geom_segment(x=dat.hdi_simu_e_500m[1],xend=dat.hdi_simu_e_500m[2],y=0,yend=0,color="blue",size=2,lineend="round")+
  geom_vline(xintercept = mean_data_500,
             color = "red", linetype = "dashed")+
  geom_vline(xintercept = dat.hdi_simu_e_500m,
             color = "red", linetype = "dashed")+
  theme_bw()





##########################################
# set data for violin plot
simu_value_250 <- st_read("./results/value_map/SK/bfp_count_cen_da_250m_value.shp")%>%
  mutate(valpp250 = vl_250m/Popultn)%>%
  mutate(simuelb = lb_simu_e_250m/100)%>%
  mutate(simueub = ub_simu_e_250m/100)%>%
  mutate(simuvallb250 = simuelb*bf__250*v_Avod_)%>%
  mutate(simuvalub250 = simueub*bf__250*v_Avod_)%>%
  mutate(simuvallbpp250 = simuvallb250/Popultn)%>%
  mutate(simuvalubpp250 = simuvalub250/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(GeoUID,simuvallb250,simuvalub250,simuvallbpp250,simuvalubpp250,valpp250)
  

simu_value_500 <- st_read("./results/value_map/SK/bfp_count_cen_da_250m_500m_value.shp")%>%
  mutate(valpp500 = vl_500m/Popultn)%>%
  mutate(simuelb = lb_simu_e_500m/100)%>%
  mutate(simueub = ub_simu_e_500m/100)%>%
  mutate(simuvallb500 = simuelb*bf__500*v_Avod_)%>%
  mutate(simuvalub500 = simueub*bf__500*v_Avod_)%>%
  mutate(simuvallbpp500 = simuvallb500/Popultn)%>%
  mutate(simuvalubpp500 = simuvalub500/Popultn)%>%
  left_join(simu_value_250)

st_write(simu_value_500, "./results/value_map/SK/simu_value_map.shp",delete_layer = T)


simu_value_250 <- st_read("./results/value_map/AB/bfp_count_cen_da_250m_value.shp")%>%
  mutate(valpp250 = vl_250m/Popultn)%>%
  mutate(simuelb = lb_simu_e_250m/100)%>%
  mutate(simueub = ub_simu_e_250m/100)%>%
  mutate(simuvallb250 = simuelb*bf__250*v_Avod_)%>%
  mutate(simuvalub250 = simueub*bf__250*v_Avod_)%>%
  mutate(simuvallbpp250 = simuvallb250/Popultn)%>%
  mutate(simuvalubpp250 = simuvalub250/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(GeoUID,simuvallb250,simuvalub250,simuvallbpp250,simuvalubpp250,valpp250)


simu_value_500 <- st_read("./results/value_map/AB/bfp_count_cen_da_250m_500m_value.shp")%>%
  mutate(valpp500 = vl_500m/Popultn)%>%
  mutate(simuelb = lb_simu_e_500m/100)%>%
  mutate(simueub = ub_simu_e_500m/100)%>%
  mutate(simuvallb500 = simuelb*bf__500*v_Avod_)%>%
  mutate(simuvalub500 = simueub*bf__500*v_Avod_)%>%
  mutate(simuvallbpp500 = simuvallb500/Popultn)%>%
  mutate(simuvalubpp500 = simuvalub500/Popultn)%>%
  left_join(simu_value_250)

st_write(simu_value_500, "./results/value_map/AB/simu_value_map.shp",delete_layer = T)


simu_value_250 <- st_read("./results/value_map/BC/bfp_count_cen_da_250m_value.shp")%>%
  mutate(valpp250 = vl_250m/Popultn)%>%
  mutate(simuelb = lb_simu_e_250m/100)%>%
  mutate(simueub = ub_simu_e_250m/100)%>%
  mutate(simuvallb250 = simuelb*bf__250*v_Avod_)%>%
  mutate(simuvalub250 = simueub*bf__250*v_Avod_)%>%
  mutate(simuvallbpp250 = simuvallb250/Popultn)%>%
  mutate(simuvalubpp250 = simuvalub250/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(GeoUID,simuvallb250,simuvalub250,simuvallbpp250,simuvalubpp250,valpp250)


simu_value_500 <- st_read("./results/value_map/BC/bfp_count_cen_da_250m_500m_value.shp")%>%
  mutate(valpp500 = vl_500m/Popultn)%>%
  mutate(simuelb = lb_simu_e_500m/100)%>%
  mutate(simueub = ub_simu_e_500m/100)%>%
  mutate(simuvallb500 = simuelb*bf__500*v_Avod_)%>%
  mutate(simuvalub500 = simueub*bf__500*v_Avod_)%>%
  mutate(simuvallbpp500 = simuvallb500/Popultn)%>%
  mutate(simuvalubpp500 = simuvalub500/Popultn)%>%
  left_join(simu_value_250)

st_write(simu_value_500, "./results/value_map/BC/simu_value_map.shp",delete_layer = T)

simu_value_250 <- st_read("./results/value_map/MB/bfp_count_cen_da_250m_value.shp")%>%
  mutate(valpp250 = vl_250m/Popultn)%>%
  mutate(simuelb = lb_simu_e_250m/100)%>%
  mutate(simueub = ub_simu_e_250m/100)%>%
  mutate(simuvallb250 = simuelb*bf__250*v_Avod_)%>%
  mutate(simuvalub250 = simueub*bf__250*v_Avod_)%>%
  mutate(simuvallbpp250 = simuvallb250/Popultn)%>%
  mutate(simuvalubpp250 = simuvalub250/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(GeoUID,simuvallb250,simuvalub250,simuvallbpp250,simuvalubpp250,valpp250)


simu_value_500 <- st_read("./results/value_map/MB/bfp_count_cen_da_250m_500m_value.shp")%>%
  mutate(valpp500 = vl_500m/Popultn)%>%
  mutate(simuelb = lb_simu_e_500m/100)%>%
  mutate(simueub = ub_simu_e_500m/100)%>%
  mutate(simuvallb500 = simuelb*bf__500*v_Avod_)%>%
  mutate(simuvalub500 = simueub*bf__500*v_Avod_)%>%
  mutate(simuvallbpp500 = simuvallb500/Popultn)%>%
  mutate(simuvalubpp500 = simuvalub500/Popultn)%>%
  left_join(simu_value_250)

st_write(simu_value_500, "./results/value_map/MB/simu_value_map.shp",delete_layer = T)

simu_value_250 <- st_read("./results/value_map/NB/bfp_count_cen_da_250m_value.shp")%>%
  mutate(valpp250 = vl_250m/Popultn)%>%
  mutate(simuelb = lb_simu_e_250m/100)%>%
  mutate(simueub = ub_simu_e_250m/100)%>%
  mutate(simuvallb250 = simuelb*bf__250*v_Avod_)%>%
  mutate(simuvalub250 = simueub*bf__250*v_Avod_)%>%
  mutate(simuvallbpp250 = simuvallb250/Popultn)%>%
  mutate(simuvalubpp250 = simuvalub250/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(GeoUID,simuvallb250,simuvalub250,simuvallbpp250,simuvalubpp250,valpp250)


simu_value_500 <- st_read("./results/value_map/NB/bfp_count_cen_da_250m_500m_value.shp")%>%
  mutate(valpp500 = vl_500m/Popultn)%>%
  mutate(simuelb = lb_simu_e_500m/100)%>%
  mutate(simueub = ub_simu_e_500m/100)%>%
  mutate(simuvallb500 = simuelb*bf__500*v_Avod_)%>%
  mutate(simuvalub500 = simueub*bf__500*v_Avod_)%>%
  mutate(simuvallbpp500 = simuvallb500/Popultn)%>%
  mutate(simuvalubpp500 = simuvalub500/Popultn)%>%
  left_join(simu_value_250)

st_write(simu_value_500, "./results/value_map/NB/simu_value_map.shp",delete_layer = T)

simu_value_250 <- st_read("./results/value_map/NS/bfp_count_cen_da_250m_value.shp")%>%
  mutate(valpp250 = vl_250m/Popultn)%>%
  mutate(simuelb = lb_simu_e_250m/100)%>%
  mutate(simueub = ub_simu_e_250m/100)%>%
  mutate(simuvallb250 = simuelb*bf__250*v_Avod_)%>%
  mutate(simuvalub250 = simueub*bf__250*v_Avod_)%>%
  mutate(simuvallbpp250 = simuvallb250/Popultn)%>%
  mutate(simuvalubpp250 = simuvalub250/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(GeoUID,simuvallb250,simuvalub250,simuvallbpp250,simuvalubpp250,valpp250)


simu_value_500 <- st_read("./results/value_map/NS/bfp_count_cen_da_250m_500m_value.shp")%>%
  mutate(valpp500 = vl_500m/Popultn)%>%
  mutate(simuelb = lb_simu_e_500m/100)%>%
  mutate(simueub = ub_simu_e_500m/100)%>%
  mutate(simuvallb500 = simuelb*bf__500*v_Avod_)%>%
  mutate(simuvalub500 = simueub*bf__500*v_Avod_)%>%
  mutate(simuvallbpp500 = simuvallb500/Popultn)%>%
  mutate(simuvalubpp500 = simuvalub500/Popultn)%>%
  left_join(simu_value_250)

st_write(simu_value_500, "./results/value_map/NS/simu_value_map.shp",delete_layer = T)


simu_value_250 <- st_read("./results/value_map/ON/bfp_count_cen_da_250m_value.shp")%>%
  mutate(valpp250 = vl_250m/Popultn)%>%
  mutate(simuelb = lb_simu_e_250m/100)%>%
  mutate(simueub = ub_simu_e_250m/100)%>%
  mutate(simuvallb250 = simuelb*bf__250*v_Avod_)%>%
  mutate(simuvalub250 = simueub*bf__250*v_Avod_)%>%
  mutate(simuvallbpp250 = simuvallb250/Popultn)%>%
  mutate(simuvalubpp250 = simuvalub250/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(GeoUID,simuvallb250,simuvalub250,simuvallbpp250,simuvalubpp250,valpp250)


simu_value_500 <- st_read("./results/value_map/ON/bfp_count_cen_da_250m_500m_value.shp")%>%
  mutate(valpp500 = vl_500m/Popultn)%>%
  mutate(simuelb = lb_simu_e_500m/100)%>%
  mutate(simueub = ub_simu_e_500m/100)%>%
  mutate(simuvallb500 = simuelb*bf__500*v_Avod_)%>%
  mutate(simuvalub500 = simueub*bf__500*v_Avod_)%>%
  mutate(simuvallbpp500 = simuvallb500/Popultn)%>%
  mutate(simuvalubpp500 = simuvalub500/Popultn)%>%
  left_join(simu_value_250)

st_write(simu_value_500, "./results/value_map/ON/simu_value_map.shp",delete_layer = T)


simu_value_250 <- st_read("./results/value_map/QC/bfp_count_cen_da_250m_value.shp")%>%
  mutate(valpp250 = vl_250m/Popultn)%>%
  mutate(simuelb = lb_simu_e_250m/100)%>%
  mutate(simueub = ub_simu_e_250m/100)%>%
  mutate(simuvallb250 = simuelb*bf__250*v_Avod_)%>%
  mutate(simuvalub250 = simueub*bf__250*v_Avod_)%>%
  mutate(simuvallbpp250 = simuvallb250/Popultn)%>%
  mutate(simuvalubpp250 = simuvalub250/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  select(GeoUID,simuvallb250,simuvalub250,simuvallbpp250,simuvalubpp250,valpp250)


simu_value_500 <- st_read("./results/value_map/QC/bfp_count_cen_da_250m_500m_value.shp")%>%
  mutate(valpp500 = vl_500m/Popultn)%>%
  mutate(simuelb = lb_simu_e_500m/100)%>%
  mutate(simueub = ub_simu_e_500m/100)%>%
  mutate(simuvallb500 = simuelb*bf__500*v_Avod_)%>%
  mutate(simuvalub500 = simueub*bf__500*v_Avod_)%>%
  mutate(simuvallbpp500 = simuvallb500/Popultn)%>%
  mutate(simuvalubpp500 = simuvalub500/Popultn)%>%
  left_join(simu_value_250)

st_write(simu_value_500, "./results/value_map/QC/simu_value_map.shp",delete_layer = T)


  
  


