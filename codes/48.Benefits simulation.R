# simulation of total benefits based on elasticity value form random effect model and
# census / bfp data

# clear memory
rm(list = ls())

# set wd (windows/mac)
#setwd("~/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I/MetaHedonicProject")#(Mac)
#setwd("C:/Users/taj771/OneDrive - University of Saskatchewan/Chapter I/MetaHedonicProject") 


## Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  tidyverse,
  sf,
  SimDesign,
  triangle
)

# load data file with bfp count (best guess, lower bound, upper bound) for 
# triangular distribution, mean dwelling value to estimate uniform distribution
# at census dissemination area level

df <- st_read("./Building_footprint/CAN/bfp_for_mc_can.shp")%>%
  as.data.frame()%>%
  select(-geometry)
  #sample_n(100)


# set seed
set.seed(1287)
# set parameters for mc

N = 10000  # number of sample
repli <- 1000 # replication
elast_250m = 0.216 # water front elasticity - RE model
se_elast_250m = 0.050 # se of waterfront elasticity - RE model
elast_500m = 0.050 # non-water front elasticity - RE model
se_elast_500m = 0.027 # se of non-waterfront elasticity - RE model
wq_change = 1/100

# create data frame with simulated elasticity distribution (normal) and uniform 
# distribution factor to multiply dwelling value

df_sim <- tibble(sim_id = 1:repli, 
                 elast_250m = rnorm(repli, elast_250m,se_elast_250m),
                 elast_500m = rnorm(repli, elast_500m,se_elast_500m),
                 house_val_multi = runif(repli, 0.9, 1.1))%>%
  mutate(across(c(elast_500m), ~ ifelse(.x < 0, 0, .x)))





# simulate building foot print data using a traingual distribution
# two data frames are simulated - one for each distance bins. later those join wtih
# df_sim based on sim_id

#250 meter boundary
Design <- df%>%
  select(GeoUID,lbbfp_250,modbfp_250,ubbfp_250)
Design

#Define generate, analyse, and summaries functions

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rtriangle(n=N, a=lbbfp_250,b=ubbfp_250,c=modbfp_250)) 
  dat
}

# Analyse
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(bf_250_=results) # copy results of each simulation
  ret
}

# Collect results by looping over the rows in design
# run the simulation
Final_bfp_250 <- runSimulation(design=Design, replications=repli,
                               generate=Generate, analyse=Analyse, summarise=Summarise)

simu_bfp_250 <- Final_bfp_250%>%
  select(-lbbfp_250,-modbfp_250,-ubbfp_250,-REPLICATIONS,-SIM_TIME,-RAM_USED,-SEED,
         -COMPLETED)%>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))



#500m meter boundary

Design <- df%>%
  select(GeoUID,lbbfp_500,modbfp_500,ubbfp_500,
         AvgDwlv,prov)
Design


#Define generate, analyse, and summaries functions

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rtriangle(n=N, a=lbbfp_500,b=ubbfp_500,c=modbfp_500))
  dat
}

# Analyse
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# Summarise
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(bf_500_=results) # mean and SD summary of the sample means
  ret
}

#Collect results by looping over the rows in design
# run the simulation
Final_bfp_500 <- runSimulation(design=Design, replications=repli,
                               generate=Generate, analyse=Analyse, summarise=Summarise)

simu_bfp_500 <- Final_bfp_500%>%
  select(-lbbfp_500,-modbfp_500,-ubbfp_500,-REPLICATIONS,-SIM_TIME,-RAM_USED,-SEED,-COMPLETED)%>% #- FATAL_TERMINATION
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))



# set wide table into long format and assign simu id- 250m
simu_bfp_250 <- simu_bfp_250%>%
  pivot_longer(cols=starts_with("bf_250_"),
               names_to='sim_id',
               values_to='bfp_250')%>%
  mutate_if(is.numeric, round)
simu_bfp_250$sim_id= substr(simu_bfp_250$sim_id, 8,12) ## chnage based on repli
simu_bfp_250$sim_id <- as.integer(simu_bfp_250$sim_id)

# set wide table into long format and assign simu id- 500m

simu_bfp_500 <- simu_bfp_500%>%
  pivot_longer(cols=starts_with("bf_500_"),
               names_to='sim_id',
               values_to='bfp_500')%>%
  mutate_if(is.numeric, round)
simu_bfp_500$sim_id= substr(simu_bfp_500$sim_id, 8,12) # change based on repli
simu_bfp_500$sim_id <- as.integer(simu_bfp_500$sim_id)

# create final simulated dataframe using (1) Simulated elasticity (df_sim),
# (2) simulated building foot print 250m (simu_bfp_250), 
# (3) simulated building foot print 500m (simu_bfp_500) - this has dwelling value at each DA also

df_sim_final <- df_sim%>%
  left_join(simu_bfp_250 )%>%
  left_join(simu_bfp_500 )%>%
  relocate(GeoUID, .after = sim_id)%>%
  relocate(AvgDwlv, .after = bfp_500)%>%
  relocate(prov, .after =AvgDwlv)%>%
  mutate(dwelval = house_val_multi*AvgDwlv)%>%
  mutate(d_wq = wq_change )%>%
  mutate(val_wf = elast_250m*bfp_250*dwelval*d_wq)%>%
  mutate(val_nwf = elast_500m*bfp_500*dwelval*d_wq)%>%
  mutate(val_tot = (val_wf+val_nwf)/1000000)
  
# save final simulated file

write_csv(df_sim_final, "./results/simulation/simulation_final_df.csv")

# read csv

df_sim_final <- read_csv("./results/simulation/simulation_final_df.csv")
  

# plot distributtion for alberta

ab <- df_sim_final%>%
  subset(prov == "AB")%>%
  select(GeoUID,val_tot,sim_id,elast_250m,elast_500m,prov,bfp_250,bfp_500)%>%
  group_by(sim_id)%>%
  summarise(val_tot_sum=sum(val_tot))%>%
  mutate(prov = "AB")

bc <- df_sim_final%>%
  subset(prov == "BC")%>%
  select(GeoUID,val_tot,sim_id,elast_250m,elast_500m,prov)%>%
  group_by(sim_id)%>%
  summarise(val_tot_sum=sum(val_tot))%>%
  mutate(prov = "BC")

mb <- df_sim_final%>%
  subset(prov == "MB")%>%
  select(GeoUID,val_tot,sim_id,elast_250m,elast_500m,prov)%>%
  group_by(sim_id)%>%
  summarise(val_tot_sum=sum(val_tot))%>%
  mutate(prov = "MB")


sk <- df_sim_final%>%
  subset(prov == "SK")%>%
  select(GeoUID,val_tot,sim_id,elast_250m,elast_500m,prov)%>%
  group_by(sim_id)%>%
  summarise(val_tot_sum=sum(val_tot))%>%
  mutate(prov = "SK")

nb <- df_sim_final%>%
  subset(prov == "NB")%>%
  select(GeoUID,val_tot,sim_id,elast_250m,elast_500m,prov)%>%
  group_by(sim_id)%>%
  summarise(val_tot_sum=sum(val_tot))%>%
  mutate(prov = "NB")

ns <- df_sim_final%>%
  subset(prov == "NS")%>%
  select(GeoUID,val_tot,sim_id,elast_250m,elast_500m,prov)%>%
  group_by(sim_id)%>%
  summarise(val_tot_sum=sum(val_tot))%>%
  mutate(prov = "NS")

on <- df_sim_final%>%
  subset(prov == "ON")%>%
  select(GeoUID,val_tot,sim_id,elast_250m,elast_500m,prov)%>%
  group_by(sim_id)%>%
  summarise(val_tot_sum=sum(val_tot))%>%
  mutate(prov = "ON")

pe <- df_sim_final%>%
  subset(prov == "PE")%>%
  select(GeoUID,val_tot,sim_id,elast_250m,elast_500m,prov)%>%
  group_by(sim_id)%>%
  summarise(val_tot_sum=sum(val_tot))%>%
  mutate(prov = "PE")

qc <- df_sim_final%>%
  subset(prov == "QC")%>%
  select(GeoUID,val_tot,sim_id,elast_250m,elast_500m,prov)%>%
  group_by(sim_id)%>%
  summarise(val_tot_sum=sum(val_tot))%>%
  mutate(prov = "QC")

nl <- df_sim_final%>%
  subset(prov == "NL")%>%
  select(GeoUID,val_tot,sim_id,elast_250m,elast_500m,prov)%>%
  group_by(sim_id)%>%
  summarise(val_tot_sum=sum(val_tot))%>%
  mutate(prov = "NL")




# Create data - AB
my_variable=c(ab$val_tot_sum)

# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 

par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(my_variable , horizontal=TRUE , ylim=c(12,80), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(my_variable , breaks = 200, col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="Simulated total value in million $ - Alberta", xlim=c(12,80))

# Create data - BC
my_variable=c(bc$val_tot_sum)

# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 

par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(my_variable , horizontal=TRUE , ylim=c(24,160), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))

hist(my_variable , breaks=150, col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="Simulated total value in million $ - British Columbia", xlim=c(24,160))

# Create data - MB
my_variable=c(mb$val_tot_sum)

# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 

par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(my_variable , horizontal=TRUE , ylim=c(1,8), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(my_variable , breaks=200 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="Simulated total value in million $ - Manitoba", xlim=c(1,8))

# Create data - SK
my_variable=c(sk$val_tot_sum)

# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 

par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(my_variable , horizontal=TRUE , ylim=c(0,25), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(my_variable , breaks=100, col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="Simulated total value in million $ - Saskatchewan", xlim=c(0,25))

# Create data - NB
my_variable=c(nb$val_tot_sum)

# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 

par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(my_variable , horizontal=TRUE , ylim=c(0,3), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(my_variable , breaks=100, col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="Simulated total value in million $ - New Brunswick", xlim=c(0,3))


# Create data - NS
my_variable=c(ns$val_tot_sum)

# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 

par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(my_variable , horizontal=TRUE , ylim=c(5,40), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(my_variable , breaks=150, col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="Simulated total value in million $ - Nova Scotia", xlim=c(5,40))

# Create data - ON
my_variable=c(on$val_tot_sum)

# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 

par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(my_variable , horizontal=TRUE , ylim=c(50,400), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(my_variable , breaks=150, col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="Simulated total value in million $ - Ontario", xlim=c(50,400))

# Create data - PE
my_variable=c(pe$val_tot_sum)

# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 

par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(my_variable , horizontal=TRUE , ylim=c(0,1.5), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(my_variable , breaks=150, col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="Simulated total value in million $ - Prince Edwards Island", xlim=c(0,1.5))

# Create data - Qc
my_variable=c(qc$val_tot_sum)

# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 

par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(my_variable , horizontal=TRUE , ylim=c(10,60), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(my_variable , breaks=150, col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="Simulated total value in million $ - Quebec", xlim=c(10,60))

#################################
# Table
#ab

confint(ab$val_tot_sum, level = 0.95)

ab_tot_simu <- mean(ab$val_tot_sum)
CI <-hBayesDM::HDIofMCMC(ab$val_tot_sum)
ab_lb <- CI[1]
ab_ub <- CI[2]
#bc
bc_tot_simu <- mean(bc$val_tot_sum)
CI <-hBayesDM::HDIofMCMC(bc$val_tot_sum)
bc_lb <- CI[1]
bc_ub <- CI[2]
#mb
mb_tot_simu <- mean(mb$val_tot_sum)
CI <-hBayesDM::HDIofMCMC(mb$val_tot_sum)
mb_lb <- CI[1]
mb_ub <- CI[2]
#nb
nb_tot_simu <- mean(nb$val_tot_sum)
CI <-hBayesDM::HDIofMCMC(nb$val_tot_sum)
nb_lb <- CI[1]
nb_ub <- CI[2]
#ns
ns_tot_simu <- mean(ns$val_tot_sum)
CI <-hBayesDM::HDIofMCMC(ns$val_tot_sum)
ns_lb <- CI[1]
ns_ub <- CI[2]
#on
on_tot_simu <- mean(on$val_tot_sum)
CI <-hBayesDM::HDIofMCMC(on$val_tot_sum)
on_lb <- CI[1]
on_ub <- CI[2]
#pe
pe_tot_simu <- mean(pe$val_tot_sum)
CI <-hBayesDM::HDIofMCMC(pe$val_tot_sum)
pe_lb <- CI[1]
pe_ub <- CI[2]
#qc
qc_tot_simu <- mean(qc$val_tot_sum)
CI <-hBayesDM::HDIofMCMC(qc$val_tot_sum)
qc_lb <- CI[1]
qc_ub <- CI[2]
#sk
sk_tot_simu <- mean(sk$val_tot_sum)
CI <-hBayesDM::HDIofMCMC(sk$val_tot_sum)
sk_lb <- CI[1]
sk_ub <- CI[2]

list1 <- list("Province" = c("Saskatchewan", "Alberta", "British Columbia", "Manitoba","New Brunswick",
                             "Nova Scotia", "Ontario", "Quebec", "Prince Edwards"),
              "Total value" = c(sk_tot, ab_tot, bc_tot, mb_tot,
                                                          nb_tot, ns_tot, on_tot,qc_tot, 
                                                          pe_tot),
              "Simulated total value" = c(sk_tot_simu, ab_tot_simu, bc_tot_simu, mb_tot_simu,
                                                          nb_tot_simu, ns_tot_simu, on_tot_simu, qc_tot_simu,
                                                          pe_tot_simu),
              "Simulated lower bound" =c(sk_lb, ab_lb, bc_lb, mb_lb,
                                                 nb_lb, ns_lb, on_lb,qc_lb,
                                                 pe_tot_simu),
              "Simulated upper bound" =c(sk_ub, ab_ub, bc_ub, mb_ub,
                                                 nb_ub, ns_ub, on_ub,qc_ub,
                                                 pe_ub)
              
)

result <- as.data.frame(list1)



library(xtable)
print(xtable(result, type = "latex"), file = "./results/Tables/Benefits_CI_table.tex")




