# This code set up the monte carlo simulation and amenity value estimations, which summerize in Tbale 4

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

N = 1000  # number of sample
repli <- 1000 # replication
elast_250m = 0.16 # water front elasticity - RE model
se_elast_250m = 0.038 # se of waterfront elasticity - RE model
elast_750m = 0.082 # non-water front elasticity - RE model
se_elast_750m = 0.033 # se of non-waterfront elasticity - RE model
wq_change = 10/100

# create data frame with simulated elasticity distribution (normal) and uniform 
# distribution factor to multiply dwelling value

df_sim <- tibble(sim_id = 1:repli, 
                 elast_250m = rnorm(repli, elast_250m,se_elast_250m),
                 elast_750m = rnorm(repli, elast_750m,se_elast_750m),
                 house_val_multi = runif(repli, 0.9, 1.1))%>%
  mutate(across(c(elast_750m), ~ ifelse(.x < 0, 0, .x)))


#save file
write_csv(df_sim, "./results/simulation/df_sim.csv")



# simulate building foot print data using a traingual distribution
# two data frames are simulated - one for each distance bins. later those join wtih
# df_sim based on sim_id

#500 meter boundary
Design <- df%>%
  select(GeoUID,lbb_500,mdb_500,ubb_500,prov)
Design

#Define generate, analyse, and summaries functions

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rtriangle(n=N, a=lbb_500,b=ubb_500,c=mdb_500)) 
  dat
}

# Analyse
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(bf_500_=results) # copy results of each simulation
  ret
}

# Collect results by looping over the rows in design
# run the simulation
Final_bfp_500 <- runSimulation(design=Design, replications=repli,
                               generate=Generate, analyse=Analyse, summarise=Summarise,
                               parallel = TRUE, ncores = 8)

simu_bfp_500 <- Final_bfp_500%>%
  select(-lbb_500,-mdb_500,-ubb_500,-REPLICATIONS,-SIM_TIME,-SEED,
         -COMPLETED)%>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))



#1000m meter boundary

Design <- df%>%
  select(GeoUID,lb_1000,md_1000,ub_1000,
         AvgDwlv,prov)
Design


#Define generate, analyse, and summaries functions

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rtriangle(n=N, a=lb_1000,b=ub_1000,c=md_1000))
  dat
}

# Analyse
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# Summarise
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(bf_1000_=results) # mean and SD summary of the sample means
  ret
}

#Collect results by looping over the rows in design
# run the simulation
Final_bfp_1000 <- runSimulation(design=Design, replications=repli,
                                generate=Generate, analyse=Analyse, summarise=Summarise,
                                parallel = TRUE, ncores = 8)

simu_bfp_1000 <- Final_bfp_1000%>%
  select(-lb_1000,-md_1000,-ub_1000,-REPLICATIONS,-SIM_TIME,-SEED,-COMPLETED)%>% #- FATAL_TERMINATION
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))



# set wide table into long format and assign simu id- 500m
simu_bfp_500 <- simu_bfp_500%>%
  pivot_longer(cols=starts_with("bf_500_"),
               names_to='sim_id',
               values_to='bfp_500')%>%
  mutate_if(is.numeric, round)
simu_bfp_500$sim_id= substr(simu_bfp_500$sim_id, 8,12) ## chnage based on repli
simu_bfp_500$sim_id <- as.integer(simu_bfp_500$sim_id)

#save file
write_csv(simu_bfp_500, "./results/simulation/simulation_final_500_df.csv")


# set wide table into long format and assign simu id- 1000m

simu_bfp_1000 <- simu_bfp_1000%>%
  pivot_longer(cols=starts_with("bf_1000_"),
               names_to='sim_id',
               values_to='bfp_1000')%>%
  mutate_if(is.numeric, round)
simu_bfp_1000$sim_id= substr(simu_bfp_1000$sim_id, 9,12) # change based on repli
simu_bfp_1000$sim_id <- as.integer(simu_bfp_1000$sim_id)


#save file
write_csv(simu_bfp_1000, "./results/simulation/simulation_final_1000_df.csv")

################################################################################
################################################################################
# create final simulated dataframe using (1) Simulated elasticity (df_sim),
# (2) simulated building foot print 500m (simu_bfp_500), 
# (3) simulated building foot print 1000m (simu_bfp_1000) - this has dwelling value at each DA also


# read file - avoid timing for simulation as already saved simulated files


df_sim <- read.csv("./results/simulation/df_sim.csv")
simu_bfp_500 <- read.csv("./results/simulation/simulation_final_500_df.csv")%>%
  select(-RAM_USED)
simu_bfp_1000 <- read.csv("./results/simulation/simulation_final_1000_df.csv")%>%
  select(-RAM_USED)


df_sim_final <- df_sim%>%
  left_join(simu_bfp_500 )%>%
  left_join(simu_bfp_1000 )%>%
  relocate(GeoUID, .after = sim_id)%>%
  relocate(AvgDwlv, .after = bfp_1000)%>%
  relocate(prov, .after =AvgDwlv)%>%
  mutate(dwelval = house_val_multi*AvgDwlv)%>%
  mutate(d_wq = wq_change )%>%
  mutate(val_wf = elast_250m*bfp_500*dwelval*d_wq)%>%
  mutate(val_nwf = elast_750m*bfp_1000*dwelval*d_wq)%>%
  mutate(val_tot = val_wf+val_nwf)

# save final simulated file

write_csv(df_sim_final, "./results/simulation/simulation_final_df.csv")

#########################################################################################################
# read csv

df_sim_final <- read_csv("./results/simulation/simulation_final_df.csv")


# Table

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  as.data.frame()%>%
  select(-geometry)%>%
  select(GeoUID,Popultn)

df_cen$GeoUID <- as.numeric(df_cen$GeoUID)


df_all <- df_sim_final%>%
  left_join(df_cen)%>%
  #group_by(prov)%>%
  #subset(prov == "QC")%>%
  select(GeoUID,val_wf,val_nwf,val_tot,sim_id,elast_250m,elast_750m,prov,bfp_500,bfp_1000,Popultn)%>%
  mutate(bfp_all = bfp_500+bfp_1000)%>%
  #drop_na()%>%
  
  group_by(GeoUID,prov)%>%
  mutate(ave_tot_wf = mean(val_wf))%>%
  mutate(lb_ave_tot_wf = quantile(val_wf, 0.25, na.rm = T))%>%
  mutate(ub_ave_tot_wf = quantile(val_wf, 0.75, na.rm = T))%>%
  
  mutate(ave_tot_nwf = mean(val_nwf))%>%
  mutate(lb_ave_tot_nwf = quantile(val_nwf, 0.25, na.rm = T))%>%
  mutate(ub_ave_tot_nwf = quantile(val_nwf, 0.75, na.rm = T))%>%
  
  mutate(ave_tot = mean(val_tot))%>%
  mutate(lb_ave_tot = quantile(val_tot, 0.25,na.rm = T))%>%
  mutate(ub_ave_tot = quantile(val_tot, 0.75,na.rm = T))%>%
  
  mutate(ave_bfp_500 = mean(bfp_500))%>%
  mutate(lb_ave_bfp_wf = quantile(bfp_500, 0.25,na.rm = T))%>%
  mutate(ub_ave_bfp_wf = quantile(bfp_500, 0.75,na.rm = T))%>%
  
  mutate(ave_bfp_1000 = mean(bfp_1000))%>%
  mutate(lb_ave_bfp_nwf = quantile(bfp_1000, 0.25,na.rm = T))%>%
  mutate(ub_ave_bfp_nwf = quantile(bfp_1000, 0.75,na.rm = T))%>%
  
  mutate(ave_bfp_tot = mean(bfp_all))%>%
  mutate(lb_ave_bfp_tot = quantile(bfp_all, 0.25,na.rm = T))%>%
  mutate(ub_ave_bfp_tot = quantile(bfp_all, 0.75,na.rm = T))%>%
  
  
  
  distinct(GeoUID, .keep_all = T)%>%
  ungroup()%>%
  group_by(prov)%>%
  mutate(ave_tot_wf  =  sum(ave_tot_wf))%>%
  mutate(lb_ave_tot_wf = sum(lb_ave_tot_wf))%>%
  mutate(ub_ave_tot_wf = sum(ub_ave_tot_wf))%>%
  
  mutate(ave_tot_nwf  =  sum(ave_tot_nwf))%>%
  mutate(lb_ave_tot_nwf = sum(lb_ave_tot_nwf))%>%
  mutate(ub_ave_tot_nwf = sum(ub_ave_tot_nwf))%>%
  
  mutate(ave_tot  =  sum(ave_tot))%>%
  mutate(lb_ave_tot = sum(lb_ave_tot))%>%
  mutate(ub_ave_tot = sum(ub_ave_tot))%>%
  
  mutate(ave_bfp_wf  =  sum(ave_bfp_500))%>%
  mutate(lb_ave_bfp_wf = sum(lb_ave_bfp_wf))%>%
  mutate(ub_ave_bfp_wf = sum(ub_ave_bfp_wf))%>%
  
  mutate(ave_bfp_nwf  =  sum(ave_bfp_1000))%>%
  mutate(lb_ave_bfp_nwf = sum(lb_ave_bfp_nwf))%>%
  mutate(ub_ave_bfp_nwf = sum(ub_ave_bfp_nwf))%>%
  
  mutate(ave_bfp_tot  =  sum(ave_bfp_tot))%>%
  mutate(lb_ave_bfp_tot = sum(lb_ave_bfp_tot))%>%
  mutate(ub_ave_bfp_tot = sum(ub_ave_bfp_tot))%>%
  
  mutate(popultn_tot = sum(Popultn))%>%
  
  mutate(val_pp = ave_tot/popultn_tot)%>%
  mutate(lb_val_pp = lb_ave_tot/popultn_tot)%>%
  mutate(ub_val_pp = ub_ave_tot/popultn_tot)%>%
  
  #mutate(prov = "AB")%>%
  dplyr::select(prov,
                ave_tot_wf,lb_ave_tot_wf,ub_ave_tot_wf,
                ave_tot_nwf,lb_ave_tot_nwf,ub_ave_tot_nwf,
                ave_tot,lb_ave_tot,ub_ave_tot,
                ave_bfp_wf,lb_ave_bfp_wf,ub_ave_bfp_wf,
                ave_bfp_nwf,lb_ave_bfp_nwf,ub_ave_bfp_nwf,
                ave_bfp_tot,lb_ave_bfp_tot,ub_ave_bfp_tot,
                val_pp, lb_val_pp, ub_val_pp
                
  )%>%
  distinct(prov,
           ave_tot_wf,lb_ave_tot_wf,ub_ave_tot_wf,
           ave_tot_nwf,lb_ave_tot_nwf,ub_ave_tot_nwf,
           ave_tot,lb_ave_tot,ub_ave_tot,
           ave_bfp_wf,lb_ave_bfp_wf,ub_ave_bfp_wf,
           ave_bfp_nwf,lb_ave_bfp_nwf,ub_ave_bfp_nwf,
           ave_bfp_tot,lb_ave_bfp_tot,ub_ave_bfp_tot,
           val_pp, lb_val_pp, ub_val_pp)



df <- df_all%>%
  dplyr::select(prov,
                ave_bfp_tot,lb_ave_bfp_tot,ub_ave_bfp_tot,
                ave_tot_wf, lb_ave_tot_wf,ub_ave_tot_wf,
                ave_tot_nwf, lb_ave_tot_nwf,ub_ave_tot_nwf,
                ave_tot,lb_ave_tot,ub_ave_tot,
                val_pp, lb_val_pp, ub_val_pp)%>%
  #mutate(val_pp = ave_tot/ave_bfp_tot)%>%
  #mutate(lb_val_pp = lb_ave_tot/lb_ave_bfp_tot)%>%
  #mutate(ub_val_pp = ub_ave_tot/ub_ave_bfp_tot)%>%
  mutate(ave_bfp_tot=ave_bfp_tot/1000)%>%
  mutate(lb_ave_bfp_tot=lb_ave_bfp_tot/1000)%>%
  mutate(ub_ave_bfp_tot=ub_ave_bfp_tot/1000)%>%
  mutate(ave_tot_wf=ave_tot_wf/1000000)%>%
  mutate(lb_ave_tot_wf=lb_ave_tot_wf/1000000)%>%
  mutate(ub_ave_tot_wf=ub_ave_tot_wf/1000000)%>%
  mutate(ave_tot_nwf=ave_tot_nwf/1000000)%>%
  mutate(lb_ave_tot_nwf=lb_ave_tot_nwf/1000000)%>%
  mutate(ub_ave_tot_nwf=ub_ave_tot_nwf/1000000)%>%
  mutate(ave_tot=ave_tot/1000000)%>%
  mutate(lb_ave_tot=lb_ave_tot/1000000)%>%
  mutate(ub_ave_tot=ub_ave_tot/1000000)%>%
  mutate(across(c("ave_bfp_tot","lb_ave_bfp_tot","ub_ave_bfp_tot",
                  "val_pp","lb_val_pp","ub_val_pp"), round, 0))%>%
  mutate(across(c("ave_tot_wf","lb_ave_tot_wf","ub_ave_tot_wf",
                  "ave_tot_nwf","lb_ave_tot_nwf","ub_ave_tot_nwf",
                  "ave_tot","lb_ave_tot","ub_ave_tot"), round, 0))



df1 <- df%>%
  dplyr::select(prov,ave_bfp_tot,ave_tot_wf,ave_tot_nwf,ave_tot,val_pp)%>%
  mutate_all(as.character)


df2 <- df%>%
  dplyr::select(prov,
                lb_ave_bfp_tot,ub_ave_bfp_tot,
                lb_ave_tot_wf,ub_ave_tot_wf,
                lb_ave_tot_nwf,ub_ave_tot_nwf,
                lb_ave_tot,ub_ave_tot,
                lb_val_pp,ub_val_pp)%>%
  modify_if(~is.numeric(.), ~round(.,0 ))%>%
  
  unite(col = "ave_conf_wf", c("lb_ave_tot_wf","ub_ave_tot_wf"), sep = ":")%>%
  mutate(ave_conf_wf = paste0("(", ave_conf_wf, ")"))%>%
  
  unite(col = "ave_conf_nwf", c("lb_ave_tot_nwf","ub_ave_tot_nwf"), sep = ":")%>%
  mutate(ave_conf_nwf = paste0("(", ave_conf_nwf, ")"))%>%
  
  unite(col = "ave_conf_tot", c("lb_ave_tot","ub_ave_tot"), sep = ":")%>%
  mutate(ave_conf_tot = paste0("(", ave_conf_tot, ")"))%>%
  
  unite(col = "ave_conf_bfp_tot", c("lb_ave_bfp_tot","ub_ave_bfp_tot"), sep = ":")%>%
  mutate(ave_conf_bfp_tot = paste0("(", ave_conf_bfp_tot, ")"))%>%
  
  unite(col = "valpp_conf", c("lb_val_pp","ub_val_pp"), sep = ":")%>%
  mutate(valpp_conf = paste0("(", valpp_conf, ")"))%>%
  
  rename("ave_bfp_tot" = "ave_conf_bfp_tot",
         "ave_tot_wf" = "ave_conf_wf",
         "ave_tot_nwf" = "ave_conf_nwf",
         "ave_tot" = "ave_conf_tot",
         "val_pp"="valpp_conf")

df_table <- rbind(df1,df2)

df_table <- df_table%>%
  select(-ave_tot)

library(xtable)
print(xtable(df_table, type = "latex"), file = "./results/Tables/Table4.tex")
