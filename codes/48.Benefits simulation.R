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
  #sample_n(25)


# set seed
set.seed(1287)
# set parameters for mc

N = 1000  # number of sample
repli <- 10 # replication
elast_250m = 0.216 # water front elasticity - RE model
se_elast_250m = 0.050 # se of waterfront elasticity - RE model
elast_500m = 0.050 # non-water front elasticity - RE model
se_elast_500m = 0.027 # se of non-waterfront elasticity - RE model
wq_change = 10/100

# create data frame with simulated elasticity distribution (normal) and uniform 
# distribution factor to multiply dwelling value

df_sim <- tibble(sim_id = 1:repli, 
                 elast_250m = rnorm(repli, elast_250m,se_elast_250m),
                 elast_500m = rnorm(repli, elast_500m,se_elast_500m),
                 house_val_multi = runif(repli, 0.9, 1.1))



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
         v_Avod_,province)
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
simu_bfp_250$sim_id= substr(simu_bfp_250$sim_id, 8,9) ## chnage based on repli
simu_bfp_250$sim_id <- as.integer(simu_bfp_250$sim_id)

# set wide table into long format and assign simu id- 500m

simu_bfp_500 <- simu_bfp_500%>%
  pivot_longer(cols=starts_with("bf_500_"),
               names_to='sim_id',
               values_to='bfp_500')%>%
  mutate_if(is.numeric, round)
simu_bfp_500$sim_id= substr(simu_bfp_500$sim_id, 8,9) # change based on repli
simu_bfp_500$sim_id <- as.integer(simu_bfp_500$sim_id)

# create final simulated dataframe using (1) Simulated elasticity (df_sim),
# (2) simulated building foot print 250m (simu_bfp_250), 
# (3) simulated building foot print 500m (simu_bfp_500) - this has dwelling value at each DA also

df_sim_final <- df_sim%>%
  left_join(simu_bfp_250 )%>%
  left_join(simu_bfp_500 )%>%
  relocate(GeoUID, .after = sim_id)%>%
  relocate(v_Avod_, .after = bfp_500)%>%
  relocate(province, .after =v_Avod_)%>%
  mutate(dwelval = house_val_multi*v_Avod_)%>%
  mutate(d_wq = wq_change )%>%
  mutate(val_wf = elast_250m*bfp_250*dwelval*d_wq)%>%
  mutate(val_nwf = elast_500m*bfp_500*dwelval*d_wq)%>%
  mutate(val_tot = val_wf+val_nwf)
  
# plot distributtion for alberta

ab <- df_sim_final%>%
  subset(province == "AB")%>%
  select(GeoUID,val_tot)

ab %>%
  ggplot( aes(x=val_tot)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  xlim(0,2000000)

min(ab$sim_val_tot)
