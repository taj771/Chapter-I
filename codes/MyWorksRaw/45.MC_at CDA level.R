# clear memory
rm(list = ls())

# set wd (windows/mac)
setwd("~/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I/MetaHedonicProject")               #(Mac)
setwd("C:/Users/taj771/OneDrive - University of Saskatchewan/Chapter I/MetaHedonicProject") 


## Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  tidyverse,
  sf,
  SimDesign,
  triangle
)

# set seed
set.seed(1287)
# set parameters

N = 1000
rep <- 10
elast_250m = 0.216
se_elast_250m = 0.050
elast_500m = 0.050
se_elast_500m = 0.027


# First we simulate distribution for elasticities measured for both waterfront and
# non-waterfront. Sinnce the elasticities are not varied by provonce, this initiallt set 
# distributions used to calculate the benefits across provinces which has individually 
# simulated dwelling values and building foot print counts

################################################################################
# Elssticity distribution
################################################################################

# 250m
GeoUID <- c(1:4000)

df <- as.data.frame(GeoUID)%>%
  mutate(elast_250m = elast_250m)%>%
  mutate(se_elast_250m = se_elast_250m)%>%
  mutate(elast_500m = elast_500m)%>%
  mutate(se_elast_500m = se_elast_500m)

# Elasticity - 250m
Design <- df%>%
  select(GeoUID,elast_250m,se_elast_250m)
Design
#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)
Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rnorm(N, elast_250m,se_elast_250m)) # distributed N(10, 5)
  dat
}
# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(simu250_=results) # mean and SD summary of the sample means
  ret
}

#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final_e_250m <- runSimulation(design=Design, replications=rep,
                              generate=Generate, analyse=Analyse, summarise=Summarise)
Final_e_250m <- Final_e_250m%>%
  select(-elast_250m,-se_elast_250m,-REPLICATIONS,-SIM_TIME,-RAM_USED,-SEED,-COMPLETED)

write.csv(Final_e_250m, "./results/simulation/simu_e_250m.csv",  row.names=FALSE)

#############################################################################
#500m

# Elasticity - 250m
Design <- df%>%
  select(GeoUID,elast_500m,se_elast_500m)
Design
#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)
Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rnorm(N, elast_500m,se_elast_500m)) # distributed N(10, 5)
  dat
}
# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(simu500_=results) # mean and SD summary of the sample means
  ret
}

#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final_e_500m <- runSimulation(design=Design, replications=rep,
                              generate=Generate, analyse=Analyse, summarise=Summarise)
Final_e_500m <- Final_e_500m%>%
  select(-elast_500m,-se_elast_500m,-REPLICATIONS,-SIM_TIME,-RAM_USED,-SEED,-COMPLETED)

write.csv(Final_e_500m, "./results/simulation/simu_e_500m.csv",  row.names=FALSE)


################################################################################
# Alberta
################################################################################

bfp <- read.csv("./Census/AB/bfp_count_for_mc.csv")%>%
  select(-X)
#bfp_ab$GeoUID <- as.character(bfp_ab$GeoUID)

df <- read.csv("./results/value_map/AB/mc_design.csv")%>%
  select(GeoUID,v_Avod.,adj_bfp_count_250,adj_bfp_count_250_500)%>%
  sample_n(5)%>%
  rename("mu_dwelvalue"="v_Avod.")%>%
  mutate(ub_dwelvalue = mu_dwelvalue+(mu_dwelvalue*0.1))%>%
  mutate(lb_dwelvalue = mu_dwelvalue-(mu_dwelvalue*0.1))%>%
  left_join(bfp, by = "GeoUID")

df$GeoUID <- as.character(df$GeoUID)


#df$GeoUID <- as.character(df$GeoUID)

#############################################################################

# Dwelling values

Design <- df%>%
  select(GeoUID,ub_dwelvalue,lb_dwelvalue)
Design

#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, runif(N, lb_dwelvalue,ub_dwelvalue)) # distributed N(10, 5)
  dat
}

# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(dwelvalue_=results) # mean and SD summary of the sample means
  ret
}

#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final_dwelling_value <- runSimulation(design=Design, replications=rep,
                              generate=Generate, analyse=Analyse, summarise=Summarise)
  
Final_dwelling_value <- Final_dwelling_value%>%
  select(-ub_dwelvalue,-lb_dwelvalue,-REPLICATIONS,-SIM_TIME,-RAM_USED,-SEED,-COMPLETED)%>%
  mutate_all(funs(ifelse(. == 0, NA, .)))


###########################################################################
#building foot print - 250m

Design <- df%>%
  select(GeoUID,adj_bfp_lb_250,adj_bfp_ub_250,adj_bfp_count_250)
Design

#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rtriangle(n=N, a=adj_bfp_lb_250,b=adj_bfp_ub_250,c=adj_bfp_count_250)) 
  dat
}

# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(bf_250_=results) # mean and SD summary of the sample means
  ret
}

#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final_bfp_250 <- runSimulation(design=Design, replications=rep,
                                      generate=Generate, analyse=Analyse, summarise=Summarise)

Final_bfp_250 <- Final_bfp_250%>%
  select(-adj_bfp_lb_250,-adj_bfp_ub_250,-adj_bfp_count_250,-REPLICATIONS,-SIM_TIME,-RAM_USED,-SEED,-COMPLETED)



################################################################################  
#building foot print - 500m

Design <- df%>%
  select(GeoUID,adj_bfp_lb_250_500,adj_bfp_ub_250_500,adj_bfp_count_250_500)
Design


#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rtriangle(n=N, a=adj_bfp_lb_250_500,b=adj_bfp_ub_250_500,c=adj_bfp_count_250_500))
  dat
}

# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(bf_500_=results) # mean and SD summary of the sample means
  ret
}

#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final_bfp_500 <- runSimulation(design=Design, replications=rep,
                               generate=Generate, analyse=Analyse, summarise=Summarise)

Final_bfp_500 <- Final_bfp_500%>%
  select(-adj_bfp_lb_250_500,-adj_bfp_ub_250_500,-adj_bfp_count_250_500,-REPLICATIONS,-SIM_TIME,-RAM_USED,-SEED,-COMPLETED, - FATAL_TERMINATION)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))


####################################################################################
# read csv with pre simulated data

df <- df%>%
  select(GeoUID)

Final_e_250m  <- read_csv("./results/simulation/simu_e_250m.csv")%>%
  sample_n(.,1116)%>%
  select(-GeoUID)%>%
  cbind(df)%>%
  relocate(GeoUID, .before = simu250_1)

Final_e_500m <- read_csv("./results/simulation/simu_e_500m.csv")%>%
  sample_n(.,1116)%>%
  select(-GeoUID)%>%
  cbind(df)%>%
  relocate(GeoUID, .before = simu500_1)
  

####################################################################################

df_250 <- data.frame(
  Map(function(x,y,z) if(all(is.numeric(x),is.numeric(y))) (x * y * z)/100 else x, Final_e_250m, Final_dwelling_value,Final_bfp_250)
)

df_500 <- data.frame(
  Map(function(x,y,z) if(all(is.numeric(x),is.numeric(y))) (x * y * z)/100 else x, Final_e_500m, Final_dwelling_value,Final_bfp_500)
)


df <- data.frame(
  Map(function(x,y) if(all(is.numeric(x),is.numeric(y))) x + y  else x, df_250, df_500)
)


df <-df%>%
  pivot_longer(cols=starts_with("simu"),
                    names_to='simu_',
                    values_to='simu')

write_csv(df, "./results/value_map/AB/ab_mcsimu.csv")

################################################################################
# British Columbia
################################################################################

bfp <- read.csv("./Census/BC/bfp_count_for_mc.csv")%>%
  select(-X)
#bfp_ab$GeoUID <- as.character(bfp_ab$GeoUID)

df <- read.csv("./results/value_map/BC/mc_design.csv")%>%
  select(GeoUID,v_Avod.,adj_bfp_count_250,adj_bfp_count_250_500)%>%
  #sample_n(25)%>%
  rename("mu_dwelvalue"="v_Avod.")%>%
  mutate(ub_dwelvalue = mu_dwelvalue+(mu_dwelvalue*0.1))%>%
  mutate(lb_dwelvalue = mu_dwelvalue-(mu_dwelvalue*0.1))%>%
  left_join(bfp, by = "GeoUID")

df$GeoUID <- as.character(df$GeoUID)


#df$GeoUID <- as.character(df$GeoUID)

#############################################################################

# Dwelling values

Design <- df%>%
  select(GeoUID,ub_dwelvalue,lb_dwelvalue)
Design

#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, runif(N, lb_dwelvalue,ub_dwelvalue)) # distributed N(10, 5)
  dat
}

# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(dwelvalue_=results) # mean and SD summary of the sample means
  ret
}

#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final_dwelling_value <- runSimulation(design=Design, replications=rep,
                                      generate=Generate, analyse=Analyse, summarise=Summarise)

Final_dwelling_value <- Final_dwelling_value%>%
  select(-ub_dwelvalue,-lb_dwelvalue,-REPLICATIONS,-SIM_TIME,-RAM_USED,-SEED,-COMPLETED)%>%
  mutate_all(funs(ifelse(. == 0, NA, .)))


###########################################################################
#building foot print - 250m

Design <- df%>%
  select(GeoUID,adj_bfp_lb_250,adj_bfp_ub_250,adj_bfp_count_250)
Design

#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rtriangle(n=N, a=adj_bfp_lb_250,b=adj_bfp_ub_250,c=adj_bfp_count_250)) 
  dat
}

# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(bf_250_=results) # mean and SD summary of the sample means
  ret
}

#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final_bfp_250 <- runSimulation(design=Design, replications=rep,
                               generate=Generate, analyse=Analyse, summarise=Summarise)

Final_bfp_250 <- Final_bfp_250%>%
  select(-adj_bfp_lb_250,-adj_bfp_ub_250,-adj_bfp_count_250,-REPLICATIONS,-SIM_TIME,-RAM_USED,-SEED,-COMPLETED,- FATAL_TERMINATION)%>%
  mutate_all(funs(ifelse(. == 0, NA, .)))



################################################################################  
#building foot print - 500m

Design <- df%>%
  select(GeoUID,adj_bfp_lb_250_500,adj_bfp_ub_250_500,adj_bfp_count_250_500)
Design


#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rtriangle(n=N, a=adj_bfp_lb_250_500,b=adj_bfp_ub_250_500,c=adj_bfp_count_250_500))
  dat
}

# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(bf_500_=results) # mean and SD summary of the sample means
  ret
}

#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final_bfp_500 <- runSimulation(design=Design, replications=rep,
                               generate=Generate, analyse=Analyse, summarise=Summarise)

Final_bfp_500 <- Final_bfp_500%>%
  select(-adj_bfp_lb_250_500,-adj_bfp_ub_250_500,-adj_bfp_count_250_500,-REPLICATIONS,-SIM_TIME,-RAM_USED,-SEED,-COMPLETED, - FATAL_TERMINATION)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))


####################################################################################
# read csv with pre simulated data

df <- df%>%
  select(GeoUID)

Final_e_250m  <- read_csv("./results/simulation/simu_e_250m.csv")%>%
  sample_n(.,950)%>%
  select(-GeoUID)%>%
  cbind(df)%>%
  relocate(GeoUID, .before = simu250_1)

Final_e_500m <- read_csv("./results/simulation/simu_e_500m.csv")%>%
  sample_n(.,950)%>%
  select(-GeoUID)%>%
  cbind(df)%>%
  relocate(GeoUID, .before = simu500_1)


####################################################################################

df_250 <- data.frame(
  Map(function(x,y,z) if(all(is.numeric(x),is.numeric(y))) (x * y * z)/100 else x, Final_e_250m, Final_dwelling_value,Final_bfp_250)
)

df_500 <- data.frame(
  Map(function(x,y,z) if(all(is.numeric(x),is.numeric(y))) (x * y * z)/100 else x, Final_e_500m, Final_dwelling_value,Final_bfp_500)
)


df <- data.frame(
  Map(function(x,y) if(all(is.numeric(x),is.numeric(y))) x + y  else x, df_250, df_500)
)


df <-df%>%
  pivot_longer(cols=starts_with("simu"),
               names_to='simu_',
               values_to='simu')

write_csv(df, "./results/value_map/BC/bc_mcsimu.csv")


################################################################################
# Manitoba
################################################################################

bfp <- read.csv("./Census/MB/bfp_count_for_mc.csv")%>%
  select(-X)
#bfp_ab$GeoUID <- as.character(bfp_ab$GeoUID)

df <- read.csv("./results/value_map/MB/mc_design.csv")%>%
  select(GeoUID,v_Avod.,adj_bfp_count_250,adj_bfp_count_250_500)%>%
  #sample_n(25)%>%
  rename("mu_dwelvalue"="v_Avod.")%>%
  mutate(ub_dwelvalue = mu_dwelvalue+(mu_dwelvalue*0.1))%>%
  mutate(lb_dwelvalue = mu_dwelvalue-(mu_dwelvalue*0.1))%>%
  left_join(bfp, by = "GeoUID")

df$GeoUID <- as.character(df$GeoUID)


#df$GeoUID <- as.character(df$GeoUID)

#############################################################################

# Dwelling values

Design <- df%>%
  select(GeoUID,ub_dwelvalue,lb_dwelvalue)
Design

#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, runif(N, lb_dwelvalue,ub_dwelvalue)) # distributed N(10, 5)
  dat
}

# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(dwelvalue_=results) # mean and SD summary of the sample means
  ret
}

#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final_dwelling_value <- runSimulation(design=Design, replications=rep,
                                      generate=Generate, analyse=Analyse, summarise=Summarise)

Final_dwelling_value <- Final_dwelling_value%>%
  select(-ub_dwelvalue,-lb_dwelvalue,-REPLICATIONS,-SIM_TIME,-RAM_USED,-SEED,-COMPLETED)%>%
  mutate_all(funs(ifelse(. == 0, NA, .)))


###########################################################################
#building foot print - 250m

Design <- df%>%
  select(GeoUID,adj_bfp_lb_250,adj_bfp_ub_250,adj_bfp_count_250)
Design

#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rtriangle(n=N, a=adj_bfp_lb_250,b=adj_bfp_ub_250,c=adj_bfp_count_250)) 
  dat
}

# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(bf_250_=results) # mean and SD summary of the sample means
  ret
}

#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final_bfp_250 <- runSimulation(design=Design, replications=rep,
                               generate=Generate, analyse=Analyse, summarise=Summarise)

Final_bfp_250 <- Final_bfp_250%>%
  select(-adj_bfp_lb_250,-adj_bfp_ub_250,-adj_bfp_count_250,-REPLICATIONS,-SIM_TIME,-RAM_USED,-SEED,-COMPLETED)%>%
  mutate_all(funs(ifelse(. == 0, NA, .)))



################################################################################  
#building foot print - 500m

Design <- df%>%
  select(GeoUID,adj_bfp_lb_250_500,adj_bfp_ub_250_500,adj_bfp_count_250_500)
Design


#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rtriangle(n=N, a=adj_bfp_lb_250_500,b=adj_bfp_ub_250_500,c=adj_bfp_count_250_500))
  dat
}

# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(bf_500_=results) # mean and SD summary of the sample means
  ret
}

#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final_bfp_500 <- runSimulation(design=Design, replications=rep,
                               generate=Generate, analyse=Analyse, summarise=Summarise)

Final_bfp_500 <- Final_bfp_500%>%
  select(-adj_bfp_lb_250_500,-adj_bfp_ub_250_500,-adj_bfp_count_250_500,-REPLICATIONS,-SIM_TIME,-RAM_USED,-SEED,-COMPLETED, - FATAL_TERMINATION)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))


####################################################################################
# read csv with pre simulated data

df <- df%>%
  select(GeoUID)

Final_e_250m  <- read_csv("./results/simulation/simu_e_250m.csv")%>%
  sample_n(.,341)%>%
  select(-GeoUID)%>%
  cbind(df)%>%
  relocate(GeoUID, .before = simu250_1)

Final_e_500m <- read_csv("./results/simulation/simu_e_500m.csv")%>%
  sample_n(.,341)%>%
  select(-GeoUID)%>%
  cbind(df)%>%
  relocate(GeoUID, .before = simu500_1)


####################################################################################

df_250 <- data.frame(
  Map(function(x,y,z) if(all(is.numeric(x),is.numeric(y))) (x * y * z)/100 else x, Final_e_250m, Final_dwelling_value,Final_bfp_250)
)

df_500 <- data.frame(
  Map(function(x,y,z) if(all(is.numeric(x),is.numeric(y))) (x * y * z)/100 else x, Final_e_500m, Final_dwelling_value,Final_bfp_500)
)


df <- data.frame(
  Map(function(x,y) if(all(is.numeric(x),is.numeric(y))) x + y  else x, df_250, df_500)
)


df <-df%>%
  pivot_longer(cols=starts_with("simu"),
               names_to='simu_',
               values_to='simu')

write_csv(df, "./results/value_map/MB/mb_mcsimu.csv")












#ggplot

ggplot(df, aes(x=simu)) +
  geom_histogram(binwidth=1000)+
  ylim(-10,100)+
  xlim(0,1000000)



df %>%
  ggplot( aes(x=simu)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)


dat.hdi_simu_tot_val <-hBayesDM::HDIofMCMC(df_ab$simu)
dat.hdi_simu_tot_val <-hBayesDM::HDIofMCMC(df_ab$simu)

mean_data <- mean(df_ab$simu)


p <- ggplot(df, aes(x=simu)) +
  geom_histogram(binwidth=100, fill="skyblue3")+
  ylim(-10,100)+
  xlim(0,1000000)+
  geom_segment(x=dat.hdi_simu_tot_val[1],xend=dat.hdi_simu_tot_val[2],y=0,yend=0,color="blue",size=2,lineend="round")+
  geom_vline(xintercept = mean_data,
             color = "red", linetype = "dashed")+
  geom_vline(xintercept = dat.hdi_simu_tot_val,
             color = "red", linetype = "dashed")+
  theme_bw()
p

p + scale_x_continuous(breaks=seq(161, 169, 1))


