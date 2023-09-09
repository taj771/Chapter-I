

# Since each study as well as within study, functional forms differ derivation of elasticity and semi elasticity is vary. SO here I take study wise
# and process data. If functional forms differ within study those process separately.

# Clean memory 
rm(list = ls())
# Load library 
library(SimDesign)
library(dplyr)
df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")
df <- subset(df, stud_new %in% c("1"))
df <- subset(df, cov_assum0 %in% c(0))
df$avgprice <- as.numeric(as.character(df$avgprice))



# 1. Time is money: Water quality’s impact on home liquidity and property values Nicholas Irwin, David Wolf  

# select only observation within that study
# Model 1 to 4 and 6 has similar specification, model 5 has different specification and later will get model 6 too
Design.38 <- df[which(df$studyid == "38"),]

#### Step 1 --- Define your conditions under study and create design data.frame

N <- 1000
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rnorm(N, wqcoef_var1, wqcoef_se_var1)) # distributed N(10, 5)
  dat
}
# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(mu=mean(results), SE=sd(results)) # mean and SD summary of the sample means
  ret
}
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final.38 <- runSimulation(design=Design.38, replications=100000,
                          generate=Generate, analyse=Analyse, summarise=Summarise)
Final.38

# calculate the elasticity using simulated parameter
Final.38 <- mutate(Final.38, elast_sim = mu,
                   elast_sim_se = SE)


# 2 Coupling Water Quality Numerical Simulation and Hedonic Models to Evaluate Impact of Changes in Nutrient Loading  
# Weizhe Weng, Kevin J. Boyle, Cayelan C. Carey, Kelly M. Cobourn, Hilary A. Dugan, Kaitlin J. Farrell, Paul C. Hanson, Sreeya Brahma, Nicole K. Ward, Kathleen C. Weathers

# select only observation within that study
Design.39 <- df[which(df$studyid == "39"),]

#### Step 1 --- Define your conditions under study and create design data.frame

N <- 1000
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rnorm(N, wqcoef_var1, wqcoef_se_var1)) # distributed N(10, 5)
  dat
}
# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(mu=mean(results), SE=sd(results)) # mean and SD summary of the sample means
  ret
}
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final.39 <- runSimulation(design=Design.39, replications=100000,
                            generate=Generate, analyse=Analyse, summarise=Summarise)
Final.39

# calculate the elasticity using simulated parameter
Final.39 <- mutate(Final.39, elast_sim = mu,
                     elast_sim_se = SE)
# 3). The Impact of Water Clarity on Home Value in Northern Wisconsin
# Thomas Kemp, PhD, Irene Ng, and Haikal Mohammad 


# select only observation within that study
Design.40 <- df[which(df$studyid == "40"),]


#### Step 1 --- Define your conditions under study and create design data.frame

N <- 1000
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rnorm(N, wqcoef_var1, wqcoef_se_var1)) # distributed N(10, 5)
  dat
}
# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(mu=mean(results), SE=sd(results)) # mean and SD summary of the sample means
  ret
}
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final.40 <- runSimulation(design=Design.40, replications=100000,
                          generate=Generate, analyse=Analyse, summarise=Summarise)
Final.40

# calculate the elasticity using simulated parameter
Final.40 <- mutate(Final.40, elast_sim = mu*(1/(3.28*avgprice)),
                   elast_sim_se = SE*1/(3.28*avgprice))


# 04). Examining Implicit Price Variation for Lake Water Quality
# Kristen Marie Swedberg 

Design.41 <- df[which(df$studyid == "41"),]
Design.41 <- subset(Design.41, cov_assum0 %in% c("0"))


#### Step 1 --- Define your conditions under study and create design data.frame

N <- 1000
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rnorm(N, wqcoef_var1, wqcoef_se_var1)) # distributed N(10, 5)
  dat
}
# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(mu=mean(results), SE=sd(results)) # mean and SD summary of the sample means
  ret
}
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final.41 <- runSimulation(design=Design.41, replications=100000,
                          generate=Generate, analyse=Analyse, summarise=Summarise)
Final.41

# calculate the elasticity using simulated parameter
Final.41 <- mutate(Final.41, elast_sim = mu,
                   elast_sim_se = SE)

# 05). Hedonic Price Estimates of Lake Water Quality: Valued Attribute, Instrumental Variables, and Ecological-Economic Benefits .
# Michael R.Moore Jonathan P.DoubekbHuiXu Bradley J.Cardinale


Design.42 <- df[which(df$studyid == "42"),]
Design.42 <- subset(Design.42, cov_assum0 %in% c("0"))


#### Step 1 --- Define your conditions under study and create design data.frame

N <- 1000
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rnorm(N, wqcoef_var1, wqcoef_se_var1)) # distributed N(10, 5)
  dat
}
# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(mu=mean(results), SE=sd(results)) # mean and SD summary of the sample means
  ret
}
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final.42 <- runSimulation(design=Design.42, replications=100000,
                          generate=Generate, analyse=Analyse, summarise=Summarise)
Final.42

# calculate the elasticity using simulated parameter
Final.42 <- mutate(Final.42, elast_sim = mu*avgwqvar,
                   elast_sim_se = SE*avgwqvar)

# 06). Valuing water quality change using a coupled economic-hydrological model
# Hongxing Liu, SathyaGopalakrishnan, DrewBrowning, GajanSivandran 

# All need variance covarianc matrix

# 08). Convergent Validity of Satellite and Secchi Disk Measures of Water Clarity in Hedonic
# Models
# David Wolf, and Thomas Kemp  

# All need variance covariance matrix



# 9. Beyond marginal: Estimating the demand for water quality David Wolf, H. AllenKlaiber, Sathya Gopalakrishnan (2022)
# One functional form and can process once the whole study
# study id - 45



# select only observation within that study

Design.45 <- df[which(df$studyid == "45"), ]


#### Step 1 --- Define your conditions under study and create design data.frame

N <- 1000
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rnorm(N, wqcoef_var1, wqcoef_se_var1)) # distributed N(10, 5)
  dat
}
# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(mu=mean(results), SE=sd(results)) # mean and SD summary of the sample means
  ret
}
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final.45 <- runSimulation(design=Design.45, replications=100000,
                       generate=Generate, analyse=Analyse, summarise=Summarise)
Final.45

# calculate the elasticity using simulated parameter
Final.45 <- mutate(Final.45, elast_sim = mu*wbsizeorig,
                elast_sim_se = SE*wbsizeorig)

# 10). Economic Valuation for Coastal Water Infrastructure Planning: Analysis of the Housing Market and Nutrient Pollution in Suffolk County, NY 
# Mark Nepf, Anthony Dvarskas, and Patrick J. Walsh

# All need var-cov matrix

# 11). Water quality and cottage prices in Ontario 
# Julia Clapper & Steven B. Caudill

Design.47.1 <- df[which(df$studyid == "47"),]
Design.47.1 <- subset(Design.47.1, specname %in% c("liner - sale price"))



#### Step 1 --- Define your conditions under study and create design data.frame

N <- 1000
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rnorm(N, wqcoef_var1, wqcoef_se_var1)) # distributed N(10, 5)
  dat
}
# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(mu=mean(results), SE=sd(results)) # mean and SD summary of the sample means
  ret
}
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final.47.1 <- runSimulation(design=Design.47.1, replications=100000,
                          generate=Generate, analyse=Analyse, summarise=Summarise)
Final.47.1

# calculate the elasticity using simulated parameter
Final.47.1 <- mutate(Final.47.1, elast_sim = mu*(avgwqvar/avgprice),
                   elast_sim_se = SE*(avgwqvar/avgprice))

####

Design.47.2 <- df[which(df$studyid == "47"),]
Design.47.2 <- subset(Design.47.2, specname %in% c("log-liner - sale price"))



#### Step 1 --- Define your conditions under study and create design data.frame

N <- 1000
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rnorm(N, wqcoef_var1, wqcoef_se_var1)) # distributed N(10, 5)
  dat
}
# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(mu=mean(results), SE=sd(results)) # mean and SD summary of the sample means
  ret
}
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final.47.2 <- runSimulation(design=Design.47.2, replications=100000,
                            generate=Generate, analyse=Analyse, summarise=Summarise)
Final.47.2

# calculate the elasticity using simulated parameter
Final.47.2 <- mutate(Final.47.2, elast_sim = mu*(avgwqvar),
                     elast_sim_se = SE*(avgwqvar))

######

Design.47.3 <- df[which(df$studyid == "47"),]
Design.47.3 <- subset(Design.47.3, specname %in% c("log-log - sale price"))



#### Step 1 --- Define your conditions under study and create design data.frame

N <- 1000
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rnorm(N, wqcoef_var1, wqcoef_se_var1)) # distributed N(10, 5)
  dat
}
# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(mu=mean(results), SE=sd(results)) # mean and SD summary of the sample means
  ret
}
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final.47.3 <- runSimulation(design=Design.47.3, replications=100000,
                            generate=Generate, analyse=Analyse, summarise=Summarise)
Final.47.3

# calculate the elasticity using simulated parameter
Final.47.3 <- mutate(Final.47.3, elast_sim = mu,
                     elast_sim_se = SE)

####

Design.47.4 <- df[which(df$studyid == "47"),]
Design.47.4 <- subset(Design.47.4, specname %in% c("liner - sale price per square foot"))



#### Step 1 --- Define your conditions under study and create design data.frame

N <- 1000
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rnorm(N, wqcoef_var1, wqcoef_se_var1)) # distributed N(10, 5)
  dat
}
# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(mu=mean(results), SE=sd(results)) # mean and SD summary of the sample means
  ret
}
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final.47.4 <- runSimulation(design=Design.47.4, replications=100000,
                            generate=Generate, analyse=Analyse, summarise=Summarise)
Final.47.4

# calculate the elasticity using simulated parameter
Final.47.4 <- mutate(Final.47.4, elast_sim = mu*(avgwqvar/(443.566)), # 236.77 waterfront footage
                     elast_sim_se = SE*(avgwqvar/(443.566)))

########
Design.47.5 <- df[which(df$studyid == "47"),]
Design.47.5 <- subset(Design.47.5, specname %in% c("log-liner - sale price per square foot"))



#### Step 1 --- Define your conditions under study and create design data.frame

N <- 1000
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rnorm(N, wqcoef_var1, wqcoef_se_var1)) # distributed N(10, 5)
  dat
}
# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(mu=mean(results), SE=sd(results)) # mean and SD summary of the sample means
  ret
}
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final.47.5 <- runSimulation(design=Design.47.5, replications=100000,
                            generate=Generate, analyse=Analyse, summarise=Summarise)
Final.47.5

# calculate the elasticity using simulated parameter
Final.47.5 <- mutate(Final.47.5, elast_sim = mu*(avgwqvar),
                     elast_sim_se = SE*(avgwqvar))


######

Design.47.6 <- df[which(df$studyid == "47"),]
Design.47.6 <- subset(Design.47.6, specname %in% c("log-log - sale price per square foot"))



#### Step 1 --- Define your conditions under study and create design data.frame

N <- 1000
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rnorm(N, wqcoef_var1, wqcoef_se_var1)) # distributed N(10, 5)
  dat
}
# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(mu=mean(results), SE=sd(results)) # mean and SD summary of the sample means
  ret
}
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final.47.6 <- runSimulation(design=Design.47.6, replications=100000,
                            generate=Generate, analyse=Analyse, summarise=Summarise)
Final.47.6

# calculate the elasticity using simulated parameter
Final.47.6 <- mutate(Final.47.6, elast_sim = mu,
                     elast_sim_se = SE)

# 12 Valuing recreational water clarity and quality: evidence from hedonic pricing models of lakeshore properties 
# Diego Calderón-Arrieta, Steven B. Caudill & Franklin G. Mixon

Design.48 <- df[which(df$studyid == "48"),]



#### Step 1 --- Define your conditions under study and create design data.frame

N <- 1000
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summaries functions
# help(Generate)

Generate <- function(condition, fixed_objects = NULL){
  dat <- with(condition, rnorm(N, wqcoef_var1, wqcoef_se_var1)) # distributed N(10, 5)
  dat
}
# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL){
  ret <- mean(dat) # mean of the sample data vector
  ret
}
# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- c(mu=mean(results), SE=sd(results)) # mean and SD summary of the sample means
  ret
}
#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Collect results by looping over the rows in design
# run the simulation
Final.48 <- runSimulation(design=Design.48, replications=100000,
                            generate=Generate, analyse=Analyse, summarise=Summarise)
Final.48

# calculate the elasticity using simulated parameter
Final.48 <- mutate(Final.48, elast_sim = mu*(267.6*avgwqvar/avgprice),
                     elast_sim_se = SE*(267.6*avgwqvar/avgprice))


df.simulated <- rbind(Final.38,Final.39, Final.40, Final.41, Final.42, Final.45, Final.47.1, Final.47.2, 
                      Final.47.3, Final.47.4, Final.47.5, Final.47.6, Final.48)

df.simulated <- df.simulated%>%
  select(elast_sim, elast_sim_se, obsid)

df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")
df <- subset(df, stud_new %in% c("1"))
df <- df%>%
  select(- elast_sim, - elast_sim_se, -X, -X.1, - X.2)
             
             
             
df <- df %>%
  left_join(df.simulated, by = c("obsid"))
df <- df%>%
  relocate(elast_sim, .after = wqpctdp)
df <- df%>%
  relocate(elast_sim_se, .after = elast_sim)

df.old <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")
df.old <- subset(df.old, stud_new %in% c("0"))
df.old <- df.old%>%
  select(-X, -X.1, -X.2)

df.final <- rbind(df.old, df)

write.csv(df.final,"./metadata/meta_dataset_water_clarity_TJ.csv" )


