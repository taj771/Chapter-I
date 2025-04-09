# Chapter I - Water quality and propery values: A meta-analysis and Canadian national benefit assesmsnr
# Main results

#Derive elasticity measures based on primry study's estimations
source("./codes/code_part1.R")

#Identifying outliers
source("./codes/code_part2.R")

#Table 1 in manuscript
source("./codes/code_part3.R")

#Figure 1 in manuscript
source("./codes/code_part4.R")

#Figure 2 and Figure 3 in manuscript
source("./codes/code_part5.R")

#Figure 4 in manuscript
source("./codes/code_part6.R") # Takes some time

#Table 2 in manuscript
source("./codes/code_part7.R") 

#Table 3 in manuscript
source("./codes/code_part8.R") 

# This code create spatial buffers around lakes - This will take a significant amount of time so 
# if time limited please use the save data
source("./codes/code_part9.R") # Takes some time

# This code extract the building foot print data within spatial buffers around lakes - This will take significant amunt of time
# so if time limited skip this and use the saved data
source("./codes/code_part10.R") # Takes some time

# This code count the umber of building foot print at lower end, upper end and mode with predefined are of polygons
# These numbers used in MC simulations 
# This will take significant amunt of time so if time limited skip this and use the saved data
source("./codes/code_part11.R") # Takes some time

# This code run the montecarlo simulation and reproduce the Table 4 in manuscript 
# This will take time to run so if want to skip the simulation which is taking time
# we have add break point we can start from there
source("./codes/code_part12.R") # Takes some time

# This code replicate Figure 5 in manuscript 
source("./codes/code_part13.R") # Takes some time

# This code replicate Figure 6 in manuscript 
source("./codes/code_part14.R") # Takes some time


###### Appendix #####################################
# Benefit transfer error 
source("./codes/code_part15.R")
# Funnel plot and publication bias corection 
source("./codes/code_part16.R")