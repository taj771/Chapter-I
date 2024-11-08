################################################################################
# Model 2 - CLuster weighted mean 
# Date 04/22/22
# Description - Replication of Guignet's meta analysis results
################################################################################

# read file full data set
df <- read.csv("./metadata/meta-dataset_for_property_values_and_water_quality.csv")


# chlorophyll a - waterfront

df1 <- filter(df, distbuf == 1)

df4 <- subset(df2, studyid == "28")
P1 <- mean(df4$wqelast)
df5 <- subset(df2, studyid == "32")
P2 <- mean(df5$wqelast) 
df6 <- subset(df2, studyid == "35")
P3 <- mean(df6$wqelast) 
clus_chl_wf <- (P1+P2+P3)/3

clus_chl_wf

t.test(clus_chl_wf, conf.level = 0.95) 
# Delta method
deltaMethod(lm(clus_chl_wf~1,cha_wf),"Intercept" )