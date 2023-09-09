#clear memory
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
  stargazer,
  xtable,
  texreg,
  modelsummary
)


# Reference : https://cran.r-project.org/web/packages/clubSandwich/vignettes/meta-analysis-with-CRVE.html
#           : https://www.jepusto.com/robust-meta-analysis-1/
#           : https://cran.r-project.org/web/packages/robumeta/vignettes/robumetaVignette.pdf
###############################################################################
# load data
#wf <- read.csv("./metadata/wo_outliers_wf.csv")
n#wf <- read.csv("./metadata/wo_outliers_nwf.csv")

df <- rbind(wf,nwf)


# load data - with outliers 
df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")

################################################################################
#                               VAC-weighted unit transfer                      #
################################################################################

#waterfront
# define weights based on variance
df.fe.vac <- df%>%
  mutate(vi = 1/sampsize)%>%
  group_by(studyid, geog) %>%
  mutate(v_bar = sum(vi)) %>%
  ungroup()%>%
  mutate(w = vi/v_bar)%>%
  group_by(studyid,geog)%>%
  mutate(w_elast = wqelast*w)%>%
  group_by(studyid,geog)%>%
  mutate(weight_elas = sum(w_elast))#%>%
  #filter(elast_sim < 2) # remove outliers if necessary

# first run the rma with all data points that later use to calculate transfer error 

fe_vac <- rma(yi = wqelast, vi, data=df.fe.vac, method="EE",weights=w,level = 95)


df.fe.vac <- df.fe.vac %>%
  select(obsid,studyid,geog,elast_sim,wqelast,elast_sim_se,vi,w,weight_elas)%>%
  group_by(studyid,geog)%>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()

# loop
result <- list()

#loop through obsid and extract betas
for(i in unique(df.fe.vac$cluster)){
  
  #construct linear model (metafor package)
  elas <- rma(yi=wqelast, vi, data = subset(df.fe.vac, df.fe.vac$cluster != i), weights=w,method = "EE", level = 95 )
 

  #create data.frame containing intercept left out and coefficient
  result.dt <- data.frame(beta = coef(elas),
                          cluster = i)
  
  #bind to list
  result[[i]] <- result.dt
}

#bind to data.frame
result <- do.call(rbind, result)

#result$elas_esti <- coef(wf_vac)

head(result)

# left_join - to calculate transfer error

transfer_error <- left_join(result, df.fe.vac, by = "cluster")%>%
  distinct(cluster, .keep_all = T)


error_wf <- mutate(transfer_error,T_error = abs((beta -weight_elas )/weight_elas)*100)

median(error_wf$T_error, na.rm = T)



################################################################################
#                               Random effect-weighted unit transfer                      #
################################################################################

df.wf.re <- df%>%
  mutate(vi = 1/sampsize)%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()


wf.re <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re)


wi.wf <- 1 / (sum(wf.re$sigma2) + df.wf.re$vi)



w.wf <- data.frame(k=c(table(df.wf.re$obsid)),
                   weight = tapply(wi.wf,df.wf.re$obsid, sum))


library(tibble)
w.wf <- rownames_to_column(w.wf, var="obsid")
w.wf$obsid <- as.numeric(w.wf$obsid)

# join weights

df.wf.re1 <- df.wf.re%>%
  select(wqelast, elast_sim,elast_sim_se,cluster,studyid,obsid,geog,vi)%>%
  left_join(w.wf)%>%
  mutate(weighted_elas = weight*wqelast)%>%
  group_by(studyid, geog)%>%
  mutate(w_elas = sum(weighted_elas))%>%
  distinct(cluster, .keep_all = T)


# loop - drop each observation and calculate elasticity for each observation

result <- list()

#loop through obsid and extract betas
for(i in unique(df.wf.re$cluster)){
  
  #construct linear model (metafor package)
  elas <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data = subset(df.wf.re, df.wf.re$cluster != i), level = 95 )
  
  #create data.frame containing intercept left out and coefficient
  result.dt <- data.frame(beta = coef(elas),
                          cluster = i)
  
  #bind to list
  result[[i]] <- result.dt
}

#bind to data.frame
result <- do.call(rbind, result)



head(result)

# left_join - to calculate transfer error

transfer_error <- left_join(result, df.wf.re1, by = "cluster")


error_wf_re <- mutate(transfer_error,T_error = abs((beta - w_elas)/w_elas)*100)

median(error_wf_re$T_error, na.rm = T)


















