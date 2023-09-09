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

# load data - with outliers 
df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")
## Subset: Main models
# waterfront
#df.wf <- df[which(df$distbuf == 1), ]
# non-waterfront
#df.nwf <- df[which(df$distbuf == 2), ]

# load data withoud outliers 
#df.wf <- read.csv("./metadata/wo_outliers_wf.csv")
#df.nwf <- read.csv("./metadata/wo_outliers_nwf.csv")


#fixed effect - unweighted
df.wf.fe.u <- df[which(df$distbuf == 1), ]%>%
  mutate(vi = 1/sampsize) #do not consider variance into the accuracy of the 
df.nwf.fe.u <- df[which(df$distbuf == 2), ]%>%
  mutate(vi = 1/sampsize) #do not consider variance into the accuracy of the 

# fixed effect - cluster adjusted

df.wf.fe.c <- df[which(df$distbuf == 1), ]%>%
  mutate(vi = 1/sampsize)%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  group_by(cluster)%>%
  mutate(count = n())%>%
  mutate(w = 1/count)


df.nwf.fe.c <- df[which(df$distbuf == 2), ]%>%
  mutate(vi = 1/sampsize)%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  group_by(cluster)%>%
  mutate(count = n())%>%
  mutate(w = 1/count)


# Fixed effect - VAC

df.wf.fe.vac <- df[which(df$distbuf == 1), ]%>%
  mutate(vi = 1/sampsize)%>%
  group_by(studyid, geog) %>%
  mutate(v_bar = sum(vi)) %>%
  ungroup()%>%
  mutate(w = vi/v_bar)


df.nwf.fe.vac <- df[which(df$distbuf == 2), ]%>%
  mutate(vi = 1/sampsize)%>%
  group_by(studyid, geog) %>%
  mutate(v_bar = sum(vi)) %>%
  ungroup()%>%
  mutate(w = vi/v_bar)


# Random effect models

df.wf.re <- df[which(df$distbuf == 1), ]%>%
  mutate(vi = 1/sampsize)%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()

df.nwf.re <- df[which(df$distbuf == 2), ]%>%
  mutate(vi = 1/sampsize)%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()



#using metafor function

panels <- list(
  "Waterfront" = list(
    "Fixed efffet - Unweighted" = rma(yi = wqelast, vi, data=df.wf.fe.u, method="EE", weighted=FALSE, level = 95),
    "Fixed effect - Cluster weighted " = rma(yi = wqelast, vi, data=df.wf.fe.c, method="EE",weights=w,level = 95),
    "Fixed effect - VAC"= rma(yi = wqelast, vi, data=df.wf.fe.vac, method="EE",weights=w,level = 95),
    "Random effect"= rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re)
  ),
  "Non-waterfront" = list(
    "Fixed efffet - Unweighted" = rma(yi = wqelast, vi, data=df.nwf.fe.u, method="EE", weighted=FALSE,level = 95),
    "Fixed effect - Cluster weighted"= rma(yi = wqelast, vi, data=df.nwf.fe.c, method="EE",weights=w,level = 95),
    "Fixed effect - VAC"= rma(yi = wqelast, vi, data=df.nwf.fe.vac, method="EE",weights=w,level = 95),
    "Random effect"= rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.nwf.re)
  )
)



f <- function(x) format(round(x, 3), big.mark=",")


gm <- list(
  list("raw" = "nobs", "clean" = "Num. of Obs.", "fmt" = f)
  )

modelsummary(
  panels,
  shape = "rbind",
  stars = TRUE, gof_map = gm,
  coef_rename = c("overall" = "Estimated mean"), output = "./results/table2.tex")


modelsummary(panels)

###################################################################################
# Simulate rma objects for benefit transfer
re.wf <- rma.mv(yi = elast_sim, vi, random = ~ 1 | cluster/obsid, data=df.wf.re)

t <- simulate.rma(re.wf, nsim = 1000, seed = 1234)
t

tt <- data.frame(t=unlist(t))

mean(tt$t)



p <- ggplot(data = tt, aes(x = t)) + 
  geom_density(binwidth = 0.003,
                 bins = 1000,
                 fill = "grey",
                 color = "black")
p
