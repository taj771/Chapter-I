###########################################################################
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

#load data
df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")

# plot graph to elicit outliers
p <- ggplot(df, aes(x = elast_sim)) +
  geom_histogram(alpha = 20, binwidth = 0.1) + 
  scale_x_continuous(limits = c(-5, 10))+
  theme_bw()+
  labs(x = "Elasticity", y = "Density")

p 


## Subset: Main models
# waterfront
df.wf <- df[which(df$distbuf == 1), ]

# non-waterfront
df.nwf <- df[which(df$distbuf == 2), ]
#df.nwf$sampsize <- as.numeric(as.character(df.nwf$sampsize))


df.wf.re <- df.wf%>%
  mutate(vi = 1/sampsize)%>% # use sample size instead of variance
  #filter(vi>0)%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()%>%
  rownames_to_column("id")

df.nwf.re <- df.nwf%>%
  mutate(vi = 1/sampsize)%>% # use sample size instead of variance
  #filter(vi>0)%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()%>%
  rownames_to_column("id")

#waterfront 
re.wf <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re)

rstud_wf <- rstudent.rma.mv(re.wf, cluster = df.wf.re$cluster )

rstud_wf <- rstud_wf$obs%>%
  as.data.frame()

rstud_wf <- rownames_to_column(rstud_wf, "obsid")

rstud_wf$obsid <- as.numeric(as.character(rstud_wf$obsid))

#use row id for the graph, obsid is different, that is equal to obs id in meta
# analysis
rstud_wf <- mutate(rstud_wf, id = row_number())


rstud_wf%>%
  ggplot(aes(x=obsid,y=resid))+
  ylab("rstudent")+
  geom_line()+
  geom_point()+
  scale_x_continuous(limits = c(0, 270), breaks = seq(0, 270, 10))+
  geom_hline(yintercept = 2)+
  geom_hline(yintercept = -2)+
  theme_bw()

# Cooks distance if want
#cook_dis_wf <- cooks.distance(re.wf, cluster = df.wf.re$cluster)
#plot(cooks.distance(re.wf), type="o", pch=19)

rstud_wf <- rstud_wf%>%
  filter(between(resid,-2,2))


df.wf.re <- df.wf.re%>%
  subset(df.wf.re$id%in% rstud_wf$obsid)

# write csv for future analysis

write.csv(df.wf.re, "./metadata/wo_outliers_wf.csv")


# non-waterfront

re.nwf <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.nwf.re)

rstud_nwf <- rstudent.rma.mv(re.nwf, cluster = df.nwf.re$cluster )

rstud_nwf <- rstud_nwf$obs%>%
  as.data.frame()

rstud_nwf <- rownames_to_column(rstud_nwf, "obsid")

rstud_nwf$obsid <- as.numeric(as.character(rstud_nwf$obsid))

#use row id for the graph, obsid is different, that is equal to obs id in meta
# analysis
rstud_nwf <- mutate(rstud_nwf, id = row_number())


rstud_nwf%>%
  ggplot(aes(x=id,y=resid))+
  ylab("rstudent")+
  geom_line()+
  geom_point()+
  scale_x_continuous(limits = c(0, 140), breaks = seq(0, 140, 10))+
  geom_hline(yintercept = 2)+
  geom_hline(yintercept = -2)+
  theme_bw()

rstud_nwf <- rstud_nwf%>%
  filter(between(resid,-2,2))

df.nwf.re <- mutate(df.nwf.re, id = row_number())


df.nwf.re <- df.nwf.re%>%
  subset(df.nwf.re$id%in% rstud_nwf$obsid)

# write csv for future analysis

write.csv(df.nwf.re, "./metadata/wo_outliers_nwf.csv")

