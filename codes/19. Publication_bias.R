rm(list = ls())
library(dplyr)
library(meta)
library(metafor)
###############################################################################



df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")%>%
  mutate(vi = log(sampsize))%>%
  group_by(studyid,geog)%>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()


res <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df)


#main figure
funnel.rma(res, main="", pch = 20,back=NA, shade=NA,hlines="lightgray",ylab = "log (sample size)", xlab = "Elasticity")

#other options

funnel(res, yaxis="vi", main="Sampling Variance")
funnel(res, yaxis="seinv", main="Inverse Standard Error")
funnel(res, yaxis="vinv", main="Inverse Sampling Variance")


#classical Egger test
# waterfront
ranktest(wqelast, vi, data=df, digits=3, exact=FALSE)
regtest(wqelast, vi, data=df, model="lm")

coef_test(res, vcov = "CR2")