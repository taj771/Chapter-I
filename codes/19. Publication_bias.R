rm(list = ls())
library(dplyr)
library(meta)
library(metafor)
###############################################################################


# read file full data set
# load data
#load data
df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")
# without outliers
wf <- read.csv("./metadata/wo_outliers_wf.csv")
nwf <- read.csv("./metadata/wo_outliers_nwf.csv")


df.wf <- df[which(df$distbuf == 1), ]%>%
  mutate(vi = 1/sampsize)%>%
  #filter(vi > 0)%>%
  group_by(studyid,geog)%>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()
  # non-waterfrontv
df.nwf <- df[which(df$distbuf == 2), ]%>%
  mutate(vi = 1/sampsize)%>%
  #filter(vi > 0)%>%
  group_by(studyid,geog)%>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()

# waterfront
res.ee.wf <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf)
summary(res.ee.wf) 
funnel.rma(res.ee.wf, yaxis="vinv", ylim = c(1, 10000), main="" , back=NA, shade=NA, hlines="lightgray", pch = 20)

# non-waterfront
res.ee.nwf <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.nwf)
summary(res.ee.nwf) 

funnel.rma(res.ee.nwf, yaxis="vinv", ylim = c(1, 10000), main="Non-waterfront", back=NA, shade=NA, hlines="lightgray", pch = 20)


### set up 2x2 array for plotting
par(mfrow=c(1,1))

# random effect model
# waterfront
res.re.wf <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf)
funnel.rma(res.re.wf, yaxis="vinv", ylim = c(1, 10000), main="", back=NA, shade=NA, hlines="lightgray", pch = 20)

res.re.wf <- rma.mv(yi = wqelast, vi, mods =vi ,random = ~ 1 | cluster/obsid, data=df.wf, test = "t")
res.re.wf

# random effect model
# non-waterfront
res.re.nwf <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.nwf)
funnel.rma(res.re.nwf, yaxis="vinv", ylim = c(1, 10000), main="", back=NA, shade=NA, hlines="lightgray", pch = 20)

res.re.nwf <- rma.mv(yi = wqelast, vi, mods =vi ,random = ~ 1 | cluster/obsid, data=df.nwf, test = "t")
res.re.nwf


### classical Egger test
# waterfront
ranktest(wqelast, vi, data=df.wf, digits=3, exact=FALSE)
regtest(wqelast, vi, data=df.wf, model="lm")

# non-waterfront
regtest(wqelast, vi, data=df.nwf, model="lm")




### set up 2x2 array for plotting
par(mfrow=c(1,2))

### draw funnel plots - waterfront
#funnel(res.ee.wf, ylim = c(0, 1), main = "Standard Error")
#funnel(res.ee.wf, yaxis="vi",ylim = c(0, 1), main="Sampling Variance")
#funnel(res.ee.wf, yaxis="seinv",ylim = c(1,500), main="Inverse Standard Error")
funnel(res.ee.wf, yaxis="vinv", ylim = c(1, 10000), main="Waterfront", back=NA, shade=NA, hlines="lightgray", pch = 20)

### draw funnel plots - non waterfront
#funnel(res.ee.nwf, ylim = c(0, 1), main = "Standard Error")
#funnel(res.ee.nwf, yaxis="vi",ylim = c(0, 1), main="Sampling Variance")
#funnel(res.ee.nwf, yaxis="seinv",ylim = c(1,500), main="Inverse Standard Error")
funnel(res.ee.nwf, yaxis="vinv", ylim = c(1, 10000), main="Non-waterfront",  back=NA, shade=NA, hlines="lightgray", pch = 20)
 

#predictor="sei" for the standard errors (the default),
#predictor="vi" for the sampling variances,
#predictor="ni" for the sample sizes,
#predictor="ninv" for the inverse of the sample sizes,
#predictor="sqrtni" for the square root of the sample sizes, or
#predictor="sqrtninv" for the inverse square root of the sample sizes

### classical Egger test
# waterfront
ranktest(wqelast, vi, data=df.wf, digits=3, exact=FALSE)
regtest(wqelast, vi, data=df.wf, model="lm")

# non-waterfront
regtest(wqelast, vi, data=df.nwf, model="lm")

# trim and fill

taf <- trimfill(res.ee.wf)
taf

