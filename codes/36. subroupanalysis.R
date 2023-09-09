# clear memories
rm(list = ls())
library(dplyr)
library(meta)
library(metafor)
library(tidyverse)
library(robumeta)
library(clubSandwich)
library(Hmisc)

# load data - without outliers 
#wf <- read.csv("./metadata/wo_outliers_wf.csv")
#nwf <- read.csv("./metadata/wo_outliers_nwf.csv")

df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")


df.wf.re <- df[which(df$distbuf == 1), ]%>%
  mutate(region = case_when(west==1~"west",
                            midwest==1~"midwest",
                            south==1~"south",
                            northeast==1~"northeast",
                            canada==1~"canada",
                            multireg==1~"multireg"))%>%
  mutate(vi = 1/sampsize)%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()

# full sample

re.wf <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re)

# seperate meta analysis - sub groups

res11 <- list(rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=west==1),
             rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=midwest==1),
             rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=south==1),
             rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=northeast==1),
             rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=multireg==1),
             rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=canada==1))

modelsummary(res11)


comp11 <- data.frame(estimate = sapply(res11, coef),
                    stderror = sqrt(sapply(res11, vcov)),
                    k        = sapply(res11, \(x) x$k))
rownames(comp11) <- paste0("region", 1:6)
t <- round(comp11, digits=2)


wld <- rma(estimate, sei=stderror, mods = ~ rownames(comp11), data=comp11, method="FE")
anova(wld)

# tau
res2 <- rma.mv(yi=wqelast, vi, mods = ~ 0 + region, random = ~ region | obsid,
               struct="DIAG", data=df.wf.re)
res0 <- update(res2, struct="ID")
anova(res0, res2)


west <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=west==1)
midwest <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=midwest==1)
south <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=south==1)
northeast <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=northeast==1)
multireg <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=multireg==1)
canada <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=canada==1)

# simulate
coef_full <- rnorm(10000, coef(re.wf)[1], sqrt(vcov(re.wf)[1,1]))
coef_west <- rnorm(10000, coef(west)[1], sqrt(vcov(west)[1,1]))
coef_midwest <- rnorm(10000, coef(midwest)[1], sqrt(vcov(midwest)[1,1]))
coef_south <- rnorm(10000, coef(south)[1], sqrt(vcov(south)[1,1]))
coef_northeast <- rnorm(10000, coef(northeast)[1], sqrt(vcov(northeast)[1,1]))
coef_multireg <- rnorm(10000, coef(multireg)[1], sqrt(vcov(multireg)[1,1]))
coef_canada <- rnorm(10000, coef(canada)[1], sqrt(vcov(canada)[1,1]))

# Poe test
library(mded)

mded(coef_full,coef_west, detail = T, independent = T)
mded(coef_midwest,coef_full, detail = T, independent = T)
mded(coef_full,coef_south, detail = T, independent = T)
mded(coef_full,coef_northeast, detail = T, independent = T)
mded(coef_full,coef_multireg, detail = T, independent = T)
mded(coef_full,coef_canada, detail = T, independent = T)


#plot
full = data.frame(coef_full)%>%
  rename("coef"="coef_full")
full = data.frame(append(full, c(group='full'), after=1))
west = data.frame(coef_west)%>%
  rename("coef"="coef_west")
west = data.frame(append(west, c(group='west'), after=1))
midwest = data.frame(coef_midwest)%>%
  rename("coef"="coef_midwest")
midwest = data.frame(append(midwest, c(group='midwest'), after=1))
south = data.frame(coef_south)%>%
  rename("coef"="coef_south")
south = data.frame(append(south, c(group='south'), after=1))
northeast = data.frame(coef_northeast)%>%
  rename("coef"="coef_northeast")
northeast = data.frame(append(northeast, c(group='northeast'), after=1))
multireg = data.frame(coef_multireg)%>%
  rename("coef"="coef_multireg")
multireg = data.frame(append(multireg, c(group='multireg'), after=1))
canada = data.frame(coef_canada)%>%
  rename("coef"="coef_canada")
canada = data.frame(append(canada, c(group='canada'), after=1))

data <- rbind(full,west,midwest,south,northeast,multireg,canada)

cols <- c("cornflowerblue", "red","orangered4","mediumpurple","lightpink4","seagreen","lightskyblue")

p <- ggplot(data, aes(x = coef, fill = group)) +
  geom_density(adjust=10,alpha = 0.4) + 
  scale_x_continuous(limits = c(-1, 1.5))+
  labs(y="Density",x="Elasticity")+
  scale_fill_manual(values = cols,
                    name = "",
                    breaks = c("west","midwest","northeast","south","canada","full","multireg"),
                    labels = c("west","midwest","northeast","south","canada","full","multireg"))+
  theme_bw()+
  ggtitle("")+
  theme(plot.title = element_text(hjust = 0.5))

p

###########################################################################################################

#non waterfront
df.nwf.re <- df[which(df$distbuf == 2), ]%>%
  mutate(region = case_when(west==1~"west",
                            midwest==1~"midwest",
                            south==1~"south",
                            northeast==1~"northeast",
                            canada==1~"canada",
                            multireg==1~"multireg"))%>%
  mutate(vi = 1/sampsize)%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()

# full sample

re.nwf <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.nwf.re)

# seperate meta analysis - sub groups

res11 <- list(rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.nwf.re, subset=midwest==1),
              rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.nwf.re, subset=south==1),
              rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.nwf.re, subset=northeast==1),
              rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.nwf.re, subset=multireg==1),
              rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.nwf.re, subset=canada==1))


comp11 <- data.frame(estimate = sapply(res11, coef),
                     stderror = sqrt(sapply(res11, vcov)),
                     k        = sapply(res11, \(x) x$k))
rownames(comp11) <- paste0("region", 1:5)
round(comp11, digits=2)

tt <- round(comp11, digits=2)

ttt <- rbind(t,tt)

library(xtable)
print.xtable(xtable(head(ttt)), file = "./results/subgroup.tex")

wld <- rma(estimate, sei=stderror, mods = ~ rownames(comp11), data=comp11, method="FE")
anova(wld)

# tau
res2 <- rma.mv(yi=elast_sim, vi, mods = ~ 0 + region, random = ~ region | obsid,
               struct="DIAG", data=df.nwf.re)
res0 <- update(res2, struct="ID")
anova(res0, res2)



midwest <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.nwf.re, subset=midwest==1)
south <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.nwf.re, subset=south==1)
northeast <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.nwf.re, subset=northeast==1)
multireg <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.nwf.re, subset=multireg==1)
canada <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df.nwf.re, subset=canada==1)




# simulate
coef_full <- rnorm(10000, coef(re.nwf)[1], sqrt(vcov(re.wf)[1,1]))
coef_midwest <- rnorm(10000, coef(midwest)[1], sqrt(vcov(midwest)[1,1]))
coef_south <- rnorm(10000, coef(south)[1], sqrt(vcov(south)[1,1]))
coef_northeast <- rnorm(10000, coef(northeast)[1], sqrt(vcov(northeast)[1,1]))
coef_multireg <- rnorm(10000, coef(multireg)[1], sqrt(vcov(multireg)[1,1]))
coef_canada <- rnorm(10000, coef(canada)[1], sqrt(vcov(canada)[1,1]))


# Poe test
library(mded)

mded(coef_midwest,coef_full, detail = T, independent = T)
mded(coef_full,coef_south, detail = T, independent = T)
mded(coef_full,coef_northeast, detail = T, independent = T)
mded(coef_full,coef_multireg, detail = T, independent = T)
mded(coef_full,coef_canada, detail = T, independent = T)


#plot
full = data.frame(coef_full)%>%
  rename("coef"="coef_full")
full = data.frame(append(full, c(group='full'), after=1))
midwest = data.frame(coef_midwest)%>%
  rename("coef"="coef_midwest")
midwest = data.frame(append(midwest, c(group='midwest'), after=1))
south = data.frame(coef_south)%>%
  rename("coef"="coef_south")
south = data.frame(append(south, c(group='south'), after=1))
northeast = data.frame(coef_northeast)%>%
  rename("coef"="coef_northeast")
northeast = data.frame(append(northeast, c(group='northeast'), after=1))
multireg = data.frame(coef_multireg)%>%
  rename("coef"="coef_multireg")
multireg = data.frame(append(multireg, c(group='multireg'), after=1))
canada = data.frame(coef_canada)%>%
  rename("coef"="coef_canada")
canada = data.frame(append(canada, c(group='canada'), after=1))

data <- rbind(full,midwest,south,northeast,multireg,canada)

cols <- c("cornflowerblue","orangered4","mediumpurple","lightpink4","seagreen","lightskyblue")

p <- ggplot(data, aes(x = coef, fill = group)) +
  geom_density(adjust=10,alpha = 0.4) + 
  scale_x_continuous(limits = c(-0.6, 0.9))+
  labs(y="Density",x="Elasticity")+
  scale_fill_manual(values = cols,
                    name = "",
                    breaks = c("west","midwest","northeast","south","canada","full","multireg"),
                    labels = c("west","midwest","northeast","south","canada","full","multireg"))+
  theme_bw()+
  ggtitle("")+
  theme(plot.title = element_text(hjust = 0.5))

p








#plot
full = data.frame(coef_full)%>%
  rename("coef"="coef_full")
full = data.frame(append(full, c(group='full'), after=1))
midwest = data.frame(coef_midwest)%>%
  rename("coef"="coef_midwest")
midwest = data.frame(append(midwest, c(group='midwest'), after=1))
south = data.frame(coef_south)%>%
  rename("coef"="coef_south")
south = data.frame(append(south, c(group='south'), after=1))

data <- rbind(full,midwest,south)

cols <- c("cornflowerblue", "red","orangered4","mediumpurple","lightpink4","seagreen","lightskyblue")

p <- ggplot(data, aes(x = coef, fill = group)) +
  geom_density(adjust=10,alpha = 0.4) + 
  scale_x_continuous(limits = c(-1, 1.5))+
  labs(y="Density",x="Elasticity")+
  scale_fill_manual(values = cols,
                    name = "",
                    breaks = c("west","midwest","northeast","south","canada","full","multireg"),
                    labels = c("west","midwest","northeast","south","canada","full","multireg"))+
  theme_bw()+
  ggtitle("Non-Waterfront")+
  theme(plot.title = element_text(hjust = 0.5))

p


























##############################################################################

df.wf.re <- df%>%
  mutate(vi = elast_sim_se^2)%>%
  filter(canada==1)%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()


wf.re <- rma.mv(yi = elast_sim, vi, random = ~ 1 | cluster/obsid, data=df.wf.re)


wi.wf <- 1 / (sum(wf.re$sigma2) + df.wf.re$vi)



w.wf <- data.frame(k=c(table(df.wf.re$obsid)),
                   weight = tapply(wi.wf,df.wf.re$obsid, sum))


library(tibble)
w.wf <- rownames_to_column(w.wf, var="obsid")
w.wf$obsid <- as.numeric(w.wf$obsid)

# join weights

df.wf.re1 <- df.wf.re%>%
  select(elast_sim,elast_sim_se,cluster,studyid,obsid,geog,vi)%>%
  left_join(w.wf)%>%
  mutate(weighted_elas = weight*elast_sim)%>%
  group_by(studyid, geog)%>%
  mutate(w_elas = sum(weighted_elas))%>%
  distinct(cluster, .keep_all = T)


# loop - drop each observation and calculate elasticity for each observation

result <- list()

#loop through obsid and extract betas
for(i in unique(df.wf.re$cluster)){
  
  #construct linear model (metafor package)
  elas <- rma.mv(yi = elast_sim, vi, random = ~ 1 | cluster/obsid, data = subset(df.wf.re, df.wf.re$cluster != i), level = 95 )
  
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





res3 <- rma(yi=elast_sim, vi, mods = ~ wbtype, scale = ~ wbtype, data=df.wf.re)
permutest(res3, seed=1234)


res12 <- list(rma.mv(yi = elast_sim, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=funcform=="lin-log"),
              rma.mv(yi = elast_sim, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=funcform=="double-log"),
              rma.mv(yi = elast_sim, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=funcform=="log-lin"),
              rma.mv(yi = elast_sim, vi, random = ~ 1 | cluster/obsid, data=df.wf.re, subset=funcform=="linear"))

comp12 <- data.frame(estimate = sapply(res12, coef),
                     stderror = sqrt(sapply(res12, vcov)),
                     k        = sapply(res12, \(x) x$k))
rownames(comp12) <- paste0("grade", 1:4)
round(comp12, digits=4)

wld <- rma(estimate, sei=stderror, mods = ~ rownames(comp12), data=comp12, method="FE")
anova(wld)



df.wf.re <- df%>%
  mutate(vi = elast_sim_se^2)%>%
  filter(funcform=="linear")%>%
  group_by(studyid, geog) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()


wf.re <- rma.mv(yi = elast_sim, vi, random = ~ 1 | cluster/obsid, data=df.wf.re)


wi.wf <- 1 / (sum(wf.re$sigma2) + df.wf.re$vi)



w.wf <- data.frame(k=c(table(df.wf.re$obsid)),
                   weight = tapply(wi.wf,df.wf.re$obsid, sum))


library(tibble)
w.wf <- rownames_to_column(w.wf, var="obsid")
w.wf$obsid <- as.numeric(w.wf$obsid)

# join weights

df.wf.re1 <- df.wf.re%>%
  select(elast_sim,elast_sim_se,cluster,studyid,obsid,geog,vi)%>%
  left_join(w.wf)%>%
  mutate(weighted_elas = weight*elast_sim)%>%
  group_by(studyid, geog)%>%
  mutate(w_elas = sum(weighted_elas))%>%
  distinct(cluster, .keep_all = T)


