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
  kableExtra,
  fixest,
  viridis,
  ipsum
)

# load data - with outliers 
df_100 <- read.csv("./metadata/Meta_data_waterfront_100.csv")%>%
  mutate(obsid = row_number())
df_200 <- read.csv("./metadata/Meta_data_waterfront_200.csv")%>%
  mutate(obsid = row_number())
df_300 <- read.csv("./metadata/Meta_data_waterfront_300.csv")%>%
  mutate(obsid = row_number())
df_400 <- read.csv("./metadata/Meta_data_waterfront_400.csv")%>%
  mutate(obsid = row_number())
df_500 <- read.csv("./metadata/Meta_data_waterfront_500.csv")%>%
  mutate(obsid = row_number())
df_600 <- read.csv("./metadata/Meta_data_waterfront_600.csv")%>%
  mutate(obsid = row_number())
df_700 <- read.csv("./metadata/Meta_data_waterfront_700.csv")%>%
  mutate(obsid = row_number())
df_800 <- read.csv("./metadata/Meta_data_waterfront_800.csv")%>%
  mutate(obsid = row_number())
df_900 <- read.csv("./metadata/Meta_data_waterfront_900.csv")%>%
  mutate(obsid = row_number())
df_1000 <- read.csv("./metadata/Meta_data_waterfront_1000.csv")%>%
  mutate(obsid = row_number())

# set up data set with required variables and necessary weighting schemes

panels <- list(
  "Waterfront" = list(
    "Fixed efffet - Unweighted" = rma(yi = elast, vi, data=df_wf, method="EE", weighted=FALSE, level = 95),
    "Fixed effect - Cluster weighted " = rma(yi = cw_elast, vi, data=df_wf, method="EE",weighted=FALSE, level = 95),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= rma(yi = cw_elast, vi, data=df_wf, method="EE",weighted=T, level = 95),
    "Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))"= rma(yi = cw_elast, vi=vi_log, data=df_wf, method="EE",weighted=T, level = 95),
    #"Random effect (sample size variance)"= rma.mv(yi = cw_elast, vi, random = ~ 1 | cluster_id/obsid, data=df_wf),
    "Random effect (log(sample size variance))"= rma.mv(yi = elast, vi_log, random = ~ 1 | cluster_id/obsid, data=df_wf)
    
  ),
  "Non-waterfront" = list(
    "Fixed efffet - Unweighted" = rma(yi = elast, vi, data=df_nwf, method="EE", weighted=FALSE, level = 95),
    "Fixed effect - Cluster weighted " = rma(yi = cw_elast, vi, data=df_nwf, method="EE",weighted=FALSE, level = 95),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= rma(yi = cw_elast, vi, data=df_nwf, method="EE",weighted=T, level = 95),
    "Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))"= rma(yi = cw_elast, vi=vi_log, data=df_nwf, method="EE",weighted=T, level = 95),
    #"Random effect (sample size variance)"= rma.mv(yi = cw_elast, vi, random = ~ 1 | cluster_id/obsid, data=df_nwf),
    "Random effect (log(sample size variance))"= rma.mv(yi = elast, vi_log, random = ~ 1 | cluster_id/obsid, data=df_nwf, control=list(rel.tol=1e-8))
  )
)



panels_robu <- list(
  "Waterfront" = list(
    "Fixed efffet - Unweighted" = robust(panels$Waterfront$`Fixed efffet - Unweighted`, cluster = obsid),
    "Fixed effect - Cluster weighted " = robust(panels$Waterfront$`Fixed effect - Cluster weighted `, cluster = cluster_id),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= robust(panels$Waterfront$`Fixed effect - Cluster weighted - Varance Adjusted (sample size)`, cluster = cluster_id),
    "Fixed effect - Cluster weighted - Varance Adjusted"= robust(panels$Waterfront$`Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))`, cluster = cluster_id),
    #"Random effect (sample size variance)"= robust(panels$Waterfront$`Random effect (sample size variance)`, cluster = cluster_id),
    "Random effect "= robust(panels$Waterfront$`Random effect (log(sample size variance))`, cluster = cluster_id)
    ),
  "Non-waterfront" = list(
    "Fixed efffet - Unweighted" =robust(panels$`Non-waterfront`$`Fixed efffet - Unweighted`, cluster = obsid),
    "Fixed effect - Cluster weighted " = robust(panels$`Non-waterfront`$`Fixed effect - Cluster weighted `, cluster = cluster_id),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= robust(panels$`Non-waterfront`$`Fixed effect - Cluster weighted - Varance Adjusted (sample size)`, cluster = cluster_id),
    "Fixed effect - Cluster weighted - Varance Adjusted"= robust(panels$`Non-waterfront`$`Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))`, cluster = cluster_id),
    #"Random effect (sample size variance)"= robust(panels$`Non-waterfront`$`Random effect (sample size variance)`, cluster = cluster_id),
    "Random effect"= robust(panels$`Non-waterfront`$`Random effect (log(sample size variance))`, cluster = cluster_id)
    
  )
)

f <- function(x) format(round(x, 3), big.mark=",")


gm <- list(
  list("raw" = "nobs", "clean" = "Num. of Obs.", "fmt" = f)
)


modelsummary(
  panels_robu,
  shape = "rbind",
  stars = TRUE, gof_map = gm)


modelsummary(
  panels_robu,
  shape = "rbind",
  stars = TRUE, gof_map = gm,
  coef_rename = c("overall" = "Estimated mean"), output = "./results/Tables/table2.tex")


#############################################################################
#forestplot weight - waterfront
#unweight
res1<-rma(yi = elast, vi, data=df_wf, method="EE", weighted=FALSE, level = 95)
#clusterweight
res2 <- rma(yi = cw_elast, vi, data=df_wf, method="EE",weighted=FALSE, level = 95)
#cluster weight + log sample size
res3 <- rma(yi = cw_elast, vi=vi_log, data=df_wf, method="EE",weighted=T, level = 95)
#cluster weight + log sample size - random effect
res4 <- rma.mv(yi = elast, vi_log, random = ~ 1 | cluster_id/obsid, data=df_wf)

df_1 <- df_wf%>%
  select(study_name,cluster_id,obsid,w_1,sampsize)%>%
  group_by(study_name)%>%
  mutate(max_ss = max(sampsize))%>%
  mutate(min_ss = min(sampsize))%>%
  mutate(dif_ss = max_ss-min_ss)


df_1_1 <- df_1%>%
  filter(dif_ss ==0)

df_1_1$ss<-paste(df_1_1$sampsize)

df_1_2 <- df_1%>%
  filter(dif_ss !=0)

df_1_2$ss <-paste(df_1_2$min_ss,df_1_2$max_ss,sep="-")

df_1 <- rbind(df_1_1,df_1_2)%>%
  select(study_name,cluster_id,obsid,w_1,ss)


res1_1 <- weights(res1)%>%
  as.data.frame()%>%
  mutate(obsid = row_number())%>%
  rename("Weight" = ".")%>%
  mutate(Weight = Weight/100*401)%>%
  mutate(type = "  Unweight")%>%
  left_join(df_1)

res1_1$Weight <- round(res1_1$Weight, digits = 0)

df_inter <- df_wf%>%
  select(obsid,w_1)%>%
  rename(Weight = w_1)

res2_2 <- weights(res2)%>%
  as.data.frame()%>%
  mutate(obsid = row_number())%>%
  rename("Weight" = ".")%>%
  select(-Weight)%>%
  left_join(df_inter)%>%
  mutate(type = " Cluster weight")%>%
  left_join(df_1)



res3_3 <- weights(res3)%>%
  as.data.frame()%>%
  mutate(obsid = row_number())%>%
  rename("Weight" = ".")%>%
  mutate(Weight = Weight/100*401)%>%
  mutate(type = " Variance adjusted cluster weight")%>%
  left_join(df_1)
  

res4_4 <- weights(res4)%>%
  as.data.frame()%>%
  mutate(obsid = row_number())%>%
  rename("Weight" = ".")%>%
  mutate(Weight = Weight/100*401)%>%
  mutate(type = "Random Effect Weight")%>%
  left_join(df_1)
  

df <- rbind(res1_1,res2_2,res3_3,res4_4)

df1 <- res1_1%>%
  group_by(study_name)%>%
  summarise( n = n())


#df1 <- df1[order(df1$n),]

df1 <- df1%>%
  mutate(id=row_number())


df <- df%>%
  left_join(df1)


df$studyname<-paste(df$study_name,df$n,sep=", N=")
df$studyname1<-paste(df$studyname, "[",df$ss, "]" )


p <- df%>%
  mutate(order = fct_reorder(study_name,n)) %>%
  ggplot( aes(x=studyname1, y=Weight,fct_reorder(n))) +
  #geom_violin(width=0.9) +
  geom_boxplot(width=0.6, alpha=10000, aes(colour =type )) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  #ylim(-1,2)+
  scale_y_continuous(breaks=seq(0,2,1), limits = c(0,2.1)) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("") +
  ylab("Weight")

p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(color='Weight Type')+
  theme(panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line())+
  theme(legend.position = "bottom")+
  facet_grid(. ~ type)





##############################################################################
# some quality check works

# test corelation of weights


cor_test <- df_wf%>%
  select(cluster_w,cluster_ss_w,re_w)

library(corrplot)

corrplot(cor(cor_test),
         method = "number",
         type = "upper" # show only upper side
)



# un-weight
# feols
feols(wqelast ~ 1, data = df_wf, weights = ~ unweight_w, vcov = ~ cluster)
# metafor
res <- rma(yi = wqelast, vi, data=df_wf, method="EE", weighted=FALSE, level = 95)
robust(res, cluster=id)
# cluster weight
# feols
feols(wqelast ~ 1, data = df_wf, weights = ~ cluster_w, vcov = ~ cluster)
# metafor
res <- rma(yi = wqelast, vi, data=df_wf, method="EE", weights=cluster_w, level = 95)
robust(res, cluster=cluster)
# cluster and sample size  weight
# feols
feols(wqelast ~ 1, data = df_wf, weights = ~ cluster_ss_w, vcov = ~ cluster)
# metafor
res <- rma(yi = wqelast, vi, data=df_wf, method="EE", weights=cluster_ss_w, level = 95)
robust(res, cluster=cluster)

################################################################################

#forestplot weight - waterfront
#unweight
res1<-rma(yi = elast, vi, data=df_wf, method="EE", weighted=FALSE, level = 95)
#clusterweight
res2 <- rma(yi = cw_elast, vi, data=df_wf, method="EE",weighted=FALSE, level = 95)
#cluster weight + log sample size
res3 <- rma(yi = cw_elast, vi=vi_log, data=df_wf, method="EE",weighted=T, level = 95)
#cluster weight + log sample size - random effect
res4 <- rma.mv(yi = elast, vi_log, random = ~ 1 | cluster_id/obsid, data=df_wf)

df_1 <- df_wf%>%
  select(study_name,cluster_id,obsid,w_1,sampsize)%>%
  group_by(study_name)%>%
  mutate(max_ss = max(sampsize))%>%
  mutate(min_ss = min(sampsize))%>%
  mutate(dif_ss = max_ss-min_ss)


df_1_1 <- df_1%>%
  filter(dif_ss ==0)

df_1_1$ss<-paste(df_1_1$sampsize)

df_1_2 <- df_1%>%
  filter(dif_ss !=0)

df_1_2$ss <-paste(df_1_2$min_ss,df_1_2$max_ss,sep="-")

df_1 <- rbind(df_1_1,df_1_2)%>%
  select(study_name,cluster_id,obsid,w_1,ss)


res1_1 <- weights(res1)%>%
  as.data.frame()%>%
  mutate(obsid = row_number())%>%
  rename("Weight" = ".")%>%
  mutate(Weight = Weight/100*401)%>%
  mutate(type = "  Unweight")%>%
  left_join(df_1)

res1_1$Weight <- round(res1_1$Weight, digits = 0)

df_inter <- df_wf%>%
  select(obsid,w_1)%>%
  rename(Weight = w_1)

res2_2 <- weights(res2)%>%
  as.data.frame()%>%
  mutate(obsid = row_number())%>%
  rename("Weight" = ".")%>%
  select(-Weight)%>%
  left_join(df_inter)%>%
  mutate(type = " Cluster weight")%>%
  left_join(df_1)



res3_3 <- weights(res3)%>%
  as.data.frame()%>%
  mutate(obsid = row_number())%>%
  rename("Weight" = ".")%>%
  mutate(Weight = Weight/100*401)%>%
  mutate(type = " Variance adjusted cluster weight")%>%
  left_join(df_1)
  

res4_4 <- weights(res4)%>%
  as.data.frame()%>%
  mutate(obsid = row_number())%>%
  rename("Weight" = ".")%>%
  mutate(Weight = Weight/100*401)%>%
  mutate(type = "Random Effect Weight")%>%
  left_join(df_1)
  

df <- rbind(res1_1,res2_2,res3_3,res4_4)

df1 <- res1_1%>%
  group_by(study_name)%>%
  summarise( n = n())


#df1 <- df1[order(df1$n),]

df1 <- df1%>%
  mutate(id=row_number())


df <- df%>%
  left_join(df1)


df$studyname<-paste(df$study_name,df$n,sep=", N=")
df$studyname1<-paste(df$studyname, "[",df$ss, "]" )


p <- df%>%
  mutate(order = fct_reorder(study_name,n)) %>%
  ggplot( aes(x=studyname1, y=Weight,fct_reorder(n))) +
  #geom_violin(width=0.9) +
  geom_boxplot(width=0.6, alpha=10000, aes(colour =type )) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  #ylim(-1,2)+
  scale_y_continuous(breaks=seq(0,2,1), limits = c(0,2.1)) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("") +
  ylab("Weight")

p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(color='Weight Type')+
  theme(panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line())+
  theme(legend.position = "bottom")+
  facet_grid(. ~ type)

#############################################################################

#forestplot weight - waterfront
#unweight
res1<-rma(yi = elast, vi, data=df_nwf, method="EE", weighted=FALSE, level = 95)
#clusterweight
res2 <- rma(yi = cw_elast, vi, data=df_nwf, method="EE",weighted=FALSE, level = 95)
#cluster weight + log sample size
res3 <- rma(yi = cw_elast, vi=vi_log, data=df_nwf, method="EE",weighted=T, level = 95)
#cluster weight + log sample size - random effect
res4 <- rma.mv(yi = elast, vi_log, random = ~ 1 | cluster_id/obsid, data=df_nwf)

df_1 <- df_nwf%>%
  select(study_name,cluster_id,obsid,w_1,sampsize)%>%
  group_by(study_name)%>%
  mutate(max_ss = max(sampsize))%>%
  mutate(min_ss = min(sampsize))%>%
  mutate(dif_ss = max_ss-min_ss)


df_1_1 <- df_1%>%
  filter(dif_ss ==0)

df_1_1$ss<-paste(df_1_1$sampsize)

df_1_2 <- df_1%>%
  filter(dif_ss !=0)

df_1_2$ss <-paste(df_1_2$min_ss,df_1_2$max_ss,sep="-")

df_1 <- rbind(df_1_1,df_1_2)%>%
  select(study_name,cluster_id,obsid,w_1,ss)


res1_1 <- weights(res1)%>%
  as.data.frame()%>%
  mutate(obsid = row_number())%>%
  rename("Weight" = ".")%>%
  mutate(Weight = Weight/100*128)%>%
  mutate(type = "  Unweight")%>%
  left_join(df_1)

res1_1$Weight <- round(res1_1$Weight, digits = 0)

df_inter <- df_wf%>%
  select(obsid,w_1)%>%
  rename(Weight = w_1)

res2_2 <- weights(res2)%>%
  as.data.frame()%>%
  mutate(obsid = row_number())%>%
  rename("Weight" = ".")%>%
  select(-Weight)%>%
  left_join(df_inter)%>%
  mutate(type = " Cluster weight")%>%
  left_join(df_1)



res3_3 <- weights(res3)%>%
  as.data.frame()%>%
  mutate(obsid = row_number())%>%
  rename("Weight" = ".")%>%
  mutate(Weight = Weight/100*128)%>%
  mutate(type = " Variance adjusted cluster weight")%>%
  left_join(df_1)


res4_4 <- weights(res4)%>%
  as.data.frame()%>%
  mutate(obsid = row_number())%>%
  rename("Weight" = ".")%>%
  mutate(Weight = Weight/100*128)%>%
  mutate(type = "Random Effect Weight")%>%
  left_join(df_1)


df <- rbind(res1_1,res2_2,res3_3,res4_4)

df1 <- res1_1%>%
  group_by(study_name)%>%
  summarise( n = n())


#df1 <- df1[order(df1$n),]

df1 <- df1%>%
  mutate(id=row_number())


df <- df%>%
  left_join(df1)


df$studyname<-paste(df$study_name,df$n,sep=", N=")
df$studyname1<-paste(df$studyname, "[",df$ss, "]" )


p <- df%>%
  mutate(order = fct_reorder(study_name,n)) %>%
  ggplot( aes(x=studyname1, y=Weight,fct_reorder(n))) +
  #geom_violin(width=0.9) +
  geom_boxplot(width=0.6, alpha=10000, aes(colour =type )) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  #ylim(-1,2)+
  scale_y_continuous(breaks=seq(0,2,1), limits = c(0,2.1)) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("") +
  ylab("Weight")

p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(color='Weight Type')+
  theme(panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line())+
  theme(legend.position = "bottom")+
  facet_grid(. ~ type)







##############################################################################
# some quality check works

# test corelation of weights


cor_test <- df_wf%>%
  select(cluster_w,cluster_ss_w,re_w)

library(corrplot)

corrplot(cor(cor_test),
         method = "number",
         type = "upper" # show only upper side
)



# un-weight
# feols
feols(wqelast ~ 1, data = df_wf, weights = ~ unweight_w, vcov = ~ cluster)
# metafor
res <- rma(yi = wqelast, vi, data=df_wf, method="EE", weighted=FALSE, level = 95)
robust(res, cluster=id)
# cluster weight
# feols
feols(wqelast ~ 1, data = df_wf, weights = ~ cluster_w, vcov = ~ cluster)
# metafor
res <- rma(yi = wqelast, vi, data=df_wf, method="EE", weights=cluster_w, level = 95)
robust(res, cluster=cluster)
# cluster and sample size  weight
# feols
feols(wqelast ~ 1, data = df_wf, weights = ~ cluster_ss_w, vcov = ~ cluster)
# metafor
res <- rma(yi = wqelast, vi, data=df_wf, method="EE", weights=cluster_ss_w, level = 95)
robust(res, cluster=cluster)




