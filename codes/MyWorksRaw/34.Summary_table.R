# clear memory
rm(list = ls())
library(dplyr)


################################################################################
# Summary table of meta data - table 1 in manuscript

# load data - with outliers 
df_wf <- read.csv("./metadata/meta_data_distance_250m.csv")

df_nwf <- read.csv("./metadata/meta_data_distance_750m.csv")

df <- rbind(df_wf,df_nwf)%>%
  rename("elast"="y_value")


df1 <- read.csv("./metadata/Meta_dataset_water_clarity_TJ.csv")


region <- df1%>%
  select(studyname,state)

samplesize <- df%>%
  select(study_name,sampsize)%>%
  group_by(study_name)%>%
  mutate(min_sam = min(sampsize))%>%
  mutate(max_sam = max(sampsize))%>%
  mutate(d = max_sam-min_sam)%>%
  mutate(c = paste(min_sam, max_sam, sep='-'))%>%
  distinct(study_name,min_sam,max_sam, .keep_all = T)%>%
  mutate(sample_n = case_when(d==0~paste0(sampsize),
                              d > 0 ~ paste0(c)))%>%
  select(study_name,sample_n)

mu_e <- df%>%
  ungroup()%>%
  select(study_name, elast)%>%
  group_by(study_name)%>%
  mutate(mu_e = mean(elast))%>%
  distinct(study_name, mu_e)


function_form <- df%>%
  select(study_name,funcform)%>%
  group_by(study_name,funcform)%>%
  distinct(study_name,funcform)

region <- df%>%
  select(study_name,west,midwest,south,northeast,multireg)%>%
  group_by(study_name)%>%
  distinct()%>%
  mutate(region = case_when(west ==1 ~ "West",
                            midwest==1 ~ "midwest",
                            south ==1 ~ "south",
                            northeast ==1 ~ "Nort East",
                            multireg ==1 ~ "Multi Regional"))%>%
  select(study_name,region)%>%
  mutate(region = ifelse(study_name == "Clapper & Caudill 2014", "Canada", region))%>%
  mutate(region = ifelse(study_name == "Calder & Arrieta 2019", "Canada", region))%>%
  mutate(region = ifelse(study_name == "TJ, Pat, Richard", "Canada", region))




state <- df1%>%
  select(studyname,state)%>%
  group_by(studyname,state)%>%
  rename("study_name"="studyname")%>%
  distinct()


table <- df%>%
  group_by(study_name)%>%
  summarise( n = n())%>%
  left_join(state)%>%
  left_join(samplesize)%>%
  left_join(function_form)%>%
  left_join(region)%>%
  left_join(mu_e)%>%
  distinct(study_name, .keep_all = T)

library(xtable)

print(xtable(table, type = "latex"), file = "./results/Tables/metadata_summary.tex")



########################################

# boxplot all in one - elasticities 

df_wf <- read.csv("./metadata/Meta_data_waterfront.csv")%>%
  select(study_name, elast)
df_nwf <- read.csv("./metadata/Meta_data_nonwaterfront.csv")%>%
  select(study_name, elast)


df <- rbind(df_wf,df_nwf)

data <- df%>%
  select(elast)

my_variable=c(data$elast)

layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

par(mar=c(0, 3.1, 1.1, 2.1))

boxplot(my_variable , horizontal=TRUE , ylim=c(-1,2), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(my_variable , breaks=250 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="Elasticity", xlim=c(-1,2))


#############################################################################

#Individual elasticity forest plot 

########################## for all elasticity within 2km

library(ggplot2)
library(viridis)
library(hrbrthemes)

df <- read.csv("./metadata/meta_data_all_distance.csv")%>%
  filter(study_name != "Moore et al 2020")%>%
  filter(study_name !="Wolf & Kemp 2021")

df1 <- df %>%
  #distinct(id, .keep_all = T)%>%
  group_by(study_name)%>%
  summarise( n = n())

df <- df%>%
  left_join(df1)
df$study_name <- gsub("TJ, Pat, Richard", "Jayalath et al 2024", df$study_name)


df$studyname<-paste(df$study_name,df$n,sep=", N=")

p <- df%>%
  ggplot( aes(x=elast, y=study_name)) +
  #geom_violin(width=0.9) +
  geom_boxplot(position = position_dodge(width=1)) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  #ylim(-1,2)+
  scale_x_continuous(breaks=seq(-1,3,0.5), limits = c(-0.25,1)) +
  #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  ylab("") +
  xlab("Change in property value %")

p

p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(xintercept = c(0), linetype = "dotted")






############################ for different distance bins 

#df_100 <- read.csv("./metadata/Meta_data_waterfront_100.csv")%>%
  #select(study_name, elast)

#load geomdata

library(hrbrthemes)
library(viridis)

df <- read.csv("./metadata/meta_data_distance_250m.csv")%>%
  mutate(obsid = row_number())%>%
  #filter(obsid != 168)%>% # outlier - 1:Swedberg el al 2020 - Multi-state model
  #filter(obsid != 167)%>% # Swedberg el al 2020 - State level model - Maine
  #filter(obsid != 155)%>%# Swedberg el al 2020 - Multi-state model
  group_by(study_name,geog)%>%
  mutate(cluster_id = cur_group_id())

df$study_name <- gsub("TJ, Pat, Richard", "Jayalath et al 2024", df$study_name)

df1 <- df %>%
  #distinct(id, .keep_all = T)%>%
  group_by(study_name)%>%
  summarise( n = n())

df <- df%>%
  left_join(df1)

df$studyname<-paste(df$study_name,df$n,sep=", N=")

p <- df%>%
  ggplot( aes(x=elast, y=studyname)) +
  #geom_violin(width=0.9) +
  geom_boxplot(position = position_dodge(width=1)) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  #ylim(-1,2)+
  scale_x_continuous(breaks=seq(-1,3,0.5), limits = c(-0.25,1)) +
  #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  ylab("") +
  xlab("Change in property value %")

p

p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(xintercept = c(0), linetype = "dotted")



df <- read.csv("./metadata/meta_data_distance_750m.csv")%>%
  mutate(obsid = row_number())
  #filter(obsid != 121)%>% # outlier - 1:Swedberg el al 2020 - Multi-state model
  #filter(obsid != 133)%>% # Swedberg el al 2020 - State level model - Maine
  #filter(obsid != 134) # Swedberg el al 2020 - Multi-state model

df1 <- df %>%
  #distinct(id, .keep_all = T)%>%
  group_by(study_name)%>%
  summarise( n = n())

df <- df%>%
  left_join(df1)

df$studyname<-paste(df$study_name,df$n,sep=", N=")

p <- df%>%
  ggplot( aes(x=elast, y=studyname)) +
  #geom_violin(width=0.9) +
  geom_boxplot(position = position_dodge(width=1)) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  #ylim(-1,2)+
  scale_x_continuous(breaks=seq(-1,3,0.5), limits = c(-0.25,1)) +
  #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  ylab("") +
  xlab("Change in property value %")

p

p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(xintercept = c(0), linetype = "dotted")



#df <- read.csv("./metadata/Meta_dataset_from_geomsmooth_filter.csv")


##################################################

# load data - with outliers 
df_100 <- read.csv("./metadata/Meta_data_waterfront_100.csv")%>%
  mutate(obsid = row_number())

#forestplot weight - waterfront
#unweight
res1<-rma(yi = elast, vi=vi_log, data=df_100, method="EE", weighted=FALSE, level = 95)
#clusterweight
res2 <- rma(yi = elast, vi=vi_log, data=df_100, method="EE",weighted=T,weights = w_1, level = 95)
#cluster weight + log sample size
res3 <- rma(yi = elast, vi=vi_log, data=df_100, method="EE",weighted=T,weights =w_2, level = 95)
#cluster weight + log sample size - random effect
res4 <- rma.mv(yi = elast, vi_log, random = ~ 1 | cluster_id/obsid, data=df_100)

df_1 <- df_100%>%
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
  mutate(Weight = (Weight/100)*401)%>%
  mutate(type = "  Unweight")%>%
  left_join(df_1)


df_inter <- df_100%>%
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

