# Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(sf)
library(ggpubr)


  

df_sk <- st_read("./results/value_map/SK/simu_value_map.shp")%>%
  mutate(total_value = vlpp250 + vlpp500)%>%
  mutate(totla_simu_value_lb = smvllbp250 + smvllbp500)%>%
  mutate(totla_simu_value_ub = smvlbp250 +smvlbp500)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "SK")%>%
  subset(total_value!=0)%>%
  subset(totla_simu_value_lb!=0)%>%
  subset(totla_simu_value_ub!=0)%>%
  select(total_value,totla_simu_value_lb,totla_simu_value_ub, province)

df_ab <- st_read("./results/value_map/AB/simu_value_map.shp")%>%
  mutate(total_value = vlpp250 + vlpp500)%>%
  mutate(totla_simu_value_lb = smvllbp250 + smvllbp500)%>%
  mutate(totla_simu_value_ub = smvlbp250 +smvlbp500)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "AB")%>%
  subset(total_value!=0)%>%
  subset(totla_simu_value_lb!=0)%>%
  subset(totla_simu_value_ub!=0)%>%
  select(total_value,totla_simu_value_lb,totla_simu_value_ub, province)

df_bc <- st_read("./results/value_map/BC/simu_value_map.shp")%>%
  mutate(total_value = vlpp250 + vlpp500)%>%
  mutate(totla_simu_value_lb = smvllbp250 + smvllbp500)%>%
  mutate(totla_simu_value_ub = smvlbp250 +smvlbp500)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "BC")%>%
  subset(total_value!=0)%>%
  subset(totla_simu_value_lb!=0)%>%
  subset(totla_simu_value_ub!=0)%>%
  select(total_value,totla_simu_value_lb,totla_simu_value_ub, province)

df_mb <- st_read("./results/value_map/MB/simu_value_map.shp")%>%
  mutate(total_value = vlpp250 + vlpp500)%>%
  mutate(totla_simu_value_lb = smvllbp250 + smvllbp500)%>%
  mutate(totla_simu_value_ub = smvlbp250 +smvlbp500)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "MB")%>%
  subset(total_value!=0)%>%
  subset(totla_simu_value_lb!=0)%>%
  subset(totla_simu_value_ub!=0)%>%
  select(total_value,totla_simu_value_lb,totla_simu_value_ub, province)


df_nb <- st_read("./results/value_map/NB/simu_value_map.shp")%>%
  mutate(total_value = vlpp250 + vlpp500)%>%
  mutate(totla_simu_value_lb = smvllbp250 + smvllbp500)%>%
  mutate(totla_simu_value_ub = smvlbp250 +smvlbp500)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "NB")%>%
  subset(total_value!=0)%>%
  subset(totla_simu_value_lb!=0)%>%
  subset(totla_simu_value_ub!=0)%>%
  select(total_value,totla_simu_value_lb,totla_simu_value_ub, province)

df_ns <- st_read("./results/value_map/NS/simu_value_map.shp")%>%
  mutate(total_value = vlpp250 + vlpp500)%>%
  mutate(totla_simu_value_lb = smvllbp250 + smvllbp500)%>%
  mutate(totla_simu_value_ub = smvlbp250 +smvlbp500)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "NS")%>%
  subset(total_value!=0)%>%
  subset(totla_simu_value_lb!=0)%>%
  subset(totla_simu_value_ub!=0)%>%
  select(total_value,totla_simu_value_lb,totla_simu_value_ub, province)


df_on <- st_read("./results/value_map/ON/simu_value_map.shp")%>%
  mutate(total_value = vlpp250 + vlpp500)%>%
  mutate(totla_simu_value_lb = smvllbp250 + smvllbp500)%>%
  mutate(totla_simu_value_ub = smvlbp250 +smvlbp500)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "ON")%>%
  subset(total_value!=0)%>%
  subset(totla_simu_value_lb!=0)%>%
  subset(totla_simu_value_ub!=0)%>%
  select(total_value,totla_simu_value_lb,totla_simu_value_ub, province)

df_qc <- st_read("./results/value_map/QC/simu_value_map.shp")%>%
  mutate(total_value = vlpp250 + vlpp500)%>%
  mutate(totla_simu_value_lb = smvllbp250 + smvllbp500)%>%
  mutate(totla_simu_value_ub = smvlbp250 +smvlbp500)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "QC")%>%
  subset(total_value!=0)%>%
  subset(totla_simu_value_lb!=0)%>%
  subset(totla_simu_value_ub!=0)%>%
  select(total_value,totla_simu_value_lb,totla_simu_value_ub, province)



data <- rbind(df_sk, df_ab, df_bc, df_mb,df_nb,df_ns, df_on, df_qc)
# all in one
p <- data%>%
  ggplot( aes(x=province, y=total_value, fill=province)) +
  geom_violin(width=1.5) +
  geom_boxplot(width=0.5, color="grey", alpha=0.4) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ylim(-10,500)+
  scale_y_continuous(limits = c(-1, 500))+
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ per capita")

p

#seperate

p1 <- data%>%
  subset(province == "SK")%>%
  ggplot( aes(x=province, y=total_value, fill=province)) +
  geom_violin(width=0.3) +
  geom_boxplot(width=0.05, color="grey", alpha=0.4) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ylim(0.1069307,500)+
  scale_y_continuous(limits = c(0.1069307, 500))+
  #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ per capita")

p1


p2 <- data%>%
  subset(province == "AB")%>%
  ggplot( aes(x=province, y=total_value, fill=province)) +
  geom_violin(width=0.3) +
  geom_boxplot(width=0.05, color="grey", alpha=0.4) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ylim(-10,500)+
  scale_y_continuous(limits = c(-10, 800))+
  #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ per capita")

p2

p3 <- data%>%
  subset(province == "BC")%>%
  ggplot( aes(x=province, y=total_value, fill=province)) +
  geom_violin(width=0.6) +
  geom_boxplot(width=0.3, color="grey", alpha=0.4) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ylim(-10,500)+
  scale_y_continuous(limits = c(-10, 1500))+
  #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ per capita")

p3

p4 <- data%>%
  subset(province == "MB")%>%
  ggplot( aes(x=province, y=total_value, fill=province)) +
  geom_violin(width=0.6) +
  geom_boxplot(width=0.2, color="grey", alpha=0.4) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ylim(-10,500)+
  scale_y_continuous(limits = c(-10, 500))+
  #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ per capita")

p4


p5 <- data%>%
  subset(province == "NB")%>%
  ggplot( aes(x=province, y=total_value, fill=province)) +
  geom_violin(width=0.6) +
  geom_boxplot(width=0.2, color="grey", alpha=0.4) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ylim(-10,500)+
  scale_y_continuous(limits = c(-10, 150))+
  #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ per capita")

p5

p6 <- data%>%
  subset(province == "NS")%>%
  ggplot( aes(x=province, y=total_value, fill=province)) +
  geom_violin(width=0.6) +
  geom_boxplot(width=0.2, color="grey", alpha=0.4) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ylim(-10,500)+
  scale_y_continuous(limits = c(-10, 500))+
  #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ per capita")

p6


p7 <- data%>%
  subset(province == "ON")%>%
  ggplot( aes(x=province, y=total_value, fill=province)) +
  geom_violin(width=0.6) +
  geom_boxplot(width=0.2, color="grey", alpha=0.4) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ylim(-10,500)+
  scale_y_continuous(limits = c(-10, 800))+
  #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ per capita")

p7

p8 <- data%>%
  subset(province == "QC")%>%
  ggplot( aes(x=province, y=total_value, fill=province)) +
  geom_violin(width=0.6) +
  geom_boxplot(width=0.2, color="grey", alpha=0.4) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ylim(-10,500)+
  scale_y_continuous(limits = c(-10, 1000))+
  #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ per capita")

p8



figure2 <- ggarrange(p5,p6,p7,p8,
                     labels = c("E", "F", "G", "H"),
                     ncol = 2, nrow = 2)
figure2


















############################################################################
# may be usful later
df_final <- read.csv("./results/simulation//simulation_results.csv")


data <- df_final %>% 
  select(tot_value_pp_sk)%>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

##############################################


p <- data%>%
  ggplot( aes(x=text, y=value, fill=text)) +
  geom_violin(width=1) +
  geom_boxplot(width=0.5, color="grey", alpha=0.4) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ylim(0,50)+
  scale_y_continuous(limits = c(0, 500))+
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ per capita")

p


ggplot(data, aes(x=text, y=value)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("cyl")










dat.hdi<-hBayesDM::HDIofMCMC(data$value)
mean_data <- mean(data$value)


df_sk <- st_read("./results/value_map/SK/bfp_count_cen_da_250m_value.shp")%>%
  mutate(vl_250m_pp = vl_250m/Popultn)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "SK")%>%
  subset(vl_250m_pp!=0)%>%
  select(vl_250m_pp, province)

dat.hdi<-hBayesDM::HDIofMCMC(df_sk$to)



ggplot(data, aes(x=value)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  geom_segment(x=dat.hdi[1],xend=dat.hdi[2],y=0,yend=0,color="blue",size=2,lineend="round")+
  geom_vline(xintercept = mean_data,
             color = "red", linetype = "dashed")+
  geom_vline(xintercept = dat.hdi,
             color = "red", linetype = "dashed")+
  theme_bw()









# violine plot
data %>%
  ggplot( aes(x=text, y=value, fill=text)) +
  geom_violin(width=0.5) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("A Violin wrapping a boxplot") +
  xlab("")


autoplot(data, type = "text")

p <- ggplot(data=data, aes(x=value, group=text, fill=text)) +
  geom_density(adjust=1.5, position="fill") +
  theme_ipsum()