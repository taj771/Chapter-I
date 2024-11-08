# Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(ggpubr)
library(sf)
library(tidyverse)


df_sim_final <- read_csv("./results/simulation/simulation_final_df.csv")

df_all <- df_sim_final%>%
  #subset(prov == "QC")%>%
  select(GeoUID,val_wf,val_nwf,val_tot,sim_id,elast_200m,elast_600m,prov,bfp_200,bfp_600)%>%
  mutate(bfp_all = bfp_200+bfp_600)%>%
  
  group_by(GeoUID,prov)%>%
  mutate(ave_tot = mean(val_tot))%>%
  mutate(ave_bfp = mean(bfp_all))%>%
  distinct(GeoUID, .keep_all = T)



df_sk <- df_all%>%
  select(ave_tot,ave_bfp,prov)%>%
  filter(prov =="SK")%>%
  mutate(vl_pp = ave_tot/ave_bfp)

df_ab <- df_all%>%
  select(ave_tot,ave_bfp,prov)%>%
  filter(prov =="AB")%>%
  mutate(vl_pp = ave_tot/ave_bfp)

df_bc <- df_all%>%
  select(ave_tot,ave_bfp,prov)%>%
  filter(prov =="BC")%>%
  mutate(vl_pp = ave_tot/ave_bfp)

df_mb <- df_all%>%
  select(ave_tot,ave_bfp,prov)%>%
  filter(prov =="MB")%>%
  mutate(vl_pp = ave_tot/ave_bfp)

df_nb <- df_all%>%
  select(ave_tot,ave_bfp,prov)%>%
  filter(prov =="NB")%>%
  mutate(vl_pp = ave_tot/ave_bfp)

df_ns <- df_all%>%
  select(ave_tot,ave_bfp,prov)%>%
  filter(prov =="NS")%>%
  mutate(vl_pp = ave_tot/ave_bfp)

df_on <- df_all%>%
  select(ave_tot,ave_bfp,prov)%>%
  filter(prov =="ON")%>%
  mutate(vl_pp = ave_tot/ave_bfp)

df_qc <- df_all%>%
  select(ave_tot,ave_bfp,prov)%>%
  filter(prov =="QC")%>%
  mutate(vl_pp = ave_tot/ave_bfp)

df_pe <- df_all%>%
  select(ave_tot,ave_bfp,prov)%>%
  filter(prov =="PE")%>%
  mutate(vl_pp = ave_tot/ave_bfp)


data <- rbind(df_sk, df_ab, df_bc, df_mb,df_nb,df_ns, df_on, df_qc,df_pe)

# all in one
p <- data%>%
  ggplot( aes(x=prov, y=ave_tot, fill=prov)) +
  #geom_violin(width=0.9) +
  geom_boxplot(width=0.15, color="grey", alpha=0.9) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ylim(0,500)+
  scale_y_continuous(limits = c(0, 70000),breaks=seq(0, 70000, 5000))+
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("Total capitalized value ($)")+
  theme(panel.background = element_blank())

p 

p+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  guides(fill="none")



p <- data%>%
  ggplot( aes(x=prov, y=vl_pp, fill=prov)) +
  #geom_violin(width=1) +
  geom_boxplot(width=0.15, color="grey", alpha=0.9) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  scale_y_continuous(limits = c(10, 1500),breaks=seq(0, 1500, 100))+
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ Value per household")


p


p+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  guides(fill="none")


###############################################################################





# seperate pp plots
p1 <- data%>%
  subset(province == "SK")%>%
  ggplot( aes(x=province, y=vl_250m_pp, fill=province)) +
  geom_violin(width=0.2) +
  geom_boxplot(width=0.02, color="grey", alpha=0.4) +
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
  ggplot( aes(x=province, y=vl_pp, fill=province)) +
  geom_violin(width=0.2) +
  geom_boxplot(width=0.02, color="grey", alpha=0.4) +
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
  geom_violin(width=0.2) +
  geom_boxplot(width=0.02, color="grey", alpha=0.4) +
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

p3

p4 <- data%>%
  subset(province == "MB")%>%
  ggplot( aes(x=province, y=total_value, fill=province)) +
  geom_violin(width=0.2) +
  geom_boxplot(width=0.02, color="grey", alpha=0.4) +
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

figure1 <- ggarrange(p1,p2,p3,p4,
                     labels = c("A", "B", "C", "D"),
                     ncol = 2, nrow = 2)
figure1

p5 <- data%>%
  subset(province == "NB")%>%
  ggplot( aes(x=province, y=total_value, fill=province)) +
  geom_violin(width=0.2) +
  geom_boxplot(width=0.02, color="grey", alpha=0.4) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ylim(-10,500)+
  scale_y_continuous(limits = c(-10, 100))+
  #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ per capita")

p5

p6 <- data%>%
  subset(province == "NS")%>%
  ggplot( aes(x=province, y=total_value, fill=province)) +
  geom_violin(width=0.2) +
  geom_boxplot(width=0.02, color="grey", alpha=0.4) +
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

p6


figure2 <- ggarrange(p5,p5,
                     labels = c("E", "F", "G", "H"),
                     ncol = 2, nrow = 2)
figure2
