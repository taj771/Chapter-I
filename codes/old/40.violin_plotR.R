# Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(ggpubr)
library(sf)



df_sk <- st_read("./results/value_map/SK/sk_value_map.shp")%>%
  mutate(vl_pp = Ttl_vl_)%>%
  mutate(vl_tot = Totl_vl)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "SK")%>%
  select(vl_pp,vl_tot, province)

df_ab <- st_read("./results/value_map/AB/ab_value_map.shp")%>%
  mutate(vl_pp = Ttl_vl_)%>%
  mutate(vl_tot = Totl_vl)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "AB")%>%
  select(vl_pp,vl_tot, province)

df_bc <- st_read("./results/value_map/BC/bc_value_map.shp")%>%
  mutate(vl_pp = Ttl_vl_)%>%
  mutate(vl_tot = Totl_vl)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "BC")%>%
  select(vl_pp,vl_tot, province)


df_mb <- st_read("./results/value_map/MB/mb_value_map.shp")%>%
  mutate(vl_pp = Ttl_vl_)%>%
  mutate(vl_tot = Totl_vl)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "MB")%>%
  select(vl_pp,vl_tot, province)


df_nb <- st_read("./results/value_map/NB/nb_value_map.shp")%>%
  mutate(vl_pp = Ttl_vl_)%>%
  mutate(vl_tot = Totl_vl)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "NB")%>%
  select(vl_pp,vl_tot, province)


df_ns <- st_read("./results/value_map/NS/ns_value_map.shp")%>%
  mutate(vl_pp = Ttl_vl_)%>%
  mutate(vl_tot = Totl_vl)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "NS")%>%
  select(vl_pp,vl_tot, province)


df_on <- st_read("./results/value_map/ON/on_value_map.shp")%>%
  mutate(vl_pp = Ttl_vl_)%>%
  mutate(vl_tot = Totl_vl)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "ON")%>%
  select(vl_pp,vl_tot, province)


df_qc <- st_read("./results/value_map/QC/qc_value_map.shp")%>%
  mutate(vl_pp = Ttl_vl_)%>%
  mutate(vl_tot = Totl_vl)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "QC")%>%
  select(vl_pp,vl_tot, province)

df_pe <- st_read("./results/value_map/PE/pe_value_map.shp")%>%
  mutate(vl_pp = Ttl_vl_)%>%
  mutate(vl_tot = Totl_vl)%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(province = "PE")%>%
  select(vl_pp,vl_tot, province)

data <- rbind(df_sk, df_ab, df_bc, df_mb,df_nb,df_ns, df_on, df_qc,df_pe)

# all in one
p <- data%>%
  ggplot( aes(x=province, y=vl_tot, fill=province)) +
  #geom_violin(width=0.9) +
  geom_boxplot(width=0.25, color="grey", alpha=0.9) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ylim(-10,500)+
  scale_y_continuous(limits = c(1000, 1000000))+
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ Total value CSD")

p



p <- data%>%
  ggplot( aes(x=province, y=vl_pp, fill=province)) +
  geom_violin(width=1) +
  geom_boxplot(width=0.2, color="grey", alpha=0.3) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  scale_y_continuous(limits = c(10, 500))+
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ Value per person")

p



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
