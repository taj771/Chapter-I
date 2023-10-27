# clear memory
rm(list = ls())

## Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  tidyverse,
  sf,
  hrbrthemes,
  viridis,
  ggpubr
)

# Parameters
# elasticity 
e_250m <- 0.216
e_500m <- 0.062
# bilding foof print area definitions
ub <- 1000
lb <- 50

# water qualiy change
wq_change <- 1/100


# building foot print
df_bf <- st_read("./Building_footprint/CAN/can.shp")%>%
  st_transform(df_bf, crs = 3348)

# census
df_cen <- st_read("./Census/CAN/can.shp")%>%
  st_transform(df_cen, crs = 3348)


# filter polygonns only that predefined area size
df_bf_filter <- df_bf%>%
  subset(areasqm > lb & areasqm < ub)
# spatial join filterbfp and census DA
bf_cen_da <- st_join(df_bf_filter,df_cen, all = TRUE )

# count bfp at each DA
bf_cen_da_count <- bf_cen_da%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_count" = "n")

write.csv(bf_cen_da_count, "./Census/CAN/bf_count_da.csv", row.names = F)


# 250 - bfp within 250m boundary
df_bf_250 <- st_read("./Building_footprint/CAN/can_250.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(df_bf_250, crs = 3348)

bf_cen_da_250 <- st_join(df_bf_250,df_cen, all = TRUE)

bf_cen_da_count_250 <- bf_cen_da_250%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_250" = "n")

write.csv(bf_cen_da_count_250,"./Census/CAN/can_250_bf_count.csv", row.names=FALSE)


# 500 - bfp within 500m boundary
df_bf_500 <- st_read("./Building_footprint/CAN/can_500.shp")%>%
  subset(areasqm > lb & areasqm < ub)%>%
  st_transform(df_bf_500, crs = 3348)

bf_cen_da_500 <- st_join(df_bf_500,df_cen, all = TRUE )

bf_cen_da_count_500 <- bf_cen_da_500%>%
  as.data.frame()%>%
  select(-geometry)%>%
  group_by(GeoUID)%>%
  count(GeoUID)%>%
  rename("bfp_500" = "n")

write.csv(bf_cen_da_count_500,"./Census/CAN/can_500_bf_count.csv",row.names=FALSE)

###############################################################################

# census
df_cen <- st_read("./Census/CAN/can.shp")%>%
  st_transform(df_cen, crs = 3348)
df_cen$GeoUID <- as.integer(df_cen$GeoUID)

bf_cen_da_count_250 <- read.csv("./Census/CAN/can_250_bf_count.csv")
bf_cen_da_count_500 <- read_csv("./Census/CAN/can_500_bf_count.csv")
bf_cen_da_count <- read_csv("./Census/CAN/bf_count_da.csv")
cen_can <- st_read("./Census/CEN/CAN/lcsd000b21a_e.shp")%>%
  rename("CSD_UID" = "CSDUID")


df_cen_value <- df_cen%>%
  left_join(bf_cen_da_count_250)%>%
  left_join(bf_cen_da_count_500)%>%
  filter(!if_all(c(bfp_250, bfp_500), is.na))%>%
  drop_na(AvgDwlv)%>%
  filter(AvgDwlv!=0)%>%
  select(GeoUID,Type,CD_UID,Dwllngs,Popultn,CSD_UID,AvgDwlv,prov,bfp_250,bfp_500)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))%>%
  mutate(bfp_250_500 = bfp_500-bfp_250)%>%
  relocate(bfp_250_500, .before=geometry)%>%
  left_join(bf_cen_da_count)%>%
  mutate(ratio = Dwllngs/bfp_count)%>%
  relocate(ratio, .before=geometry)%>%
  mutate(adj_bfp_250 = bfp_250*ratio)%>%
  mutate(adj_bfp_250_500 = bfp_250_500*ratio)%>%
  mutate_at(vars(adj_bfp_250, adj_bfp_250_500), list(~ round(., 0)))%>%
  mutate(e_250 = e_250m)%>%
  mutate(e_500 = e_500m)%>%
  mutate(d_wq = wq_change )%>%
  mutate(val_wf = e_250*adj_bfp_250*AvgDwlv*d_wq)%>%
  mutate(val_nwf = e_500*adj_bfp_250_500*AvgDwlv*d_wq)%>%
  mutate(val_tot = val_wf+val_nwf)%>%
  mutate(val_wf_pp = val_wf/Popultn)%>%
  mutate(val_nwf_pp = val_nwf/Popultn)%>%
  mutate(val_tot_pp = val_wf_pp + val_nwf_pp )%>%
  select(GeoUID,val_wf,val_nwf,val_tot,val_wf_pp,val_nwf_pp,val_tot_pp,prov,ratio,adj_bfp_250,adj_bfp_250_500,CSD_UID)%>%
  rename("modbfp_250" = "adj_bfp_250" )%>%
  rename("modbfp_500" = "adj_bfp_250_500" )
  
  
df_cen_value$GeoUID <- as.character(df_cen_value$GeoUID)


st_write(df_cen_value, "./results/value_map/CAN/can.shp", delete_layer = T)


#AB
df <- df_cen_value%>%
  subset(prov == "AB")
ab_250_bfp <- sum(df$modbfp_250)
ab_250_val <- sum(df$val_wf/1000000)
ab_500_bfp <- sum(df$modbfp_500)
ab_500_val <- sum(df$val_nwf/1000000)
ab_tot_pp <- mean(df$val_tot_pp)
ab_tot <- sum(df$val_tot/1000000)

df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_250 = sum(modbfp_250),
            modbfp_500 = sum(modbfp_500))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 48)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/AB/ab_value_map.shp", delete_layer = T)

#BC
df <- df_cen_value%>%
  subset(prov == "BC")
bc_250_bfp <- sum(df$modbfp_250)
bc_250_val <- sum(df$val_wf/1000000)
bc_500_bfp <- sum(df$modbfp_500)
bc_500_val <- sum(df$val_nwf/1000000)
bc_tot_pp <- mean(df$val_tot_pp)
bc_tot <- sum(df$val_tot/1000000)


df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_250 = sum(modbfp_250),
            modbfp_500 = sum(modbfp_500))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 59)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

st_write(cen_da, "./results/value_map/BC/bc_value_map.shp", delete_layer = T)

#MB
df <- df_cen_value%>%
  subset(prov == "MB")
mb_250_bfp <- sum(df$modbfp_250)
mb_250_val <- sum(df$val_wf/1000000)
mb_500_bfp <- sum(df$modbfp_500)
mb_500_val <- sum(df$val_nwf/1000000)
mb_tot_pp <- mean(df$val_tot_pp)
mb_tot <- sum(df$val_tot/1000000)


df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_250 = sum(modbfp_250),
            modbfp_500 = sum(modbfp_500))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 46)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/MB/mb_value_map.shp", delete_layer = T)



#NB
df <- df_cen_value%>%
  subset(prov == "NB")
nb_250_bfp <- sum(df$modbfp_250)
nb_250_val <- sum(df$val_wf/1000000)
nb_500_bfp <- sum(df$modbfp_500)
nb_500_val <- sum(df$val_nwf/1000000)
nb_tot_pp <- mean(df$val_tot_pp)
nb_tot <- sum(df$val_tot/1000000)


df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_250 = sum(modbfp_250),
            modbfp_500 = sum(modbfp_500))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 13)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

st_write(cen_da, "./results/value_map/NB/nb_value_map.shp", delete_layer = T)


#NL
df <- df_cen_value%>%
  subset(prov == "NL")
nl_250_bfp <- sum(df$modbfp_250)
nl_250_val <- sum(df$val_wf/1000000)
nl_500_bfp <- sum(df$modbfp_500)
nl_500_val <- sum(df$val_nwf/1000000)
nl_tot_pp <- mean(df$val_tot_pp)
nl_tot <- sum(df$val_tot/1000000)


df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_250 = sum(modbfp_250),
            modbfp_500 = sum(modbfp_500))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 10)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/NL/nl_value_map.shp", delete_layer = T)

#NS
df <- df_cen_value%>%
  subset(prov == "NS")
ns_250_bfp <- sum(df$modbfp_250)
ns_250_val <- sum(df$val_wf/1000000)
ns_500_bfp <- sum(df$modbfp_500)
ns_500_val <- sum(df$val_nwf/1000000)
ns_tot_pp <- mean(df$val_tot_pp)
ns_tot <- sum(df$val_tot/1000000)


df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_250 = sum(modbfp_250),
            modbfp_500 = sum(modbfp_500))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 12)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/NS/ns_value_map.shp", delete_layer = T)



#ON
df <- df_cen_value%>%
  subset(prov == "ON")
on_250_bfp <- sum(df$modbfp_250)
on_250_val <- sum(df$val_wf/1000000)
on_500_bfp <- sum(df$modbfp_500)
on_500_val <- sum(df$val_nwf/1000000)
on_tot_pp <- mean(df$val_tot_pp)
on_tot <- sum(df$val_tot/1000000)


df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_250 = sum(modbfp_250),
            modbfp_500 = sum(modbfp_500))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 35)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

st_write(cen_da, "./results/value_map/ON/on_value_map.shp", delete_layer = T)


#PE
df <- df_cen_value%>%
  subset(prov == "PE")
pe_250_bfp <- sum(df$modbfp_250)
pe_250_val <- sum(df$val_wf/1000000)
pe_500_bfp <- sum(df$modbfp_500)
pe_500_val <- sum(df$val_nwf/1000000)
pe_tot_pp <- mean(df$val_tot_pp)
pe_tot <- sum(df$val_tot/1000000)


df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_250 = sum(modbfp_250),
            modbfp_500 = sum(modbfp_500))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 11)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
st_write(cen_da, "./results/value_map/PE/pe_value_map.shp", delete_layer = T)



#QC
df <- df_cen_value%>%
  subset(prov == "QC")
qc_250_bfp <- sum(df$modbfp_250)
qc_250_val <- sum(df$val_wf/1000000)
qc_500_bfp <- sum(df$modbfp_500)
qc_500_val <- sum(df$val_nwf/1000000)
qc_tot_pp <- mean(df$val_tot_pp)
qc_tot <- sum(df$val_tot/1000000)


df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_250 = sum(modbfp_250),
            modbfp_500 = sum(modbfp_500))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 24)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

st_write(cen_da, "./results/value_map/QC/qc_value_map.shp", delete_layer = T)



#SK
df <- df_cen_value%>%
  subset(prov == "SK")
sk_250_bfp <- sum(df$modbfp_250)
sk_250_val <- sum(df$val_wf/1000000)
sk_500_bfp <- sum(df$modbfp_500)
sk_500_val <- sum(df$val_nwf/1000000)
sk_tot_pp <- mean(df$val_tot_pp)
sk_tot <- sum(df$val_tot/1000000)


df <- df%>%
  group_by(CSD_UID)%>%
  summarise(val_wf = sum(val_wf),
            val_nwf = sum(val_nwf),
            val_tot = sum(val_tot),
            val_wf_pp = mean(val_wf_pp),
            val_nwf_pp = mean(val_nwf_pp),
            val_tot_pp = mean(val_tot_pp),
            modbfp_250 = sum(modbfp_250),
            modbfp_500 = sum(modbfp_500))%>%
  as.data.frame()%>%
  select(-geometry)
cen_da <- cen_can%>%
  subset(PRUID == 47)%>%
  select(CSD_UID)%>%
  left_join(df)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

st_write(cen_da, "./results/value_map/SK/sk_value_map.shp", delete_layer = T)


###########################################################################
# value summary table


list1 <- list("Province" = c("Saskatchewan", "Alberta", "British Columbia", "Manitoba","New Brunswick",
                             "Nova Scotia", "Ontario", "Quebec", "Prince Edwards"),
              "Numbber of affected house within 250m" = c(sk_250_bfp, ab_250_bfp, bc_250_bfp, mb_250_bfp,
                                                          nb_250_bfp, ns_250_bfp, on_250_bfp,qc_250_bfp, 
                                                          pe_250_bfp),
              "Numbber of affected house within 500m" = c(sk_500_bfp, ab_500_bfp, bc_500_bfp, mb_500_bfp,
                                                          nb_500_bfp, ns_500_bfp, on_500_bfp, qc_500_bfp,
                                                          pe_500_bfp),
              "Capitalized value within 250m" =c(sk_250_val, ab_250_val, bc_250_val, mb_250_val,
                                                 nb_250_val, ns_250_val, on_250_val,qc_250_val,
                                                 pe_250_val),
              "Capitalized value within 500m" =c(sk_500_val, ab_500_val, bc_500_val, mb_500_val,
                                                 nb_500_val, ns_500_val, on_500_val,qc_500_val,
                                                 pe_500_val),
              "Total capitalized value per person" =c(sk_tot_pp, ab_tot_pp, bc_tot_pp, mb_tot_pp,
                                                 nb_tot_pp, ns_tot_pp, on_tot_pp,qc_tot_pp,
                                                 pe_tot_pp)
              
)
result <- as.data.frame(list1)



library(xtable)
print(xtable(result, type = "latex"), file = "./results/Tables/Benefits_table.tex")

####################################################################
# graph
# all in one - violine plot - total value
p <- df_cen_value%>%
  ggplot( aes(x=prov, y=val_tot, fill=prov)) +
  #geom_violin(width=0.9) +
  geom_boxplot(width=0.25, color="grey", alpha=0.9) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ylim(-10,500)+
  scale_y_continuous(limits = c(1000, 100000))+
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ Total value CSD")

p

# all in one - violine plot - total value pp
p <- df_cen_value%>%
  ggplot( aes(x=prov, y=val_tot_pp, fill=prov)) +
  #geom_violin(width=1.1,alpha=0.3) +
  geom_boxplot(width=0.2, color="grey",alpha=0.9) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ylim(0,250)+
  #scale_y_continuous(limits = c(100, 100000))+
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  ggtitle("") +
  xlab("Province") +
  ylab("$ Tota value per-person CSD")

p


