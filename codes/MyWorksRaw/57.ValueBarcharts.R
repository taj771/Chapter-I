# clear memory
rm(list = ls())



df_sim_final <- read_csv("./results/simulation/simulation_final_df.csv")

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  as.data.frame()%>%
  select(-geometry)%>%
  select(GeoUID,Popultn)

df_cen$GeoUID <- as.numeric(df_cen$GeoUID)


df_all <- df_sim_final%>%
  left_join(df_cen)%>%
  #group_by(prov)%>%
  #subset(prov == "QC")%>%
  select(GeoUID,val_wf,val_nwf,val_tot,sim_id,elast_250m,elast_750m,prov,bfp_500,bfp_1000,Popultn)%>%
  mutate(bfp_all = bfp_500+bfp_1000)%>%
  #drop_na()%>%
  
  group_by(GeoUID,prov)%>%
  mutate(ave_tot_wf = mean(val_wf))%>%
  mutate(lb_ave_tot_wf = quantile(val_wf, 0.25, na.rm = T))%>%
  mutate(ub_ave_tot_wf = quantile(val_wf, 0.75, na.rm = T))%>%
  
  mutate(ave_tot_nwf = mean(val_nwf))%>%
  mutate(lb_ave_tot_nwf = quantile(val_nwf, 0.25, na.rm = T))%>%
  mutate(ub_ave_tot_nwf = quantile(val_nwf, 0.75, na.rm = T))%>%
  
  mutate(ave_tot = mean(val_tot))%>%
  mutate(lb_ave_tot = quantile(val_tot, 0.25,na.rm = T))%>%
  mutate(ub_ave_tot = quantile(val_tot, 0.75,na.rm = T))%>%
  
  mutate(ave_bfp_500 = mean(bfp_500))%>%
  mutate(lb_ave_bfp_wf = quantile(bfp_500, 0.25,na.rm = T))%>%
  mutate(ub_ave_bfp_wf = quantile(bfp_500, 0.75,na.rm = T))%>%
  
  mutate(ave_bfp_1000 = mean(bfp_1000))%>%
  mutate(lb_ave_bfp_nwf = quantile(bfp_1000, 0.25,na.rm = T))%>%
  mutate(ub_ave_bfp_nwf = quantile(bfp_1000, 0.75,na.rm = T))%>%
  
  mutate(ave_bfp_tot = mean(bfp_all))%>%
  mutate(lb_ave_bfp_tot = quantile(bfp_all, 0.25,na.rm = T))%>%
  mutate(ub_ave_bfp_tot = quantile(bfp_all, 0.75,na.rm = T))%>%
  
  
  
  distinct(GeoUID, .keep_all = T)%>%
  ungroup()%>%
  group_by(prov)%>%
  mutate(ave_tot_wf  =  sum(ave_tot_wf))%>%
  mutate(lb_ave_tot_wf = sum(lb_ave_tot_wf))%>%
  mutate(ub_ave_tot_wf = sum(ub_ave_tot_wf))%>%
  
  mutate(ave_tot_nwf  =  sum(ave_tot_nwf))%>%
  mutate(lb_ave_tot_nwf = sum(lb_ave_tot_nwf))%>%
  mutate(ub_ave_tot_nwf = sum(ub_ave_tot_nwf))%>%
  
  mutate(ave_tot  =  sum(ave_tot))%>%
  mutate(lb_ave_tot = sum(lb_ave_tot))%>%
  mutate(ub_ave_tot = sum(ub_ave_tot))%>%
  
  mutate(ave_bfp_wf  =  sum(ave_bfp_500))%>%
  mutate(lb_ave_bfp_wf = sum(lb_ave_bfp_wf))%>%
  mutate(ub_ave_bfp_wf = sum(ub_ave_bfp_wf))%>%
  
  mutate(ave_bfp_nwf  =  sum(ave_bfp_1000))%>%
  mutate(lb_ave_bfp_nwf = sum(lb_ave_bfp_nwf))%>%
  mutate(ub_ave_bfp_nwf = sum(ub_ave_bfp_nwf))%>%
  
  mutate(ave_bfp_tot  =  sum(ave_bfp_tot))%>%
  mutate(lb_ave_bfp_tot = sum(lb_ave_bfp_tot))%>%
  mutate(ub_ave_bfp_tot = sum(ub_ave_bfp_tot))%>%
  
  mutate(popultn_tot = sum(Popultn))%>%
  
  mutate(val_pp = ave_tot/popultn_tot)%>%
  mutate(lb_val_pp = lb_ave_tot/popultn_tot)%>%
  mutate(ub_val_pp = ub_ave_tot/popultn_tot)


# library
library(ggplot2)


# Total affected houses ####################################################

t1 <- df_all%>%
  select(GeoUID,prov,ave_bfp_tot,lb_ave_bfp_tot,ub_ave_bfp_tot)%>%
  mutate(sum_bfp = ave_bfp_tot/1000)%>%
  mutate(lb_ave_bfp_tot=lb_ave_bfp_tot/1000)%>%
  mutate(ub_ave_bfp_tot=ub_ave_bfp_tot/1000)%>%
  mutate_if(is.numeric, round)%>%
  distinct(prov, .keep_all = T)



t1$prov <- gsub("ON", "Ontario", t1$prov)
t1$prov <- gsub("QC", "Quebec", t1$prov)
t1$prov <- gsub("BC", "British Columbia", t1$prov)
t1$prov <- gsub("NS", "Nova Scotia", t1$prov)
t1$prov <- gsub("NL", "Newfoundland L.", t1$prov)
t1$prov <- gsub("MB", "Manitoba", t1$prov)
t1$prov <- gsub("SK", "Saskatchewan", t1$prov)
t1$prov <- gsub("AB", "Alberta", t1$prov)
t1$prov <- gsub("NB", "New Brunswick", t1$prov)
t1$prov <- gsub("PE", "Prince Edward", t1$prov)

t1 <- t1[!duplicated(t1$prov), ]

# Basic Barplot
nb.cols <- 10

mycolors <- met.brewer(
  "VanGogh2",
  10,
  type = c("continuous"),
  override.order = FALSE
)



#mycolors <- colorRampPalette(brewer.pal(10, "Dark2"))(nb.cols)

p1 <- ggplot(t1, aes(x = reorder(prov, -sum_bfp), y = sum_bfp, fill = prov)) + 
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = mycolors) +
  scale_y_continuous(breaks=seq(0,600,100),limits = c(0, 600))+
  geom_text(aes(label = sum_bfp), vjust = -0.5, fontface = "bold", size = 2.6) +
  ylab("Total affected houses in 1000's") +
  xlab("")+
  #ggtitle("Total affected houses in 1000's")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  theme(
    axis.text.x = element_text(size = 13),  # X-axis text size
    axis.text.y = element_text(size = 10),  # Y-axis text size
    axis.title.x = element_text(size = 20), # X-axis title size
    axis.title.y = element_text(size = 10), # Y-axis title size
    plot.title = element_text(size = 15)    # Plot title size
  )+
  theme(
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )+
  theme(
    axis.title.x = element_text(face = "bold"), # Bold x-axis title
    axis.title.y = element_text(face = "bold")  # Bold y-axis title
  )+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")  # Remove the legend


################## value waterfront ########################################


t1 <- df_all%>%
  select(GeoUID,prov,ave_tot_wf)%>%
  mutate(ave_tot_wf = ave_tot_wf/1000000)%>%
  mutate_if(is.numeric, round)%>%
  distinct(prov, .keep_all = T)



t1$prov <- gsub("ON", "Ontario", t1$prov)
t1$prov <- gsub("QC", "Quebec", t1$prov)
t1$prov <- gsub("BC", "British Columbia", t1$prov)
t1$prov <- gsub("NS", "Nova Scotia", t1$prov)
t1$prov <- gsub("NL", "Newfoundland L.", t1$prov)
t1$prov <- gsub("MB", "Manitoba", t1$prov)
t1$prov <- gsub("SK", "Saskatchewan", t1$prov)
t1$prov <- gsub("AB", "Alberta", t1$prov)
t1$prov <- gsub("NB", "New Brunswick", t1$prov)
t1$prov <- gsub("PE", "Prince Edward", t1$prov)

t1 <- t1[!duplicated(t1$prov), ]

# Basic Barplot
nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(10, "Set1"))(nb.cols)

p2 <- ggplot(t1, aes(x = reorder(prov, -ave_tot_wf), y = ave_tot_wf, fill = prov)) + 
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = mycolors) +
  geom_text(aes(label = ave_tot_wf), vjust = -0.5, fontface = "bold") +
  ylab("Total value in Waterfront bin ($ millions)") +
  xlab("")+
  #ggtitle("Total affected houses in 1000's")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  theme(
    axis.text.x = element_text(size = 13),  # X-axis text size
    axis.text.y = element_text(size = 10),  # Y-axis text size
    axis.title.x = element_text(size = 20), # X-axis title size
    axis.title.y = element_text(size = 10), # Y-axis title size
    plot.title = element_text(size = 15)    # Plot title size
  )+
  theme(
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )+
  theme(
    axis.title.x = element_text(face = "bold"), # Bold x-axis title
    axis.title.y = element_text(face = "bold")  # Bold y-axis title
  )+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")  # Remove the legend


################## value Near waterfront waterfront ########################################


t1 <- df_all%>%
  select(GeoUID,prov,ave_tot_nwf)%>%
  mutate(ave_tot_nwf = ave_tot_nwf/1000000)%>%
  mutate_if(is.numeric, round)%>%
  distinct(prov, .keep_all = T)



t1$prov <- gsub("ON", "Ontario", t1$prov)
t1$prov <- gsub("QC", "Quebec", t1$prov)
t1$prov <- gsub("BC", "British Columbia", t1$prov)
t1$prov <- gsub("NS", "Nova Scotia", t1$prov)
t1$prov <- gsub("NL", "Newfoundland L.", t1$prov)
t1$prov <- gsub("MB", "Manitoba", t1$prov)
t1$prov <- gsub("SK", "Saskatchewan", t1$prov)
t1$prov <- gsub("AB", "Alberta", t1$prov)
t1$prov <- gsub("NB", "New Brunswick", t1$prov)
t1$prov <- gsub("PE", "Prince Edward", t1$prov)

t1 <- t1[!duplicated(t1$prov), ]

# Basic Barplot
nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(10, "Set1"))(nb.cols)

p3 <- ggplot(t1, aes(x = reorder(prov, -ave_tot_nwf), y = ave_tot_nwf, fill = prov)) + 
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = mycolors) +
  geom_text(aes(label = ave_tot_nwf), vjust = -0.5, fontface = "bold") +
  ylab("Total value in Near-waterfront bin ($ millions)") +
  xlab("")+
  #ggtitle("Total affected houses in 1000's")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  theme(
    axis.text.x = element_text(size = 13),  # X-axis text size
    axis.text.y = element_text(size = 10),  # Y-axis text size
    axis.title.x = element_text(size = 20), # X-axis title size
    axis.title.y = element_text(size = 10), # Y-axis title size
    plot.title = element_text(size = 15)    # Plot title size
  )+
  theme(
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )+
  theme(
    axis.title.x = element_text(face = "bold"), # Bold x-axis title
    axis.title.y = element_text(face = "bold")  # Bold y-axis title
  )+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")  # Remove the legend


################## total value ########################################


t1 <- df_all %>%
  select(GeoUID, prov, ave_tot, lb_ave_tot, ub_ave_tot) %>%
  mutate(ave_tot = ave_tot / 1000000) %>%
  mutate(lb_ave_tot = lb_ave_tot / 1000000) %>%
  mutate(ub_ave_tot = ub_ave_tot / 1000000) %>%
  mutate_if(is.numeric, round) %>%
  distinct(prov, .keep_all = TRUE) %>%
  
  # Combine ave_tot, lb_ave_tot, and ub_ave_tot with ":" as separator into a new column
  mutate(ave_conf_tot = paste(lb_ave_tot, ub_ave_tot, sep = ":"))

t1$merged_column <- paste(t1$ave_tot, "\n(", t1$ave_conf_tot, ")", sep = "")
  

t1$prov <- gsub("ON", "Ontario", t1$prov)
t1$prov <- gsub("QC", "Quebec", t1$prov)
t1$prov <- gsub("BC", "British Columbia", t1$prov)
t1$prov <- gsub("NS", "Nova Scotia", t1$prov)
t1$prov <- gsub("NL", "Newfoundland L.", t1$prov)
t1$prov <- gsub("MB", "Manitoba", t1$prov)
t1$prov <- gsub("SK", "Saskatchewan", t1$prov)
t1$prov <- gsub("AB", "Alberta", t1$prov)
t1$prov <- gsub("NB", "New Brunswick", t1$prov)
t1$prov <- gsub("PE", "Prince Edward", t1$prov)

t1 <- t1[!duplicated(t1$prov), ]

# Basic Barplot
#nb.cols <- 10
#mycolors <- colorRampPalette(brewer.pal(10, "Dark2"))(nb.cols)

p4 <- ggplot(t1, aes(x = ave_tot, y = reorder(prov, ave_tot), fill = prov)) + 
  geom_bar(stat = "identity", width = 0.6, position = position_dodge(width = 0.7)) +  # Adjust position if needed
  
  # Add error bars
  geom_errorbar(aes(xmin = lb_ave_tot, xmax = ub_ave_tot), width = 0.2, color = "black") +
  
  scale_fill_manual(values = mycolors) +
  scale_x_continuous(breaks = seq(0, 6200, 1000), limits = c(0, 6200), expand = expansion(mult = c(0.001, 0))) +  # Adjust expansion
  
  # Place the labels at the end of the error bars
  geom_text(aes(x = ub_ave_tot, 
                label = paste0(ave_tot)),
            hjust = -0.3,   # Adjust for fine control over horizontal positioning
            fontface = "bold", 
            size = 5) +    # Increased size of the text
  
  xlab("Total Capitalized value ($ millions)") +
  ylab("") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 16),  # X-axis text size
    axis.text.y = element_text(size = 16),  # Y-axis text size
    axis.title.x = element_text(size = 16), # X-axis title size
    axis.title.y = element_text(size = 16, face = "bold"), # Y-axis title size
    plot.title = element_text(size = 15, hjust = 0.5)    # Plot title size and center
  ) +
  theme(
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  ) +
  theme(legend.position = "none")  # Remove the legend

p4



################## value pr household ########################################

t1 <- df_all %>%
  select(GeoUID, prov, val_pp,lb_val_pp,ub_val_pp) %>%
  mutate_if(is.numeric, round)%>%
  mutate_if(is.numeric, round) %>%
  distinct(prov, .keep_all = TRUE) %>%
  
  # Combine ave_tot, lb_ave_tot, and ub_ave_tot with ":" as separator into a new column
  mutate(ave_con_pp = paste(lb_val_pp, ub_val_pp, sep = ":"))

t1$merged_column <- paste(t1$val_pp, "\n(", t1$ave_con_pp, ")", sep = "")




t1$prov <- gsub("ON", "Ontario", t1$prov)
t1$prov <- gsub("QC", "Quebec", t1$prov)
t1$prov <- gsub("BC", "British Columbia", t1$prov)
t1$prov <- gsub("NS", "Nova Scotia", t1$prov)
t1$prov <- gsub("NL", "Newfoundland L.", t1$prov)
t1$prov <- gsub("MB", "Manitoba", t1$prov)
t1$prov <- gsub("SK", "Saskatchewan", t1$prov)
t1$prov <- gsub("AB", "Alberta", t1$prov)
t1$prov <- gsub("NB", "New Brunswick", t1$prov)
t1$prov <- gsub("PE", "Prince Edward", t1$prov)

t1 <- t1[!duplicated(t1$prov), ]

p5 <- ggplot(t1, aes(x = val_pp, y = reorder(prov, val_pp), fill = prov)) + 
  geom_bar(stat = "identity", width = 0.6, position = position_dodge(width = 0.7)) +  # Adjust position if needed
  
  # Add error bars
  geom_errorbar(aes(xmin = lb_val_pp, xmax = ub_val_pp), width = 0.2, color = "black") +
  
  scale_fill_manual(values = mycolors) +
  scale_x_continuous(breaks = seq(0, 3200, 500), limits = c(0, 3200), expand = expansion(mult = c(0.001, 0))) +  # Adjust expansion
  
  # Place the labels at the end of the error bars
  geom_text(aes(x = ub_val_pp, 
                label = paste0(val_pp)),
            hjust = -0.3,   # Adjust for fine control over horizontal positioning
            fontface = "bold", 
            size = 5) +    # Size of the text
  
  xlab("Total Capitalized value ($ millions)") +
  ylab("") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 16),  # X-axis text size
    axis.text.y = element_text(size = 16),  # Y-axis text size
    axis.title.x = element_text(size = 16, face = "bold"), # X-axis title size
    axis.title.y = element_text(size = 16, face = "bold"), # Y-axis title size
    plot.title = element_text(size = 16, hjust = 0.5)    # Plot title size and center
  ) +
  theme(
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  ) +
  theme(legend.position = "none")  # Remove the legend

p5 








combined_plot <- p1 | p4| p5
combined_plot




