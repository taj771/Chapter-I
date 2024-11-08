library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggdist)
library(ggtext)
library(patchwork)

# clear memory
rm(list = ls())

df_sim_final <- read_csv("./results/simulation/simulation_final_df.csv")

# Table

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

  mutate(ave_tot = mean(val_tot))%>%
  mutate(lb_ave_tot = quantile(val_tot, 0.25,na.rm = T))%>%
  mutate(ub_ave_tot = quantile(val_tot, 0.75,na.rm = T))%>%
  
  distinct(GeoUID, .keep_all = T)%>% 
  
  group_by(prov)%>%
  mutate(count = n())



df_all$prov <- gsub("ON", "Ontario", df_all$prov)
df_all$prov <- gsub("QC", "Quebec", df_all$prov)
df_all$prov <- gsub("BC", "British Columbia", df_all$prov)
df_all$prov <- gsub("NS", "Nova Scotia", df_all$prov)
df_all$prov <- gsub("NL", "Newfoundland L.", df_all$prov)
df_all$prov <- gsub("MB", "Manitoba", df_all$prov)
df_all$prov <- gsub("SK", "Saskatchewan", df_all$prov)
df_all$prov <- gsub("AB", "Alberta", df_all$prov)
df_all$prov <- gsub("NB", "New Brunswick", df_all$prov)
df_all$prov <- gsub("PE", "Prince Edward", df_all$prov)




# sort dataframe by mean_price
df_all <- df_all %>% arrange(desc(ave_tot))
df_all$prov <- factor(df_all$prov, levels = unique(df_all$prov))

df_all <- df_all%>%
  mutate(ave_tot = ave_tot/1000000)


mean_value <- mean(df_all$ave_tot, na.rm = TRUE)
median_value <- median(df_all$ave_tot, na.rm = TRUE)
#n_rental_posts <- nrow(subset(, !is.na(title)))


bg_color <- "grey97"
font_family <- "Fira Sans"


library(MetBrewer)

p <- df_all %>% 
  ggplot(aes(prov, ave_tot)) +
  stat_halfeye(fill_type = "gradient", alpha = 0.3) +
  theme_bw()+
  stat_interval() +
  stat_summary(geom = "point", fun = median) +

  annotate("text", x = 10, y = 0, label = "bedrooms)",
           family = "Fira Sans", size = 3, hjust = 0.5) +
  stat_summary(
    aes(y = count),
    geom = "text",
    fun.data = function(x) {
      data.frame(
        y = 0,
        label = sprintf("(%s)", scales::number(mean(ifelse(x > 0, x, NA), na.rm = TRUE), accuracy = 0.1)))},
    family = font_family, size = 2.5
  ) +
  geom_hline(yintercept = median_value, col = "grey30", lty = "dashed") +
  annotate("text", x = 10, y = median_value + 50, label = "Median Value",
           family = "Fira Sans", size = 3, hjust = 0) +
  #scale_x_discrete(labels = toupper) +
  scale_y_continuous(breaks = seq(0, 40, 1), expand = c(0.01, 0)) +
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
  coord_flip(ylim = c(-0.001, 9), clip = "off") +
  guides(col = "none") +
  labs(
    title = toupper(""),
    #subtitle = plot_subtitle,
    caption = "",
    x = NULL,
    y = "Total value within a CDA ($ millions)"
  ) +
  #theme_minimal(base_family = font_family) +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.1, color = "grey75"),
    plot.title = element_text(family = "Fira Sans SemiBold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 4, b = 16), size = 10),
    plot.caption = element_textbox_simple(
      margin = margin(t = 12), size = 7
    ),
    plot.caption.position = "plot",
    #axis.text.y = element_text(hjust = 0, margin = margin(r = -10), family = "Fira Sans SemiBold"),
    plot.margin = margin(4, 4, 4, 4)
  )+
  theme(
    axis.title = element_text(face = "bold"),      # Bold axis labels
    axis.text = element_text(face = "bold")        # Bold axis tick labels
  )
p

# create the dataframe for the legend (inside plot)

rent_title_words <- read_csv("./metadata/rent_title_words.csv")


df_for_legend <- rent_title_words %>% 
  filter(word == "beautiful")

p_legend <- df_for_legend %>% 
  ggplot(aes(word, price)) +
  stat_halfeye(fill_type = "segments", alpha = 0) +
  stat_interval() +
  stat_summary(geom = "point", fun = median) +
  annotate(
    "richtext",
    x = c(0.8, 0.8, 0.8, 1.4, 1.8),
    y = c(1000, 5000, 3000, 2400, 4000),
    label = c("50 % of prices<br>fall within this range", "95 % of prices", 
              "80 % of prices", "Median", "Distribution<br>of prices"),
    fill = NA, label.size = 0, family = font_family, size = 2, vjust = 1,
  ) +
  geom_curve(
    data = data.frame(
      x = c(0.7, 0.80, 0.80, 1.225),
      xend = c(0.95, 0.95, 0.95, 1.075), 
      y = c(1800, 5000, 3000, 2300),
      yend = c(1800, 5000, 3000, 2100)),
    aes(x = x, xend = xend, y = y, yend = yend),
    stat = "unique", curvature = 0.2, size = 0.2, color = "grey12",
    arrow = arrow(angle = 20, length = unit(1, "mm"))
  ) +
  
  annotate("text", 
           x = 0.66,  # Position near the arrow
           y = 1800,  # Position near the arrow
           label = "50% of the values fall within this range", 
           hjust = 0, 
           size = 2.5, 
           color = "grey12") +
  
  annotate("text", 
           x = 0.7,  # Position near the arrow
           y = 5000,  # Position near the arrow
           label = "95% of the values fall 
                    within this range", 
           hjust = 0, 
           size = 2.5, 
           color = "grey12") +
  
  annotate("text", 
           x = 0.75,  # Position near the arrow
           y = 3000,  # Position near the arrow
           label = "80% of the values fall within this range", 
           hjust = 0, 
           size = 2.5, 
           color = "grey12") +
  
  annotate("text", 
           x = 1.225,  # Position near the arrow
           y = 2300,  # Position near the arrow
           label = "Median", 
           hjust = 0, 
           size = 2.5, 
           color = "grey12") +
  
  
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
  coord_flip(xlim = c(0.75, 1.3), ylim = c(0, 6000), expand = TRUE) +
  guides(color = "none") +
  labs(title = "Legend") +
  theme_void(base_family = font_family) +
  theme(plot.title = element_text(family = "Fira Sans SemiBold", size = 9,
                                  hjust = 0.075),
        plot.background = element_rect(color = "grey30", size = 0.2, fill = bg_color))

# Insert the custom legend into the plot
p + inset_element(p_legend, l = 0.45, r = 0.99,  t = 0.99, b = 0.7, clip = FALSE)



