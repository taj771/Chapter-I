df_sim_final <- read_csv("./results/simulation/simulation_final_df.csv")

df_sim_final <- df_sim_final%>%
  mutate(tot_bfp = bfp_500 + bfp_1000)



###### number of affected houses ################

t <- df_sim_final %>%
  filter( GeoUID ==35490210 )%>%
  group_by(prov)%>%
  mutate(count = n())

# sort dataframe by mean_price
t <- t %>% arrange(desc(tot_bfp))
t$prov <- factor(t$prov, levels = unique(t$prov))


mean_value <- mean(t$tot_bfp, na.rm = TRUE)
median_value <- median(t$tot_bfp, na.rm = TRUE)
#n_rental_posts <- nrow(subset(, !is.na(title)))


bg_color <- "grey97"
font_family <- "Fira Sans"


library(MetBrewer)
library(ggdist)
library(ggtext)



library(ggstance)



p <- t %>% 
  ggplot(aes(tot_bfp)) +
  stat_halfeye(fill_type = "gradient", alpha = 0.3) +
  theme_bw() +
  stat_interval() +
  scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(-0.01, 1)) +
  scale_x_continuous(breaks = seq(2800, 2935, 30), limits = c(2829, 2935)) +
  
  #stat_summary(geom = "point", fun = median)+
  #geom_vline(xintercept = median_value, col = "grey30", lty = "dashed")+
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3"))+
  guides(col = "none") +
  labs(
    title = toupper(""),
    #subtitle = plot_subtitle,
    caption = "",
    x = "Number of Affected Houses",
    y = "Density"
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
  )
p


# create the dataframe for the legend (inside plot)

rent_title_words <- read_csv("./metadata/rent_title_words.csv")


df_for_legend <- rent_title_words %>% 
  filter(word == "beautiful")

p_legend <- df_for_legend %>% 
  ggplot(aes(word, price)) +
  stat_halfeye(fill_type = "segments", alpha = 0.3) +
  stat_interval() +
  #stat_summary(geom = "point", fun = median) +
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
      x = c(0.7, 0.80, 0.80, 1.8),
      xend = c(0.95, 0.95, 0.95, 1.8), 
      y = c(1800, 5000, 3000, 3800),
      yend = c(1800, 5000, 3000, 2500)),
    aes(x = x, xend = xend, y = y, yend = yend),
    stat = "unique", curvature = 0.2, size = 0.2, color = "grey12",
    arrow = arrow(angle = 20, length = unit(1, "mm"))
  ) +
  
  annotate("text", 
           x = 0.66,  # Position near the arrow
           y = 1800,  # Position near the arrow
           label = "50% of Data", 
           hjust = 0, 
           size = 1.5, 
           color = "grey12") +
  
  annotate("text", 
           x = 0.7,  # Position near the arrow
           y = 5000,  # Position near the arrow
           label = "95% of Data", 
           hjust = 0, 
           size = 1.5, 
           color = "grey12") +
  
  annotate("text", 
           x = 0.75,  # Position near the arrow
           y = 3000,  # Position near the arrow
           label = "80% of the Data", 
           hjust = 0, 
           size = 1.5, 
           color = "grey12") +
  
  annotate("text", 
           x = 1.8,  # Position near the arrow
           y = 4000,  # Position near the arrow
           label = "Distribution of 
           Number of Affected Houses", 
           hjust = 0, 
           size = 1.5, 
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
p + inset_element(p_legend, l = 0.005, r = 0.4,  t = 0.99, b = 0.7, clip = FALSE)



###### dwelling values ################

t <- df_sim_final %>%
  filter( GeoUID ==35490210 )%>%
  group_by(prov)%>%
  mutate(count = n())

# sort dataframe by mean_price
#t <- t %>% arrange(desc(dwelval))
#t$dwelval <- factor(t$dwelval, levels = unique(t$dwelval))

#t$dwelval <- as.numeric(t$dwelval)

#mean_value <- mean(t$dwelval, na.rm = TRUE)
#median_value <- median(t$dwelval, na.rm = TRUE)
#n_rental_posts <- nrow(subset(, !is.na(title)))


bg_color <- "grey97"
font_family <- "Fira Sans"


library(MetBrewer)
library(ggdist)
library(ggtext)



library(ggstance)



p <- t %>% 
  ggplot(aes(dwelval)) +
  stat_halfeye(fill_type = "gradient", alpha = 0.3) +
  stat_interval() +
  scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(-0.01, 1)) +
  #scale_x_continuous(breaks = seq(695000, 850000, 30), limits = c(695000, 850000)) +
  theme_bw() +
  
  #stat_summary(geom = "point", fun = median)+
  #geom_vline(xintercept = median_value, col = "grey30", lty = "dashed")+
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3"))+
  guides(col = "none") +
  labs(
    title = toupper(""),
    #subtitle = plot_subtitle,
    caption = "",
    x = "Property Price",
    y = "Density"
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
  )
p


# create the dataframe for the legend (inside plot)

rent_title_words <- read_csv("./metadata/rent_title_words.csv")


df_for_legend <- rent_title_words %>% 
  filter(word == "beautiful")

p_legend <- df_for_legend %>% 
  ggplot(aes(word, price)) +
  stat_halfeye(fill_type = "segments", alpha = 0.3) +
  stat_interval() +
  #stat_summary(geom = "point", fun = median) +
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
      x = c(0.7, 0.80, 0.80, 1.8),
      xend = c(0.95, 0.95, 0.95, 1.8), 
      y = c(1800, 5000, 3000, 3800),
      yend = c(1800, 5000, 3000, 2500)),
    aes(x = x, xend = xend, y = y, yend = yend),
    stat = "unique", curvature = 0.2, size = 0.2, color = "grey12",
    arrow = arrow(angle = 20, length = unit(1, "mm"))
  ) +
  
  annotate("text", 
           x = 0.66,  # Position near the arrow
           y = 1800,  # Position near the arrow
           label = "50% of Data", 
           hjust = 0, 
           size = 1.5, 
           color = "grey12") +
  
  annotate("text", 
           x = 0.7,  # Position near the arrow
           y = 5000,  # Position near the arrow
           label = "95% of Data", 
           hjust = 0, 
           size = 1.5, 
           color = "grey12") +
  
  annotate("text", 
           x = 0.75,  # Position near the arrow
           y = 3000,  # Position near the arrow
           label = "80% of Data", 
           hjust = 0, 
           size = 1.5, 
           color = "grey12") +
  
  annotate("text", 
           x = 1.8,  # Position near the arrow
           y = 4000,  # Position near the arrow
           label = "Distribution of Property Price", 
           hjust = 0, 
           size = 1.5, 
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
p + inset_element(p_legend, l = 0.65, r = 1,  t = 1, b = 0.79, clip = FALSE)



###### elasticity - watefront values ################

t <- df_sim_final %>%
  filter( GeoUID ==35490210 )%>%
  group_by(prov)%>%
  mutate(count = n())

# sort dataframe by mean_price
#t <- t %>% arrange(desc(elast_250m))
#t$elast_250m <- factor(t$elast_250m, levels = unique(t$elast_250m))

#mean_value <- mean(t$dwelval, na.rm = TRUE)
#median_value <- median(t$dwelval, na.rm = TRUE)
#n_rental_posts <- nrow(subset(, !is.na(title)))


bg_color <- "grey97"
font_family <- "Fira Sans"


library(MetBrewer)
library(ggdist)
library(ggtext)



library(ggstance)



p <- t %>% 
  ggplot(aes(elast_250m)) +
  stat_halfeye(fill_type = "gradient", alpha = 0.3) +
  stat_interval() +
  scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(-0.01, 1)) +
  scale_x_continuous(breaks = seq(0.015, 0.28, 0.05), limits = c(0.01582520, 0.2898611)) +
  theme_bw() +
  
  #stat_summary(geom = "point", fun = median)+
  #geom_vline(xintercept = median_value, col = "grey30", lty = "dashed")+
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3"))+
  guides(col = "none") +
  labs(
    title = toupper(""),
    #subtitle = plot_subtitle,
    caption = "",
    x = "Distribution of Elasticity - 250m",
    y = "Density"
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
  )
p


# create the dataframe for the legend (inside plot)

rent_title_words <- read_csv("./metadata/rent_title_words.csv")


df_for_legend <- rent_title_words %>% 
  filter(word == "beautiful")

p_legend <- df_for_legend %>% 
  ggplot(aes(word, price)) +
  stat_halfeye(fill_type = "segments", alpha = 0.3) +
  stat_interval() +
  #stat_summary(geom = "point", fun = median) +
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
      x = c(0.7, 0.80, 0.80, 1.8),
      xend = c(0.95, 0.95, 0.95, 1.8), 
      y = c(1800, 5000, 3000, 3800),
      yend = c(1800, 5000, 3000, 2500)),
    aes(x = x, xend = xend, y = y, yend = yend),
    stat = "unique", curvature = 0.2, size = 0.2, color = "grey12",
    arrow = arrow(angle = 20, length = unit(1, "mm"))
  ) +
  
  annotate("text", 
           x = 0.66,  # Position near the arrow
           y = 1800,  # Position near the arrow
           label = "50% of Data", 
           hjust = 0, 
           size = 1.5, 
           color = "grey12") +
  
  annotate("text", 
           x = 0.7,  # Position near the arrow
           y = 5000,  # Position near the arrow
           label = "95% of Data", 
           hjust = 0, 
           size = 1.5, 
           color = "grey12") +
  
  annotate("text", 
           x = 0.75,  # Position near the arrow
           y = 3000,  # Position near the arrow
           label = "80% of Data", 
           hjust = 0, 
           size = 1.5, 
           color = "grey12") +
  
  annotate("text", 
           x = 1.8,  # Position near the arrow
           y = 4000,  # Position near the arrow
           label = "Distribution of Property Price", 
           hjust = 0, 
           size = 1.5, 
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
p + inset_element(p_legend, l = 0.65, r = 1,  t = 1, b = 0.77, clip = FALSE)







