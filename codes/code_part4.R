#This code replicate the Figure 1 in manuscript. 
# We use the derived elasticities based on fuctional forms and distance interaction if there are any


library(ggplot2)
library(viridis)
library(hrbrthemes)

df <- read.csv("./metadata/meta_data_all_distance.csv")%>%
  filter(study_name != "Moore et al 2020")%>% # outliers
  filter(study_name !="Wolf & Kemp 2021") #outliers

df1 <- df %>%
  #distinct(id, .keep_all = T)%>%
  group_by(study_name)%>%
  summarise( n = n())

df <- df%>%
  left_join(df1)

# Rename study name - originally it named as "TJ, Pat, Richard"
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

# Save the plot to a file 
ggsave("./results/Figures/Figure1.png", plot = p, width = 6, height = 9, dpi = 300, bg = "white")

