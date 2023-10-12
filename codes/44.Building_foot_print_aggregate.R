# clear memory
rm(list = ls())


## Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  tidyverse
)

# 250m

bfp_mode_250_ratio <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_50_1000sqm_at_each_cen_da.shp")%>%
  as.data.frame()%>%
  mutate(ratio_mode_da_bfp = Dwllngs/Join_Count)%>%
  rename("mode_bfp_count_250_da" = "Join_Count")%>%
  select(GeoUID,Dwllngs,ratio_mode_da_bfp,mode_bfp_count_250_da)

bfp_lb_250 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_lb_250m.shp")%>%
  as.data.frame()%>%
  rename("row_lb_count_250" = "Join_Count")%>%
  select(GeoUID,row_lb_count_250)
  
bfp_lb_250_ratio <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_50_100sqm_at_each_cen_da.shp")%>%
  as.data.frame()%>%
  mutate(ratio_lb_da_bfp = Dwllngs/Join_Count)%>%
  rename("lb_bfp_count_250_da" = "Join_Count")%>%
  select(GeoUID,ratio_lb_da_bfp,lb_bfp_count_250_da)

bfp_ub_250 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_ub_250m.shp")%>%
  as.data.frame()%>%
  rename("row_ub_count_250" = "Join_Count")%>%
  select(GeoUID,row_ub_count_250)

bfp_ub_250_ratio <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_2000sqm_at_each_cen_da.shp")%>%
  as.data.frame()%>%
  mutate(ratio_ub_da_bfp = Dwllngs/Join_Count)%>%
  rename("ub_bfp_count_250_da" = "Join_Count")%>%
  select(GeoUID,ratio_ub_da_bfp,ub_bfp_count_250_da)

bfp_250 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_mode_250m.shp")%>%
  as.data.frame()%>%
  rename("row_mode_count_250" = "Join_Count")%>%
  select(GeoUID,row_mode_count_250)%>%
  left_join(bfp_mode_250_ratio)%>%
  mutate(adj_mode_count_250 = ratio_mode_da_bfp*row_mode_count_250)%>%
  left_join(bfp_lb_250)%>%
  left_join(bfp_lb_250_ratio)%>%
  mutate(adj_lb_count_250 = ratio_lb_da_bfp*row_lb_count_250)%>%
  left_join(bfp_ub_250)%>%
  left_join(bfp_ub_250_ratio)%>%
  mutate(adj_ub_count_250 = ratio_ub_da_bfp*row_ub_count_250)%>% # if we do not need seperate bfp count sperate not need following
  select(GeoUID,ratio_mode_da_bfp,row_mode_count_250,row_lb_count_250,row_ub_count_250)%>%
  mutate(adj_mode_count_250 = ratio_mode_da_bfp*row_mode_count_250)%>%
  mutate(adj_lb_count_250 = ratio_mode_da_bfp*row_lb_count_250)%>%
  mutate(adj_ub_count_250 = ratio_mode_da_bfp*row_ub_count_250)%>%
  mutate_if(is.numeric, round)%>%
  select(GeoUID,adj_mode_count_250,adj_lb_count_250,adj_ub_count_250)

#############################################################################

# 500m


bfp_mode_500_ratio <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_50_1000sqm_at_each_cen_da.shp")%>%
  as.data.frame()%>%
  mutate(ratio_mode_da_bfp = Dwllngs/Join_Count)%>%
  rename("mode_bfp_count_500_da" = "Join_Count")%>%
  select(GeoUID,Dwllngs,ratio_mode_da_bfp,mode_bfp_count_500_da)

bfp_lb_500 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_lb_500m.shp")%>%
  as.data.frame()%>%
  rename("row_lb_count_500" = "Join_Count")%>%
  select(GeoUID,row_lb_count_500)

bfp_lb_500_ratio <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_50_100sqm_at_each_cen_da.shp")%>%
  as.data.frame()%>%
  mutate(ratio_lb_da_bfp = Dwllngs/Join_Count)%>%
  rename("lb_bfp_count_500_da" = "Join_Count")%>%
  select(GeoUID,ratio_lb_da_bfp,lb_bfp_count_500_da)

bfp_ub_500 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_ub_500m.shp")%>%
  as.data.frame()%>%
  rename("row_ub_count_500" = "Join_Count")%>%
  select(GeoUID,row_ub_count_500)

bfp_ub_500_ratio <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_2000sqm_at_each_cen_da.shp")%>%
  as.data.frame()%>%
  mutate(ratio_ub_da_bfp = Dwllngs/Join_Count)%>%
  rename("ub_bfp_count_500_da" = "Join_Count")%>%
  select(GeoUID,ratio_ub_da_bfp,ub_bfp_count_500_da)

bfp_500 <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_mode_500m.shp")%>%
  as.data.frame()%>%
  rename("row_mode_count_500" = "Join_Count")%>%
  select(GeoUID,row_mode_count_500)%>%
  left_join(bfp_mode_500_ratio)%>%
  mutate(adj_mode_count_500 = ratio_mode_da_bfp*row_mode_count_500)%>%
  left_join(bfp_lb_500)%>%
  left_join(bfp_lb_500_ratio)%>%
  mutate(adj_lb_count_500 = ratio_lb_da_bfp*row_lb_count_500)%>%
  left_join(bfp_ub_500)%>%
  left_join(bfp_ub_500_ratio)%>%
  mutate(adj_ub_count_500 = ratio_ub_da_bfp*row_ub_count_500)%>% # if we do not need seperate bfp count sperate not need following
  select(GeoUID,ratio_mode_da_bfp,row_mode_count_500,row_lb_count_500,row_ub_count_500)%>%
  mutate(adj_mode_count_500 = ratio_mode_da_bfp*row_mode_count_500)%>%
  mutate(adj_lb_count_500 = ratio_mode_da_bfp*row_lb_count_500)%>%
  mutate(adj_ub_count_500 = ratio_mode_da_bfp*row_ub_count_500)%>%
  mutate_if(is.numeric, round)%>%
  select(GeoUID,adj_mode_count_500,adj_lb_count_500,adj_ub_count_500)

df <- bfp_250%>%
  left_join(bfp_500)
  

write.csv(df,"~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Census/AB/bfp_count_for_mc.csv" ) 

################################################################################

