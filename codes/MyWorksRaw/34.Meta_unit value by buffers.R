# clear memory
rm(list = ls())


## Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  meta,
  metafor,
  tidyverse,
  robumeta,
  clubSandwich,
  pandoc,
  tinytex,
  modelsummary,
  kableExtra,
  fixest,
  viridis,
  ipsum
)

# load data - with outliers 
df_100 <- read.csv("./metadata/Meta_data_waterfront_100.csv")%>%
  mutate(obsid = row_number())%>%
  filter(obsid != 168)%>% # outlier - 1:Swedberg el al 2020 - Multi-state model
  filter(obsid != 167)%>% # Swedberg el al 2020 - State level model - Maine
  filter(obsid != 155)%>%# Swedberg el al 2020 - Multi-state model
  group_by(study_name,geog)%>%
  mutate(cluster_id = cur_group_id())
  
df_200 <- read.csv("./metadata/Meta_data_waterfront_200.csv")%>%
  mutate(obsid = row_number())%>%
  filter(obsid != 121)%>% # outlier - 1:Swedberg el al 2020 - Multi-state model
  filter(obsid != 133)%>% # Swedberg el al 2020 - State level model - Maine
  filter(obsid != 134) # Swedberg el al 2020 - Multi-state model

df_300 <- read.csv("./metadata/Meta_data_waterfront_300.csv")%>%
  mutate(obsid = row_number())%>%
  filter(obsid != 121)%>% # outlier - 1:Swedberg el al 2020 - Multi-state model
  filter(obsid != 133)%>% # Swedberg el al 2020 - State level model - Maine
  filter(obsid != 134) # Swedberg el al 2020 - Multi-state model

df_400 <- read.csv("./metadata/Meta_data_waterfront_400.csv")%>%
  mutate(obsid = row_number())%>%
  filter(obsid != 121)%>% # outlier - 1:Swedberg el al 2020 - Multi-state model
  filter(obsid != 133)%>% # Swedberg el al 2020 - State level model - Maine
  filter(obsid != 134) # Swedberg el al 2020 - Multi-state model

df_500 <- read.csv("./metadata/Meta_data_waterfront_500.csv")%>%
  mutate(obsid = row_number())%>%
  filter(obsid != 121)%>% # outlier - 1:Swedberg el al 2020 - Multi-state model
  filter(obsid != 133)%>% # Swedberg el al 2020 - State level model - Maine
  filter(obsid != 134) # Swedberg el al 2020 - Multi-state model

df_600 <- read.csv("./metadata/Meta_data_waterfront_600.csv")%>%
  mutate(obsid = row_number())%>%
  filter(obsid != 121)%>% # outlier - 1:Swedberg el al 2020 - Multi-state model
  filter(obsid != 133)%>% # Swedberg el al 2020 - State level model - Maine
  filter(obsid != 134) # Swedberg el al 2020 - Multi-state model

df_700 <- read.csv("./metadata/Meta_data_waterfront_700.csv")%>%
  mutate(obsid = row_number())%>%
  filter(obsid != 121)%>% # outlier - 1:Swedberg el al 2020 - Multi-state model
  filter(obsid != 133)%>% # Swedberg el al 2020 - State level model - Maine
  filter(obsid != 134) # Swedberg el al 2020 - Multi-state model

df_800 <- read.csv("./metadata/Meta_data_waterfront_800.csv")%>%
  mutate(obsid = row_number())%>%
  filter(obsid != 121)%>% # outlier - 1:Swedberg el al 2020 - Multi-state model
  filter(obsid != 133)%>% # Swedberg el al 2020 - State level model - Maine
  filter(obsid != 134) # Swedberg el al 2020 - Multi-state model

df_900 <- read.csv("./metadata/Meta_data_waterfront_900.csv")%>%
  mutate(obsid = row_number())%>%
  filter(obsid != 121)%>% # outlier - 1:Swedberg el al 2020 - Multi-state model
  filter(obsid != 133)%>% # Swedberg el al 2020 - State level model - Maine
  filter(obsid != 134) # Swedberg el al 2020 - Multi-state model

df_1000 <- read.csv("./metadata/Meta_data_waterfront_1000.csv")%>%
  mutate(obsid = row_number())%>%
  filter(obsid != 121)%>% # outlier - 1:Swedberg el al 2020 - Multi-state model
  filter(obsid != 133)%>% # Swedberg el al 2020 - State level model - Maine
  filter(obsid != 134) # Swedberg el al 2020 - Multi-state model


# set up data set with required variables and necessary weighting schemes



panels <- list(
  "Waterfront distance-100m" = list(
    "Fixed efffet - Unweighted" = rma(yi = elast, vi=vi, data=df_100, method="EE", weighted=FALSE, level = 95),
    "Fixed effect - Cluster weighted " = rma(yi = elast, vi=vi, data=df_100, method="EE",weighted=T,weights = w_1, level = 95),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= rma(yi = cw_elast, vi, data=df_wf, method="EE",weighted=T, level = 95),
    "Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))"= rma(yi = elast, vi=vi, data=df_100, method="EE",weighted=T,weights =w_2, level = 95),
    #"Random effect (sample size variance)"= rma.mv(yi = cw_elast, vi, random = ~ 1 | cluster_id/obsid, data=df_wf),
    "Random effect (log(sample size variance))"= rma.mv(yi = elast, vi, random = ~ 1 | cluster_id/obsid, data=df_100)
    
  ),
  "Waterfront distance-200m" = list(
    "Fixed efffet - Unweighted" = rma(yi = elast, vi=vi, data=df_200, method="EE", weighted=FALSE, level = 95),
    "Fixed effect - Cluster weighted " = rma(yi = elast, vi=vi, data=df_200, method="EE",weighted=T,weights = w_1, level = 95),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= rma(yi = cw_elast, vi, data=df_nwf, method="EE",weighted=T, level = 95),
    "Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))"= rma(yi = elast, vi=vi, data=df_200, method="EE",weighted=T,weights = w_2,level = 95),
    #"Random effect (sample size variance)"= rma.mv(yi = cw_elast, vi, random = ~ 1 | cluster_id/obsid, data=df_nwf),
    "Random effect (log(sample size variance))"= rma.mv(yi = elast, vi, random = ~ 1 | cluster_id/obsid, data=df_200, control=list(rel.tol=1e-8))
  ),
  "Waterfront distance-300m" = list(
    "Fixed efffet - Unweighted" = rma(yi = elast, vi=vi, data=df_300, method="EE", weighted=FALSE, level = 95),
    "Fixed effect - Cluster weighted " = rma(yi = elast, vi=vi, data=df_300, method="EE",weighted=T,weights = w_1, level = 95),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= rma(yi = cw_elast, vi, data=df_nwf, method="EE",weighted=T, level = 95),
    "Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))"= rma(yi = elast, vi=vi, data=df_300, method="EE",weighted=T,weights = w_2, level = 95),
    #"Random effect (sample size variance)"= rma.mv(yi = cw_elast, vi, random = ~ 1 | cluster_id/obsid, data=df_nwf),
    "Random effect (log(sample size variance))"= rma.mv(yi = elast, vi, random = ~ 1 | cluster_id/obsid, data=df_300, control=list(rel.tol=1e-8))
  ),
  "Waterfront distance-400m" = list(
    "Fixed efffet - Unweighted" = rma(yi = elast, vi=vi, data=df_400, method="EE", weighted=FALSE, level = 95),
    "Fixed effect - Cluster weighted " = rma(yi = elast, vi=vi, data=df_400, method="EE",weighted=T,weights = w_1, level = 95),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= rma(yi = cw_elast, vi, data=df_nwf, method="EE",weighted=T, level = 95),
    "Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))"= rma(yi = elast, vi=vi, data=df_400, method="EE",weighted=T,weights = w_2, level = 95),
    #"Random effect (sample size variance)"= rma.mv(yi = cw_elast, vi, random = ~ 1 | cluster_id/obsid, data=df_nwf),
    "Random effect (log(sample size variance))"= rma.mv(yi = elast, vi, random = ~ 1 | cluster_id/obsid, data=df_400, control=list(rel.tol=1e-8))
  ),
  "Waterfront distance-500m" = list(
    "Fixed efffet - Unweighted" = rma(yi = elast, vi=vi, data=df_500, method="EE", weighted=FALSE, level = 95),
    "Fixed effect - Cluster weighted " = rma(yi = elast, vi=vi, data=df_500, method="EE",weighted=T,weights = w_1, level = 95),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= rma(yi = cw_elast, vi, data=df_nwf, method="EE",weighted=T, level = 95),
    "Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))"= rma(yi = elast, vi=vi, data=df_500, method="EE",weighted=T,weights = w_1, level = 95),
    #"Random effect (sample size variance)"= rma.mv(yi = cw_elast, vi, random = ~ 1 | cluster_id/obsid, data=df_nwf),
    "Random effect (log(sample size variance))"= rma.mv(yi = elast, vi, random = ~ 1 | cluster_id/obsid, data=df_500, control=list(rel.tol=1e-8))
  ),
  "Waterfront distance-600m" = list(
    "Fixed efffet - Unweighted" = rma(yi = elast, vi=vi, data=df_600, method="EE", weighted=FALSE, level = 95),
    "Fixed effect - Cluster weighted " = rma(yi = elast, vi=vi, data=df_600, method="EE",weighted=T,weights = w_1, level = 95),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= rma(yi = cw_elast, vi, data=df_nwf, method="EE",weighted=T, level = 95),
    "Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))"= rma(yi = elast, vi=vi, data=df_600, method="EE",weighted=T,weights = w_2, level = 95),
    #"Random effect (sample size variance)"= rma.mv(yi = cw_elast, vi, random = ~ 1 | cluster_id/obsid, data=df_nwf),
    "Random effect (log(sample size variance))"= rma.mv(yi = elast, vi, random = ~ 1 | cluster_id/obsid, data=df_600, control=list(rel.tol=1e-8))
  ),
  "Waterfront distance-700m" = list(
    "Fixed efffet - Unweighted" = rma(yi = elast, vi=vi, data=df_700, method="EE", weighted=FALSE, level = 95),
    "Fixed effect - Cluster weighted " = rma(yi = elast, vi=vi, data=df_700, method="EE",weighted=T,weights = w_1, level = 95),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= rma(yi = cw_elast, vi, data=df_nwf, method="EE",weighted=T, level = 95),
    "Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))"= rma(yi = elast, vi=vi, data=df_700, method="EE",weighted=T,weights = w_1, level = 95),
    #"Random effect (sample size variance)"= rma.mv(yi = cw_elast, vi, random = ~ 1 | cluster_id/obsid, data=df_nwf),
    "Random effect (log(sample size variance))"= rma.mv(yi = elast, vi, random = ~ 1 | cluster_id/obsid, data=df_700, control=list(rel.tol=1e-8))
  ),
  "Waterfront distance-800m" = list(
    "Fixed efffet - Unweighted" = rma(yi = elast, vi=vi, data=df_800, method="EE", weighted=FALSE, level = 95),
    "Fixed effect - Cluster weighted " = rma(yi = elast, vi, data=df_800, method="EE",weighted=T,weights = w_1, level = 95),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= rma(yi = cw_elast, vi, data=df_nwf, method="EE",weighted=T, level = 95),
    "Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))"= rma(yi = elast, vi=vi, data=df_800, method="EE",weighted=T,weights = w_2, level = 95),
    #"Random effect (sample size variance)"= rma.mv(yi = cw_elast, vi, random = ~ 1 | cluster_id/obsid, data=df_nwf),
    "Random effect (log(sample size variance))"= rma.mv(yi = elast, vi, random = ~ 1 | cluster_id/obsid, data=df_800, control=list(rel.tol=1e-8))
  ),
  "Waterfront distance-900m" = list(
    "Fixed efffet - Unweighted" = rma(yi = elast, vi=vi, data=df_900, method="EE", weighted=FALSE, level = 95),
    "Fixed effect - Cluster weighted " = rma(yi = elast, vi=vi, data=df_900, method="EE",weighted=T,weights = w_1, level = 95),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= rma(yi = cw_elast, vi, data=df_nwf, method="EE",weighted=T, level = 95),
    "Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))"= rma(yi = elast, vi=vi, data=df_900, method="EE",weighted=T,weights = w_2, level = 95),
    #"Random effect (sample size variance)"= rma.mv(yi = cw_elast, vi, random = ~ 1 | cluster_id/obsid, data=df_nwf),
    "Random effect (log(sample size variance))"= rma.mv(yi = elast, vi, random = ~ 1 | cluster_id/obsid, data=df_900, control=list(rel.tol=1e-8))
  ),
  "Waterfront distance-1000m" = list(
    "Fixed efffet - Unweighted" = rma(yi = elast, vi=vi, data=df_1000, method="EE", weighted=FALSE, level = 95),
    "Fixed effect - Cluster weighted " = rma(yi = elast, vi=vi, data=df_1000, method="EE",weighted=T,weights = w_1, level = 95),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= rma(yi = cw_elast, vi, data=df_nwf, method="EE",weighted=T, level = 95),
    "Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))"= rma(yi = elast, vi=vi, data=df_1000, method="EE",weighted=T,weights = w_1, level = 95),
    #"Random effect (sample size variance)"= rma.mv(yi = cw_elast, vi, random = ~ 1 | cluster_id/obsid, data=df_nwf),
    "Random effect (log(sample size variance))"= rma.mv(yi = elast, vi, random = ~ 1 | cluster_id/obsid, data=df_1000, control=list(rel.tol=1e-8))
  )
)


panels_robu <- list(
  "Waterfront distance-100m" = list(
    "Fixed efffet - Unweighted" = robust(panels$`Waterfront distance-100m`$`Fixed efffet - Unweighted`, cluster = obsid),
    "Fixed effect - Cluster weighted " = robust(panels$`Waterfront distance-100m`$`Fixed effect - Cluster weighted `, cluster = cluster_id),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= robust(panels$Waterfront$`Fixed effect - Cluster weighted - Varance Adjusted (sample size)`, cluster = cluster_id),
    "Fixed effect - Cluster weighted - Varance Adjusted"= robust(panels$`Waterfront distance-100m`$`Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))`, cluster = cluster_id),
    #"Random effect (sample size variance)"= robust(panels$Waterfront$`Random effect (sample size variance)`, cluster = cluster_id),
    "Random effect "= robust(panels$`Waterfront distance-100m`$`Random effect (log(sample size variance))`, cluster = cluster_id)
  ),
  "Waterfront distance-200m" = list(
    "Fixed efffet - Unweighted" =robust(panels$`Waterfront distance-200m`$`Fixed efffet - Unweighted`, cluster = obsid),
    "Fixed effect - Cluster weighted " = robust(panels$`Waterfront distance-200m`$`Fixed effect - Cluster weighted `, cluster = cluster_id),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= robust(panels$`Non-waterfront`$`Fixed effect - Cluster weighted - Varance Adjusted (sample size)`, cluster = cluster_id),
    "Fixed effect - Cluster weighted - Varance Adjusted"= robust(panels$`Waterfront distance-200m`$`Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))`, cluster = cluster_id),
    #"Random effect (sample size variance)"= robust(panels$`Non-waterfront`$`Random effect (sample size variance)`, cluster = cluster_id),
    "Random effect"= robust(panels$`Waterfront distance-200m`$`Random effect (log(sample size variance))`, cluster = cluster_id)
    
  ),
  "Waterfront distance-300m" = list(
    "Fixed efffet - Unweighted" =robust(panels$`Waterfront distance-300m`$`Fixed efffet - Unweighted`, cluster = obsid),
    "Fixed effect - Cluster weighted " = robust(panels$`Waterfront distance-300m`$`Fixed effect - Cluster weighted `, cluster = cluster_id),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= robust(panels$`Non-waterfront`$`Fixed effect - Cluster weighted - Varance Adjusted (sample size)`, cluster = cluster_id),
    "Fixed effect - Cluster weighted - Varance Adjusted"= robust(panels$`Waterfront distance-300m`$`Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))`, cluster = cluster_id),
    #"Random effect (sample size variance)"= robust(panels$`Non-waterfront`$`Random effect (sample size variance)`, cluster = cluster_id),
    "Random effect"= robust(panels$`Waterfront distance-300m`$`Random effect (log(sample size variance))`, cluster = cluster_id)
    
  ),
  "Waterfront distance-400m" = list(
    "Fixed efffet - Unweighted" =robust(panels$`Waterfront distance-400m`$`Fixed efffet - Unweighted`, cluster = obsid),
    "Fixed effect - Cluster weighted " = robust(panels$`Waterfront distance-400m`$`Fixed effect - Cluster weighted `, cluster = cluster_id),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= robust(panels$`Non-waterfront`$`Fixed effect - Cluster weighted - Varance Adjusted (sample size)`, cluster = cluster_id),
    "Fixed effect - Cluster weighted - Varance Adjusted"= robust(panels$`Waterfront distance-400m`$`Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))`, cluster = cluster_id),
    #"Random effect (sample size variance)"= robust(panels$`Non-waterfront`$`Random effect (sample size variance)`, cluster = cluster_id),
    "Random effect"= robust(panels$`Waterfront distance-400m`$`Random effect (log(sample size variance))`, cluster = cluster_id)
    
  ),
  "Waterfront distance-500m" = list(
    "Fixed efffet - Unweighted" =robust(panels$`Waterfront distance-500m`$`Fixed efffet - Unweighted`, cluster = obsid),
    "Fixed effect - Cluster weighted " = robust(panels$`Waterfront distance-500m`$`Fixed effect - Cluster weighted `, cluster = cluster_id),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= robust(panels$`Non-waterfront`$`Fixed effect - Cluster weighted - Varance Adjusted (sample size)`, cluster = cluster_id),
    "Fixed effect - Cluster weighted - Varance Adjusted"= robust(panels$`Waterfront distance-500m`$`Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))`, cluster = cluster_id),
    #"Random effect (sample size variance)"= robust(panels$`Non-waterfront`$`Random effect (sample size variance)`, cluster = cluster_id),
    "Random effect"= robust(panels$`Waterfront distance-500m`$`Random effect (log(sample size variance))`, cluster = cluster_id)
    
  ),
  "Waterfront distance-600m" = list(
    "Fixed efffet - Unweighted" =robust(panels$`Waterfront distance-600m`$`Fixed efffet - Unweighted`, cluster = obsid),
    "Fixed effect - Cluster weighted " = robust(panels$`Waterfront distance-600m`$`Fixed effect - Cluster weighted `, cluster = cluster_id),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= robust(panels$`Non-waterfront`$`Fixed effect - Cluster weighted - Varance Adjusted (sample size)`, cluster = cluster_id),
    "Fixed effect - Cluster weighted - Varance Adjusted"= robust(panels$`Waterfront distance-600m`$`Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))`, cluster = cluster_id),
    #"Random effect (sample size variance)"= robust(panels$`Non-waterfront`$`Random effect (sample size variance)`, cluster = cluster_id),
    "Random effect"= robust(panels$`Waterfront distance-600m`$`Random effect (log(sample size variance))`, cluster = cluster_id)
  ),
  "Waterfront distance-700m" = list(
    "Fixed efffet - Unweighted" =robust(panels$`Waterfront distance-700m`$`Fixed efffet - Unweighted`, cluster = obsid),
    "Fixed effect - Cluster weighted " = robust(panels$`Waterfront distance-700m`$`Fixed effect - Cluster weighted `, cluster = cluster_id),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= robust(panels$`Non-waterfront`$`Fixed effect - Cluster weighted - Varance Adjusted (sample size)`, cluster = cluster_id),
    "Fixed effect - Cluster weighted - Varance Adjusted"= robust(panels$`Waterfront distance-700m`$`Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))`, cluster = cluster_id),
    #"Random effect (sample size variance)"= robust(panels$`Non-waterfront`$`Random effect (sample size variance)`, cluster = cluster_id),
    "Random effect"= robust(panels$`Waterfront distance-700m`$`Random effect (log(sample size variance))`, cluster = cluster_id)
  ),
  "Waterfront distance-800m" = list(
    "Fixed efffet - Unweighted" =robust(panels$`Waterfront distance-800m`$`Fixed efffet - Unweighted`, cluster = obsid),
    "Fixed effect - Cluster weighted " = robust(panels$`Waterfront distance-800m`$`Fixed effect - Cluster weighted `, cluster = cluster_id),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= robust(panels$`Non-waterfront`$`Fixed effect - Cluster weighted - Varance Adjusted (sample size)`, cluster = cluster_id),
    "Fixed effect - Cluster weighted - Varance Adjusted"= robust(panels$`Waterfront distance-800m`$`Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))`, cluster = cluster_id),
    #"Random effect (sample size variance)"= robust(panels$`Non-waterfront`$`Random effect (sample size variance)`, cluster = cluster_id),
    "Random effect"= robust(panels$`Waterfront distance-800m`$`Random effect (log(sample size variance))`, cluster = cluster_id)
  ),
  "Waterfront distance-900m" = list(
    "Fixed efffet - Unweighted" =robust(panels$`Waterfront distance-900m`$`Fixed efffet - Unweighted`, cluster = obsid),
    "Fixed effect - Cluster weighted " = robust(panels$`Waterfront distance-900m`$`Fixed effect - Cluster weighted `, cluster = cluster_id),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= robust(panels$`Non-waterfront`$`Fixed effect - Cluster weighted - Varance Adjusted (sample size)`, cluster = cluster_id),
    "Fixed effect - Cluster weighted - Varance Adjusted"= robust(panels$`Waterfront distance-900m`$`Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))`, cluster = cluster_id),
    #"Random effect (sample size variance)"= robust(panels$`Non-waterfront`$`Random effect (sample size variance)`, cluster = cluster_id),
    "Random effect"= robust(panels$`Waterfront distance-900m`$`Random effect (log(sample size variance))`, cluster = cluster_id)
  ),
  "Waterfront distance-1000m" = list(
    "Fixed efffet - Unweighted" =robust(panels$`Waterfront distance-1000m`$`Fixed efffet - Unweighted`, cluster = obsid),
    "Fixed effect - Cluster weighted " = robust(panels$`Waterfront distance-1000m`$`Fixed effect - Cluster weighted `, cluster = cluster_id),
    #"Fixed effect - Cluster weighted - Varance Adjusted (sample size)"= robust(panels$`Non-waterfront`$`Fixed effect - Cluster weighted - Varance Adjusted (sample size)`, cluster = cluster_id),
    "Fixed effect - Cluster weighted - Varance Adjusted"= robust(panels$`Waterfront distance-1000m`$`Fixed effect - Cluster weighted - Varance Adjusted (log(sample size))`, cluster = cluster_id),
    #"Random effect (sample size variance)"= robust(panels$`Non-waterfront`$`Random effect (sample size variance)`, cluster = cluster_id),
    "Random effect"= robust(panels$`Waterfront distance-1000m`$`Random effect (log(sample size variance))`, cluster = cluster_id)
  )
)

f <- function(x) format(round(x, 3), big.mark=",")


gm <- list(
  list("raw" = "nobs", "clean" = "Num. of Obs.", "fmt" = f)
)


modelsummary(
  panels_robu,
  shape = "rbind",
  stars = TRUE, gof_map = gm)


modelsummary(
  panels_robu,
  shape = "rbind",
  stars = TRUE, gof_map = gm,
  coef_rename = c("overall" = "Estimated mean"), output = "./results/Tables/table2_distabuffer_elast.tex")


