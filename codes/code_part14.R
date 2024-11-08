# This code replicate the Figure 6 in manuscript

# clear memory
rm(list = ls())

# Load required libraries
library(sf)
library(dplyr)
library(tidyverse)

############# BC BC BC ###########################################################
# Step 1: Load the point shapefile
points <- st_read("./Building_footprint/BC/bc_all_004_1000m.shp")

# Step 2: Set a projected CRS with units in meters, if necessary
# Example: UTM Zone 13N (EPSG:32613)
points <- st_transform(points, crs = 32613)

# Step 3: Create a grid over the extent of the points with a specified resolution
resolution <- 10000  # Set the desired resolution in meters
grid <- st_make_grid(points, cellsize = resolution, what = "polygons")

# Step 4: Keep only grid cells that intersect with the points
# Use st_intersects to find overlapping cells
intersecting_cells <- grid[lengths(st_intersects(grid, points)) > 0]


df_sim_final <- read_csv("./results/simulation/simulation_final_df.csv")%>%
  filter(prov=="BC")

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
  ungroup()

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  select(GeoUID)

df_cen$GeoUID <- as.numeric(df_cen$GeoUID)


df_cen <- df_cen%>%
  left_join(df_all)

# Ensure both shapefiles use the same CRS
intersecting_cells <- st_transform(intersecting_cells, st_crs(df_cen))

# Step 2: Perform a spatial join to attach CDA attributes to overlapping grid cells
# This will add CDA attributes to grid cells where they overlap

grid_with_cda_attributes <- st_join(st_as_sf(intersecting_cells), st_as_sf(df_cen), left = FALSE)


grid_with_cda_attributes <- grid_with_cda_attributes%>%
  filter(ave_tot!=0)%>%
  select(ave_tot)


# Ensure it is still an sf object and save as a shapefile
if (inherits(grid_with_cda_attributes, "sf")) {
  st_write(grid_with_cda_attributes, "./Census/BC/BC_valuegrids.shp", delete_dsn = T)
} else {
  message("The result is not an sf object.")
}


st_write(grid_with_cda_attributes, "./Census/BC/BC_valuegrids.shp", delete_dsn = T)


############# AB AB AB ###########################################################
# Step 1: Load the point shapefile
points <- st_read("./Building_footprint/AB/ab_all_004_1000m.shp")

# Step 2: Set a projected CRS with units in meters, if necessary
# Example: UTM Zone 13N (EPSG:32613)
points <- st_transform(points, crs = 32613)

# Step 3: Create a grid over the extent of the points with a specified resolution
resolution <- 10000  # Set the desired resolution in meters
grid <- st_make_grid(points, cellsize = resolution, what = "polygons")

# Step 4: Keep only grid cells that intersect with the points
# Use st_intersects to find overlapping cells
intersecting_cells <- grid[lengths(st_intersects(grid, points)) > 0]


df_sim_final <- read_csv("./results/simulation/simulation_final_df.csv")%>%
  filter(prov=="AB")

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  as.data.frame()%>%
  select(-geometry)%>%
  filter(prov=="AB")%>%
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
  ungroup()

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  filter(prov=="AB")%>%
  select(GeoUID)

df_cen$GeoUID <- as.numeric(df_cen$GeoUID)


df_cen <- df_cen%>%
  left_join(df_all)

# Ensure both shapefiles use the same CRS
intersecting_cells <- st_transform(intersecting_cells, st_crs(df_cen))

# Step 2: Perform a spatial join to attach CDA attributes to overlapping grid cells
# This will add CDA attributes to grid cells where they overlap

grid_with_cda_attributes <- st_join(st_as_sf(intersecting_cells), st_as_sf(df_cen), left = FALSE)


grid_with_cda_attributes <- grid_with_cda_attributes%>%
  filter(ave_tot!=0)%>%
  select(ave_tot)


# Ensure it is still an sf object and save as a shapefile
if (inherits(grid_with_cda_attributes, "sf")) {
  st_write(grid_with_cda_attributes, "./Census/AB/AB_valuegrids.shp", delete_dsn = T)
} else {
  message("The result is not an sf object.")
}

############# SK SK SK ###########################################################
# Step 1: Load the point shapefile
points <- st_read("./Building_footprint/SK/sk_all_004_1000m.shp")

# Step 2: Set a projected CRS with units in meters, if necessary
# Example: UTM Zone 13N (EPSG:32613)
points <- st_transform(points, crs = 32613)

# Step 3: Create a grid over the extent of the points with a specified resolution
resolution <- 10000  # Set the desired resolution in meters
grid <- st_make_grid(points, cellsize = resolution, what = "polygons")

# Step 4: Keep only grid cells that intersect with the points
# Use st_intersects to find overlapping cells
intersecting_cells <- grid[lengths(st_intersects(grid, points)) > 0]


df_sim_final <- read_csv("./results/simulation/simulation_final_df.csv")%>%
  filter(prov=="SK")

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  as.data.frame()%>%
  select(-geometry)%>%
  filter(prov=="SK")%>%
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
  ungroup()

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  filter(prov=="SK")%>%
  select(GeoUID)

df_cen$GeoUID <- as.numeric(df_cen$GeoUID)


df_cen <- df_cen%>%
  left_join(df_all)

# Ensure both shapefiles use the same CRS
intersecting_cells <- st_transform(intersecting_cells, st_crs(df_cen))

# Step 2: Perform a spatial join to attach CDA attributes to overlapping grid cells
# This will add CDA attributes to grid cells where they overlap

grid_with_cda_attributes <- st_join(st_as_sf(intersecting_cells), st_as_sf(df_cen), left = FALSE)


grid_with_cda_attributes <- grid_with_cda_attributes%>%
  filter(ave_tot!=0)%>%
  select(ave_tot)


# Ensure it is still an sf object and save as a shapefile
if (inherits(grid_with_cda_attributes, "sf")) {
  st_write(grid_with_cda_attributes, "./Census/SK/SK_valuegrids.shp", delete_dsn = T)
} else {
  message("The result is not an sf object.")
}


############# MB MB MB ###########################################################
# Step 1: Load the point shapefile
points <- st_read("./Building_footprint/MB/mb_all_004_1000m.shp")

# Step 2: Set a projected CRS with units in meters, if necessary
# Example: UTM Zone 13N (EPSG:32613)
points <- st_transform(points, crs = 32613)

# Step 3: Create a grid over the extent of the points with a specified resolution
resolution <- 10000  # Set the desired resolution in meters
grid <- st_make_grid(points, cellsize = resolution, what = "polygons")

# Step 4: Keep only grid cells that intersect with the points
# Use st_intersects to find overlapping cells
intersecting_cells <- grid[lengths(st_intersects(grid, points)) > 0]


df_sim_final <- read_csv("./results/simulation/simulation_final_df.csv")%>%
  filter(prov=="MB")

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  as.data.frame()%>%
  select(-geometry)%>%
  filter(prov=="MB")%>%
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
  ungroup()

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  filter(prov=="MB")%>%
  select(GeoUID)

df_cen$GeoUID <- as.numeric(df_cen$GeoUID)


df_cen <- df_cen%>%
  left_join(df_all)

# Ensure both shapefiles use the same CRS
intersecting_cells <- st_transform(intersecting_cells, st_crs(df_cen))

# Step 2: Perform a spatial join to attach CDA attributes to overlapping grid cells
# This will add CDA attributes to grid cells where they overlap

grid_with_cda_attributes <- st_join(st_as_sf(intersecting_cells), st_as_sf(df_cen), left = FALSE)


grid_with_cda_attributes <- grid_with_cda_attributes%>%
  filter(ave_tot!=0)%>%
  select(ave_tot)


# Ensure it is still an sf object and save as a shapefile
if (inherits(grid_with_cda_attributes, "sf")) {
  st_write(grid_with_cda_attributes, "./Census/MB/MB_valuegrids.shp", delete_dsn = T)
} else {
  message("The result is not an sf object.")
}


############# ON ON ON ###########################################################
# Step 1: Load the point shapefile
points <- st_read("./Building_footprint/ON/on_all_004_1000m.shp")

# Step 2: Set a projected CRS with units in meters, if necessary
# Example: UTM Zone 13N (EPSG:32613)
points <- st_transform(points, crs = 32613)

# Step 3: Create a grid over the extent of the points with a specified resolution
resolution <- 10000  # Set the desired resolution in meters
grid <- st_make_grid(points, cellsize = resolution, what = "polygons")

# Step 4: Keep only grid cells that intersect with the points
# Use st_intersects to find overlapping cells
intersecting_cells <- grid[lengths(st_intersects(grid, points)) > 0]


df_sim_final <- read_csv("./results/simulation/simulation_final_df.csv")%>%
  filter(prov=="ON")

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  as.data.frame()%>%
  select(-geometry)%>%
  filter(prov=="ON")%>%
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
  ungroup()

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  filter(prov=="ON")%>%
  select(GeoUID)

df_cen$GeoUID <- as.numeric(df_cen$GeoUID)


df_cen <- df_cen%>%
  left_join(df_all)

# Ensure both shapefiles use the same CRS
intersecting_cells <- st_transform(intersecting_cells, st_crs(df_cen))

# Step 2: Perform a spatial join to attach CDA attributes to overlapping grid cells
# This will add CDA attributes to grid cells where they overlap

grid_with_cda_attributes <- st_join(st_as_sf(intersecting_cells), st_as_sf(df_cen), left = FALSE)


grid_with_cda_attributes <- grid_with_cda_attributes%>%
  filter(ave_tot!=0)%>%
  select(ave_tot)


# Ensure it is still an sf object and save as a shapefile
if (inherits(grid_with_cda_attributes, "sf")) {
  st_write(grid_with_cda_attributes, "./Census/ON/ON_valuegrids.shp", delete_dsn = T)
} else {
  message("The result is not an sf object.")
}

############# QE QE QE ###########################################################
# Step 1: Load the point shapefile
points <- st_read("./Building_footprint/QC/qc_all_004_1000m.shp")

# Step 2: Set a projected CRS with units in meters, if necessary
# Example: UTM Zone 13N (EPSG:32613)
points <- st_transform(points, crs = 32613)

# Step 3: Create a grid over the extent of the points with a specified resolution
resolution <- 10000  # Set the desired resolution in meters
grid <- st_make_grid(points, cellsize = resolution, what = "polygons")

# Step 4: Keep only grid cells that intersect with the points
# Use st_intersects to find overlapping cells
intersecting_cells <- grid[lengths(st_intersects(grid, points)) > 0]


df_sim_final <- read_csv("./results/simulation/simulation_final_df.csv")%>%
  filter(prov=="QC")

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  as.data.frame()%>%
  select(-geometry)%>%
  filter(prov=="QC")%>%
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
  ungroup()

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  filter(prov=="QC")%>%
  select(GeoUID)

df_cen$GeoUID <- as.numeric(df_cen$GeoUID)


df_cen <- df_cen%>%
  left_join(df_all)

# Ensure both shapefiles use the same CRS
intersecting_cells <- st_transform(intersecting_cells, st_crs(df_cen))

# Step 2: Perform a spatial join to attach CDA attributes to overlapping grid cells
# This will add CDA attributes to grid cells where they overlap

grid_with_cda_attributes <- st_join(st_as_sf(intersecting_cells), st_as_sf(df_cen), left = FALSE)


grid_with_cda_attributes <- grid_with_cda_attributes%>%
  filter(ave_tot!=0)%>%
  select(ave_tot)


# Ensure it is still an sf object and save as a shapefile
if (inherits(grid_with_cda_attributes, "sf")) {
  st_write(grid_with_cda_attributes, "./Census/QC/QC_valuegrids.shp", delete_dsn = T)
} else {
  message("The result is not an sf object.")
}


############# NS NS NS ###########################################################
# Step 1: Load the point shapefile
points <- st_read("./Building_footprint/NS/ns_all_004_1000m.shp")

# Step 2: Set a projected CRS with units in meters, if necessary
# Example: UTM Zone 13N (EPSG:32613)
points <- st_transform(points, crs = 32613)

# Step 3: Create a grid over the extent of the points with a specified resolution
resolution <- 10000  # Set the desired resolution in meters
grid <- st_make_grid(points, cellsize = resolution, what = "polygons")

# Step 4: Keep only grid cells that intersect with the points
# Use st_intersects to find overlapping cells
intersecting_cells <- grid[lengths(st_intersects(grid, points)) > 0]


df_sim_final <- read_csv("./results/simulation/simulation_final_df.csv")%>%
  filter(prov=="NS")

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  as.data.frame()%>%
  select(-geometry)%>%
  filter(prov=="NS")%>%
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
  ungroup()

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  filter(prov=="NS")%>%
  select(GeoUID)

df_cen$GeoUID <- as.numeric(df_cen$GeoUID)


df_cen <- df_cen%>%
  left_join(df_all)

# Ensure both shapefiles use the same CRS
intersecting_cells <- st_transform(intersecting_cells, st_crs(df_cen))

# Step 2: Perform a spatial join to attach CDA attributes to overlapping grid cells
# This will add CDA attributes to grid cells where they overlap

grid_with_cda_attributes <- st_join(st_as_sf(intersecting_cells), st_as_sf(df_cen), left = FALSE)


grid_with_cda_attributes <- grid_with_cda_attributes%>%
  filter(ave_tot!=0)%>%
  select(ave_tot)


# Ensure it is still an sf object and save as a shapefile
if (inherits(grid_with_cda_attributes, "sf")) {
  st_write(grid_with_cda_attributes, "./Census/NS/NS_valuegrids.shp", delete_dsn = T)
} else {
  message("The result is not an sf object.")
}


############# NL NL NL ###########################################################
# Step 1: Load the point shapefile
points <- st_read("./Building_footprint/NL/nl_all_004_1000m.shp")

# Step 2: Set a projected CRS with units in meters, if necessary
# Example: UTM Zone 13N (EPSG:32613)
points <- st_transform(points, crs = 32613)

# Step 3: Create a grid over the extent of the points with a specified resolution
resolution <- 10000  # Set the desired resolution in meters
grid <- st_make_grid(points, cellsize = resolution, what = "polygons")

# Step 4: Keep only grid cells that intersect with the points
# Use st_intersects to find overlapping cells
intersecting_cells <- grid[lengths(st_intersects(grid, points)) > 0]


df_sim_final <- read_csv("./results/simulation/simulation_final_df.csv")%>%
  filter(prov=="NL")

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  as.data.frame()%>%
  select(-geometry)%>%
  filter(prov=="NL")%>%
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
  ungroup()

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  filter(prov=="NL")%>%
  select(GeoUID)

df_cen$GeoUID <- as.numeric(df_cen$GeoUID)


df_cen <- df_cen%>%
  left_join(df_all)

# Ensure both shapefiles use the same CRS
intersecting_cells <- st_transform(intersecting_cells, st_crs(df_cen))

# Step 2: Perform a spatial join to attach CDA attributes to overlapping grid cells
# This will add CDA attributes to grid cells where they overlap

grid_with_cda_attributes <- st_join(st_as_sf(intersecting_cells), st_as_sf(df_cen), left = FALSE)


grid_with_cda_attributes <- grid_with_cda_attributes%>%
  filter(ave_tot!=0)%>%
  select(ave_tot)


# Ensure it is still an sf object and save as a shapefile
if (inherits(grid_with_cda_attributes, "sf")) {
  st_write(grid_with_cda_attributes, "./Census/NL/NL_valuegrids.shp", delete_dsn = T)
} else {
  message("The result is not an sf object.")
}

############# NB NB NB ###########################################################
# Step 1: Load the point shapefile
points <- st_read("./Building_footprint/NB/nb_all_004_1000m.shp")

# Step 2: Set a projected CRS with units in meters, if necessary
# Example: UTM Zone 13N (EPSG:32613)
points <- st_transform(points, crs = 32613)

# Step 3: Create a grid over the extent of the points with a specified resolution
resolution <- 10000  # Set the desired resolution in meters
grid <- st_make_grid(points, cellsize = resolution, what = "polygons")

# Step 4: Keep only grid cells that intersect with the points
# Use st_intersects to find overlapping cells
intersecting_cells <- grid[lengths(st_intersects(grid, points)) > 0]


df_sim_final <- read_csv("./results/simulation/simulation_final_df.csv")%>%
  filter(prov=="NB")

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  as.data.frame()%>%
  select(-geometry)%>%
  filter(prov=="NB")%>%
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
  ungroup()

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  filter(prov=="NB")%>%
  select(GeoUID)

df_cen$GeoUID <- as.numeric(df_cen$GeoUID)


df_cen <- df_cen%>%
  left_join(df_all)

# Ensure both shapefiles use the same CRS
intersecting_cells <- st_transform(intersecting_cells, st_crs(df_cen))

# Step 2: Perform a spatial join to attach CDA attributes to overlapping grid cells
# This will add CDA attributes to grid cells where they overlap

grid_with_cda_attributes <- st_join(st_as_sf(intersecting_cells), st_as_sf(df_cen), left = FALSE)


grid_with_cda_attributes <- grid_with_cda_attributes%>%
  filter(ave_tot!=0)%>%
  select(ave_tot)


# Ensure it is still an sf object and save as a shapefile
if (inherits(grid_with_cda_attributes, "sf")) {
  st_write(grid_with_cda_attributes, "./Census/NB/NB_valuegrids.shp", delete_dsn = T)
} else {
  message("The result is not an sf object.")
}

############# PE PE PE ###########################################################
# Step 1: Load the point shapefile
points <- st_read("./Building_footprint/PE/pe_all_004_1000m.shp")

# Step 2: Set a projected CRS with units in meters, if necessary
# Example: UTM Zone 13N (EPSG:32613)
points <- st_transform(points, crs = 32613)

# Step 3: Create a grid over the extent of the points with a specified resolution
resolution <- 10000  # Set the desired resolution in meters
grid <- st_make_grid(points, cellsize = resolution, what = "polygons")

# Step 4: Keep only grid cells that intersect with the points
# Use st_intersects to find overlapping cells
intersecting_cells <- grid[lengths(st_intersects(grid, points)) > 0]


df_sim_final <- read_csv("./results/simulation/simulation_final_df.csv")%>%
  filter(prov=="PE")

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  as.data.frame()%>%
  select(-geometry)%>%
  filter(prov=="PE")%>%
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
  ungroup()

# census
df_cen <- st_read("./Census/CAN/can.shp")%>% # census data for er person values - population
  filter(prov=="PE")%>%
  select(GeoUID)

df_cen$GeoUID <- as.numeric(df_cen$GeoUID)


df_cen <- df_cen%>%
  left_join(df_all)

# Ensure both shapefiles use the same CRS
intersecting_cells <- st_transform(intersecting_cells, st_crs(df_cen))

# Step 2: Perform a spatial join to attach CDA attributes to overlapping grid cells
# This will add CDA attributes to grid cells where they overlap

grid_with_cda_attributes <- st_join(st_as_sf(intersecting_cells), st_as_sf(df_cen), left = FALSE)


grid_with_cda_attributes <- grid_with_cda_attributes%>%
  filter(ave_tot!=0)%>%
  select(ave_tot)


# Ensure it is still an sf object and save as a shapefile
if (inherits(grid_with_cda_attributes, "sf")) {
  st_write(grid_with_cda_attributes, "./Census/PE/PE_valuegrids.shp", delete_dsn = T)
} else {
  message("The result is not an sf object.")
}


bc <- st_read("./Census/BC/BC_valuegrids.shp")
ab <- st_read("./Census/AB/AB_valuegrids.shp")
sk <- st_read("./Census/SK/SK_valuegrids.shp")
mb <- st_read("./Census/MB/MB_valuegrids.shp")
on <- st_read("./Census/ON/ON_valuegrids.shp")
qc <- st_read("./Census/QC/QC_valuegrids.shp")
ns <- st_read("./Census/NS/NS_valuegrids.shp")
nl <- st_read("./Census/NL/NL_valuegrids.shp")
nb <- st_read("./Census/NB/NB_valuegrids.shp")
pe <- st_read("./Census/PE/PE_valuegrids.shp")



df <- rbind(bc,ab,sk,mb,on,qc,ns,nl,nb,pe)



if (inherits(df, "sf")) {
  st_write(df, "./Census/CAN/CAN_valuegrids.shp", delete_dsn = T)
} else {
  message("The result is not an sf object.")
}






