# This code create spatial buffers at the distcance of 500 meters and 1000 meters  arounf the lakes that are larger than 4km2
# Lakes data acess through Canvec data series https://open.canada.ca/data/en/dataset/9d96e8c9-22fe-4ad2-b5e8-94a6991b744b
# this will yake some time to run
# clear memory
rm(list = ls())

library(sf)

#setwd(".~/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/")               # Tim's working directory (Mac)

buffer1=500
buffer2=1000

#AB
PR <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_AB_Hydro/ab004.shp")

PR_500 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer1) # buffer by 500 meters

PR_1000 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer2) # buffer by 100 meters

PR_500 <- st_transform(PR_500, crs = 3348)
PR_1000 <- st_transform(PR_1000, crs = 3348)

st_write(PR_500, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_AB_hydro/ab_all_004_500m.shp", delete_dsn = T)
st_write(PR_1000, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_AB_hydro/ab_all_004_1000m.shp", delete_dsn = T)

#BC
PR <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_BC_hydro/bc004.shp")

PR_500 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer1) # buffer by 500 meters

PR_1000 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer2) # buffer by 1000 meters

PR_500 <- st_transform(PR_500, crs = 3348)
PR_1000 <- st_transform(PR_1000, crs = 3348)

st_write(PR_500, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_BC_hydro/bc_all_004_500m.shp", delete_dsn = T)
st_write(PR_1000, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_BC_hydro/bc_all_004_1000m.shp", delete_dsn = T)

#MB
PR <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_MB_Hydro/mb004.shp")

PR_500 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer1) # buffer by 500 meters

PR_1000 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer2) # buffer by 1000 meters

PR_500 <- st_transform(PR_500, crs = 3348)
PR_1000 <- st_transform(PR_1000, crs = 3348)

st_write(PR_500, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_MB_hydro/mb_all_004_500m.shp", delete_dsn = T)
st_write(PR_1000, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_MB_hydro/mb_all_004_1000m.shp", delete_dsn = T)

#NB
PR <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_NB_hydro/nb004.shp")

PR_500 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer1) # buffer by 500 meters

PR_1000 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer2) # buffer by 1000 meters

PR_500 <- st_transform(PR_500, crs = 3348)
PR_1000 <- st_transform(PR_1000, crs = 3348)

st_write(PR_500, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_NB_hydro/nb_all_004_500m.shp", delete_dsn = T)
st_write(PR_1000, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_NB_hydro/nb_all_004_1000m.shp", delete_dsn = T)

#NL
PR <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_NL_hydro/nl004.shp")

PR_500 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer1) # buffer by 500 meters

PR_1000 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer2) # buffer by 1000 meters

PR_500 <- st_transform(PR_500, crs = 3348)
PR_1000 <- st_transform(PR_1000, crs = 3348)

st_write(PR_500, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_NL_hydro/nl_all_004_500m.shp", delete_dsn = T)
st_write(PR_1000, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_NL_hydro/nl_all_004_1000m.shp", delete_dsn = T)



#NS
PR <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_NS_hydro/ns004.shp")

PR_500 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer1) # buffer by 500 meters

PR_1000 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer2) # buffer by 1000 meters

PR_500 <- st_transform(PR_500, crs = 3348)
PR_1000 <- st_transform(PR_1000, crs = 3348)

st_write(PR_500, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_NS_hydro/ns_all_004_500m.shp", delete_dsn = T)
st_write(PR_1000, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_NS_hydro/ns_all_004_1000m.shp", delete_dsn = T)

#PE
PR <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_PE_hydro/pe004.shp")

PR_500 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer1) # buffer by 500 meters

PR_1000 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer2) # buffer by 1000 meters

PR_500 <- st_transform(PR_500, crs = 3348)
PR_1000 <- st_transform(PR_1000, crs = 3348)

st_write(PR_500, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_PE_hydro/pe_all_004_500m.shp", delete_dsn = T)
st_write(PR_1000, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_PE_hydro/pe_all_004_1000m.shp", delete_dsn = T)

#ON
PR <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_ON_hydro/onlakes004.shp")

PR_500 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer1) # buffer by 500 meters

PR_1000 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer2) # buffer by 1000 meters

PR_500 <- st_transform(PR_500, crs = 3348)
PR_1000 <- st_transform(PR_1000, crs = 3348)

st_write(PR_500, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_ON_hydro/on_all_004_500m.shp", delete_dsn = T)
st_write(PR_1000, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_ON_hydro/on_all_004_1000m.shp", delete_dsn = T)


#QC
PR <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_QC_hydro/qc004.shp")

PR_500 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer1) # buffer by 500 meters

PR_1000 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer2) # buffer by 1000 meters

PR_500 <- st_transform(PR_500, crs = 3348)
PR_1000 <- st_transform(PR_1000, crs = 3348)

st_write(PR_500, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_QC_hydro/qc_all_004_500m.shp", delete_dsn = T)
st_write(PR_1000, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_QC_hydro/qc_all_004_1000m.shp", delete_dsn = T)

#SK
PR <- st_read("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_SK_hydro/sk004.shp")

PR_500 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer1) # buffer by 500 meters

PR_1000 <- PR%>%
  sf::st_transform(5683) %>%  # transform to a metric CRS
  sf::st_buffer(buffer2) # buffer by 1000 meters

PR_500 <- st_transform(PR_500, crs = 3348)
PR_1000 <- st_transform(PR_1000, crs = 3348)

st_write(PR_500, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_SK_hydro/sk_all_004_500m.shp", delete_dsn = T)
st_write(PR_1000, "~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter I old/meta_analysis/Shapefile/canvec_50K_SK_hydro/sk_all_004_1000m.shp", delete_dsn = T)


