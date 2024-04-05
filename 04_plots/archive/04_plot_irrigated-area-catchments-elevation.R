# _______________________________#
# Environment
# Plot 04 Plot irrigated area and catchments
# Stallman
# Started 2023-03-16
# Last edited: 
#________________________________#
# Startup

rm(list = ls())


# bring in the packages, folders, paths ----

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))
  

    irrigated_mask_sf <- readRDS(file = file.path(data_clean,"shape-files",paste0("irrigated_mask_sf_",buffer_size/1000,"_km.rds")))
    buffer_polygons   <- readRDS( file = file.path(data_clean,"shape-files",paste0("buffer_polygons_",buffer_size/1000,"_km.rds")))
    elevation_masked <- readRDS( file = file.path(data_clean,"shape-files",paste0("elevation_masked_",buffer_size/1000,"_km.rds")))
    china_dams       <- readRDS(file = file.path(data_clean,"china_dams_geodar.rds"))

    china_dams <- st_transform(china_dams, crs = st_crs(buffer_polygons))
  
    tmap_mode("view")
    
    
tm_shape(irrigated_mask_sf) +
  tm_dots("2019",col = "blue",
          alpha = .5) +
  tm_shape(buffer_polygons)+
  tm_polygons(col = "grey",
              alpha = .3) +
  # tm_shape(elevation_masked)+
  # tm_raster("file8dcfc41f0f", palette = terrain.colors(8)) +
  tm_shape(china_dams) +
  tm_dots(col = "red")
  





