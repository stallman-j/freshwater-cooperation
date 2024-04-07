# _______________________________#
# Environment
# Plot 04 Plot Basin-Treaty Info
# Stallman
# Started 2023-03-16
# Last edited: 
#________________________________#
# Startup

rm(list = ls())


# bring in the packages, folders, paths ----

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

# read in data ----

  library(terra) # spatial package for rasters
  library(raster) # to use tmap with raster
  
  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  china <- world[world$name == "China",]
  
  year <- c(2018:2019)
  
 china_catchment  <- readRDS(file = file.path(data_clean,"china_catchment.rds"))
  
  #china_dams       <- readRDS(file = file.path(data_clean,"china_dams_geodar.rds"))
  
  #irrigated_df <- readRDS(file.path(data_clean,"shape-files","irrigated_df.rds"))
  
  irrigated_land   <- readRDS(file = file.path(data_clean,"shape-files",paste0("irrigated_cropland_",as.character(min(year)),"_to_",as.character(max(year)),".rds")))
  #china_reservoirs <- readRDS(file = file.path(data_clean,"china_reservoirs_geodar.rds"))
  irrigated_land_crs <- crs(irrigated_land)
  rm(irrigated_land)
  

# Make CRS the same across all of these ----
  
  china_catchment <- st_transform(china_catchment, irrigated_land_crs)
  #china_dams <- st_transform(china_dams, crs(irrigated_land))
  china <- st_transform(china, irrigated_land_crs)

  
  saveRDS(china_catchment,
          file = file.path(data_clean,"shape-files","china_catchment.rds"))
  
  china_catchment <- readRDS(    file = file.path(data_clean,"shape-files","china_catchment.rds"))

  
# make buffer ----
  
  buffer_size <- 1000 # in m
  
  system.time(
    simplify_catchment <- st_simplify(china_catchment, dTolerance = 200)
  )
  
  # user  system elapsed 
  # 3.46    0.08    7.31
  # 
  
  object.size(china_catchment)
  # 57363232 bytes

  
  object.size(simplify_catchment)
  #  10040408 bytes
  
  system.time(
  catchment_neg_buffers <- st_simplify(simplify_catchment, dist = -buffer_size)
  )
  
  

  # not simplified
  # user  system elapsed 
  # 1.67    0.13    3.69
  
  # simplified
 
  #  user  system elapsed 
  #  0.15    0.00    0.42 
  
  system.time(
  catchment_buffers <- st_buffer(simplify_catchment, dist = buffer_size)
  )
  
  # not simplified is like 3 mins
  
  # simplified
  # user  system elapsed 
  # 41.46    0.03   79.70 
  # 
  saveRDS(catchment_buffers,
          file = file.path(data_clean,"shape-files",paste0("catchment_buffers_",buffer_size/1000,"_km.rds")))
  
  catchment_buffers <- readRDS( file = file.path(data_clean,"shape-files",paste0("catchment_buffers_",buffer_size/1000,"_km.rds")))
  
  saveRDS(catchment_neg_buffers,
          file = file.path(data_clean,"shape-files",paste0("catchment_neg_buffers_",buffer_size/1000,"_km.rds")))
  
  catchment_neg_buffers <- readRDS( file = file.path(data_clean,"shape-files",paste0("catchment_neg_buffers_",buffer_size/1000,"_km.rds")))
  
  rm(china_catchment)
  
# Randomly pick to show example ----
  
  set.seed(4)
  IDs <- sample(nrow(catchment_buffers), size = 1)
  
  sample_buffers <- catchment_buffers[IDs, ]
  
  sample_catchment <- simplify_catchment[IDs,]
  
  sample_neg_buffers <- catchment_neg_buffers[IDs,]
  
  saveRDS(sample_buffers,
          file = file.path(data_clean,"shape-files","sample_buffers.rds"))
  saveRDS(sample_catchment,
          file = file.path(data_clean,"shape-files","sample_catchment.rds"))
  
  saveRDS(sample_neg_buffers,
          file = file.path(data_clean,"shape-files","sample_neg_buffers.rds"))
  
  sample_buffers <- readRDS(file = file.path(data_clean,"shape-files","sample_buffers.rds"))
  
  sample_catchment <- readRDS(file = file.path(data_clean,"shape-files","sample_catchment.rds"))
  
  sample_neg_buffers <- readRDS(file = file.path(data_clean,"shape-files","sample_neg_buffers.rds"))
  
  # get the part outside the catchment area ----
  
  system.time(
    outside_sample <- st_difference(sample_buffers,sample_catchment)
  )
  
  # user  system elapsed 
  # 0       0       0 
  
  # get the part inside the catchment area ----
  system.time(
    inside_sample  <- st_intersection(sample_catchment,sample_neg_buffers)
  )
  
  # user  system elapsed 
  # 0       0       0 
  # 
  
  plot(st_geometry(outside_sample))
  plot(st_geometry(inside_sample))
  
    
  map <- ggplot() +
    geom_sf(data = sample_buffers, color = yale_medblue,
            fill = yale_medblue,
            alpha = 0.3,
            linewidth = .3) +
    geom_sf(data = sample_catchment, color = my_green,
            fill = my_green,
            alpha = 0.3,
            linewidth = .3) +
    labs(title = "Comparisons of Catchment and buffers",
         caption = c("Data from Yang et al. 2022, rnaturalearth and GeoDAR. Green: Catchment; Blue: Outside buffer")) +
    theme_map()
  
  map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = "china_buffers_catchments.png",
           width = 9,
           height = 5,
           dpi  = 300)

  
  # get the part outside the catchment area ----
  
  system.time(
    outside_catchment <- st_difference(catchment_buffers,china_catchment)
  )
  
  # get the part inside the catchment area ----
  system.time(
    inside_catchment  <- st_intersection(china_catchment,catchment_neg_buffers)
  )
  
  
  
  saveRDS(inside_catchment,
          file = file.path(data_clean,"shape-files",paste0("inside_catchment_",buffer_size/1000,"_km.rds")))
  
  saveRDS(outside_catchment,
          file = file.path(data_clean,"shape-files",paste0("outside_catchment_",buffer_size/1000,"_km.rds")))
  
  outside_catchment <- readRDS(file = file.path(data_clean,"shape-files",paste0("outside_catchment_",buffer_size/1000,"_km.rds")))
  
  inside_catchment  <- readRDS(file = file.path(data_clean,"shape-files",paste0("inside_catchment_",buffer_size/1000,"_km.rds")))
  
  
  # user  system elapsed 
  # 96.70    0.14  195.23
  # 
  
  # for one
  # user  system elapsed 
  # 99.46    0.14  195.56
  
  # plot to see if it worked, yes
  #plot(st_geometry(catchment_buffers))
  
  
# get the part outside the catchment area ----
  
  system.time(
  outside_catchment <- st_difference(catchment_buffers,china_catchment)
  )
  
# get the part inside the catchment area ----
  system.time(
  inside_catchment  <- st_intersection(china_catchment,catchment_neg_buffers)
  )
  
  saveRDS(inside_catchment,
          file = file.path(data_clean,"shape-files",paste0("inside_catchment_",buffer_size/1000,"_km.rds")))
  
  saveRDS(outside_catchment,
          file = file.path(data_clean,"shape-files",paste0("outside_catchment_",buffer_size/1000,"_km.rds")))
  
  outside_catchment <- readRDS(file = file.path(data_clean,"shape-files",paste0("outside_catchment_",buffer_size/1000,"_km.rds")))
  
  inside_catchment  <- readRDS(file = file.path(data_clean,"shape-files",paste0("inside_catchment_",buffer_size/1000,"_km.rds")))

  # add in elevation ----
  # elevation_china  <- readRDS(file = file.path(data_clean,"shape-files","elevation_china_zoom_04.rds"))
  # 
  # elevation_china <- terra::project(elevation_china, irrigated_land)
  # 
  #  elevation_china_df <- as.data.frame(elevation_china,
  #                                     xy = TRUE)
  # 
  #  #china_reservoirs <- st_transform(china_reservoirs, crs(irrigated_land))
  
  
  
  #pos_ring <- catchment_pos_buffer
  
  pos_buffer_ring <- positive_buffer_ring(sf = simplify_catchment,
                                          size = buffer_size)
  
  is_valid <- st_is_valid(catchment_pos_buffer)
  pos_ring <- catchment_pos_buffer[1,]
  
  system.time(
    for (i in 1:nrow(catchment_pos_buffer)){
      st_geometry(pos_ring[i,]) <- 
        tryCatch(
          st_geometry(st_difference(catchment_pos_buffer[i,],simplify_catchment[i,])),
          error = function(e) return(NA)
        )
    } 
  )
  
  test <- st_difference(catchment_pos_buffer[248,],simplify_catchment[248,])
  
  catchment_pos_buffer[248,] %>% view()
  
  # try samples ----
  
  
  set.seed(4)
  sample_size <- 1000
  IDs <- sample(nrow(catchment_pos_buffer), size = sample_size)
  
  sample_pos_buffers <- catchment_pos_buffer[IDs, ]
  
  sample_catchment <- simplify_catchment[IDs,]
  
  pos_ring <- sample_pos_buffers
  
  pos_buffer_ring <- positive_buffer_ring(sf = sample_catchment,
                                          size = buffer_size)
  
  system.time(
    for (i in 1:nrow(pos_ring)){
      st_geometry(pos_ring[i,]) <- 
        tryCatch(
          st_geometry(st_difference(sample_pos_buffers[i,],sample_catchment[i,])),
          error = function(e) return(NA)
        )
    } 
  )
  
  plot(st_geometry(pos_ring))
  
  
  
  map <- ggplot() +
    geom_sf(data = simplify_catchment, color = "green",
            fill = "green",
            alpha = 0.5,
            linewidth = .3) +
    geom_sf(data = pos_ring, color = "blue",
            fill = "blue",
            alpha = 0.5,
            linewidth = .3) +
    # geom_sf(data = neg_buffer, color = "red",
    #         fill = "red",
    #         alpha = 0.5,
    #         linewidth = .3) +
    
    labs(title = "Buffer Rings",
         caption = "Positive ring in blue, negative in red, original in green")
  
  map
  
  # 
  saveRDS(catchment_pos_ring,
          file = file.path(data_clean,"shape-files",paste0("catchment_pos_ring_",buffer_size/1000,"_km.rds")))
  
  catchment_pos_ring <- readRDS( file = file.path(data_clean,"shape-files",paste0("catchment_pos_ring_",buffer_size/1000,"_km.rds")))
  
  saveRDS(catchment_neg_ring,
          file = file.path(data_clean,"shape-files",paste0("catchment_neg_ring_",buffer_size/1000,"_km.rds")))
  
  catchment_neg_ring <- readRDS( file = file.path(data_clean,"shape-files",paste0("catchment_neg_ring_",buffer_size/1000,"_km.rds")))
  
  
  # Randomly pick to show example ----
  
  set.seed(4)
  IDs <- sample(nrow(catchment_buffers), size = 1)
  
  sample_buffers <- catchment_buffers[IDs, ]
  
  sample_catchment <- simplify_catchment[IDs,]
  
  sample_neg_buffers <- catchment_neg_buffers[IDs,]
  
  saveRDS(sample_buffers,
          file = file.path(data_clean,"shape-files","sample_buffers.rds"))
  saveRDS(sample_catchment,
          file = file.path(data_clean,"shape-files","sample_catchment.rds"))
  
  saveRDS(sample_neg_buffers,
          file = file.path(data_clean,"shape-files","sample_neg_buffers.rds"))
  
  sample_buffers <- readRDS(file = file.path(data_clean,"shape-files","sample_buffers.rds"))
  
  sample_catchment <- readRDS(file = file.path(data_clean,"shape-files","sample_catchment.rds"))
  
  sample_neg_buffers <- readRDS(file = file.path(data_clean,"shape-files","sample_neg_buffers.rds"))
  
  # get the part outside the catchment area ----
  
  system.time(
    outside_sample <- st_difference(sample_buffers,sample_catchment)
  )
  
  # user  system elapsed 
  # 0       0       0 
  
  # get the part inside the catchment area ----
  system.time(
    inside_sample  <- st_intersection(sample_catchment,sample_neg_buffers)
  )
  
  # user  system elapsed 
  # 0       0       0 
  # 
  
  plot(st_geometry(outside_sample))
  plot(st_geometry(inside_sample))
  
  
  map <- ggplot() +
    geom_sf(data = sample_buffers, color = yale_medblue,
            fill = yale_medblue,
            alpha = 0.3,
            linewidth = .3) +
    geom_sf(data = sample_catchment, color = my_green,
            fill = my_green,
            alpha = 0.3,
            linewidth = .3) +
    labs(title = "Comparisons of Catchment and buffers",
         caption = c("Data from Yang et al. 2022, rnaturalearth and GeoDAR. Green: Catchment; Blue: Outside buffer")) +
    theme_map()
  
  map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = "china_buffers_catchments.png",
           width = 9,
           height = 5,
           dpi  = 300)
  
  
  # get the part outside the catchment area ----
  
  system.time(
    outside_catchment <- st_difference(catchment_buffers,china_catchment)
  )
  
  # get the part inside the catchment area ----
  system.time(
    inside_catchment  <- st_intersection(china_catchment,catchment_neg_buffers)
  )
  
  
  
  saveRDS(inside_catchment,
          file = file.path(data_clean,"shape-files",paste0("inside_catchment_",buffer_size/1000,"_km.rds")))
  
  saveRDS(outside_catchment,
          file = file.path(data_clean,"shape-files",paste0("outside_catchment_",buffer_size/1000,"_km.rds")))
  
  outside_catchment <- readRDS(file = file.path(data_clean,"shape-files",paste0("outside_catchment_",buffer_size/1000,"_km.rds")))
  
  inside_catchment  <- readRDS(file = file.path(data_clean,"shape-files",paste0("inside_catchment_",buffer_size/1000,"_km.rds")))
  
  
  # user  system elapsed 
  # 96.70    0.14  195.23
  # 
  
  # for one
  # user  system elapsed 
  # 99.46    0.14  195.56
  
  # plot to see if it worked, yes
  #plot(st_geometry(catchment_buffers))
  
  
  # get the part outside the catchment area ----
  
  system.time(
    outside_catchment <- st_difference(catchment_buffers,china_catchment)
  )
  
  # get the part inside the catchment area ----
  system.time(
    inside_catchment  <- st_intersection(china_catchment,catchment_neg_buffers)
  )
  
  saveRDS(inside_catchment,
          file = file.path(data_clean,"shape-files",paste0("inside_catchment_",buffer_size/1000,"_km.rds")))
  
  saveRDS(outside_catchment,
          file = file.path(data_clean,"shape-files",paste0("outside_catchment_",buffer_size/1000,"_km.rds")))
  
  outside_catchment <- readRDS(file = file.path(data_clean,"shape-files",paste0("outside_catchment_",buffer_size/1000,"_km.rds")))
  
  inside_catchment  <- readRDS(file = file.path(data_clean,"shape-files",paste0("inside_catchment_",buffer_size/1000,"_km.rds")))
  
  # add in elevation ----
  # elevation_china  <- readRDS(file = file.path(data_clean,"shape-files","elevation_china_zoom_04.rds"))
  # 
  # elevation_china <- terra::project(elevation_china, irrigated_land)
  # 
  #  elevation_china_df <- as.data.frame(elevation_china,
  #                                     xy = TRUE)
  # 
  #  #china_reservoirs <- st_transform(china_reservoirs, crs(irrigated_land))
  
  
  
   