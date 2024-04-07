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
  
  #irrigated_land   <- readRDS(file = file.path(data_clean,"shape-files",paste0("irrigated_cropland_",as.character(min(year)),"_to_",as.character(max(year)),".rds")))
  #china_reservoirs <- readRDS(file = file.path(data_clean,"china_reservoirs_geodar.rds"))
  #irrigated_land_crs <- crs(irrigated_land)
  #rm(irrigated_land)
  

# Make CRS the same across all of these ----
  
 # set crs as something that works with distances
  china_catchment <- st_transform(china_catchment, crs = 7801)
  #china_dams <- st_transform(china_dams, crs = 7801)
  china <- st_transform(china, crs = 7801)


  
# make buffer ----
  
  buffer_size <- 10000 # in m
  
  system.time(
    simplify_catchment <- st_simplify(china_catchment, dTolerance = 200)
  )
  
  # user  system elapsed 
  # 0.11    0.01    0.61 
  # 

  saveRDS(simplify_catchment,
          file = file.path(data_clean,"shape-files","simplify_catchment.rds"))
  
  
  # user  system elapsed 
  # 3.46    0.08    7.31
  # 
  
  object.size(china_catchment)
  # 57363232 bytes

  
  object.size(simplify_catchment)
  #  10040408 bytes
  
 # get rid of the larger file
   rm(china_catchment)
  
# remove empty geometries ----
   
   catchment_nonempty <- simplify_catchment[!st_is_empty(simplify_catchment),]
  
   saveRDS(catchment_nonempty,
           file = file.path(data_clean,"shape-files","catchment_nonempty.rds"))
           
   catchment_nonempty <- readRDS( file = file.path(data_clean,"shape-files","catchment_nonempty.rds"))
  

   # get buffers ----
   
   system.time(
   catchment_pos_ring <- positive_buffer_ring(sf = catchment_nonempty,
                                              size = buffer_size)
   )
   
   # user  system elapsed 
   # 198.95    2.51  404.34
   # 
   
   # user  system elapsed 
   # 163.27    2.61  345.09
   
   saveRDS(catchment_pos_ring,
           file = file.path(data_clean,"shape-files",paste0("catchment_pos_ring_",buffer_size/1000,"_km.rds")))
   
   catchment_pos_ring <- readRDS(file = file.path(data_clean,"shape-files",paste0("catchment_pos_ring_",buffer_size/1000,"_km.rds")))
   
   system.time(
     catchment_neg_ring <- negative_buffer_ring(sf = catchment_nonempty,
                                                size = buffer_size)
   )
   # 
   # user  system elapsed 
   # 116.87    2.09  272.52 
   # 
   # 
   # user  system elapsed 
   # 137.27    2.20  284.50
   # 
   
   saveRDS(catchment_neg_ring,
           file = file.path(data_clean,"shape-files",paste0("catchment_neg_ring_",buffer_size/1000,"_km.rds")))
   
   catchment_neg_ring <- readRDS(file = file.path(data_clean,"shape-files",paste0("catchment_neg_ring_",buffer_size/1000,"_km.rds")))
   
   
# plot to see it makes sense ----
   map <- ggplot() +
     geom_sf(data = catchment_nonempty, color = "green",
             fill = "green",
             alpha = 0.5,
             linewidth = .3) +
     geom_sf(data = catchment_pos_ring, color = "blue",
             fill = "blue",
             alpha = 0.5,
             linewidth = .3) +
     geom_sf(data = catchment_neg_ring, color = "red",
             fill = "red",
             alpha = 0.5,
             linewidth = .3) +
     theme_map() +
     labs(title = "Catchment Buffer Rings",
          caption = "Positive in blue, negative in red, original in green")
   
   save_map(output_folder = output_maps,
            plotname = map,
            filename = "catchment_buffer_rings.png",
            width = 9,
            height = 5,
            dpi  = 300)
   
   map
  
   tmap_mode("view")
   
   tm_shape(catchment_nonempty) + 
     tm_polygons(col = "grey") +
   tm_shape(catchment_pos_ring) + 
     tm_polygons(col = "blue",
                 alpha = 0.5) +
     tm_shape(catchment_neg_ring) +
     tm_polygons(col = "red",
                 alpha = 0.5)

   
# try samples ----
   
   
   set.seed(4)
   sample_size <- 5
   IDs <- sample(nrow(catchment_pos_buffer), size = sample_size)
   
   #sample_pos_buffers <- catchment_pos_buffer[IDs, ]
   
   sample_catchment <- simplify_catchment[IDs,]
   
   #neg_buffer <- st_buffer(sample_catchment, -buffer_size)
   
   neg_buffer_ring <- negative_buffer_ring(sf = large_catchment,
                                           size = 1000)
  
   large_catchment <- catchment_nonempty[catchment_nonempty$Outlet_id == 43003453,]
   
   #sf_use_s2(FALSE) 
   neg_buffer <- st_buffer(large_catchment, dist = -1000)
   
   
   tm_shape(large_catchment) +
     tm_polygons(col = "blue",
                 alpha = 0.5) +
   # tm_shape(neg_buffer) +
   #   tm_polygons(col = "red",
   #               alpha = 0.4) +
     tm_shape(neg_buffer_ring) +
     tm_polygons(col = "green",
                 alpha = 0.4)
   
   
   neg_buffers <- st_buffer(large_catchment, -buffer_size)

    
   map <- ggplot() +
     geom_sf(data = catchment_nonempty, color = "green",
             fill = "green",
             alpha = 0.5,
             linewidth = .3) +
     geom_sf(data = catchment_pos_ring, color = "blue",
             fill = "blue",
             alpha = 0.5,
             linewidth = .3) +
     geom_sf(data = catchment_neg_ring, color = "red",
             fill = "red",
             alpha = 0.5,
             linewidth = .3) +

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
   