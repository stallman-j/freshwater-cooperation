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
  #library(raster) # to use tmap with raster
  
  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  china <- world[world$name == "China",]
  
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
   
# Save ----
   
  saveRDS(catchment_pos_ring,
          file = file.path(data_clean,"shape-files",paste0("catchment_pos_ring_",buffer_size/1000,"_km.rds")))
  
  catchment_pos_ring <- readRDS( file = file.path(data_clean,"shape-files",paste0("catchment_pos_ring_",buffer_size/1000,"_km.rds")))
  
  saveRDS(catchment_neg_ring,
          file = file.path(data_clean,"shape-files",paste0("catchment_neg_ring_",buffer_size/1000,"_km.rds")))
  
  catchment_neg_ring <- readRDS( file = file.path(data_clean,"shape-files",paste0("catchment_neg_ring_",buffer_size/1000,"_km.rds")))
  
# Plot to see it makes sense ----
  
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

  
# combine inner and outer rings to get a polygon that elevation will use ----
  # put all of the polygons together
  big_df <- rbind(catchment_neg_ring,catchment_pos_ring)

  # combine them dplyr style 
  buffer_polygons <- catchment_neg_ring
  
  # the geometry of the buffer is the union of the same row in the pos and neg catchment rings
  
  system.time(
  for (i in 1:nrow(buffer_polygons)) {
    st_geometry(buffer_polygons[i,]) <- st_union(st_geometry(catchment_neg_ring[i,]),st_geometry(catchment_pos_ring[i,]))
  }
  )
  
  saveRDS(buffer_polygons,
          file = file.path(data_clean,"shape-files",paste0("buffer_polygons_",buffer_size/1000,"_km.rds")))
  
  buffer_polygons <- readRDS( file = file.path(data_clean,"shape-files",paste0("buffer_polygons_",buffer_size/1000,"_km.rds")))
  
  

# check that it went through
  
  tm_shape(china_catchment) +
    tm_polygons(col = "blue",
                alpha = 0.5) +
    tm_shape(buffer_polygons) +
    tm_polygons(col = "green",
                alpha = 0.4)
    
  # add in elevation ----
  
  
  elevation_china     <- readRDS(file = file.path(data_clean,"shape-files","elevation_china_zoom_04.rds"))
  buffer_polygons     <- st_transform(buffer_polygons, crs = crs(elevation_china))
  saveRDS(buffer_polygons,
          file = file.path(data_clean,"shape-files",paste0("buffer_polygons_",buffer_size/1000,"_km.rds")))
  

  #elevation_china     <- terra::project(elevation_china, y= as.character(crs_for_calcs))
  
  elevation_masked    <- mask(elevation_china, buffer_polygons)
  
  saveRDS(elevation_masked,
          file = file.path(data_clean,"shape-files",paste0("elevation_masked_",buffer_size/1000,"_km.rds")))
  
  elevation_masked <- readRDS( file = file.path(data_clean,"shape-files",paste0("elevation_masked_",buffer_size/1000,"_km.rds")))
  
  elevation_masked_df <- as.data.frame(elevation_masked,
                                       xy = TRUE)
  
  
  # project onto same extent as the irrigated
  elevation_reproject <- terra::project(elevation_masked,irrigated_masked)
  
  irrigation_elevation <- c(irrigated_masked,elevation_reproject)
  
  
  
  # for calculating distance b/w points and lines or border of polygons
  if (!require("geosphere")) install.packages("geosphere")
  library(geosphere)
  
  catchment_nonempty <- readRDS( file = file.path(data_clean,"shape-files","catchment_nonempty.rds"))
  catchment_nonempty <- st_transform(catchment_nonempty, crs = crs(elevation_china))

  # geosphere needs a sp object
  china_catchment_sp <- as(catchment_nonempty,"Spatial")
  
  system.time(
  # get ID of nearest border
  distances <- dist2Line(elevation_masked_df[,1:2],line = china_catchment_sp,
                         distfun = distGeo)
  )
  
  saveRDS(irrigation_elevation,
          file = file.path(data_clean,"shape-files",paste0("irrigation_elevation_",buffer_size/1000,"_km.rds")))
  
  
  # make sure it works
  plot(elevation_masked)
 
# get irrigated land in the masks ----
  
  irrigated_land   <- readRDS(file = file.path(data_clean,"shape-files",paste0("irrigated_cropland_",as.character(min(year)),"_to_",as.character(max(year)),".rds")))
  
  # change extent and map to the china elevation style
  irrigated_land   <- terra::project(irrigated_land,elevation_china )
  
  #irrigated_df <- readRDS(file.path(data_clean,"shape-files","irrigated_df.rds"))
  
  # get a mask over irrigated land within the buffers
  
  irrigated_masked <- mask(irrigated_land, buffer_polygons)
  

  
  # make sure it works
  
  irrigated_mask_df <- as.data.frame(irrigated_masked,
                                     xy = TRUE)

  china <- st_transform(china, crs = crs(elevation_china))
  
  saveRDS(irrigated_mask_df,
          file = file.path(data_clean,"shape-files",paste0("irrigated_mask_df_",buffer_size/1000,"_km.rds")))
  
  saveRDS(irrigated_masked,
          file = file.path(data_clean,"shape-files",paste0("irrigated_masked_",buffer_size/1000,"_km.rds")))
  
  
  map <- ggplot() +
    geom_sf(data = china, color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    geom_tile(data = irrigated_mask_df,
              aes(x = x, y=y, fill = "2019"),
              colour = yale_blue)+
    labs(title = "Irrigated Areas within buffers, 2019",
         caption = c("10 km buffers. Data from Yang et al. 2022")) +
    theme_map(legend_position = "none")

  map  
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = "china_irrigated_areas_in_buffers.png",
           width = 9,
           height = 5,
           dpi  = 300)
  
# get elevations for the irrigated mask ----
  
  if (!require("elevatr")) install.packages("elevatr")
  library(elevatr)
  
  # if we want with points
  # irrigated_elev <- get_elev_point(irrigated_mask_df,
  #                                  prj = paste0("EPSG:",as.character(crs_for_calcs)),
  #                                  src = "epqs")
  # 
  # saveRDS(irrigated_elev,
  #         file = file.path(data_clean,"shape-files",paste0("irrigated_elev_",buffer_size/1000,"_km.rds")))
  

  saveRDS(irrigation_elevation,
          file = file.path(data_clean,"shape-files",paste0("irrigation_elevation_",buffer_size/1000,"_km.rds")))
  
  tmap_mode("view")
  
  # generated an sf object
  
  irrigated_mask_sf <- st_as_sf(irrigated_mask_df,
                                coords = c("x","y"),
                                crs = crs(elevation_china))
  
  saveRDS(irrigated_mask_sf,
          file = file.path(data_clean,"shape-files",paste0("irrigated_mask_sf_",buffer_size/1000,"_km.rds")))
  
  # convert to sp so we can get elevation
  irrigated_mask_sp<- as(irrigated_mask_sf,"Spatial")
  
  system.time(
  elev_irrigated <- get_elev_point(irrigated_mask_sp,
                                   prj = crs(elevation_china),
                                   src = "aws")
  )
  
  elev_irrigated_sf <- st_as_sf(elev_irrigated)

  # user  system elapsed 
  # 1.83    0.07    7.56
  