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
  
  irrigated_land   <- readRDS(file = file.path(data_clean,"shape-files",paste0("irrigated_cropland_",as.character(min(year)),"_to_",as.character(max(year)),".rds")))
  
  china_catchment  <- readRDS(file = file.path(data_clean,"china_catchment.rds"))
  china_reservoirs <- readRDS(file = file.path(data_clean,"china_reservoirs_geodar.rds"))
  elevation_china  <- readRDS(file = file.path(data_clean,"shape-files","elevation_china_zoom_04.rds"))
  
  china_dams       <- readRDS(file = file.path(data_clean,"china_dams_geodar.rds"))
  
  irrigated_df <- readRDS(file.path(data_clean,"shape-files","irrigated_df.rds"))
  


# Make CRS the same across all of these ----
  
  china_reservoirs <- st_transform(china_reservoirs, crs(irrigated_land))
  china_catchment <- st_transform(china_catchment, crs(irrigated_land))
  china_dams <- st_transform(china_dams, crs(irrigated_land))
  china <- st_transform(china, crs(irrigated_land))
  elevation_china <- terra::project(elevation_china, irrigated_land)
  
  # elevation_china_df <- as.data.frame(elevation_china,
  #                                     xy = TRUE)
  
# make 10 km buffer ----
