# _______________________________#
# Environment
# Plot 02 Merge Elevation, Irrigation and Catchments
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

  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  china <- world[world$name == "China",]
  

  china_catchment     <- readRDS(file = file.path(data_clean,"china_catchment.rds"))
  elevation_china     <- readRDS(file = file.path(data_clean,"shape-files","elevation_china_zoom_04.rds"))
  catchment_pos_ring  <- readRDS(file = file.path(data_clean,"shape-files",paste0("catchment_pos_ring_",buffer_size/1000,"_km.rds")))
  catchment_neg_ring  <- readRDS(file = file.path(data_clean,"shape-files",paste0("catchment_neg_ring_",buffer_size/1000,"_km.rds")))
  #irrigated_df        <- readRDS(file.path(data_clean,"shape-files","irrigated_df.rds"))
  #irrigated_land      <- readRDS(file = file.path(data_clean,"shape-files",paste0("irrigated_cropland_",as.character(min(year)),"_to_",as.character(max(year)),".rds")))
  #china_reservoirs <- readRDS(file = file.path(data_clean,"china_reservoirs_geodar.rds"))
  
  