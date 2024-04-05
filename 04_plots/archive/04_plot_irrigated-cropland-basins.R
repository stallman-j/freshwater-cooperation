# _______________________________#
# Environment
# Plot Irrigated cropland, basins and dams
# 
# Stallman
# Started 2023-04-04
# Last edited: 
#________________________________#


# https://figshare.com/articles/dataset/The_500-m_irrigated_cropland_maps_in_China_during_2000-2019_based_on_a_synergy_mapping_method/19352501/1

# Startup

rm(list = ls())


# bring in the packages, folders, paths

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
  
  elevation_china_df <- as.data.frame(elevation_china,
                                      xy = TRUE)
  
  # plot ----
  map <- ggplot() +
    geom_sf(data = china, color = "gray70",
                 fill = "gray99",
                 alpha = 0.5,
                 linewidth = .3) +
    geom_tile(data = elevation_china_df,
              aes(x = x, y=y, fill = "file4ab071a2797a" ))
  
  map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = "china_elevation_map.png",
           width = 15,
           height = 10,
           dpi  = 400)
  
  
    geom_sf(data = china_reservoirs, color = yale_lblue,
            fill = yale_medblue,
            alpha = 0.3,
            linewidth = .3) +
    geom_sf(data = china_catchment, color = yale_lblue,
            fill = yale_lblue,
            alpha = 0.3,
            linewidth = .3) +
    geom_tile(data = irrigated_df,
              aes(x = x, y=y, fill = "2019"),
              colour = my_green,
              alpha = 0.5)+
    geom_sf(data = china_dams,
            color = yale_medblue,
            alpha = 1,
            size = 0.5,
            shape = 24) +
    labs(title = "Irrigated Areas, 2019",
         caption = c("Data from Yang et al. 2022, rnaturalearth and GeoDAR")) +
    theme_map()
  
  map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = "china_irrigation_catchments_dams.png",
           width = 15,
           height = 10,
           dpi  = 400)
  
  
  # plot ----
  if (!require(basemaps)) install.packages("basemaps") #shapes
  
  library(basemaps)
  
  set_defaults(map_service = "osm",
               map_type = "topographic")
  
  data(ext)
  basemap_magick(china)
  
  map <- ggplot() +
    basemap_gglayer(china) +
    geom_sf(data = china, color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    geom_sf(data = china_reservoirs, color = yale_lblue,
            fill = yale_lblue,
            alpha = 0.3,
            linewidth = .3) +
    # geom_tile(data = irrigated_df,
    #           aes(x = x, y=y, fill = "2019"),
    #           colour = my_green,
    #           alpha = 0.5)+
    geom_sf(data = china_dams,
            color = yale_medblue,
            alpha = 1,
            size = 0.5,
            shape = 24) +
    labs(title = "Dams and Reservoirs in China",
         caption = c("Data from Yang et al. 2022, rnaturalearth and GeoDAR")) +
    theme_map()
  
  map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = "china_dams_and_reservoirs.png",
           width = 9,
           height = 5,
           dpi  = 300)
  
  
  library(stars)
  tmap_mode("view")
  # check that these worked
  
  # extract just the first layer and convert to a raster (package) object
  

  
  

  