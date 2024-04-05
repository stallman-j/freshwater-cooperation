# _______________________________#
# Environment
# Clean 02: Clean Yang et al 2022 Irrigated Cropland China
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

  year <- c(2018:2019)
  
  path <- file.path(data_raw,"yang-et-al_2022_china_irrigated-cropland",paste0(as.character(year),".tif"))
  
  
  multi_layer_cropland <- rast(path)
  
  # view available layers
  st_layers(path)
  

  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  #sf_use_s2(FALSE) # removes spherical geometry
  

  china <- world[world$name == "China",]
  
  saveRDS(multi_layer_cropland,
          file = file.path(data_clean,"shape-files",paste0("irrigated_cropland_",as.character(min(year)),"_to_",as.character(max(year)),".rds")))
  
  irrigated_land <- readRDS(file = file.path(data_clean,"shape-files",paste0("irrigated_cropland_",as.character(min(year)),"_to_",as.character(max(year)),".rds")))
  
  china <- st_transform(china, crs(irrigated_land))
  
  
  # show what's in here
  irrigated_land
  # just give a sense of what's in here and where
  irrigated_land[[1]]
  
  # class       : SpatRaster 
  # dimensions  : 8482, 29093, 1  (nrow, ncol, nlyr)
  # resolution  : 0.004617, 0.004617  (x, y)
  # extent      : 45.67815, 180.0005, 15.45662, 54.61801  (xmin, xmax, ymin, ymax)
  # coord. ref. : lon/lat WGS 84 (EPSG:4326) 
  # source(s)   : memory
  # name        : 2018 
  # min value   :    1 
  # max value   :    1 
  # 
  
  hasValues(irrigated_land[[1]])
  # TRUE

  plot(irrigated_land,
       col = my_green)

  #irrigated_2019_spdf <- as(irrigated_land[[2]], "SpatialPixelsDataFrame")
  irrigated_df <- as.data.frame(irrigated_land,
                                     xy = TRUE)
  
  saveRDS(irrigated_df,
          file = file.path(data_clean,"shape-files","irrigated_df.rds"))
  
  
  
  # plot ----
  map <- ggplot() +
    geom_sf(data = china, color = "gray70",
                 fill = "gray99",
                 alpha = 0.5,
                 linewidth = .3) +
    geom_tile(data = irrigated_df,
              aes(x = x, y=y, fill = "2019"),
              colour = yale_blue)+
    labs(title = "Irrigated Areas, 2019",
         caption = c("Data from Yang et al. 2022")) +
    theme_map(legend_position = "none")
  
  map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = "china_irrigated_areas.png",
           width = 9,
           height = 5,
           dpi  = 300)
  
  tmap_mode("view")
  # check that these worked
  
  tm_shape(china_catchment) + 
    tm_polygons(china_catchment)+
    tm_bubbles(china_dams) +
    tm_grid(labels.show = TRUE,
            labels.inside.frame = TRUE) # add lat long
  
  

  