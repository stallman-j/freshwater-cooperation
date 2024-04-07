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
  # for interactive ggplot
  if (!require(plotly)) install.packages("plotly")
  
  library(plotly)
  
  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  south_america <- readRDS(file = file.path(data_clean,"south_america.rds"))
  rivers_sa <- readRDS(file = file.path(data_clean,"rivers_sa.rds"))
  sa_dams <- readRDS(file = file.path(data_clean,"sa_dams.rds"))
  
  
  rivers_domestic <- readRDS(file = file.path(data_clean,"rivers_domestic.rds"))
  
  rivers_international <- readRDS(file = file.path(data_clean,"rivers_international.rds"))
  
  map <- ggplot(data = south_america) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    # geom_sf(data = basin_shapes,
    #         alpha = .2,
    #         color = yale_lblue,
    #         fill  = yale_lblue) +
    geom_sf(data = rivers_sa,
            alpha = .3,
            color = yale_lblue,
            linewidth = .3) +
    geom_sf(data = rivers_international,
            alpha = 1,
            color = yale_blue,
            linewidth = 1) +
    geom_sf(data = sa_dams,
            alpha = .4,
            color = "brown",
            size = .5) +
    labs(title = "Rivers of South America",
         caption = c(paste0(nrow(rivers_international)," int'l river segments\n",
                            nrow(rivers_domestic)," domestic river segments\n",
                            nrow(sa_dams)," dams\n",
                            "Data from GeoDAR (2022) and GRLW (2018)."))) +
    theme_map()
  
  map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = "sa_rivers.png",
           width = 9,
           height = 5,
           dpi  = 300)
  
  
  map <- ggplot(data = world) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    # geom_sf(data = basin_shapes,
    #         alpha = .2,
    #         color = yale_lblue,
    #         fill  = yale_lblue) +
    geom_sf(data = rivers_sa,
            alpha = .3,
            color = yale_lblue,
            linewidth = .3) +
    geom_sf(data = rivers_international,
            alpha = 1,
            color = yale_blue,
            linewidth = 1) +
    geom_sf(data = sa_dams,
            alpha = .4,
            color = "brown",
            size = .5) +
    labs(title = "Rivers of South America",
         caption = c(paste0(nrow(rivers_international)," int'l river segments\n",
                            nrow(rivers_domestic)," domestic river segments\n",
                            nrow(sa_dams)," dams\n",
                            "Data from GeoDAR (2022) and GRLW (2018)."))) +
    theme_map()
  
  map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = "sa_rivers.png",
           width = 9,
           height = 5,
           dpi  = 300)
  
  
  # plot rivers if intersection is true
  

  # take a look more closely
  ggplotly(map)
  