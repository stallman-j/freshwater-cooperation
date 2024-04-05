# _______________________________#
# International Agreements
# Plot 04 Plot IEA Network
# # # https://stackoverflow.com/questions/42960248/how-to-plot-networks-over-a-map-with-the-least-overlap
# almost verbatim from the above
# Stallman
# Started 2022-12-18
# Last edited: 
#________________________________#
# Startup

  rm(list = ls())


# bring in the packages, folders, paths ----

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

  library(sf)
  
  basin_shapes <- readRDS(file = file.path(data_clean,"shape-files","basin_shapes.rds"))
  basin_country_shapes <- readRDS(file = file.path(data_clean,"shape-files","basin_country_shapes.rds"))
  
  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  basin_country_map <- ggplot(data = world) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    geom_sf(data = basin_shapes,
            alpha = .2,
            color = yale_lblue,
            fill  = yale_lblue) +
    labs(title = "International Water Basins",
         caption = c("Data from Transboundary Freshwater Dispute Database, Oregon State University 2022 \n
                     310 total international basins.")) +
    theme_map()
  
  save_map(output_folder = output_maps,
           plotname = basin_country_map,
           filename = "basin_country_map.png",
           width = 9,
           height = 5,
           dpi  = 300)
  
  
  basin_country_shapes_map <- ggplot(data = world) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    geom_sf(data = basin_country_shapes,
            alpha = .2,
            color = yale_lblue,
            fill  = yale_lblue) +
    labs(title = "International Water Basins with Country Boundaries",
         caption = c("Data from Transboundary Freshwater Dispute Database, Oregon State University 2022 \n
                     310 total international basins, splits into 814 country-basin polygons.")) +
    theme_map()
  
  save_map(output_folder = output_maps,
           plotname = basin_country_shapes_map,
           filename = "basin_country_shapes_map.png",
           width = 9,
           height = 5,
           dpi  = 300)
  
  
  
  