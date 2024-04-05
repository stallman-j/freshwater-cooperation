# _______________________________#
# Environment
# Clean 02: Clean GSRB global subnational river borders
# 
# Stallman
# Started 2023-04-04
# Last edited: 
#________________________________#



# Startup

rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

# read in data ----
  
  path <- file.path(data_raw,"gsrb_global-subnational-river-borders","GSRB")
  
  st_layers(path)
  
  # download the countries
  world_grsb <- st_read(dsn = path,
                        layer = "GSRB_Level0")

  world <- readRDS(file = file.path(data_clean,"world.rds"))
  basin_shapes <- readRDS(file = file.path(data_clean,"shape-files","basin_shapes.rds"))
  
# plot ----
  river_country_map <- ggplot(data = world) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    geom_sf(data = basin_shapes,
            alpha = .2,
            color = yale_lblue,
            fill  = yale_lblue) +
    geom_sf(data = world_grsb,
            alpha = 1,
            color = yale_blue,
            linewidth = 1) +
    labs(title = "International Border Rivers and Basins",
         caption = c("Data from GSRB and TFDD")) +
    theme_map()
  
  river_country_map
  
  save_map(output_folder = output_maps,
           plotname = river_country_map,
           filename = "world_rivers_borders_basins.png",
           width = 9,
           height = 5,
           dpi  = 300)
  
  
  