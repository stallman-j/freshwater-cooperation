# _______________________________#
# Environment
# Plot Rivers Dams and DHS
# 
# Stallman
# Started 2023-05-23
# Last edited: 
#________________________________#



# Startup

rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","freshwater-cooperation")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
   ggplot2 
  )
# read in data ----
  
  #library(terra) # spatial package for rasters
  
  #library(raster) # to use tmap with raster
  # for interactive ggplot
  if (!require(plotly)) install.packages("plotly")
  
  library(plotly)
  
  
  # read in data ----
  
  
  system.time(
    world <- readRDS(file.path(data_external_clean,"world.rds")) 
  )
  
  africa <- world %>% filter(continent == "Africa")
  
  dams_africa <- readRDS(file.path(data_external_clean,"GDAT_global-dam-tracker","GDAT_dams-af"))

  #dams_africa <- readRDS(file.path(data_external_clean,"GDAT_global-dam-tracker","GDAT_dams-af"))
  
  merged_df <- readRDS(file = file.path(data_external_clean,"merged","many","dhs_gdat_adhi.rds"))
  
  map <- ggplot(data = africa) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    geom_point(data = merged_df,
               aes(fill = factor(type),
                   shape = factor(type),
                   colour = factor(type),
                   x = lon,
                   y = lat),
               size = 1) +
    labs(title = paste0("Dams, Hydro Stations, and DHS Clusters in Africa"),
         caption = c("Data from GDAT (2023), DHS (2023), ADHI (2019)")) +
    theme_map(axis_text_x = element_blank(),
              axis_text_y = element_blank(),
              axis_title_x = element_blank(),
              axis_title_y = element_blank())
  
  
  map
  
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = paste0("gdat_adhi_dhs_africa.png"),
           width = 8,
           height = 9,
           dpi  = 300)
  
  
  
  
# plot ----
  map <- ggplot(data = africa) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    # geom_sf(data = catchments_africa,
    #         fill = yale_lblue,
    #         alpha = .3
    #         ) +
    geom_sf(data = dams_africa,
            color = "brown",
            shape = 17) +
    labs(title = paste0("Dam Locations in Africa"),
         caption = c("Data from GDAT (2023)")) +
    theme_map()
  
  map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = paste0("gdat_dams_in_africa.png"),
           width = 8,
           height = 9,
           dpi  = 300)
  
  
  
  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  #units_name <- "Malawi"
  units_name <- "Zambezi"
  
  hydro_rivers_units <- readRDS(file = file.path(data_clean,"shape-files",paste0(units_name,"_hydro_rivers.rds")))
  
  units <- readRDS(file = file.path(data_clean,"shape-files",paste0(units_name,"_.rds")))
  

  # https://blog.benthies.de/blog/mapping-streams-and-rivers-with-ggplot-sf/
  
  
  map <- ggplot(data = units) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    geom_sf(data = hydro_rivers_units,
            alpha = hydro_rivers_units$width,
            color = yale_blue,
            linewidth = hydro_rivers_units$width) +
    labs(title = paste0("Rivers of ",units_name),
         caption = c("Data from HydroRIVERS (2019)")) +
    theme_map()
  
 # map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = paste0(units_name,"_hydro_rivers_units_hydroRIVERS.png"),
           width = 9,
           height = 5,
           dpi  = 300)
  
  # take a look more closely
  #ggplotly(map)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # 
  # 
  # 
  # map <- ggplot(data = south_america) +
  #   geom_sf(color = "gray70",
  #           fill = "gray99",
  #           alpha = 0.5,
  #           linewidth = .3) +
  #   # geom_sf(data = basin_shapes,
  #   #         alpha = .2,
  #   #         color = yale_lblue,
  #   #         fill  = yale_lblue) +
  #   geom_sf(data = rivers_sa,
  #           alpha = .3,
  #           color = yale_lblue,
  #           linewidth = .3) +
  #   geom_sf(data = rivers_international,
  #           alpha = 1,
  #           color = yale_blue,
  #           linewidth = 1) +
  #   geom_sf(data = sa_dams,
  #           alpha = .4,
  #           color = "brown",
  #           size = .5) +
  #   labs(title = "Rivers of South America",
  #        caption = c(paste0(nrow(rivers_international)," int'l river segments\n",
  #                           nrow(rivers_domestic)," domestic river segments\n",
  #                           nrow(sa_dams)," dams\n",
  #                           "Data from GeoDAR (2022) and GRLW (2018)."))) +
  #   theme_map()
  # 
  # map
  # 
  # save_map(output_folder = output_maps,
  #          plotname = map,
  #          filename = "sa_rivers.png",
  #          width = 9,
  #          height = 5,
  #          dpi  = 300)
  # 
  # 
  # map <- ggplot(data = world) +
  #   geom_sf(color = "gray70",
  #           fill = "gray99",
  #           alpha = 0.5,
  #           linewidth = .3) +
  #   # geom_sf(data = basin_shapes,
  #   #         alpha = .2,
  #   #         color = yale_lblue,
  #   #         fill  = yale_lblue) +
  #   geom_sf(data = rivers_sa,
  #           alpha = .3,
  #           color = yale_lblue,
  #           linewidth = .3) +
  #   geom_sf(data = rivers_international,
  #           alpha = 1,
  #           color = yale_blue,
  #           linewidth = 1) +
  #   geom_sf(data = sa_dams,
  #           alpha = .4,
  #           color = "brown",
  #           size = .5) +
  #   labs(title = "Rivers of South America",
  #        caption = c(paste0(nrow(rivers_international)," int'l river segments\n",
  #                           nrow(rivers_domestic)," domestic river segments\n",
  #                           nrow(sa_dams)," dams\n",
  #                           "Data from GeoDAR (2022) and GRLW (2018)."))) +
  #   theme_map()
  # 
  # map
  # 
  # save_map(output_folder = output_maps,
  #          plotname = map,
  #          filename = "sa_rivers.png",
  #          width = 9,
  #          height = 5,
  #          dpi  = 300)
  # 
  # 


  
