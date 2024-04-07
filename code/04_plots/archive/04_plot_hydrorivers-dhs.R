# _______________________________#
# Environment
# Plot HydroRIVERS and DHS
# 
# Stallman
# Started 2023-05-23
# Last edited: 
#________________________________#



# Startup

rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

# Plot Elevation ----
  
  
  dhs_gps_filename <- "UGGE7AFL"
  
  dhs_gps_data <- readRDS(file.path(data_external_temp,
                                    "DHS","GPS",
                                    paste0(dhs_gps_filename,"_","GADM_ADM_2.rds")))
  
  
  dhs_gps_elev <- elevatr::get_elev_point(dhs_gps_data, src = "aws")
  
  cols <- rev(brewer.pal(11, 'RdYlGn'))
  
  map <- ggplot()+
    geom_sf(data = test_country_cast,
            fill = "grey80",
            alpha = .2)+
    geom_point(data = dhs_gps_elev,
               aes(x = LONGNUM,
                   y = LATNUM,
                   colour = elevation)
    )+
    labs(title = paste0("Survey Clusters, Districts, and Elevation, Uganda 2016"),
         caption = c("Data from DHS (2016), GADM (2022), AWS (2023)"),
         colour = "Elevation (m)")+
    xlab("") + ylab("")+
    theme_map(legend_position = c(0.9,0.3))  +
    scale_colour_gradientn(colours = cols)
  #  scale_fill_manual(values = randomcoloR::distinctColorPalette(k=length(unique(dhs_gps_data$DHSCLUST)) ))
  
  map
  
  save_map(output_folder = file.path(output_maps,"DHS"),
           plotname = map,
           filename = paste0("survey_clusters_elevation_ADM_level_2_2016_UGA.png"),
           width = 8,
           height = 9,
           dpi  = 400)
  
  
# read in data ----
  
  #library(terra) # spatial package for rasters
  
  #library(raster) # to use tmap with raster
  # for interactive ggplot
  if (!require(plotly)) install.packages("plotly")
  
  library(plotly)
  
  library(tmap)
  data(World)
  
  #world <- readRDS(file = file.path(data_clean,"world.rds"))
  
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


  