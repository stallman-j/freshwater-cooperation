# _______________________________#
# International Agreements
# Clean 02: Clean grwl global river widths from landsat
# 
# Stallman
# Started 2023-05-10
# Last edited: 
#________________________________#


# https://gee-community-catalog.org/projects/grwl/
# https://www.science.org/doi/10.1126/science.aat0636
# global extent of rivers and streams, allen and pavelsky 2018, Science

# Startup

#rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

# some parameters ----
  current_continent <- current_continent # set in 00_startup_master.R
  
# packages ----
  
  if (!require("rnaturalearth")) install.packages("rnaturalearth")
  if (!require("sf")) install.packages("sf")
  
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(sf)
  #library(cgal)
  #library(gdal)
  
 #if (!require("pprepr")) devtools::install_github("dickoa/pprepr")
  
 #library(pprepr) # package to validate and automatically repair planar partitions
  
  #https://twitter.com/dickoah/status/1325863367224029187/photo/1
  
  
  sf_countries <- rnaturalearth::ne_countries(continent = current_continent,
                                              returnclass = "sf")
  
  
  all(st_is_valid(sf_countries))
  
  sf_use_s2(FALSE) # don't use S2 for now
  
  sf_countries <- st_make_valid(sf_countries)
 
  # st_is_pp_valid(sf_countries)
  
  
  sf_continent <- sf_countries %>%
                  group_by(continent) %>%
                  summarize()
  
  plot(sf::st_geometry(sf_continent))
  
  path <- file.path(data_clean,"shape-files")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) 
  
  path <- file.path(path, paste0("sf_countries_",current_continent,".rds"))
  saveRDS(sf_countries,
          file = path)
  


  # plot to check
  
  # map <- ggplot(data = sf_countries) +
  #   geom_sf(color = "gray70",
  #           fill = "gray99",
  #           alpha = 0.5,
  #           linewidth = .3) +
  #   # geom_sf(data = basin_shapes,
  #   #         alpha = .2,
  #   #         color = yale_lblue,
  #   #         fill  = yale_lblue) +
  #   # geom_sf(data = continent_rivers,
  #   #         alpha = .3,
  #   #         color = yale_lblue,
  #   #         linewidth = .3) +
  #   # geom_sf(data = rivers_international,
  #   #         alpha = 1,
  #   #         color = yale_blue,
  #   #         linewidth = 1) +
  #   # geom_sf(data = china_dams,
  #   #         alpha = .4,
  #   #         color = yale_blue,
  #   #         size = .5) +
  #   labs(title = "South America",
  #        caption = c("Data from Rnaturalearth")) +
  #   theme_map()
  # 
  # #map
  # 
  # save_map(output_folder = output_maps,
  #          plotname = map,
  #          filename = "continents.png",
  #          width = 9,
  #          height = 5,
  #          dpi  = 300)
  
  
  # plot rivers if intersection is true
  
#   # for interactive ggplot
#   if (!require(plotly)) install.packages("plotly")
#   
#   library(plotly)
#   
#   # take a look more closely
#   ggplotly(map)
#   
# ## use fancier data ----
#   
#   # mask
#   path <- file.path(data_raw,"GRWL_global-river-widths-landsat","GRWL_vector_V01.01","GRWL_vector_V01.01")
#   
#   # view available layers
#   st_layers(path)
#   
#   
#   
#   test_read <- st_read(path)
#   
#    
#   map <- ggplot(data = world) +
#     geom_sf(color = "gray70",
#             fill = "gray99",
#             alpha = 0.5,
#             linewidth = .3) +
#     # geom_sf(data = basin_shapes,
#     #         alpha = .2,
#     #         color = yale_lblue,
#     #         fill  = yale_lblue) +
#     geom_sf(data = test_read,
#             alpha = .3,
#             color = yale_medblue,
#             linewidth = 1) +
#     # geom_sf(data = china_dams,
#     #         alpha = .4,
#     #         color = yale_blue,
#     #         size = .5) +
#     labs(title = "Finding the data",
#          caption = c("Data from GRLW (2018)")) +
#     theme_map()
#   
#   map
#   
#   ggplotly(map)
#   
#   
