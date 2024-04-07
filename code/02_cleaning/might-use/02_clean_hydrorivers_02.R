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
  continent_name <- "Africa"
  
# read in data ----
  
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(sf)
  #library(cgal)
  #library(gdal)
  
 # if (!require("pprepr")) devtools::install_github("dickoa/pprepr")
  
#  library(pprepr) # package to validate and automatically repair planar partitions
  
  #https://twitter.com/dickoah/status/1325863367224029187/photo/1
  
  sf_countries <- rnaturalearth::ne_countries(continent = continent_name,
                                              returnclass = "sf")
  
  
  all(st_is_valid(sf_countries))
  
  sf_countries <- st_make_valid(sf_countries)
 
  # st_is_pp_valid(sf_countries)
  
  
  sf_continent <- sf_countries %>%
                  group_by(continent) %>%
                  summarize()
  
  plot(sf::st_geometry(sf_continent))
  
  
  path <- file.path(data_raw,"hydroSHEDS","HydroRIVERS_v10_sa.gdb","HydroRIVERS_v10_sa.gdb")
  
  hydro_rivers <- st_read(dsn = path)



  names(hydro_rivers)
  # [1] "HYRIV_ID"     "NEXT_DOWN"    "MAIN_RIV"     "LENGTH_KM"    "DIST_DN_KM"   "DIST_UP_KM"   "CATCH_SKM"   
  # [8] "UPLAND_SKM"   "ENDORHEIC"    "DIS_AV_CMS"   "ORD_STRA"     "ORD_CLAS"     "ORD_FLOW"     "HYBAS_L12"   
  # [15] "Shape_Length" "Shape" 
  # 
  # 
  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  # unique(world$continent)
  # [1] Asia                    Africa                  Europe                  South America           Seven seas (open ocean) Oceania                
  # [7] North America 
  # 
  # 
  
  
  
  my_continent <- world %>%
                  filter(continent == continent_name)
  
  plot(st_geometry(my_continent))
  
  st_crs(world)
  
  # change crs to be same as the rivers
  my_continent <- my_continent %>%
                  st_transform(crs = st_crs(rivers))
  
  
  # get rivers for just my continent
  continent_rivers <- hydro_rivers[my_continent,]
  
  saveRDS(continent_rivers,
          file = file.path(data_clean,paste0(continent_name,"_rivers.rds")))
  
  continent_rivers <- readRDS(file =  file.path(data_clean,paste0(continent_name,"_rivers.rds")))
  
  # 1,620,963 obs 
  
  # randomly sample some of these
  
  sample_id <- sample(nrow(my_continent),
                         size = 1000,
                         replace= FALSE)
  
  river_sample <- continent_rivers[sample_id,]
  
  ## CHANGE THIS WHEN NOT SAMPLING
  
  #my_continent <- river_sample
  
  saveRDS(my_continent,
          file = file.path(data_clean,"shape-files",paste0(continent_name,".rds")))
  
  my_continent <- readRDS(file =  file.path(data_clean,"shape-files",paste0(continent_name,".rds")))
  
  # take out the world rivers for space
  
  # map <- ggplot(data = my_continent) +
  #   geom_sf(color = "gray70",
  #           fill = "gray99",
  #           alpha = 0.5,
  #           linewidth = .3) +
  #   geom_sf(data = continent_rivers,
  #           alpha = .3,
  #           color = yale_lblue,
  #           linewidth = .5) +
  #   labs(title = "Rivers of South America",
  #        caption = c("Data from HydroRIVERS (2019)")) +
  #   theme_map()
  # 
  # save_map(output_folder = output_maps,
  #          plotname = map,
  #          filename = "continent_rivers_hydroRIVERS.png",
  #          width = 9,
  #          height = 5,
  #          dpi  = 300)
  # 
  # map
  # 
  # # for interactive ggplot
  # if (!require(plotly)) install.packages("plotly")
  # 
  # library(plotly)
  # 
  # # take a look more closely
  # ggplotly(map)
  
  
  

  
  
# Get the rivers that intersect a country boundary ----
  
  library(rnaturalearth) # has ne_coastline() which is global coastlines
  if (!require(rnaturalearthdata)) install.packages("rnaturalearthdata")
  
  library(rnaturalearthdata)
  
  # scale medium means a medium-sized file, better detail
  coastlines <- ne_coastline(scale = "medium", returnclass = "sf")
  
  
    # cast the country polygon borders into lines
  
  continent_country_lines <- st_cast(my_continent, "MULTILINESTRING", 
                              group_or_split = TRUE)
  
  continent_outline <- st_cast(st_union(my_continent), "MULTILINESTRING",
                        group_or_split = TRUE)
  
  # create a yes-no insersection if the geometries intersect
  
  # a list is returned for st_intersects, so take the ones where there's a positive length of intersection
  # ie that the river intersects a country line
  
  # 
  # generates a logical vector of length of continent_rivers
  intersects_border_yesno <- lengths(st_intersects(continent_rivers,continent_country_lines)) > 0
  coastline_yesno <- lengths(st_intersects(continent_rivers, coastlines)) >0
  outline_TF      <- lengths(st_intersects(continent_rivers, continent_outline)) >0
  
  # verify that this is getting what we want
  check_border <- intersects_border_yesno[1:30]
  check_coastline  <- coastline_yesno[1:30]
  
  check_international <- check_border & !check_coastline
  
  intersects_international_yesno <- intersects_border_yesno & !coastline_yesno & !outline_TF
  

  # so now we want IS international, but is NOT intersecting a coast
  
  rivers_international <- continent_rivers[intersects_international_yesno,]
  rivers_domestic      <- continent_rivers[!intersects_international_yesno,]
  
  saveRDS(rivers_international,
          file = file.path(data_clean,"rivers_international_sa.rds"))
  saveRDS(rivers_domestic,
          file = file.path(data_clean,"rivers_domestic_sa.rds"))
  
  
  # try now with a function
  
  # rivers_intl_fn <- get_in_out_intersections(sf_of_interest = continent_rivers,
  #                                            yes_sf = continent_country_lines,
  #                                            no_sf  = coastlines)
  # 
  # 
  
  #plot(st_geometry(rivers_international))
  
  # plot to check
  
  map <- ggplot(data = my_continent) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    # geom_sf(data = basin_shapes,
    #         alpha = .2,
    #         color = yale_lblue,
    #         fill  = yale_lblue) +
    geom_sf(data = continent_rivers,
            alpha = .3,
            color = yale_lblue,
            linewidth = .3) +
    geom_sf(data = rivers_international,
            alpha = 1,
            color = yale_blue,
            linewidth = 1) +
    # geom_sf(data = china_dams,
    #         alpha = .4,
    #         color = yale_blue,
    #         size = .5) +
    labs(title = "Rivers of South America",
         caption = c("Data from HydroRIVERS (2019). International segments in medium blue")) +
    theme_map()
  
  #map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = "continent_rivers_hydrosheds.png",
           width = 9,
           height = 5,
           dpi  = 300)
  
  
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
