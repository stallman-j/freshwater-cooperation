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

rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))
  
# read in data ----
  
  library(sf)
  
  
  path <- file.path(data_raw,"hydroSHEDS","HydroRIVERS_v10_sa.gdb","HydroRIVERS_v10_sa.gdb")
  
  sa_rivers <- st_read(dsn = path)



  names(sa_rivers)
  # [1] "HYRIV_ID"     "NEXT_DOWN"    "MAIN_RIV"     "LENGTH_KM"    "DIST_DN_KM"   "DIST_UP_KM"   "CATCH_SKM"   
  # [8] "UPLAND_SKM"   "ENDORHEIC"    "DIS_AV_CMS"   "ORD_STRA"     "ORD_CLAS"     "ORD_FLOW"     "HYBAS_L12"   
  # [15] "Shape_Length" "Shape" 
  # 
  # 
  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  
  
  south_america <- world %>%
                  filter(continent == "South America")
  
  plot(st_geometry(south_america))
  
  st_crs(world)
  
  # change crs to be same as the rivers
  south_america <- south_america %>%
                  st_transform(crs = st_crs(sa_rivers))
  
  
  # spatial subset so we can work with just south american rivers
  #sa_rivers <- world_rivers[south_america,]
  
  saveRDS(sa_rivers,
          file = file.path(data_clean,"sa_rivers.rds"))
  
  #sa_rivers <- readRDS(file = file.path(data_clean,"sa_rivers.rds"))
  
  # 1,620,963 obs 
  
  # randomly sample some of these
  
  sample_id <- sample(nrow(sa_rivers),
                         size = 1000,
                         replace= FALSE)
  
  river_sample <- sa_rivers[sample_id,]
  
  ## CHANGE THIS WHEN NOT SAMPLING
  
  #sa_rivers <- river_sample
  
  saveRDS(south_america,
          file = file.path(data_clean,"south_america.rds"))
  
  south_america <- readRDS(file = file.path(data_clean,"south_america.rds"))
  
  # take out the world rivers for space
  #rm(world_rivers)
  
  # map <- ggplot(data = south_america) +
  #   geom_sf(color = "gray70",
  #           fill = "gray99",
  #           alpha = 0.5,
  #           linewidth = .3) +
  #   geom_sf(data = sa_rivers,
  #           alpha = .3,
  #           color = yale_lblue,
  #           linewidth = .5) +
  #   labs(title = "Rivers of South America",
  #        caption = c("Data from HydroRIVERS (2019)")) +
  #   theme_map()
  # 
  # save_map(output_folder = output_maps,
  #          plotname = map,
  #          filename = "sa_rivers_hydroRIVERS.png",
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
  
  sa_country_lines <- st_cast(south_america, "MULTILINESTRING", 
                              group_or_split = TRUE)
  
  sa_outline <- st_cast(st_union(south_america), "MULTILINESTRING",
                        group_or_split = TRUE)
  
  # create a yes-no insersection if the geometries intersect
  
  # a list is returned for st_intersects, so take the ones where there's a positive length of intersection
  # ie that the river intersects a country line
  
  # 
  # generates a logical vector of length of sa_rivers
  intersects_border_yesno <- lengths(st_intersects(sa_rivers,sa_country_lines)) > 0
  coastline_yesno <- lengths(st_intersects(sa_rivers, coastlines)) >0
  outline_TF      <- lengths(st_intersects(sa_rivers, sa_outline)) >0
  
  # verify that this is getting what we want
  check_border <- intersects_border_yesno[1:30]
  check_coastline  <- coastline_yesno[1:30]
  
  check_international <- check_border & !check_coastline
  
  intersects_international_yesno <- intersects_border_yesno & !coastline_yesno & !outline_TF
  

  # so now we want IS international, but is NOT intersecting a coast
  
  rivers_international <- sa_rivers[intersects_international_yesno,]
  rivers_domestic      <- sa_rivers[!intersects_international_yesno,]
  
  saveRDS(rivers_international,
          file = file.path(data_clean,"rivers_international_sa.rds"))
  saveRDS(rivers_domestic,
          file = file.path(data_clean,"rivers_domestic_sa.rds"))
  
  
  # try now with a function
  
  # rivers_intl_fn <- get_in_out_intersections(sf_of_interest = sa_rivers,
  #                                            yes_sf = sa_country_lines,
  #                                            no_sf  = coastlines)
  # 
  # 
  
  #plot(st_geometry(rivers_international))
  
  # plot to check
  
  map <- ggplot(data = south_america) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    # geom_sf(data = basin_shapes,
    #         alpha = .2,
    #         color = yale_lblue,
    #         fill  = yale_lblue) +
    geom_sf(data = sa_rivers,
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
           filename = "sa_rivers_hydrosheds.png",
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
