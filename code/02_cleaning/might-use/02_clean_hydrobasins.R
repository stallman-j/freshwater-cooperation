# _______________________________#
# Environment
# Clean HydroRIVERS generic
# 
# Stallman
# Started 2023-05-10
# Last edited: 
#________________________________#


# Get all countries intersecting with a basin

# Startup

rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

# some parameters ----
  
  #my_units <- c("Zambia","Zimbabwe","Angola","Malawi","Mozambique","Botswana","Tanzania","Namibia")
  #units_name <- c("Zambezi")
  #my_units <- c("Malawi")
  
  #units_name <- c("Malawi")
  
# read in data ----
  
  library(sf)
  library(tictoc)
  library(parallel)
  library(tmap)
  #library(doParallel)
  
# Select basins ----
  
  path <- file.path(data_external_raw,"HydroSHEDS","HydroATLAS","BasinATLAS_Data_v10","BasinATLAS_v10.gdb")

  layers <- sf::st_layers(dsn = path)
  
  # lev01 is the continental region
  # lev02 is polygons about the size of India
  # lev03 is 292 major basins
  
  system.time(
  basin_atlas <- sf::st_read(dsn = path,
                             layer = "BasinATLAS_v10_lev03" )
  )
  
  plot(st_geometry(basin_atlas))

  geometry_type <- st_geometry_type(basin_atlas) # multipolygons
  
  
  names(basin_atlas)
  # 297 variables
  
# select basins we want ----   
  
  # turn off spherical geometry
  
  sf_use_s2(FALSE)

  # set to scrollable view
  tmap_mode("view")
  
  tm_shape(basin_atlas) +
    tm_polygons(alpha = .3)
  
## manually scroll over and get the basin IDs we want ----
  basin_ids <- c(1030022420,1030040210,1030040190,1030040280,1030040310,
                         1030040220,1030020050,1030021940,1030022430,1030023300,
                         1030021940)
  

# split out the countries and the hydroRIVERS with the countries into a parallelizable separate list ----
  countries_vec <- get_countries_from_basins(basin_ids) 
  
  my_list <- get_countries_for_splitting(countries = countries_vec)
  
  my_countries <- my_list[[2]]
  split_list   <- my_list[[1]]
  
  print("Running parallel intersections")
  n_cores <- my_list[[3]]
  
  cl <- makeCluster(n_cores)
  clusterEvalQ(cl, library(sf))
  clusterExport(cl, c("my_countries"))
  
  split_results <- parLapply(cl, split_list, function(x) st_intersection(x, my_countries))
  
  print("Completed in parallel the intersections")
  stopCluster(cl)
  
  print("Saving the HydroRIVERS sf")
  hydro_rivers_sf <- do.call("rbind",split_results) %>%
    mutate(width = 1/as.numeric(ORD_CLAS))
  
  # save
  new_list <- save_countries_and_hydrorivers(hydro_rivers_sf,
                                             my_countries)
  
  
  
  my_countries <- readRDS(file.path("P:","data","03_clean","shape-files","test_countries.rds"))
  hydro_rivers_sf <- readRDS(file.path("P:","data","03_clean","shape-files","test_hydroRIVERS.rds"))
  
  basin_shapefile <-   sf::st_read(dsn = file.path("E:","data","01_raw","HydroSHEDS","HydroATLAS","BasinATLAS_Data_v10","BasinATLAS_v10.gdb"),
                                                layer = "BasinATLAS_v10_lev03" ) %>% # read in the data
                        dplyr::filter(HYBAS_ID %in% basin_ids) %>%
                        st_transform(crs = 4326)
  
  continent_shapefile <- sf::st_read(dsn = file.path("E:","data","01_raw","HydroSHEDS","HydroATLAS","BasinATLAS_Data_v10","BasinATLAS_v10.gdb"),
                                     layer = "BasinATLAS_v10_lev01" ) %>% # read in the data
                         st_transform(crs = 4326)
  
  
  # check some plots
  tm_shape(basin_ids) +
    tm_polygons(alpha = .3)
  
  
  library(ggplot2)
  map <- ggplot(data = my_countries) +
    geom_sf(color = "gray30",
            fill = "white",
            alpha = 0.5,
            linewidth = .5) +
    geom_sf(data = basin_shapefile,
            alpha = .1,
            fill = "red4",
            color = "red4",
            linewidth = .5) +
    geom_sf(data = hydro_rivers_sf,
            alpha = hydro_rivers_sf$width,
            color = yale_lblue,
            linewidth = hydro_rivers_sf$width) +
    labs(title = paste0("Rivers in a Selection of Sahara-Sahel Basins"),
         caption = c("Data from HydroRIVERS (2019)")) +
    theme_map()
  
  map
  # map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = paste0("hydro_rivers_north_africa_hydroRIVERS.png"),
           width = 9,
           height = 5,
           dpi  = 300)
  
  # take a look more closely
  #ggplotly(map)

  
# # Get the rivers that intersect a country boundary ----
#   
#   library(rnaturalearth) # has ne_coastline() which is global coastlines
#   if (!require(rnaturalearthdata)) install.packages("rnaturalearthdata")
#   
#   library(rnaturalearthdata)
#   
#   # scale medium means a medium-sized file, better detail
#   coastlines <- ne_coastline(scale = "medium", returnclass = "sf")
#   
#   
#     # cast the country polygon borders into lines
#   
#   sa_country_lines <- st_cast(south_america, "MULTILINESTRING", 
#                               group_or_split = TRUE)
#   
#   sa_outline <- st_cast(st_union(south_america), "MULTILINESTRING",
#                         group_or_split = TRUE)
#   
#   # create a yes-no insersection if the geometries intersect
#   
#   # a list is returned for st_intersects, so take the ones where there's a positive length of intersection
#   # ie that the river intersects a country line
#   
#   # 
#   # generates a logical vector of length of hydro_rivers_units
  intersects_border_yesno <- lengths(st_intersects(hydro_rivers_sf,my_countries)) > 0
  coastline_yesno <- lengths(st_intersects(hydro_rivers_sf, continent_shapefile)) >0
  #outline_TF      <- lengths(st_intersects(hydro_rivers_sf, sa_outline)) >0

#   # verify that this is getting what we want
#   check_border <- intersects_border_yesno[1:30]
#   check_coastline  <- coastline_yesno[1:30]
#   
#   check_international <- check_border & !check_coastline
#   
#   intersects_international_yesno <- intersects_border_yesno & !coastline_yesno & !outline_TF
#   
# 
#   # so now we want IS international, but is NOT intersecting a coast
#   
  rivers_international <- hydro_rivers_sf[intersects_international_yesno,]
  rivers_domestic      <- hydro_rivers_sf[!intersects_international_yesno,]
  
#   
  saveRDS(rivers_international,
          file = file.path(data_clean,"shape-files","north_africa_rivers_international.rds"))
  saveRDS(rivers_domestic,
          file = file.path(data_clean,"shape-files","north_africa_rivers_domestic.rds"))


  
    map <- ggplot(data = my_countries) +
      geom_sf(color = "gray70",
              fill = "gray99",
              alpha = 0.5,
              linewidth = .3) +
      geom_sf(data = rivers_international,
              alpha = hydro_rivers_sf$width,
              color = "green4",
              linewidth = hydro_rivers_sf$width) +
      geom_sf(data = rivers_domestic,
              alpha = hydro_rivers_sf$width,
              color = yale_lblue,
              linewidth = hydro_rivers_sf$width) +blue,
      #         size = .5) +
      labs(title = "International and Domestic Rivers in a Selection of Sahara-Sahel Basins ",
           caption = c("Data from HydroRIVERS (2019). International segments in medium blue")) +
      theme_map()

    #map

    save_map(output_folder = output_maps,
             plotname = map,
             filename = "hydro_rivers_north_africa_domestic_international.png",
             width = 9,
             height = 5,
             dpi  = 300)
  
  
#   # try now with a function
#   
#   # rivers_intl_fn <- get_in_out_intersections(sf_of_interest = hydro_rivers_units,
#   #                                            yes_sf = sa_country_lines,
#   #                                            no_sf  = coastlines)
#   # 
#   # 
#   
#   #plot(st_geometry(rivers_international))
#   
#   # plot to check
#   
#   map <- ggplot(data = south_america) +
#     geom_sf(color = "gray70",
#             fill = "gray99",
#             alpha = 0.5,
#             linewidth = .3) +
#     # geom_sf(data = basin_shapes,
#     #         alpha = .2,
#     #         color = yale_lblue,
#     #         fill  = yale_lblue) +
#     geom_sf(data = hydro_rivers_units,
#             alpha = .3,
#             color = yale_lblue,
#             linewidth = .3) +
#     geom_sf(data = rivers_international,
#             alpha = 1,
#             color = yale_blue,
#             linewidth = 1) +
#     # geom_sf(data = china_dams,
#     #         alpha = .4,
#     #         color = yale_blue,
#     #         size = .5) +
#     labs(title = "Rivers of South America",
#          caption = c("Data from HydroRIVERS (2019). International segments in medium blue")) +
#     theme_map()
#   
#   #map
#   
#   save_map(output_folder = output_maps,
#            plotname = map,
#            filename = "hydro_rivers_units_hydrosheds.png",
#            width = 9,
#            height = 5,
#            dpi  = 300)
#   
#   
#   # plot rivers if intersection is true
#   
# #   # for interactive ggplot
# #   if (!require(plotly)) install.packages("plotly")
# #   
# #   library(plotly)
# #   
# #   # take a look more closely
# #   ggplotly(map)
# #   
# # ## use fancier data ----
# #   
# #   # mask
# #   path <- file.path(data_raw,"GRWL_global-river-widths-landsat","GRWL_vector_V01.01","GRWL_vector_V01.01")
# #   
# #   # view available layers
# #   st_layers(path)
# #   
# #   
# #   
# #   test_read <- st_read(path)
# #   
# #    
# #   map <- ggplot(data = world) +
# #     geom_sf(color = "gray70",
# #             fill = "gray99",
# #             alpha = 0.5,
# #             linewidth = .3) +
# #     # geom_sf(data = basin_shapes,
# #     #         alpha = .2,
# #     #         color = yale_lblue,
# #     #         fill  = yale_lblue) +
# #     geom_sf(data = test_read,
# #             alpha = .3,
# #             color = yale_medblue,
# #             linewidth = 1) +
# #     # geom_sf(data = china_dams,
# #     #         alpha = .4,
# #     #         color = yale_blue,
# #     #         size = .5) +
# #     labs(title = "Finding the data",
# #          caption = c("Data from GRLW (2018)")) +
# #     theme_map()
# #   
# #   map
# #   
# #   ggplotly(map)
# #   
# #   
  
  
  #' Get Basins ----
  #' @description
  #' Given a vector (possibly just one) of basin IDs from HydroBASINS, select the countries which 
  #' spatially intersect the basin
  #' @param basin_ids a vector of numerics giving the HYBAS_ID in HydroATLAS of the desired basins
  #' @param basin_atlas_path path leading to the BasinATLAS.gdb data
  #' @param output_path path for outputting the RDS file that holds the vector of countries which intersect the basin
  #' @param output_filename the name of the RDS file to store the vector of country names
  #' @returns a vector of three-letter ISO_a3 country codes of countries intersected by the basin(S) and saves this in the output_path path
  #' 
  get_countries_from_basins <- function(basin_ids = c(1030022420,1030040210),
                                        basin_atlas_path = file.path("E:","data","01_raw","HydroSHEDS","HydroATLAS","BasinATLAS_Data_v10","BasinATLAS_v10.gdb"),
                                        output_path      = file.path("P:","data","02_temp","saved-information"),
                                        output_filename  = "countries_to_include.rds"
  ){
    
    # create the output directory if it's not already there
    if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE) # recursive lets you create any needed subdirectories
    
    if (!require("tmap")) install.packages("tmap")
    if (!require("sf")) install.packages("sf")
    
    library(tmap)
    library(sf)
    
    data(World)
    
    sf_use_s2(FALSE)
    
    basin_atlas <- sf::st_read(dsn = basin_atlas_path,
                               layer = "BasinATLAS_v10_lev03" )
    
    selection <- basin_atlas[basin_atlas$HYBAS_ID %in% basin_ids,] %>%
      st_transform(crs = 4326)
    
    World <- sf::st_transform(World, crs = 4326)
    
    intersected_basins <- sf::st_intersection(World,selection)
    
    included_countries <- unique(as.character(intersected_basins$iso_a3))
    
    saveRDS(included_countries,
            file = file.path(output_path,output_filename))
    
    return(included_countries)
  }
