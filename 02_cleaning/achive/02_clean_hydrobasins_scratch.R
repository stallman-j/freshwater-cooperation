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
  n_cores <- detectCores(logical = TRUE) - 4
  
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
  
# select basins ----   
  
  # turn off spherical geometry
  
  sf_use_s2(FALSE)

  # set to scrollable view
  tmap_mode("view")
  
  tm_shape(basin_atlas) +
    tm_polygons(alpha = .3)
  
## manually scroll over and get the basin IDs we want ----
  desired_basin_ids <- c(1030022420,1030040210,1030040190,1030040280,1030040310,
                         1030040220,1030020050,1030021940,1030022430,1030023300,
                         1030021940)
  

## select the basin_atlas with just these basins ----

  # choose the basins which are our desired IDs
  selection <- basin_atlas[basin_atlas$HYBAS_ID %in% desired_basin_ids,
  ]
  
  tm_shape(selection) +
    tm_polygons(alpha = .3)
  
## intersect these basins with country polygons ----
  
  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  # change CRS
  world <- world %>%
    st_transform(crs = st_crs(basin_atlas))

  # gives a df with information from world which creates a new row for each country-basin intersection
  intersected_basins <- st_intersection(world,selection)
  
  tm_shape(intersected_basins) +
    tm_polygons(alpha = .3)
  

  included_countries <- unique(as.character(intersected_basins$iso_a3))

  saveRDS(included_countries,
          file = file.path(data_temp,"saved-information","countries_to_use.rds"))

# create a "width" variable for plotting purposes depending on hydrologic discharge ----
  
  hydro_rivers_units <- hydro_rivers_units %>%
                            mutate(width = 1/as.numeric(ORD_CLAS))
  

  saveRDS(hydro_rivers_units,
          file = file.path(data_clean,"shape-files",paste0(units_name,"_hydro_rivers.rds")))
  
  
  hydro_rivers_units <- readRDS(file = file.path(data_clean,"shape-files",paste0(units_name,"_hydro_rivers.rds")))
  
  
  
  
  
# Work to do ----
  # # randomly sample some of these
  # 
  # sample_id <- sample(nrow(hydro_rivers_units),
  #                        size = 100,
  #                        replace= FALSE)
  # 
  # river_sample <- hydro_rivers_units[sample_id,]
  # 
  # ## CHANGE THIS WHEN NOT SAMPLING
  # 
  # #hydro_rivers_units <- river_sample
  # 
  # # take out the world rivers for space
  # rm(hydro_rivers)
  # 
  # 


  

  
#   
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
#   intersects_border_yesno <- lengths(st_intersects(hydro_rivers_units,sa_country_lines)) > 0
#   coastline_yesno <- lengths(st_intersects(hydro_rivers_units, coastlines)) >0
#   outline_TF      <- lengths(st_intersects(hydro_rivers_units, sa_outline)) >0
#   
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
#   rivers_international <- hydro_rivers_units[intersects_international_yesno,]
#   rivers_domestic      <- hydro_rivers_units[!intersects_international_yesno,]
#   
#   saveRDS(rivers_international,
#           file = file.path(data_clean,"rivers_international_sa.rds"))
#   saveRDS(rivers_domestic,
#           file = file.path(data_clean,"rivers_domestic_sa.rds"))
#   
#   
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
