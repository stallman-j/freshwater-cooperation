# _______________________________#
# Environment
# Clean 02: Merge DHS and GPS in order to get rive rnetworks
# 
# Stallman
# Started: 2023-10-23
# Last edited: 2023-11-13
# Edit: added get elevation to merge function merge_dhs_gps
#________________________________#


# https://dhsprogram.com/data/Guide-to-DHS-Statistics/Adult_Mortality_Rates.htm

# Startup

  rm(list = ls())


# bring in the packages, folders, paths ----
  
  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    stringr, # string operations
    countrycode, # country naming conversions
    sf, # vector spatial geometry
    rdhs, # getting DHS data
    elevatr, # for getting elevation from points
    data.table,
    RColorBrewer, # for getting gradients and colors
    parallel, # for parallelizing operations
    tictoc # timing # more ability to customize to output to latex. use with kableExtra to output tables
    # to console, latex, Rmarkdown, html etc.
  )

# parameters ----

  n_cores <- 4
  max_distance_to_snap <- 10000
  equal_area_crs   <- "ESRI:102022"
  
# paths ---

  temp_path <- file.path(data_external_temp,"merged","DHS_GLOW")
  if (!dir.exists(temp_path)) dir.create(temp_path, recursive = TRUE)
  
# bring in data ----

  
  dhs_gps <- readRDS(
          file = file.path(data_external_temp,"DHS","GPS","merged","africa_DHS_GPS.rds"))
  
  system.time(
    river_locations <- readRDS(file = file.path(data_external_temp,"GLOW_global-long-term-river-width",
                                              "africa_river_width_locations_equal_area.rds"))
    
  )
  
  
  path <- file.path(data_external_raw,"HydroSHEDS","HydroRIVERS_v10_af.gdb","HydroRIVERS_v10_af.gdb")
  
  system.time(
    hydro_rivers <- st_read(dsn = path) %>% 
      st_transform(crs = equal_area_crs) %>% 
      rename(geometry = Shape) # need this to use riverdist package
  )
  
#   get the main rivers ----

  main_rivers_all <- hydro_rivers %>% st_drop_geometry() %>% select(MAIN_RIV) %>%
    unique() %>% as.vector() %>% .[[1]]
  
  

# do spatial join ----

  # want to get: for each DHS cluster, the ID of the 
  get_closest_points <- function(dhs_cluster){
    
    nearest_indices <- st_nearest_feature(dhs_cluster,river_locations)
    
    # this is also fast
    closest_points <- dhs_cluster %>%
      mutate(my_linestring = st_nearest_points(dhs_cluster,river_locations[nearest_indices,], pairwise = TRUE),
             closest_point = st_cast(my_linestring, 'POINT')[seq(2, nrow(.)*2, 2)],
             distance_to_river      = st_distance(dhs_cluster, river_locations[nearest_indices,], by_element = TRUE),
             snapped_point_cond = st_sfc(ifelse(as.numeric(distance_to_river) <= max_distance_to_snap, st_geometry(closest_point),geometry),crs = st_crs(equal_area_crs))
      ) %>%
      cbind(st_drop_geometry(river_locations[nearest_indices,]))
    
    # add in X Y coordinates in projected form
    closest_points$X <- st_coordinates(closest_points)[,1]
    closest_points$Y <- st_coordinates(closest_points)[,2]
    
  }
  
# get closest crow-flies points ----
  # 
  tic("Getting closest point and distances")
  closest_points <- dhs_gps %>%
    mutate(my_linestring = st_nearest_points(dhs_gps,river_locations[nearest_indices,], pairwise = TRUE),
           closest_point = st_cast(my_linestring, 'POINT')[seq(2, nrow(.)*2, 2)],
           dist_to_river_locations      = st_distance(dhs_gps, river_locations[nearest_indices,], by_element = TRUE),
           numeric_dist_to_cs  = as.numeric(dist_to_river_locations),
           snapped_point_cond = st_sfc(ifelse(as.numeric(dist_to_river_locations) <= max_distance_to_snap, 
                                              st_geometry(closest_point),geometry),crs = st_crs(equal_area_crs))
    ) %>%
    cbind(st_drop_geometry(river_locations[nearest_indices,]))
  
  toc()
  # add in X Y coordinates in projected form
  closest_points$X <- st_coordinates(closest_points)[,1]
  closest_points$Y <- st_coordinates(closest_points)[,2]
  
  close_points <- closest_points %>% 
    dplyr::filter(numeric_dist_to_cs<=10000)
  
  tic("Spatially joined DHS clusters to river widths cross section")
  merged <- st_join(dhs_gps, river_locations,
                    join = st_nearest_feature)
  
  toc()
  
  # Spatially joined DHS clusters to river widths cross section: 3.72 sec elapsed
  
