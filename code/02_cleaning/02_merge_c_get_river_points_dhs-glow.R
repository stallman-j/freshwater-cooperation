# _______________________________#
# Environment
# Merge 02 C: Get River Points for XY Coordinates
# 
# Stallman
# Started 2023-10-11
# Last edited: 
#________________________________#


# need 02_merge_a_era5_dhs


# Startup

  rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

# requires having run
# 02_merge_a_era5_dhs
# 02_clean_dhs_child-mortality_annual

# packages ----

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    tictoc, #measuring time to run operations
    countrycode, # for translating between country names
    rdhs, # for dealing with DHS in R
    sf, # vector operations
    terra, # raster operations
    parallel,
    zoo # time series stuff, easy calc of rolling averages
  )

# prior cleaning code ----  
  
  # 02_merge_a_all-dhs-gps.R
  # DHS GPS data
  
  if (!file.exists(file.path(data_external_clean,"DHS","datasets-for-selection",
                             paste0("gps_datasets_africa.rds")))) {
    
    source(file.path(code_clean,"02_clean_choose-DHS-countries.R"))
  }
  
  # 02_clean_glow_global-long-term-river-width
  # River Widths
  
  if (!file.exists(file.path(data_external_temp,"GLOW_global-long-term-river-width",
                                    "africa_river_width_locations_equal_area.rds"))) {
    
    source(file.path(code_clean,"02_clean_glow_global-long-term-river-width.R"))
  }
  
  # 02_clean_GADM.R
  # Administrative boundary polygons
  gadm_in_path <- file.path(data_external_clean,"GADM","global")
  gadm_in_filename <- paste0("GADM_global_ADM_",level,".rds")
  
  if (!file.exists(file.path(gadm_in_path,gadm_in_filename))) {
    
    source(file.path(code_clean,"02_clean_GADM.R"))
  }
  
# parameters ----

  level <- 1 # GADM administrative level, for polygons
  n_cores  <- 14 # 16 put 100% CPU; 14 was 93-97% CPU, just about right

# bring in datasets ----

  countries_DHS_africa <- readRDS(
    file= file.path(data_external_clean,"DHS","datasets-for-selection",
                    paste0("countries_DHS_africa.rds")))
  
  gps_datasets_africa <- readRDS(
    file= file.path(data_external_clean,"DHS","datasets-for-selection",
                    paste0("gps_datasets_africa.rds")))

  path <- file.path(data_external_raw,"HydroSHEDS","HydroRIVERS_v10_af.gdb","HydroRIVERS_v10_af.gdb")
  
  tic("Bringing in HydroRIVERS data")
  
    hydro_rivers <- st_read(dsn = path) %>% 
      st_transform(crs = equal_area_crs) %>% 
      rename(geometry = Shape) # need this to use riverdist package
  
  toc()
 
  tic("Bringing in GADM data")
  gadm_data  <- readRDS(file.path(gadm_in_path,gadm_in_filename)) %>%
    st_transform(crs = equal_area_crs)
  
  toc()
  
  gadm_data$continent <- countrycode(gadm_data$GID_0,
                                     origin = "iso3c",
                                     destination = "continent")
  
  dhs_data <- readRDS(file = file.path(data_external_temp,"DHS","GPS","merged","africa_DHS_GPS.rds")) 
  
  tic("Bringing in GLOW river locations")
    river_locations <- readRDS(file = file.path(data_external_temp,"GLOW_global-long-term-river-width",
                                                "africa_river_width_locations_equal_area.rds"))
  toc()


  # dhs_tmp   <- dhs_data %>% 
  #              rename(ID = DHSID,
  #                     lat = LATNUM,
  #                     lon = LONGNUM) %>%
  #              select(ID,lat,lon,geometry)
  
# bring in distances data  ----
  
  # get the folder where the dyad distances are located
  dyad_distances_path <- file.path(data_external_temp,"merged","DHS_HydroSHEDS","dyad-distances")
  
  # get the filenames of the countries in our list 
  distances_filenames <- list.files(path = dyad_distances_path,
                                    pattern = "dyad_distances")
  
  
  
  #distances_filenames <- distances_filenames[1:500]
  
  main_rivs <- str_remove_all(distances_filenames,"DHS_MAIN_RIV_") %>%
    str_remove_all("_dyad_distances.rds")
  
  # rbind all the rows we want to bring in   
  tic("Looped over and rbinded all data in")
  
  for (filename in distances_filenames){
    
    temp_data <- readRDS(file.path(dyad_distances_path,filename)) %>% 
      mutate(MAIN_RIV = str_remove_all(filename,"DHS_MAIN_RIV_") %>% str_remove_all("_dyad_distances.rds"),
             n_towns  = length(unique(downstream)))
    
    number_of_towns <- temp_data$n_towns %>% unique()
    
    main_river <- temp_data$MAIN_RIV %>% unique()
    
    print(paste0("Currently working on MAIN_RIV ",main_river,". It has ",nrow(temp_data)," rows and ",number_of_towns," towns."))
    
    #print(paste0("dataset ",filename," has ",nrow(temp_data)," rows."))
    if (filename == distances_filenames[1]){
      
      distance_data <- temp_data
      
      
    } else {
      
      
      if (number_of_towns>100) { # ignoring big rivers
        
        rm(temp_data)
        gc() 
        
      }  else {
        distance_data  <- rbind(distance_data,temp_data)
      }
      
      
      
    } # end ifelse to get the first df to be setting things up
    
  } # end loop over filenames
  
  toc()
  #Looped over and rbinded all data in: 781.85 sec elapsed
  # this is like 955 seconds
  
  out_path <- file.path(data_external_clean,"merged","DHS_ERA5_HydroSHEDS")
  
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  distance_data <- distance_data %>%
    mutate(same_river_segment = if_else((upstream!=downstream) & (distance_m ==0),
                                        true = 1,
                                        false = 0),
           point_to_itself    = if_else(upstream==downstream,
                                        true = 1,
                                        false = 0)) 
  
  saveRDS(distance_data, file = file.path(out_path,"river-distances_under-100-towns.rds"))
  
  
  
  single <- distance_data %>%
    filter(n_towns == 1)
  
  saveRDS(single, file = file.path(out_path,"singletons_node-level.rds"))
  
  dyads <- distance_data %>%
    filter(n_towns == 2) 
  
  
  saveRDS(dyads, file = file.path(out_path,"dyads_node-level.rds"))
  
  
  triads <- distance_data %>% filter(n_towns ==3 )
  
  saveRDS(triads, file = file.path(out_path,"triads_node-level.rds"))
  
  
  singletons <- distance_data %>%
    filter(n_towns == 1) %>%
    left_join(era5_gps_childmort,
              by = c("downstream" = "DHSID"),keep = TRUE) %>%
    filter(!is.na(precip_current_annual_avg_mm_month))
  
  
  
  saveRDS(singletons, file = file.path(out_path,"singletons_panel.rds"))

# bring in the data ----
  
  
  singletons <- readRDS(file = file.path(data_external_clean,"merged","DHS_ERA5_HydroSHEDS","singletons_node-level.rds"))
  dyads      <- readRDS(file = file.path(data_external_clean,"merged","DHS_ERA5_HydroSHEDS","dyads_node-level.rds"))
  triads     <- readRDS(file = file.path(data_external_clean,"merged","DHS_ERA5_HydroSHEDS","triads_node-level.rds"))
  under_100  <-  readRDS(file = file.path(data_external_clean,"merged","DHS_ERA5_HydroSHEDS","river-distances_under-100-towns.rds"))
# select main rivers to go over ----
  
  main_rivers_singletons <- singletons %>% select(MAIN_RIV) %>% unique() %>% as.vector()%>% .[[1]]
  
  main_rivers_dyads <- dyads %>% select(MAIN_RIV) %>% unique() %>% as.vector() %>% .[[1]]
  
  main_rivers_triads <- triads %>% select(MAIN_RIV) %>% unique() %>% as.vector() %>% .[[1]]
    
  main_rivers_under_100 <- under_100 %>% select(MAIN_RIV) %>% unique() %>% as.vector() %>% .[[1]]
    
  # requires:
  # dhs_tmp
  # hydro_rivers
    
  get_dhs_rivers_points_glow <- function(main_river,
                                      data_clean_path = file.path("E:","data","03_clean"),
                                      data_temp_path  = file.path("E:","data","02_temp"),
                                      river_network_path = file.path("E:","data","03_clean","HydroSHEDS","river_networks"),
                                      hydro_rivers_units_path = file.path("E:","data","03_clean","HydroSHEDS","shape-files"),
                                      glow_countries_units_path = file.path("E:","data","02_temp","GLOW_global-long-term-river-width","shape-files"),
                                      gadm_union_shapefile_path = file.path("E:","data","03_clean","shape-files","GADM"),
                                      max_distance_to_snap = 100000,
                                      equal_area_crs = "ESRI:102022"
  ){
    
    tryCatch( {
      # create output folders
      checked_river_path         <- file.path(data_temp_path,"merged","DHS_GLOW_HydroSHEDS","checked-main-rivers","dhs-glow-river-points")
      river_network_missing_path <- file.path(data_temp_path,"HydroSHEDS","river-network-missing")
      river_points_path        <- file.path(data_temp_path,"merged","DHS_GLOW_HydroSHEDS","river-points")
      hydro_rivers_units_missing_path <- file.path(data_temp_path,"HydroSHEDS","hydro_rivers_units_missing")
      glow_countries_units_missing_path  <- file.path(data_temp_path,"GLOW_global-long-term-river-width","glow_units_missing")
      
      paths_to_create <- c(checked_river_path,
                           river_network_missing_path,
                           river_points_path,
                           hydro_rivers_units_missing_path,
                           glow_countries_units_missing_path
      )
      
      for (path in paths_to_create){
        
        if (!dir.exists(path)) dir.create(path, recursive = TRUE)
        
      }
      
      # run through conditions that should stop the function 
      # 
      # Stop if riverr network directory doesn't exist
      if (!dir.exists(river_network_path)) {
        
        stop(paste0("River network directory in ",river_network_path," doesn't exist. You either need to create the river networks or check that you wrote the path correctly."))
        
      }
      
      # Stop and exit if there's an incorrectly specified path for the country-specific hydrorivers units, exit
      if (!dir.exists(hydro_rivers_units_path)) {
        
        stop(paste0("Path ",hydro_rivers_units_path," doesn't exist. You either need to create the country-specific river shapefiles or check that you wrote the path correctly."))
        
      }
      
      # if the current river network doesn't exist, create output that says the network is missing, and stop function
      if (!file.exists(file = file.path(data_clean_path,"HydroSHEDS","river_networks",paste0("MAIN_RIV_",main_river,"_cleaned_river_network.rds")))){
        
        saveRDS(main_river,file.path(river_network_missing_path,paste0("MAIN_RIV_",main_river,"_network_doesnt_exist.rds")))
        
        stop(paste0("Current river network for main_river ",main_river," doesn't exist. Create it first and come back."))
        
        
      } 
      # Stop if the river has already been checked, don't check it again
      if (file.exists(file = file.path(checked_river_path,paste0("MAIN_RIV_",main_river,"_already_checked.rds")))){
        
        stop(paste0("These river points for main_river ",main_river," have already been calculated. No need to go through this again."))
        
      }
      
      # bring in river network
      
      current_river_network <- readRDS(file = file.path(river_network_path,paste0("MAIN_RIV_",main_river,"_cleaned_river_network.rds")))
      
      single_river <- hydro_rivers %>%
        dplyr::filter(MAIN_RIV == main_river) %>%
        dplyr::mutate(width = 1/as.numeric(ORD_CLAS))
      
      
      mouth_stretch <- single_river %>% filter(NEXT_DOWN ==0)
      # get polygon boundary of river network
      
      # a little extra distance around the river than what the maximum snap will be
      polygon_boundary <- single_river %>% st_buffer(10100) %>% st_convex_hull() %>% st_union()
      
      #     plot(st_geometry(polygon_boundary))
      
      # check which countries intersect
      intersecting_indices <- lengths(st_intersects(gadm_data, polygon_boundary))>0
      
      # get the iso3c of the countries involved
      intersecting_countries <- gadm_data[intersecting_indices,] %>% select(GID_0) %>%
        st_drop_geometry() %>% unique() %>% .[,1]
      
      # restrict GADM data to just those countries
      river_countries <- gadm_data %>%
        filter(GID_0 %in% intersecting_countries & continent == "Africa")
      
      countries <- unique(river_countries$GID_0)
      
      countries_DHS    <- countrycode(countries,
                                      origin = "iso3c",
                                      destination = "dhs")
      
      dhs_river_countries <- dhs_data %>%
        filter(DHSCC %in% countries_DHS) %>%
        rename(ID = DHSID,
               lat = LATNUM,
               lon = LONGNUM) %>%
        select(ID,lat,lon,geometry)
       
      
      
      if (nrow(dhs_river_countries)==0){
        saveRDS(main_river,
                file.path(checked_river_path,paste0("MAIN_RIV_",main_river,"_already_checked.rds")))
        
        stop(paste0("No DHS datasets intersect with MAIN_RIV ",main_river,"."))
      }
      
      river_polygons <- gadm_data %>% 
        filter(GID_0 %in% intersecting_countries & continent == "Africa")%>%
        group_by(GID_0) %>%
        summarize(geometry = st_union(geom)) %>%
        st_make_valid()
      
      
      #plot(st_geometry(river_polygons))
      
      
      # check if gadm shapefile that's the union of relevant countries exists. If it exists, read it in. If not, create it.
      
      if (file.exists(file.path(gadm_union_shapefile_path,paste0(paste(countries,collapse="_"),"_union_shapefile.rds")))) {
        
        units <- readRDS(file = file.path(gadm_union_shapefile_path,paste0(paste(countries,collapse="_"),"_union_shapefile.rds")))
        
      } else {
        units <- gadm_data %>% filter(GID_0 %in% countries) %>% st_union() %>% st_make_valid() 
        
        saveRDS(units,
                file = file.path(gadm_union_shapefile_path,paste0(paste(countries,collapse="_"),"_union_shapefile.rds")))
      } # end ifelse over if the GADM file exists then bring it in, otherwise create it
      
      
      if (!file.exists( file.path(hydro_rivers_units_path,paste0(paste(countries,collapse = "_"),"_hydrorivers.rds")))) {
        
        # flag that the units are missing
        saveRDS(c(countries),
                file.path(hydro_rivers_units_missing_path,paste0(paste(countries,collapse = "_"),"hydro_rivers_units_missing.rds")))
        
        stop(paste0("HydroRIVERS file for countries ",paste(countries,collapse = " "), "is missing. Create elsewhere first (it takes a while) then come back."))
        
      } 
      
      
      hydro_rivers_units <- readRDS(file = file.path(hydro_rivers_units_path,paste0(paste(countries,collapse = "_"),"_hydrorivers.rds")))
      
      
      if (!file.exists( file.path(glow_countries_units_path,paste0(paste(countries,collapse = "_"),"_glow.rds")))) {
        
        # flag that the units are missing
        saveRDS(c(countries),
                file.path(glow_countries_units_missing_path,paste0(paste(countries,collapse = "_"),"glow_rivers_units_missing.rds")))
        
        stop(paste0("GLOW file for countries ",paste(countries,collapse = " "), "is missing. Create elsewhere first (it takes a while) then come back."))
        
      } 
      
      
      glow_country_units <- readRDS(file = file.path(glow_countries_units_path,paste0(paste(countries,collapse = "_"),"_glow.rds")))
      
      points_data <- rbind(glow_country_units,dhs_river_countries)
      
      # get closest points of the DHS to the rivers
      
      # this is a fast operation
      nearest_indices <- st_nearest_feature(points_data,hydro_rivers_units)
      
      # this is also fast
      closest_points <- points_data %>%
        mutate(my_linestring = st_nearest_points(points_data,hydro_rivers_units[nearest_indices,], pairwise = TRUE),
               closest_point = st_cast(my_linestring, 'POINT')[seq(2, nrow(.)*2, 2)],
               distance_to_river      = st_distance(points_data, hydro_rivers_units[nearest_indices,], by_element = TRUE),
               snapped_point_cond = st_sfc(ifelse(as.numeric(distance_to_river) <= max_distance_to_snap, st_geometry(closest_point),geometry),crs = st_crs(equal_area_crs))
        ) %>%
        cbind(st_drop_geometry(hydro_rivers_units[nearest_indices,]))
      
      # add in X Y coordinates in projected form
      closest_points$X <- st_coordinates(closest_points)[,1]
      closest_points$Y <- st_coordinates(closest_points)[,2]
      
      current_points <- closest_points %>% filter(MAIN_RIV == main_river)
      
      
      # save 
      
      if (nrow(current_points)==0) {
        saveRDS(main_river,
                file.path(checked_river_path,paste0("MAIN_RIV_",main_river,"_already_checked.rds")))
        
        stop(paste0("No points in points data match with MAIN_RIV ",main_river))
        
        
      }
      
      saveRDS(object = current_points,
              file =file.path(river_points_path,paste0("DHS_GLOW_MAIN_RIV_",main_river,"_river_points.rds")))
      
      saveRDS(main_river,
              file.path(checked_river_path,paste0("MAIN_RIV_",main_river,"_already_checked.rds")))
      
    }, # main part of the function for tryCatch
    
    error = function(e) {
      # code executed in event of an error
      return(0) },
    warning = function(w) {
      # code executed in event of a warning
      return(1)
    }
    
    
    ) # end tryCatch
  } # end function
  
  system.time(
  get_dhs_rivers_points_glow(main_river = 10006117)
  )
  
  
# parallelize ----
  
  
  
  
  tic(paste0("Get DHS and HydroRIVERS points data"))
  
  # 18 cores: CPU was at 100%, too many
  # 16 cores puts CPU close to 100% and memory at about 80, probably not the best
  # 14 cores used 100% cpu with other processes, try 12
  # 12 cores uses 85-100% CPU and 50-60GB RAM, about right
  # running this on 8 cores over all the main_rivers_all (16352 rivers in Africa) used up 50-60% CPU and hovered around 27-32 GB of RAM
  
  cl <- makeCluster(n_cores) # n_cores # runs low on RAM if hydro_rivers gets sent to too many places, try building up
  clusterEvalQ(cl,library(sf)) # send these separately, clusterEvalQ(cl, fun) is the call format
  clusterEvalQ(cl,library(riverdist))
  clusterEvalQ(cl, library(dplyr))
  clusterEvalQ(cl, library(countrycode))
  clusterEvalQ(cl, library(ggplot2))
  
  
  clusterExport(cl, c("hydro_rivers","gadm_data")) # this is a big export b/c the hydro_rivers is huge
  #clusterExport(cl, c("points_data"))
  clusterExport(cl, c("dhs_data"))
  
    #clusterExport(cl, c("max_distance_to_snap"))
  #clusterExport(cl, c("equal_area_crs"))
  
 
  parLapply(cl, main_rivers_under_100, get_dhs_rivers_points_glow)
  
  
  # parLapply(cl, main_rivers_singletons, get_dhs_rivers_points_glow)
  # parLapply(cl, main_rivers_dyads, get_dhs_rivers_points_glow)
  # parLapply(cl, main_rivers_triads, get_dhs_rivers_points_glow)
  # 
  
  
  
  gc()
  toc()
  
  stopCluster(cl)
  
  


    
