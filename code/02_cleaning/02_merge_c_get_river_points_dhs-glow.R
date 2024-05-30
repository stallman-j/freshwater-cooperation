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

  # parameters ----
  
  level <- 1 # GADM administrative level, for polygons
  n_cores  <- 12 # 16 put 100% CPU; 14 was 93-97% CPU, just about right
  
  
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
  gadm_in_filename <- paste0("GADM_global_ADM_1.rds")
  
  if (!file.exists(file.path(gadm_in_path,gadm_in_filename))) {
    
    source(file.path(code_clean,"02_clean_GADM.R"))
  }
  
  # source(file.path(code_folder,"02_cleaning","02_clean_adhi_africa-database-hydrometric-indices.R"))
  # source(file.path(code_folder,"02_cleaning","02_clean_gdat_global-dam-tracker.R"))
  # source(file.path(code_folder,"02_cleaning","02_merge_a_all-dhs-gps.R"))
  # source(file.path(code_folder,"02_cleaning","02_merge_b_gdat-adhi-dhs-gps_for-river-points.R"))
  
  

# bring in datasets ----

  
  
  # GPS_data <- readRDS(file = file.path(data_external_temp,"DHS","GPS","merged","africa_DHS_GPS.rds")) %>%
  #   mutate(country_iso3c = countrycode::countrycode(DHSCC,
  #                                                   origin = "dhs",
  #                                                   destination = "iso3c"))
  # 
  
  GPS_data <- readRDS(file.path(data_external_clean,"merged","many","dhs_gdat_adhi.rds"))
  
  
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
  
  tic("Bringing in GLOW river locations")
    river_locations <- readRDS(file = file.path(data_external_temp,"GLOW_global-long-term-river-width",
                                                "africa_river_width_locations_equal_area.rds"))
  toc()

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
    
  # need to have run the following: 
  # source(file.path(code_folder,"02_cleaning","02_merge_a_all-dhs-gps.R"))
  

  system.time(
  get_river_points(main_river = 10877433,
                   points_data = GPS_data
                   )
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
  clusterExport(cl, c("GPS_data"))
  #clusterExport(cl, c("dhs_data"))
  
    #clusterExport(cl, c("max_distance_to_snap"))
  #clusterExport(cl, c("equal_area_crs"))
  
 
  #parLapply(cl, main_rivers_under_100, get_river_points, points_data = GPS_data)
  
  tic("Getting river points for singletons")
  parLapply(cl, main_rivers_singletons, get_river_points_safe, points_data = GPS_data)
  toc()
  # Getting river points for singletons: 2979.62 sec elapsed
  
  # 
  tic("Getting river points for the dyads")
  parLapply(cl, main_rivers_dyads, get_river_points_safe, points_data = GPS_data)
  toc()
  # Getting river points for the dyads: 949.93 sec elapsed

  tic("Getting river points for the triads")
  parLapply(cl, main_rivers_triads, get_river_points_safe, points_data = GPS_data)
  toc()
  
  # Getting river points for the triads: 636.69 sec elapsed

  
  tic("Getting river points for the sub-100-town rivers")
  parLapply(cl, main_rivers_under_100, get_river_points_safe, points_data = GPS_data)
  toc()
  # Getting river points for the sub-100-town rivers: 3012.93 sec elapsed
  # 
  
  # parLapply(cl, main_rivers_singletons, get_dhs_rivers_points_glow)
  # parLapply(cl, main_rivers_dyads, get_dhs_rivers_points_glow)
  # parLapply(cl, main_rivers_triads, get_dhs_rivers_points_glow)
  
  
  
  gc()
  toc()
  
  stopCluster(cl)
  
  


    
