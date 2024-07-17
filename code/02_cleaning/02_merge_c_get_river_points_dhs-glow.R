# _______________________________#
# Environment
# Merge 02 C: Get River Points for XY Coordinates
# 
# Stallman
# Started 2023-10-11
# Last edited: 2024-05-31
#________________________________#


# need 02_merge_a_era5_dhs


# Startup

  rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","freshwater-cooperation")
  
  source(file.path(home_folder,"code","00_startup_master.R"))
  source(file.path(code_startup_project_specific,"get_river_points.R")) # function for getting along-river points
# 02_merge_a_era5_dhs

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
  n_cores  <- 12 # 16 put 100% CPU; 14 was 93-97% CPU, just about right, 12 
  
  
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

  main_rivs <- hydro_rivers %>% sf::st_drop_geometry() %>%
              select(MAIN_RIV) %>% unique() %>%
              #dplyr::sample_n(size = 4) %>%
              as.vector()%>% .[[1]]
  
  # some rivers with just 1 town on them to try
  # [1] "11327713" "10539521" "11113306" "11317581" "11458329"
  # [6] "11232185" "10037612" "10065667" "11429171" "10301817"
  # [11] "11157421" "11301586" "11516789" "10541316" "10355346"
  # [16] "11349773" "10560899" "11417243" "11456457" "10736117"
  # # 10877433
  
  system.time(
  get_river_points_safe(main_river = 10877433,
                   points_data = GPS_data
                   )
  )
  
  system.time(
    get_river_points_safe(main_river = 11113306,
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
  
  
  tic(paste0("Exporting hydro_rivers and gadm_data and GPS_data to ",n_cores," cores."))
  clusterExport(cl, c("hydro_rivers","gadm_data")) # this is a big export b/c the hydro_rivers is huge
  clusterExport(cl, c("GPS_data"))
  toc()
  
  tic("Getting river points")
  parLapply(cl, main_rivs, get_river_points_safe, points_data = GPS_data)
  toc()
  # haven't checked how long this'll take with all rivers

  
  gc()
  toc()
  
  stopCluster(cl)
  
# get a data frame of all the towns and measurement counts 

  path <- file.path(data_external_temp,"merged","DHS_GLOW_HydroSHEDS","river-points")
  
  
  main_rivers_list <- stringr::str_remove(list.files(path),"DHS_GLOW_MAIN_RIV_") %>%
    stringr::str_remove("_river_points.rds")
  
  
  
  for (main_river in main_rivers_list){
    
    
    # bring in river network
    points_filename_string <- paste0(points_leading_string,main_river,"_river_points.rds")
    
    current_river_network <- readRDS(file = file.path(river_network_path,paste0("MAIN_RIV_",main_river,"_cleaned_river_network.rds")))
    
    current_points        <- readRDS(file = file.path(points_data_path,points_filename_string))
    towns_points <- current_points %>%
      dplyr::filter(type == "DHS_town")
    
    measurement_points <- current_points %>%
      dplyr::filter(type == "GLOW")
    
    
    # print(paste0("In main_river ", main_river, "there are ",nrow(towns_points)," towns and ",nrow(measurement_points)," measurements."))
    
    temp_df <- data.frame(MAIN_RIV = main_river,
                          n_towns = nrow(towns_points),
                          n_measurements = nrow(measurement_points))
    
    if (main_river == main_rivers_list[1]){
      df <- temp_df
    } else {
      df <- rbind(df,temp_df)
    }
    
    saveRDS(df, file = file.path(data_external_temp, "merged","DHS_GLOW_HydroSHEDS","towns_measurement_counts.rds"))
  } # end ifelse
  


    
