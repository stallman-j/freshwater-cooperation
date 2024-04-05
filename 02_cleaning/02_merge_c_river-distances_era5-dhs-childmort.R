# _______________________________#
# Environment
# Merge 02: Extract points with precipitation from points
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
    zoo # time series stuff, easy calc of rolling averages
  )

# parameters ----
# 
  equal_area_crs   <- "ESRI:102022"
  level <- 1
  
# bring in some datasets ----
  
  data <- readRDS(file= file.path(data_external_clean,"merged",
                                  paste0("Africa_all_years_DHS_HH_infant_mortality_with_GPS.rds")))
  
  
  
  
  countries_DHS <- readRDS(
    file= file.path(data_external_clean,"DHS","datasets-for-selection",
                    paste0("countries_DHS.rds")))
  
  gps_datasets_all <- readRDS(
    file= file.path(data_external_clean,"DHS","datasets-for-selection",
                    paste0("gps_datasets_all.rds")))

  path <- file.path(data_external_raw,"HydroSHEDS","HydroRIVERS_v10_af.gdb","HydroRIVERS_v10_af.gdb")
  
  system.time(
    hydro_rivers <- st_read(dsn = path) %>% 
      st_transform(crs = equal_area_crs) %>% 
      rename(geometry = Shape) # need this to use riverdist package
  )
  
  gadm_in_path <- file.path(data_external_clean,"GADM","global")
  
  gadm_in_filename <- paste0("GADM_global_ADM_",level,".rds")
  tic("Bringing in GADM data")
  gadm_data  <- readRDS(file.path(gadm_in_path,gadm_in_filename)) %>%
    st_transform(crs = equal_area_crs)
  
  toc()
  gadm_data$continent <- countrycode(gadm_data$GID_0,
                                     origin = "iso3c",
                                     destination = "continent")
  
  dhs_data <- readRDS(file = file.path(data_external_temp,"DHS","GPS","merged","all_Africa_DHS_GPS.rds")) %>%
    st_transform(crs = equal_area_crs)
  
  
# bring in data ----

  era5_gps_childmort <- readRDS(file.path(data_external_clean,"merged",paste0("Africa_all_years_DHS_HH_infant_mortality_GPS_ERA5.rds")))

  
  #era5_gps_childmort_sen <- era5_gps_childmort %>% filter(DHSCC=="SN")
  
  names(era5_gps_childmort)

# choose countries or rivers to include ----
  
  # use iso3c for this
  countries_iso3c <- c("SEN")
  
  countrynames <- countrycode(countries_iso3c,
                             origin = "iso3c",
                             destination = "country.name")
  
  dhs_countrycodes <- countrycode(countries_iso3c,
                             origin = "iso3c",
                             destination = "dhs")
  
  
  countries_DHS_iso3c <- countrycode(countries_DHS,
                                     origin = "dhs",
                                     destination = "iso3c")
  
  # get the folder where the dyad distances are located
  # this is the
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
      
      
      if (number_of_towns>2) { # ignoring big rivers
      
        rm(temp_data)
        gc() 
        
      }  else {
      distance_data  <- rbind(distance_data,temp_data)
      }
      
      
      
    } # end ifelse to get the first df to be setting things up
    
  } # end loop over filenames
  
  toc()
  
  out_path <- file.path(data_external_clean,"merged","DHS_ERA5_HydroSHEDS","annual")
  
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  
  singletons <- distance_data %>%
                filter(n_towns == 1) %>%
                left_join(era5_gps_childmort,
                          by = c("downstream" = "DHSID")) %>%
                filter(!is.na(precip_current_annual_avg_mm_month))
  
  
  
  
  
  
  
  get_dhs_rivers_points <- function(main_river,
                                      data_clean_path = file.path("E:","data","03_clean"),
                                      data_temp_path  = file.path("E:","data","02_temp"),
                                      river_network_path = file.path("E:","data","03_clean","HydroSHEDS","river_networks"),
                                      hydro_rivers_units_path = file.path("E:","data","03_clean","HydroSHEDS","shape-files"),
                                      gadm_union_shapefile_path = file.path("E:","data","03_clean","shape-files","GADM"),
                                      max_distance_to_snap = 10000,
                                      equal_area_crs = "ESRI:102022"
  ){
    
    tryCatch( {
      # create output folders
      checked_river_path         <- file.path(data_temp_path,"merged","DHS_HydroSHEDS","checked-main-rivers","dhs-river-points")
      river_network_missing_path <- file.path(data_temp_path,"HydroSHEDS","river-network-missing")
      river_points_path        <- file.path(data_temp_path,"merged","DHS_HydroSHEDS","river-points")
      hydro_rivers_units_missing_path <- file.path(data_temp_path,"HydroSHEDS","hydro_rivers_units_missing")
      
      paths_to_create <- c(checked_river_path,
                           river_network_missing_path,
                           river_points_path,
                           gadm_union_shapefile_path
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
        
        stop(paste0("DHS cluster distances for main_river ",main_river," have already been calculated. No need to go through this again."))
        
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
        filter(DHSCC %in% countries_DHS)
      
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
      
      
      
      # get closest points of the DHS to the rivers
      
      # this is a fast operation
      nearest_indices <- st_nearest_feature(dhs_data,hydro_rivers_units)
      
      # this is also fast
      closest_points <- dhs_data %>%
        mutate(my_linestring = st_nearest_points(dhs_data,hydro_rivers_units[nearest_indices,], pairwise = TRUE),
               closest_point = st_cast(my_linestring, 'POINT')[seq(2, nrow(.)*2, 2)],
               distance_to_river      = st_distance(dhs_data, hydro_rivers_units[nearest_indices,], by_element = TRUE),
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
        
        stop(paste0("No points in DHS data match with MAIN_RIV ",main_river))
        
        
      }
      
      saveRDS(object = current_points,
              file =file.path(river_points_path,paste0("DHS_MAIN_RIV_",main_river,"_river_points.rds")))
      
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
  
  
  get_dhs_rivers_points(main_river = 10004825)
  
  # Looped over and rbinded all data in: 1.29 sec elapsed
  # Looped over and rbinded all data in: 50.42 sec elapsed
  
  dyad_data_upstream <- left_join(distance_data, era5_gps_childmort,
                                  by = c("upstream" = "DHSID"),
                                  keep = TRUE)
  
  
  # filter years that don't actually exist, and create variables 
  dyad_data_updown <- left_join(dyad_data_upstream, era5_gps_childmort,
                                by = c("downstream" = "DHSID", "year" = "year"),
                                suffix = c("_u","_d")) %>%
    filter(!is.na(precip_current_annual_avg_mm_month_u) & !is.na(precip_current_annual_avg_mm_month_d)) # take out the NAs from common distance but not common years
    
    
    
    mutate(R_U10MR_ud = R_U10MR_u/R_U10MR_d,
                              R_U5MR_ud = R_U5MR_u/R_U5MR_d,
                              R_IMR_ud   = R_IMR_u/R_IMR_d,
                              R_NNMR_ud  = R_NNMR_u/R_NNMR_d,
                              urban_u_rural_d = ifelse((URBAN_RURA_u == "U" & URBAN_RURA_d == "R"), yes = 1, no = 0 ),
                              urban_u_urban_d = ifelse((URBAN_RURA_u == "U" & URBAN_RURA_d == "U"), yes = 1, no = 0 ),
                              rural_u_rural_d = ifelse((URBAN_RURA_u == "R" & URBAN_RURA_d == "R"), yes = 1, no = 0 ),
                              rural_u_urban_d = ifelse((URBAN_RURA_u == "R" & URBAN_RURA_d == "U"), yes = 1, no = 0 ),
                              cross_country   = ifelse((DHSCC_u != DHSCC_d), yes = 1, no = 0),
                              cross_adm_1     = ifelse((GID_1_u != GID_1_d), yes = 1, no = 0),
                              cross_adm_2     = ifelse((GID_2_u != GID_2_d), yes = 1, no = 0)
                              ) 
  

  
  
  dyad_data_updown_cut <- dyad_data_updown %>%
    filter(WN_NNMR_u >= 15 & WN_NNMR_d >=15 & 
             WN_IMR_u >=15 & WN_IMR_d >=15) %>%
    filter(R_NNMR_u > quantile(R_NNMR_u, .5,na.rm = TRUE) & R_NNMR_u < quantile(R_NNMR_u, .90,na.rm = TRUE) &
           R_NNMR_d > quantile(R_NNMR_d, .5,na.rm = TRUE) & R_NNMR_d < quantile(R_NNMR_d, .90,na.rm = TRUE) &
           R_IMR_u > quantile(R_IMR_u, .5,na.rm = TRUE) & R_IMR_u < quantile(R_IMR_u, .90,na.rm = TRUE) &
             R_IMR_d > quantile(R_IMR_d, .5,na.rm = TRUE) & R_IMR_d < quantile(R_IMR_d, .90,na.rm = TRUE))

  
  out_path <- file.path(data_external_clean,"merged","DHS_GPS_childmort_ERA5","country-level","annual")
  
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  if (i==1){
  
  saveRDS(dyad_data_updown,
          file = file.path(out_path,paste0(paste(countries,collapse = "|"),"_",period_length,"_month_window_child_mortality_GPS_ERA5_annual.rds")))
  
  
  
  saveRDS(dyad_data_updown_cut,
          file = file.path(out_path,paste0(paste(countries,collapse = "|"),"_",period_length,"_month_window_child_mortality_GPS_ERA5_annual_cut.rds")))
  
  } else {
    
    saveRDS(dyad_data_updown,
            file = file.path(out_path,paste0("Africa_all_countries_",period_length,"_month_window_child_mortality_GPS_ERA5_annual.rds")))
    
    
    
    saveRDS(dyad_data_updown_cut,
            file = file.path(out_path,paste0("Africa_all_countries_",period_length,"_month_window_child_mortality_GPS_ERA5_annual_cut.rds")))
    
    
  } # end ifelse over how to name
  
