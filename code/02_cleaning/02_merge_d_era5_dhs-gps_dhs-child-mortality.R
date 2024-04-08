# _______________________________#
# Environment
# Merge 02: Merge (ERA5+DHS GPS) and (DHS GPS + DHS BR child mortality)
# 
# Stallman
# Started 2023-10-11
# Last edited: 
#________________________________#



# Startup

rm(list = ls())


# bring in the packages, folders, paths
  
  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

# packages ----

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    tictoc, #measuring time to run operations
    countrycode, # for translating between country names
    rdhs, # for dealing with DHS in R
    sf, # vector operations
    terra, # raster operations
    data.table, # fast data manipulation
    lubridate, # for dates
    zoo # time series stuff, easy calc of rolling averages
    
  )

# parameters ----
# 
  min_time      <- "1940-01-01"
  max_time      <- "2023-09-01"
  current_file  <- "total_precipitation"
  #func           <- "weighted_sum"
  #weights       <- "area"
  level         <- 2
  levels        <- 1:5
  time_interval <- "months"
  layer_substrings  <- c("tp_expver=1")
  #vector_sf_path         <- file.path(data_external_clean,"GADM","global")
  my_function <- "mean"
  my_weights  <- NULL


# merge ERA5 + DHS GPS + DHS Child mortality
# bring in ERA5+DHSGPS
  in_path     <- file.path(data_external_clean,"merged","DHS_ERA5","annual")
  in_filename <- "africa_dhs_gps_era5_annual_with_lr_vars.rds"
  era5_dhsgps <- readRDS(file.path(in_path,in_filename)) 
  
# bring in DHS Child mortality

  dhs_childmort <- readRDS(file= file.path(data_external_clean,"merged",
                                           paste0("Africa_all_years_DHS_HH_infant_mortality_with_GPS.rds")))
  
  dhs_childmort <- dhs_childmort %>%
    select(-c(vector_sf_id,vector_cast_id))

  names(dhs_childmort)
  
  names(era5_dhsgps)


## some cleanup ----
# restrict years of the era5 to those which exist in the DHS

years_to_keep <- dhs_childmort$year %>% unique()

era5_dhsgps <- era5_dhsgps %>%
  filter(year %in% years_to_keep) 

#test <- era5_dhsgps[26,]

joined_df <- inner_join(era5_dhsgps,dhs_childmort,
                        by = c("DHSID","year","DHSYEAR","DHSCC","DHSCLUST","CCFIPS","ADM1FIPS","ADM1FIPSNA","ADM1SALBNA","ADM1SALBCO",
                               "ADM1DHS","ADM1NAME","DHSREGCO","DHSREGNA","SOURCE","URBAN_RURA","LATNUM","LONGNUM","ALT_GPS","ALT_DEM",
                               "DATUM", "geometry"))

test <- joined_df %>% sample_n(size = 100)

# save 

  out_path    <- file.path(data_external_clean,"merged","DHS_ERA5","all-countries")
  
  
  saveRDS(joined_df,file.path(data_external_clean,"merged",paste0("Africa_all_years_DHS_HH_infant_mortality_GPS_ERA5.rds")))

# merge in river points ----

  df <- joined_df %>% st_drop_geometry() %>% as.data.table()
  
  system.time(
  df <- readRDS(file.path(data_external_clean,"merged",paste0("Africa_all_years_DHS_HH_infant_mortality_GPS_ERA5.rds"))) %>% st_drop_geometry() %>%
    as.data.table()
  )
  # user  system elapsed 
  # 7.58    0.24   36.03

   
  river_points_path        <- file.path(data_external_temp,"merged","DHS_GLOW_HydroSHEDS","river-points")
  river_points_files <- list.files(river_points_path)
  
  file <- river_points_files[1]

  
  river_points_df <- readRDS(file =file.path(river_points_path,file)) %>% st_drop_geometry() %>% 
    select(-my_linestring,-closest_point,-snapped_point_cond) %>% as.data.table() %>%
    mutate(has_dam  = as.integer(stringr::str_detect(type,"Dam")),
           has_adhi = as.integer(stringr::str_detect(type,"hydrology_station")),
           has_dhs  = as.integer(stringr::str_detect(type,"DHS_town")),
           has_glow = as.integer(stringr::str_detect(type,"GLOW")))
           
  

  # system.time(
  # temp_df <- left_join(df, river_points_df,
  #                      
  #                      by = c("DHSID" = "ID"), keep = TRUE)
  # )
  # 
  
  setkey(df,DHSID)
  
  
  big_df <- df[river_points_df]
  

  tic("Brought together river points with DHS Data")
  for (i in 2:length(river_points_files)) {
    
    file <- river_points_files[i]
    
  river_points_df <- readRDS(file = file.path(river_points_path,file)) %>% st_drop_geometry() %>% 
    select(-my_linestring,-closest_point,-snapped_point_cond) %>% as.data.table() %>%
    mutate(has_dam = as.integer(stringr::str_detect(type,"Dam")),
           has_adhi = as.integer(stringr::str_detect(type,"hydrology_station")),
           has_dhs  = as.integer(stringr::str_detect(type,"DHS_town")),
           has_glow = as.integer(stringr::str_detect(type,"GLOW")))
  
  setkey(river_points_df,ID)
  
  little_df <- df[river_points_df]
  
  big_df <- rbind(big_df,little_df)
  
  if (i %% 10 == 0){
    gc()
    print(paste0("On iteration ",i))
  }
  }
  
  toc()

  # 1471 rivers
  #Brought together river points with DHS Data: 367.43 sec elapsed  
  # big_df has: 1190052 obs

  # big_df for 1-3 towns: has 172655 obs

  # create an ID for unique values of X,Y which are the XY coordinates that the river points use to snap
  # to the river
  
  big_df_dhs <- big_df %>%
                dplyr::filter(type == "DHS_town") %>%
                dplyr::mutate(river_point_id = consecutive_id(X,Y))
  
  big_df_dhs_test <- big_df_dhs %>%
                    group_by(DHSID) %>%
                    slice(1) %>% ungroup()
   
  length(unique(big_df_dhs$river_point_id)) # 11312
  length(unique(big_df_dhs$DHSID)) # 11380
  
  # 1036139 obs; so 1/2 that is actual exposure, around 500k
  saveRDS(big_df_dhs,
          file.path(file.path(data_external_clean,"merged",paste0("DHS_HH_infant_mortality_GPS_ERA5_river_points_under_100_towns.rds")))
  )
  
  
  big_df_dams <- big_df %>%
    dplyr::filter(type == "Dam") %>%
    dplyr::mutate(river_point_id = consecutive_id(X,Y))
  
  length(unique(big_df_dams$river_point_id)) # 2689
  length(unique(big_df_dams$DHSID)) # 2725
  
  saveRDS(big_df_dams,
          file.path(file.path(data_external_clean,"merged",paste0("GDAT_ERA5_river_points_under_100_towns.rds")))
  )
  
  big_df_glow <- big_df %>%
    dplyr::filter(type == "GLOW") %>%
    dplyr::mutate(river_point_id = consecutive_id(X,Y))
  
  length(unique(big_df_glow$river_point_id)) # 150904
  length(unique(big_df_glow$DHSID)) # 150904
  
  saveRDS(big_df_dhs,
          file.path(file.path(data_external_clean,"merged",paste0("DHS_HH_infant_mortality_GPS_ERA5_river_points_under_100_towns.rds")))
  )
  
  
  big_df_adhi <- big_df %>%
    dplyr::filter(type == "hydrology_station") %>%
    dplyr::mutate(river_point_id = consecutive_id(X,Y))
  
  length(unique(big_df_adhi$river_point_id)) # 283
  length(unique(big_df_adhi$DHSID)) # 284
  
  saveRDS(big_df_adhi,
          file.path(file.path(data_external_clean,"merged",paste0("ADHI_ERA5_river_points_under_100_towns.rds")))
  )
  
  
  saveRDS(big_df,file.path(data_external_clean,"merged",paste0("DHS_HH_infant_mortality_GPS_ERA5_dams_adhi_river_points_under_100_towns.rds")))
  
  infant_mort_df <- readRDS(file.path(data_external_clean,"merged",paste0("DHS_HH_infant_mortality_GPS_ERA5_rivers_under_100_towns.rds")))
  
  
  
  
# clean the distances ----

  river_dist_path        <- file.path(data_external_temp,"merged","DHS_GLOW_HydroSHEDS","dyad-distances")
  river_dist_files <- list.files(river_dist_path)
  
  file <- river_dist_files[1]
  
  
  df <- readRDS(file =file.path(river_dist_path,file)) 
  
  tic("Rbinding river dist dfs")
  for (i in 2:length(river_dist_files)) {
    
    file <- river_dist_files[i]
    
    temp_df <- readRDS(file = file.path(river_dist_path,file))
    
    df <- rbind(temp_df,df)
    
    if (i %% 10 == 0){
      gc()
      print(paste0("On iteration ",i))
    }
  }
  toc()
  
  saveRDS(df,file.path(data_external_temp,"merged","DHS_GLOW_HydroSHEDS",paste0("DHS_GLOW_all_river_distances_1_to_3_towns.rds")))
  
  all_distances <- readRDS(file.path(data_external_temp,"merged","DHS_GLOW_HydroSHEDS",paste0("DHS_GLOW_all_river_distances_1_to_3_towns.rds")))
  
  
  # the GLOW IDs have 18 characters; DHS IDs have 14 characters

  towns_distances <- all_distances %>% 
                     filter(nchar(as.character(downstream))==14 | nchar(as.character(upstream))==14) %>%
                     filter(!((nchar(as.character(downstream))==14) & (nchar(as.character(upstream))==14))) %>%
                     filter(distance_m!=0)

  saveRDS(towns_distances,file.path(data_external_temp,"merged","DHS_GLOW_HydroSHEDS",paste0("DHS_GLOW_closest_river_distances_1_to_3_towns.rds")))
  
  towns_distances <- readRDS(file.path(data_external_temp,"merged","DHS_GLOW_HydroSHEDS",paste0("DHS_GLOW_closest_river_distances_1_to_3_towns.rds"))) %>%
                     as.data.table()

  

                           
  tmp <- towns_distances %>%
          filter(downstream == "ZA201700000308" | 
                   upstream == "ZA201700000308")
  
# merge on the dates for river distances ----

  width_path <- file.path(data_external_raw,"GLOW_global-long-term-river-width","Width","width","GLOW_width_region_1.csv")
  
  # read in using data.table
  system.time(
  width_dt <- fread(width_path)
  )

  # create year variable
  system.time(
  width_dt2 <- width_dt[,year := year(date)]
  )
  
  # user  system elapsed 
  # 3.21    0.06    5.60
  
  # 76 million observations 
  # 
  
  # testing grounds
  # test <- width_dt[1:10,]
  # 
  # test2 <- test[, year := year(date)]
  # 
  # test3 <-  test2[, .(mean_width = mean(width)), by=list(ID,year)]
  # 
  
  # summarize by ID and year to get mean annual width
  system.time(
    width_dt2 <- width_dt2[, .(mean_width = mean(width)), by=list(ID,year)]
  )
  

  saveRDS(width_dt2,file.path(data_external_temp,"GLOW_global-long-term-river-width",paste0("GLOW_Africa_annual-widths.rds")))
  
  width_dt <- readRDS(file.path(data_external_temp,"GLOW_global-long-term-river-width",paste0("GLOW_Africa_annual-widths.rds")))
  
  
  test_annual <- width_dt %>%
                filter(ID == "R12100320XS0237227")


  #test_annual <- width_dt[1:100,]
  
  # set that the ID is what we merge on
  setkey(test_annual,ID)
  
  
  

  # first set the key of towns to be downstream, merge on downstream, then we'll switch keys and merge to upstream
  setkey(towns_distances,downstream)
  setkey(width_dt,ID)
  
  # return rows of width_dt with a match in towns_distances
  # and 
  
  
  upstream_towns <- towns_distances %>%
                           filter(nchar(as.character(upstream))==14) %>%
                           mutate(DHSID = upstream)
                    
  
  downstream_towns <- towns_distances %>%
    filter(nchar(as.character(downstream))==14) %>%
    mutate(DHSID = downstream)
  
  test_upstream <- left_join(upstream_towns,width_dt,
                           by = c("downstream"="ID"),
                           keep = TRUE,
                           relationship = "many-to-many") %>%
                    rename(downstream_river_width = ID) %>%
                    group_by(DHSID,downstream)%>%
                    mutate(n_downstream_river_obs = n()) %>%
                    ungroup()

# first take the one with minimum distance, and then if distances are the same 
# take the one which has the most observations present

  test_reduce_upstream <- test_upstream %>%
                          group_by(DHSID) %>%
                          slice_max(order_by = n_downstream_river_obs)
  
  test_downstream <- left_join(downstream_towns,width_dt,
                           by = c("upstream"="ID"),
                           keep = TRUE,
                           relationship = "many-to-many") %>%
                  rename(upstream_river_width = ID)
  
  test_merge3 <- left_join(test_merge2,towns_distances,
                           by = c("ID" = "upstream"))
  
  system.time(
  test_merge <- width_dt[towns_distances,
                         list(downstream = ID,
                              upstream,
                              year,
                              mean_width),]
  
  )
  
  setkey(test_merge,upstream)
  setkey(width_dt, ID)
  
  # return rows of width_dt with a match in the test_merge
  system.time(
    test_merge2 <- width_dt[test_merge,
                           list(downstream = ID,
                                upstream,
                                year,
                                mean_width),
                           allow.cartesian = TRUE]
    
  )

# merge town and annual width ----
# 

  
