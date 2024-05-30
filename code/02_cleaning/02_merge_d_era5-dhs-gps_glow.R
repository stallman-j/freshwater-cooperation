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
  
  home_folder <- file.path("P:","Projects","freshwater-cooperation")
  
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

# merge ERA5 + DHS GPS + DHS HR + DHS BR
# bring in ERA5+DHSGPS

  era5_dhsgps <- readRDS(file.path(data_external_clean,"merged","DHS_ERA5","annual","africa_dhs_gps_era5_annual_with_lr_vars.rds") )
  
  names(era5_dhsgps)
  # [1] "DHSID"                       "year"                       
  # [3] "precip_annual_mean"          "precip_rolling_avg_3_years" 
  # [5] "precip_rolling_avg_5_years"  "precip_rolling_avg_11_years"
  # [7] "DHSCC"                       "DHSYEAR"                    
  # [9] "DHSCLUST"                    "CCFIPS"                     
  # [11] "ADM1FIPS"                    "ADM1FIPSNA"                 
  # [13] "ADM1SALBNA"                  "ADM1SALBCO"                 
  # [15] "ADM1DHS"                     "ADM1NAME"                   
  # [17] "DHSREGCO"                    "DHSREGNA"                   
  # [19] "SOURCE"                      "URBAN_RURA"                 
  # [21] "LATNUM"                      "LONGNUM"                    
  # [23] "ALT_GPS"                     "ALT_DEM"                    
  # [25] "DATUM"                       "geometry"                   
  # [27] "precip_lr_avg"               "precip_lr_sd"               
  # [29] "precip_lr_zscore"
  
  sum(is.na(era5_dhsgps$DHSID)) # 0
  
## some cleanup ----
# restrict years of the era5 to those which exist in GLOW

  years_to_keep <- 1984:2022
  
  df <- era5_dhsgps %>%
    filter(year %in% years_to_keep) %>%
    as.data.table()

# AO 2006 is for some reason not in the BR data at the moment
 # test_eragps <- df %>% filter(year == 2002 & DHSID == "AO201100000051")

  river_points_path        <- file.path(data_external_temp,"merged","DHS_GLOW_HydroSHEDS","river-points")
  river_points_files <- list.files(river_points_path)
  
  file <- river_points_files[1]

  
  river_points_df <- readRDS(file =file.path(river_points_path,file)) %>% st_drop_geometry() %>% 
    select(-my_linestring,-closest_point,-snapped_point_cond)  %>%
    group_by(MAIN_RIV) %>%
    mutate(Count = n(),
           has_dam  = +("Dam" %in% type),
           has_adhi = +("hydrology_station" %in% type),
           has_dhs  = +("DHS_town" %in% type),
           has_glow = +("GLOW" %in% type)) %>%
    ungroup() %>% as.data.table()
           
  

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
    select(-my_linestring,-closest_point,-snapped_point_cond) %>%
    group_by(MAIN_RIV) %>%
    mutate(Count = n(),
           has_dam  = +("Dam" %in% type),
           has_adhi = +("hydrology_station" %in% type),
           has_dhs  = +("DHS_town" %in% type),
           has_glow = +("GLOW" %in% type)) %>%
    ungroup() %>% as.data.table()
  
  setkey(river_points_df,ID)
  
  little_df <- df[river_points_df]
  
  big_df <- rbind(big_df,little_df)
  
  if (i %% 10 == 0){
    gc()
    print(paste0("On iteration ",i))
  }
  }
  
  toc()

  saveRDS(big_df,file.path(data_external_clean,"merged",paste0("DHS_GPS_ERA5_dams_adhi_river_points.rds")))
  
  nrow(big_df)
  # 597733
  # 
  
  big_df <- readRDS(file.path(data_external_clean,"merged",paste0("DHS_GPS_ERA5_dams_adhi_river_points.rds")))
  
  
  
  length(unique(big_df$MAIN_RIV))
  
  # 1471 unique rivers
  #
# Brought together river points with DHS Data: 109.56 sec elapsed

  # create an ID for unique values of X,Y which are the XY coordinates that the river points use to snap
  # to the river
  
  unique(big_df$type)
  # [1] "DHS_town"          "Dam"               "hydrology_station"
  # [4] "GLOW"
  
  big_df_width_town <- big_df %>%
                       filter(has_dhs == 1 & has_glow == 1)
 
  saveRDS(big_df_width_town,
          file.path(file.path(data_external_clean,"merged",paste0("DHS_GPS_ERA5_river_points_with_GLOW.rds")))
  )
  
  big_df_width_town <- readRDS(file.path(file.path(data_external_clean,"merged",paste0("DHS_GPS_ERA5_river_points_with_GLOW.rds"))))
  
  
  
  
  
  
  
  big_df_width_dam <- big_df %>%
    filter(has_dam == 1 & has_glow == 1)
  
  saveRDS(big_df_width_dam,
          file.path(file.path(data_external_clean,"merged",paste0("GDAT_ERA5_river_points_with_GLOW.rds")))
  )
  
  big_df_width_dam <- readRDS(file.path(file.path(data_external_clean,"merged",paste0("GDAT_ERA5_river_points_with_GLOW.rds"))))
  
  
  
  #river_df <- readRDS(file.path(data_external_clean,"merged",paste0("DHS_GPS_ERA5_rivers_under_100_towns.rds")))
  
  dams_df <- big_df_width_dam %>%
              filter(type != "DHS_town" & type != "hydrology_station")
  
  
# clean the distances ----

  river_dist_path        <- file.path(data_external_temp,"merged","DHS_GLOW_HydroSHEDS","dyad-distances")
  river_dist_files <- list.files(river_dist_path)
  
  file <- river_dist_files[1]
  

  df <- readRDS(file =file.path(river_dist_path,file)) 
  
  tic("Rbinding river dist dfs")
  for (i in 2:length(river_dist_files)) {
    
    #for (i in 2:5) {
      
    file <- river_dist_files[i]
    
    temp_df <- readRDS(file = file.path(river_dist_path,file))
    
    df <- rbind(temp_df,df)
    
    if (i %% 10 == 0){
      gc()
      print(paste0("On iteration ",i))
    }
  }
  toc()
  
  # Rbinding river dist dfs: 462.59 sec elapsed
  # 18 million distance observations
  
  saveRDS(df,file.path(data_external_temp,"merged","DHS_GLOW_HydroSHEDS",paste0("DHS_GLOW_river_distances.rds")))
  
  all_distances <- readRDS(file.path(data_external_temp,"merged","DHS_GLOW_HydroSHEDS",paste0("DHS_GLOW_river_distances.rds")))
  
  
  # the GLOW IDs have 18 characters; DHS IDs have 14 characters

  towns_distances <- all_distances %>% 
                     filter(nchar(as.character(downstream))==14 | nchar(as.character(upstream))==14) %>%
                     filter(!((nchar(as.character(downstream))==14) & (nchar(as.character(upstream))==14))) 
  


  saveRDS(towns_distances,file.path(data_external_temp,"merged","DHS_GLOW_HydroSHEDS",paste0("DHS_GLOW_closest_river_distances.rds")))
  
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
  
  
  test_annual_width <- width_dt %>%
                filter(ID == "R12100320XS0237227")

  
# Generate test ----

  # this is GLOW + DHS 
  test_dhs_df <- big_df %>%
                filter(MAIN_RIV == "11398814") %>%
                filter(type == "DHS_town") %>%
                filter(DHSID == "MD201600000333")

  # distances for this river
  current_distance_index <- stringr::str_detect(river_dist_files,"11398814")
  current_distance_file   <- river_dist_files[current_distance_index]
  
  distance_df <- readRDS(file =file.path(river_dist_path,current_distance_file)) %>%
    filter(downstream == "MD201600000333" | upstream == "MD201600000333") %>%
    #filter(nchar(as.character(upstream))==14 | nchar(as.character(downstream))==14) %>%
    filter(distance_m <20000)
  
  # 193 obs now
  
  # 328k distances calculated on a river with 937 width locations, 4 towns, 0 dams 0 hydro stations
  
  # Towns
  # MD200800000540, MD201600000333, MD201600000535
  
  # went from 1093 obs 
  
  # merge on the river widths
  
  system.time(
  test_dhs_df_merge <- left_join(test_dhs_df,width_dt,
                           by = c("DHSID"="ID"))
  )
  
  # user  system elapsed 
  # 0.09    0.00    0.66
  
  # 16k obs
  
  # set that the ID is what we merge on
  setkey(test_annual,ID)
  
  
  

  # first set the key of towns to be downstream, merge on downstream, then we'll switch keys and merge to upstream
  setkey(towns_distances,downstream)
  setkey(width_dt,ID)
  
  # return rows of width_dt with a match in towns_distances
  # and 
  
  
  upstream_towns <- towns_distances %>%
                           filter(nchar(as.character(upstream))==14 | nchar(as.character(downstream))==14)
  
  %>%
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

  
