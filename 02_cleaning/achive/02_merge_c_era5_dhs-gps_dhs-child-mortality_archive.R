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


## # merge ERA5 + DHS GPS + DHS Child mortality ----

# use annual for now

## bring in data ----
# bring in ERA5+DHSGPS
  in_path <- file.path(data_external_clean,"merged","DHS_ERA5","all-countries")
  in_filename <- paste0("all_countries_",current_file,"_",min_time,"_to_",max_time,"_GADM_ADM_",level,"_annual.rds")
  
  era5_dhsgps <- readRDS(file.path(in_path,in_filename)) %>%
    ungroup()%>%
    select(-c(vector_sf_id,vector_cast_id,`tp_expver=1_mean`)) # these ids didn't go through right

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
                               "DATUM","GID_0","GID_1","NAME_1","NL_NAME_1","GID_2","NAME_2","VARNAME_2","NL_NAME_2","TYPE_2",
                               "CC_2","ENGTYPE_2","HASC_2","elevation","elev_units","COUNTRY"))

test <- joined_df %>% sample_n(size = 100)

# save 

  out_path    <- file.path(data_external_clean,"merged","DHS_ERA5","all-countries")
  
  
  saveRDS(joined_df,file.path(data_external_clean,"merged",paste0("Africa_all_years_DHS_HH_infant_mortality_GPS_ERA5.rds")))

# merge in river points ----

  system.time(
  df <- readRDS(file.path(data_external_clean,"merged",paste0("Africa_all_years_DHS_HH_infant_mortality_GPS_ERA5.rds"))) %>% st_drop_geometry() %>%
    as.data.table()
  )
  

  # user  system elapsed 
  # 5.86    0.37   41.39
  # 
  # 
  river_points_path        <- file.path(data_external_temp,"merged","DHS_GLOW_HydroSHEDS","river-points")
  river_points_files <- list.files(river_points_path)
  
  file <- river_points_files[1]

  
  river_points_df <- readRDS(file =file.path(river_points_path,file)) %>% st_drop_geometry() %>% 
    select(-my_linestring,-closest_point,-snapped_point_cond) %>% as.data.table()
  

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
    select(-my_linestring,-closest_point,-snapped_point_cond) %>% as.data.table()
  
  setkey(river_points_df,ID)
  
  little_df <- df[river_points_df]
  
  big_df <- rbind(big_df,little_df)
  
  if (i %% 10 == 0){
    gc()
    print(paste0("On iteration ",i))
  }
  }
  
  toc()


 # Brought together river points with DHS Data: 225.1 sec elapsed

  # big_df: has 172655 obs

  saveRDS(big_df,file.path(data_external_clean,"merged",paste0("DHS_HH_infant_mortality_GPS_ERA5_rivers_1_to_3_towns.rds")))
  
