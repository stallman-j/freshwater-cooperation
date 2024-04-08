# _______________________________#
# Environment
# Merge 02: Extract points with precipitation from points
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

# required files ----

  #source(file.path(code_clean,"02_clean_era5.R"))
  #source(file.path(code_clean,"02_clean_choose-DHS-countries.R"))

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
  min_time      <- "1940-01-01"
  max_time      <- "2023-09-01"
  #func           <- "weighted_sum"
  #weights       <- "area"
  level         <- 2
  levels        <- 1:5
  time_interval <- "months"
  layer_substrings  <- c("tp_expver=1")
  #vector_sf_path         <- file.path(data_external_clean,"GADM","global")
  my_function <- "mean"
  my_weights  <- NULL
  period_length <- 60
  
# bring in data ----
  
  # to check that the raster rotate works and to run this through with a raw file
  #rast_in_path <- file.path(data_external_raw,"ERA_5",paste0(current_file,".nc"))
  #terra_raster <- terra::rast(x=rast_in_path)
  
  # 
  # rast_in_path <- file.path(data_external_clean,"ERA_5","raster",paste0(current_file,
  #                                                                       "_monthly_",
  #                                                                       min_time,"_to_",
  #                                                                       max_time,".rds"))
  
  rast_in_path <- file.path(data_external_clean,"ERA_5","total_precipitation_era5_clean.tif")
  
  tic("Reading in terra raster")
  terra_raster <- terra::rast(x= rast_in_path)
  toc()
  # Reading in terra raster: 28.44 sec elapsed
  
  
  # easy example file
  #rast_in_path <- file.path(data_external_clean,"ERA_5","example_era5_clean.tif")
  
  #terra_raster <- terra::rast(x = rast_in_path)
  

  
  terra::crs(terra_raster) <- "epsg:4326"
  
# set up a loop for DHS datasets
  
  # need to have run the following: 
  # source(file.path(code_folder,"02_cleaning","02_merge_a_all-dhs-gps.R"))
   
  GPS_data <- readRDS(file = file.path(data_external_temp,"DHS","GPS","merged","africa_DHS_GPS.rds"))
  
  tic("Extracted ERA5 to DHS clusters")
  out_df <- raster_extract_to_long_df(terra_raster = terra_raster,
                                        vector_sf = GPS_data,
                                        cast_vector_sf = FALSE,
                                        vector_cast_out_path  = NULL, 
                                        vector_cast_out_filename = NULL, #"vector_cast.rds",
                                        save_raster_copy = FALSE,
                                        raster_out_path = NULL, #getwd(),
                                        raster_out_filename = NULL, #"terra_raster_rotated_and_wgs84.tif",
                                        extracted_out_path = file.path(data_external_clean,"merged","DHS_ERA5"), #getwd(),
                                        extracted_out_filename = "era5_monthly_precip_extracted_to_DHS.rds",
                                        layer_substrings = "tp_expver=1",
                                        long_df_colname  = "precip",
                                        layer_names_vec = NULL,
                                        layer_names_title = "date",
                                        func = "mean", #"weighted_sum" if polygons
                                        weights = NULL, #"area" if polygons
                                        #time_interval = "months",
                                        remove_files = FALSE
  )
  
  toc()
  
  # the thing that took a long time is saving as a RDS since there are 71 million obs (71k towns x 1005 months)
  # Extracted ERA5 to DHS clusters: 132.52 sec elapsed

  # small version
  # let's get 2 DHSIds
  # 
  test_out_df <- out_df %>% filter(DHSID == "AO200600000001" | DHSID == "AO200600000002")
  
  
# add precipitation variables 
  
  # total precipitation conversions:
  #https://confluence.ecmwf.int/pages/viewpage.action?pageId=197702790

  tic("Collapsed monthly DHS-ERA5 to annual")
   annual_dhs_era5 <- collapse_panel_to_annual(in_df = out_df,
                           value_varname_old = "precip_mean",
                           value_varname_new = "precip",
                           id_varname = "DHSID",
                           rolling_average_years = c(3,5,11),
                           out_path = file.path(data_external_temp,"merged","DHS_ERA5","annual"),
                           out_filename = "africa_dhs_gps_era5_annual.rds",
                           merge_df = GPS_data)
                            
  toc()
  # Collapsed monthly DHS-ERA5 to annual: 448.49 sec elapsed
  
  # create a test df
  test_df <- annual_dhs_era5 %>% filter(DHSID == "AO200600000001" | DHSID == "AO200600000022") 
  
  panel_df_out <- create_long_run_vars(panel_df = annual_dhs_era5,
                       id_varname = "DHSID",
                       time_varname = "year",
                       variable_to_manipulate = "precip_annual_mean",
                       variable_namestub      = "precip",
                       out_path = file.path(data_external_clean,"merged","DHS_ERA5","annual"),
                       out_filename = "africa_dhs_gps_era5_annual_with_lr_vars.rds")
  
  
  
# Create a monthly df ----
# TO DO: fix this up if you want monthly, turn it into a function
  monthly_temp_df <- out_df %>%
    filter(!is.na(date)) %>% # take out some missing vals
    group_by(date) %>% # arrange chronologically
    arrange(vector_cast_id) %>% # sorted now by vector_cast_id and chronoligcally within
    ungroup() %>%
    mutate(precip = `tp_expver=1_mean`,
           n_days_in_month = days_in_month(date),
           month = month(date),
           year  = year(date),
           dhs_gps_filename = dhs_gps_filename,
           monthly_precip_mm = precip*n_days_in_month*1000,
           ) %>%
    filter(!is.na(precip)) %>% # take out if the precip value is actually just NA
    group_by(vector_cast_id) %>% # group by 
    mutate(precip_003m_ra = zoo::rollmean(monthly_precip_mm, k=3,   fill = NA, align = "right"), # rolling averages of the k prior months
           precip_013m_ra = zoo::rollmean(monthly_precip_mm, k=13,  fill = NA, align = "right"),
           precip_025m_ra = zoo::rollmean(monthly_precip_mm, k=25,  fill = NA, align = "right"),
           precip_037m_ra = zoo::rollmean(monthly_precip_mm, k=37,  fill = NA, align = "right"),
           precip_121m_ra = zoo::rollmean(monthly_precip_mm, k=121, fill = NA, align = "right")) %>%
    group_by(vector_cast_id,month) %>% # generate monthly precip stats
    mutate(precip_lr_monthly_avg = mean(monthly_precip_mm),
           precip_lr_monthly_sd  = sd(monthly_precip_mm),
           precip_monthly_zscore = (monthly_precip_mm - mean(monthly_precip_mm))/sd(monthly_precip_mm)) %>%
    ungroup() %>%
    group_by(vector_cast_id,year) %>% # generate current
    mutate(precip_current_annual_avg_mm_month  = mean(monthly_precip_mm),
           precip_current_annual_sd_mm_month   = sd(monthly_precip_mm),
           precip_annual_zscore_mm_month       = (monthly_precip_mm - mean(monthly_precip_mm))/sd(monthly_precip_mm)) %>% 
    ungroup() %>%
    group_by(vector_cast_id) %>% # generate long-run averages
    mutate(precip_lr_sd = sd(monthly_precip_mm),
           precip_lr_mean = mean(monthly_precip_mm),
           precip_lr_zscore  = (monthly_precip_mm - mean(monthly_precip_mm))/sd(monthly_precip_mm),
           precip_lr_sd_deviation = precip_current_annual_sd_mm_month - sd(monthly_precip_mm)) 
  
  out_path <- file.path(data_external_clean,"merged","DHS_ERA5","survey-level","monthly")
  
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  out_filename <- paste0(country,"_",current_file,"_",dhs_gps_filename,"_",min_time,"_to_",max_time,"_GADM_ADM_",level,"_monthly.rds")
  
  
  saveRDS(monthly_temp_df, file= file.path(out_path,out_filename))
  
  
  
  out_path <- file.path(data_external_clean,"merged","DHS_ERA5","survey-level","annual")
  
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  out_filename <- paste0(country,"_",current_file,"_",dhs_gps_filename,"_",min_time,"_to_",max_time,"_GADM_ADM_",level,"_annual.rds")
  
  
