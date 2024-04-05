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

home_folder <- file.path("P:","Projects","environment")

source(file.path(home_folder,"code","00_startup_master.R"))

# required files ----

  source(file.path(code_clean,"02_clean_era5.R"))
  source(file.path(code_clean,"02_clean_choose-DHS-countries.R"))

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
  period_length <- 60
  
# bring in data ----
  
  # to check that the raster rotate works and to run this through with a raw file
  #rast_in_path <- file.path(data_external_raw,"ERA_5",paste0(current_file,".nc"))
  #terra_raster <- terra::rast(x=rast_in_path)
  
  
  rast_in_path <- file.path(data_external_clean,"ERA_5","raster",paste0(current_file,
                                                                        "_monthly_",
                                                                        min_time,"_to_",
                                                                        max_time,".rds"))
  
  tic("Reading in terra raster")
  terra_raster <- terra::rast(x= rast_in_path)
  toc()
  
  # Reading in terra raster: 28.44 sec elapsed

  
  terra::crs(terra_raster) <- "epsg:4326"
  

  source(file.path(code_startup_general,"raster_extract_to_panel.R"))

# set up a loop for DHS datasets
  
  countries_DHS <- readRDS(
    file= file.path(data_external_clean,"DHS","datasets-for-selection",
                    paste0("countries_DHS.rds")))
  
  gps_datasets_all <- readRDS(
    file= file.path(data_external_clean,"DHS","datasets-for-selection",
                    paste0("gps_datasets_all.rds")))
  
  
  #country <- "SN"
  
  #country <- "BJ"
  tic("Got ERA5 + DHS GPS data for all countries")
  for (country in countries_DHS) {
    
    
    continent <- countrycode(country,
                             origin = "dhs",
                             destination = "continent")
    
    countryname <- countrycode(country,
                               origin = "dhs",
                               destination = "country.name")
    
    print(paste0("Country is ",country," ie ", countryname))
    
    if (continent!= "Africa") {
      print("Continent of DHS country not in Africa, deal with it later")
    } else{ # if the continent is africa, move ahead
      
      # get the GPS data first
      gps_datasets   <- dhs_datasets() %>% 
        filter(DatasetType == "GPS Datasets") %>% 
        filter(FileType == "Geographic Data") %>% 
        filter(DHS_CountryCode == country)%>% 
        filter(SurveyType == "DHS") %>%
        arrange(DHS_CountryCode,SurveyYear)
      
      # pick out the years the GPS data exists for 
      years <- unique(gps_datasets$SurveyYear)
      
      # now go through and do the exercise for each year
      
      for (year in years) {

        
        
        gps_dataset <- gps_datasets %>% filter(SurveyYear == year)
        
        
        dhs_gps_filename <- stringr::str_extract(gps_dataset$FileName,"[^.]+")

        path_gps <- file.path(data_external_temp,"DHS","GPS",
                                paste0(dhs_gps_filename,"_GADM_ADM_",level,".rds"))
          
          
          if ( !file.exists(path_gps)){
            
            print("GPS file does not exist")
            
          } else {
            
            GPS_data <- readRDS(file = path_gps) %>% st_transform(crs = "epsg:4326")
            
            

# extract raster data to points
    
  out_df <- raster_extract_to_panel(terra_raster = terra_raster,
                          vector_sf    = GPS_data,
                          cast_vector_sf = FALSE,
                          save_raster_copy = FALSE,
                          raster_out_path = NULL,
                          raster_out_filename = NULL,
                          vector_cast_out_path = file.path(data_external_clean,"GADM","global"),
                          vector_cast_out_filename = paste0("GADM_ADM_",level,"_cast.rds"),
                          layer_substrings = layer_substrings,
                          func = my_function, #"weighted_sum"
                          weights = my_weights, #"area",
                          time_interval = "months",
                          remove_files = FALSE
                          )

# add precipitation variables 
  
  # total precipitation conversions:
  #https://confluence.ecmwf.int/pages/viewpage.action?pageId=197702790
  
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
  

  # annual version
  annual_temp_df <- monthly_temp_df %>%
                    filter(year!=2023) %>% # take out 2023 which is an incomplete year
                    group_by(vector_cast_id,year) %>%
                    filter(row_number()==1) %>%
                    select(-c(month,precip_003m_ra,precip_013m_ra,precip_025m_ra,precip_037m_ra,precip_121m_ra,
                              precip_lr_monthly_avg,precip_lr_monthly_sd,precip_monthly_zscore)) %>% # month is just 1 now, remove
                    ungroup() %>%
                    group_by(vector_cast_id) %>% # group by 
                    mutate(precip_003y_ra = zoo::rollmean(monthly_precip_mm, k=3,   fill = NA, align = "right"), # rolling averages of the k prior months
                           precip_005y_ra = zoo::rollmean(monthly_precip_mm, k=13,  fill = NA, align = "right"),
                           precip_011y_ra = zoo::rollmean(monthly_precip_mm, k=25,  fill = NA, align = "right"))
  
  saveRDS(annual_temp_df,
          file = file.path(out_path,
                           out_filename))
  
  # start rbinding these 
  if (year == years[1]){
    
    annual_df  <- annual_temp_df
    monthly_df <- monthly_temp_df
    
  } else{
    
    annual_df  <- rbind(annual_df,annual_temp_df)
    monthly_df <- rbind(monthly_df,monthly_temp_df)
    
    
    
  }
  
  rm(annual_temp_df,monthly_temp_df)
  gc()
  
          } # end IFELSE statement whether GPS dataset exists

      } # end loop over years
      
} # end ifelse statement if continent of the DHS datset is Africa
    
    
    
    out_path <- file.path(data_external_clean,"merged","DHS_ERA5","country-level","monthly")
    
    if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
    
    out_filename <- paste0(country,"_",current_file,"_",min_time,"_to_",max_time,"_GADM_ADM_",level,"_monthly.rds")
    
    
    saveRDS(monthly_df, file= file.path(out_path,out_filename))
    
    out_path <- file.path(data_external_clean,"merged","DHS_ERA5","country-level","annual")
    
    if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
    
    out_filename <- paste0(country,"_",current_file,"_",min_time,"_to_",max_time,"_GADM_ADM_",level,"_annual.rds")
    
    saveRDS(annual_df,
            file = file.path(out_path,
                             out_filename))
    
    rm(annual_df,monthly_df)
    
    gc()
  } # end loop over all countries DHSs
  toc() # end toc of going through to raster extract for all countries
    
 # Got ERA5 + DHS GPS data for all countries: 1918.46 sec elapsed

  # create merged datasets by country ----
  
  #countries_DHS # all countries
  
  # clear out some space
  rm(terra_raster)
  gc()
  
  time_type <- c("annual","monthly")
  
  time_type <- c("annual")
  
  # WARNING: this gets big in RAM, up to around 20GB for the annual, over 36GB for the monthly
  # final filesize: 140MB for annual; 1.4GB for monthly
  
  tic("Merged countries ERA5 DHS")
  for (time in time_type){
  
  for (country in countries_DHS){
    
    in_path     <- file.path(data_external_clean,"merged","DHS_ERA5","country-level",time)
    in_filename <- paste0(country,"_",current_file,"_",min_time,"_to_",max_time,"_GADM_ADM_",level,"_",time,".rds")
    

    if (country == countries_DHS[1]){
    
      country_dhs_era5  <- readRDS(file = file.path(in_path,in_filename))

    } else{
      
      temp  <- readRDS(file = file.path(in_path,in_filename))

      country_dhs_era5  <- rbind(country_dhs_era5,temp)

      
    } # end ifelse if first country then create the df, otherwise rbind to it

    
  } # end loop over countries
    
    
    out_path    <- file.path(data_external_clean,"merged","DHS_ERA5","all-countries")
    
    out_filename <- paste0("all_countries_",current_file,"_",min_time,"_to_",max_time,"_GADM_ADM_",level,"_",time,".rds")
    if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
    
    
    saveRDS(country_dhs_era5, file = file.path(out_path, out_filename))
    
    rm(country_dhs_era5,temp)
    gc()
  } # end loop over time type

  toc() # Merged countries ERA5 DHS: 82.1 sec elapsed
