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

# code to run first ----

# source(file.path(home_folder,"code","02_cleaning","02_clean_dhs_child-mortality_annual.R"))


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
  surveyyear_start <- 2018 %>% as.character()
  surveyyear_end   <- 2020 %>% as.character()
  my_function <- "mean"
  my_weights  <- NULL
  
# data ----
  
  # 1) bring in precipitation and make annual
  # 2) 
  # to check that the raster rotate works and to run this through with a raw file
  #rast_in_path <- file.path(data_external_raw,"ERA_5",paste0(current_file,".nc"))
  #terra_raster <- terra::rast(x=rast_in_path)
  
  dhs_mortality_data <- readRDS(file = file.path(data_external_clean,"merged",
                                                 paste0("Africa_all_years_DHS_HH_infant_mortality_with_GPS.rds"))) %>%
    st_as_sf(crs = 4326) %>%
    st_transform(crs = equal_area_crs)
  
  
  
  rast_in_path <- file.path(data_external_clean,"ERA_5","raster",paste0(current_file,
                                                                        "_monthly_",
                                                                        min_time,"_to_",
                                                                        max_time,".rds"))
  
  tic("Reading in terra raster")
  terra_raster <- terra::rast(x= rast_in_path) 
  toc()
  
  terra::crs(terra_raster) <- "epsg:4326"
  
  #raster_out_filename <- paste0(current_file,"_monthly_",min(time(terra_raster)),"_to_",max(time(terra_raster)),".rds")
  #raster_out_path     <- file.path(data_external_clean,"ERA_5","raster")

  source(file.path(code_startup_general,"raster_extract_to_panel.R"))

# set up a loop for DHS datasets
  
  countries_DHS <-  dhs_datasets() %>% 
    dplyr::filter(DatasetType == "GPS Datasets") %>% 
    dplyr::filter(FileType == "Geographic Data") %>%
    mutate(continent = countrycode(DHS_CountryCode, origin = "dhs",destination = "continent"))%>%
    dplyr::filter(continent == "Africa") %>%
    dplyr::select(DHS_CountryCode) %>% 
    dplyr::filter(DHS_CountryCode!="LB") %>% # something off with Liberia
    dplyr::filter(DHS_CountryCode!="TG") %>% # something off with Togo
    unique() %>%
    .[,1] # get just the character
  

  
  gps_datasets_all   <- dhs_datasets() %>% 
    dplyr::filter(DatasetType == "GPS Datasets") %>% 
    dplyr::filter(FileType == "Geographic Data") 
  
  country <- "SN"
  
  #country <- "BJ"
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
        #filter(SurveyYear >= surveyyear_start & SurveyYear <= surveyyear_end) %>%
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
            
            
    extracted_out_path <- file.path(data_external_temp,"merged","DHS_ERA5")
            
    if (!dir.exists(extracted_out_path)) dir.create(extracted_out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
    
    extracted_out_filename <- paste0(min_time,"_to_",
                                   max_time,"_",dhs_gps_filename,"_panel_monthly_precip_tmp.rds")
  
# extract raster data to points ----
    
  out_df <- raster_extract_to_panel(terra_raster = terra_raster,
                          vector_sf    = GPS_data,
                          cast_vector_sf = FALSE,
                          save_raster_copy = FALSE,
                          raster_out_path = NULL,
                          raster_out_filename = NULL,
                          extracted_out_path  = extracted_out_path,
                          extracted_out_filename = extracted_out_filename,
                          vector_cast_out_path = file.path(data_external_clean,"GADM","global"),
                          vector_cast_out_filename = paste0("GADM_ADM_",level,"_cast.rds"),
                          layer_substrings = layer_substrings,
                          func = my_function, #"weighted_sum"
                          weights = my_weights, #"area",
                          time_interval = "months",
                          remove_files = FALSE
                          )
  
  out_filename <- paste0(min_time,"_to_",max_time,"_GADM_ADM_",level,"_",dhs_gps_filename,"_panel_",
                         current_file,"_monthly_df_with_vars.rds")
  out_path <- file.path(data_external_clean,"merged","DHS_ERA5")
  
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories

# add precipitation variables ----
  
  temp_df <- out_df %>%
    group_by(date) %>% # arrange chronologically
    arrange(vector_cast_id) %>% # sorted now by vector_cast_id and chronoligcally within
    ungroup() %>%
    mutate(precip = `tp_expver=1_mean`/1000000) %>%
    group_by(vector_cast_id) %>% # group by 
    mutate(precip_003m_ra = zoo::rollmean(precip, k=3,   fill = NA, align = "right"), # rolling averages of the k prior months
           precip_013m_ra = zoo::rollmean(precip, k=13,  fill = NA, align = "right"),
           precip_025m_ra = zoo::rollmean(precip, k=25,  fill = NA, align = "right"),
           precip_037m_ra = zoo::rollmean(precip, k=37,  fill = NA, align = "right"),
           precip_121m_ra = zoo::rollmean(precip, k=121, fill = NA, align = "right")) %>%
    group_by(vector_cast_id,month(date)) %>% # generate monthly precip stats
    mutate(precip_lr_monthly_avg = mean(precip),
           precip_lr_monthly_sd  = sd(precip),
           precip_monthly_zscore = (precip - mean(precip))/sd(precip)) %>%
    ungroup() %>%
    group_by(vector_cast_id,year(date)) %>% # generate current
    mutate(precip_current_annual_avg  = mean(precip),
           precip_current_annual_sd   = sd(precip),
           precip_annual_zscore       = (precip - mean(precip))/sd(precip)) %>% 
    ungroup() %>%
    group_by(vector_cast_id) %>% # generate long-run averages
    mutate(precip_lr_sd = sd(precip),
           precip_lr_mean = mean(precip),
           precip_zscore  = (precip - mean(precip))/sd(precip),
           precip_sd_deviation = precip_current_annual_sd - sd(precip)) 
  
  
  saveRDS(temp_df, file= file.path(out_path,out_filename))
  
  out_filename <- paste0(min_time,"_to_",max_time,"_GADM_ADM_",level,"_",dhs_gps_filename,"_panel_",
                         current_file,"_annual_df_with_vars.rds")
  
# create an annual version ----
  
  
  
  rm(temp_df)
  
  gc()
  
  
          } # end IFELSE statement whether GPS dataset exists

      } # end loop over years
} # end ifelse statement if continent of the DHS datset is Africa
    
  } # end loop over all countries DHSs
    

  
