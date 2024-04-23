# _______________________________#
# Environment
# download 01: download datasets and extract from their zip files
# 
# Stallman
# Started: 2023-04-13
# Last edited: 
#________________________________#


# Startup

  rm(list = ls())

  # Only run this the first time, otherwise start from "cleaning!"
  
# bring in the packages, folders, paths ----
  
  code_folder <- file.path("P:","Projects","freshwater-cooperation","code")
  source(file.path(code_folder,"00_startup_master.R"))
  
   
# DHS Data ----
   
   # Walkthrough 
   # https://docs.ropensci.org/rdhs/articles/introduction.html
   
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
     httr,
     stringr, # string operations
     countrycode, # country naming conversions
     rdhs, # getting DHS data
     tictoc # timing # more ability to customize to output to latex. use with kableExtra to output tables
     # to console, latex, Rmarkdown, html etc.
  )
  
   
   paths <- c(file.path(data_temp,"DHS","cache"),
              file.path(data_raw,"DHS"),
              file.path(data_clean,"DHS"))
   
   for (path in paths){
   if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
   }

   set_rdhs_config(email = "jillian.stallman@yale.edu",
                    project = "Weather and Mortality",
                    cache_path = file.path(data_temp,"DHS","Weather and Mortality","cache"),
                    config_path = "rdhs.json",
                    verbose_download = TRUE,
                    verbose_setup    = TRUE,
                    password_prompt  = FALSE,
                    global = FALSE # so that we have specific directories
                    )
   
   sc <- dhs_survey_characteristics()
   
   
   system.time(
   datasets_available <- get_available_datasets(clear_cache = FALSE)
   )
   
   # this took:
   
   # show country IDs
   # see countries available
    countries_crosswalk <- dhs_countries(returnFields = c("CountryName","DHS_CountryCode"))
   # let's take AO angola
    
    
   all_datasets <- dhs_datasets()
   
   gps_datasets     <- all_datasets %>% filter(DatasetType == "GPS Datasets")
   flat_datasets    <- dhs_datasets(fileFormat = "SV")
   
   ## TO DO: limit the flat datasets so that we're not getting like HIV datasets
   
   desired_datasets <- rbind(gps_datasets,flat_datasets)
   # Storage.
   flag <- integer()
   

   # for GPS datasets
   for (i in 1:nrow(desired_datasets)){
   tryCatch(
     {
     download_paths <- get_datasets(dataset_filenames =  desired_datasets$FileName[i],
                                  download_option = "rds",
                                  reformat = FALSE,
                                  all_lower = TRUE,
                                  output_dir_root = file.path(data_external_raw,"DHS"),
                                  clear_cache = FALSE)
    
     },
     # Handle the errors.
     error = function(err) {
       message('On iteration',i,'there was an error: ',err)
       flag <<- c(flag, i)
     }
   )

   }
    print(flag) 
    
    
    
    #saveRDS(flag, file = file.path(data_external_raw,"DHS",""))
   
   
  
