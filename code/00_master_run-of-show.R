# _______________________________#
# International Agreements
# Clean 02: get country centroids
# 
# Stallman
# Started 2022-12-16
# Last edited: 
#________________________________#



# Startup

rm(list = ls())


# bring in the packages, folders, paths
  
  home_folder <- file.path("P:","Projects","environment")
  
  # packages and start-up functions
  source(file.path(home_folder,"code","00_startup_master.R"))
  

# current run of show ----
  
  ## 01 download ----
  source(file.path(code_download,"01_download_datasets_in_use.R"))
  source(file.path(code_download,"01_download_dhs.R"))
  
  ## 02 cleaning ----
  
  source(file.path(code_clean,"02_clean_GADM.R"))
  source(file.path(code_clean,"02_clean_era5.R"))
  source(file.path(code_clean,"02_merge_dhs_gps.R")) # need this before running the child mortality
  source(file.path(code_clean,"02_clean_dhs_child-mortality.R"))
  source(file.path(code_clean,"02_merge_era5_gadm.R"))
  source(file.path(code_clean,"02_clean_era5.R"))
  

# _______________________________#
# Turning on scripts ----
# 1 means "on," anything else is "don't run"
# _______________________________#
  
  # 01 download
  download_datasets_in_use                                 <-    1
  
  # 02 cleaning
  
  merge_tfdd_cruts                                         <-    0
  
  # 03 analysis

# _______________________________#
# Running Files  ----
# _______________________________#
  
  ## 01 download ----
  
  if (download_datasets_in_use==1){
    source(file.path(code_download,"01_download_datasets_in_use.R"))
  }
  
  
  ## 02 cleaning ----
  
  if (clean_rnaturalearth_country_continents==1){
    source(file.path(code_clean,"02_clean_01_rnaturalearth_country-continent-shapefiles.R"))
  }
  

  
  