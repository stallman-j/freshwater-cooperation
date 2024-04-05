# _______________________________#
# Environment
# Clean 02: Clean GADM country shapefiles
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
# 
 if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
   sf, # spatial files
   archive # unzipping files
  )

# this takes a very long time!
  unzip_files <- list.files(file.path(data_external_raw,"MERIT_Hydro"))
  
  for (path in unzip_files)  archive::archive_extract(file.path(data_external_raw,"MERIT_Hydro",path),
                                                      dir = file.path(data_external_raw,"MERIT_Hydro","tifs"))
  
  

  path <- file.path(data_external_raw,"MERIT_Hydro","tifs","dir_n00e000","n00e005_dir.tif")
  
  merit_test <- st_read(dsn = path)
  
  st_layers(path)
  
  # turn off spherical geometry
  # see https://github.com/r-spatial/sf/issues/1902
  sf_use_s2(FALSE)
  
  out_path <- file.path(data_external_clean,"GADM","global")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  levels <- 1:6
  
  level_gadm <- st_read(dsn = path, 
                        layer = "ADM_6")
  #level <- 1
  for (level in levels){
  # download the countries
  level_gadm <- st_read(dsn = path,
                        layer = paste0("ADM_",level)) %>%
                st_make_valid()
    
  
  saveRDS(level_gadm,
          file = file.path(out_path,paste0("GADM_global_ADM_",level,".rds")))
          
  rm(level_gadm)
  }
