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

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    sf,
    tmap,
    countrycode,
    tictoc
  )
  
  path <- file.path(data_external_raw,"GADM","gadm_410-levels","gadm_410-levels.gpkg")
  
  st_layers(path)
  
  # turn off spherical geometry
  # see https://github.com/r-spatial/sf/issues/1902
  sf_use_s2(FALSE)
  
  out_path <- file.path(data_external_clean,"GADM","global")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  levels <- 1:6
  
  # level_gadm <- st_read(dsn = path, 
  #                       layer = "ADM_1")
  #                     
  #level <- 1
  for (level in levels){
  # download the countries
  level_gadm <- st_read(dsn = path,
                        layer = paste0("ADM_",level)) %>%
                st_make_valid()
  
  level_gadm$continent <- countrycode(sourcevar = level_gadm$GID_0,
                              origin = "iso3c",
                              destination = "continent")
  
  #   
  # if (level == 1){
  #   africa <- level_gadm %>%
  #     filter(continent == "Africa") %>%
  #     st_make_valid()%>%
  #     st_union()
  #   
  #   saveRDS(africa,
  #           file = file.path(out_path,paste0("GADM_africa.rds")))
  #   
  #   
  # }
  
  saveRDS(level_gadm,
          file = file.path(out_path,paste0("GADM_global_ADM_",level,".rds")))
          
  rm(level_gadm)
  }
  

