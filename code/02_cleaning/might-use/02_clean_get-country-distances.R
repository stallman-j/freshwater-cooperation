# _______________________________#
# International Agreements
# Clean 02: get country distances
# 
# Stallman
# Started 2022-12-16
# Last edited: 
#________________________________#



# Startup

  rm(list = ls())


# bring in the packages, folders, paths
  
  home_folder <- file.path("P:","Projects","international-agreements")
  
  source(file.path(home_folder,"code","00_master_ia.R"))


# get country distances ----

# https://rdrr.io/cran/cshapes/man/distmatrix.html

  library(cshapes)
  
  # 2020 was out of range, 2019 most recent
  # this with min distance is an expensive algorithm b/c polygons are big
  
  # list will be easier to merge because this is larger than needed as a matrix
  
  # current_distmatrix <- distmatrix(date = as.Date("2019-01-01",
  #                           type = "mindist"),
  #                           keep = 0.5,
  #                          useGW = TRUE,
  #                          dependencies = FALSE)
  
  current_distlist   <- distlist(date = as.Date("2019-01-01",
                                                  type = "mindist"),
                                   keep = 0.5,
                                   useGW = TRUE,
                                   dependencies = TRUE)
  
  
  
  
   # saveRDS(current_distmatrix,
   #       file = file.path(data_clean,"distmatrix_2019.rds"))
   # 
   
   path <- file.path(data_clean,"edge-level")
   if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
   
   saveRDS(current_distlist,
           file = file.path(path,"distlist_2019.rds"))
   
   
   
   path <- file.path(data_clean,"edge-level")
   
  distlist_2019 <- readRDS(file = file.path(data_clean,"edge-level","distlist_2019.rds"))
  
  
  
  # merge on country codes 