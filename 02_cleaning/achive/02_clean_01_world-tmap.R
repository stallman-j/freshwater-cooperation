# ___________________________#
# Environment
# 02 Clean World Tmap for mapping
# Stallman
# Started 2022-11-14
# Last edited: 
#
# ___________________________#


# clear everything out

rm(list = ls())

# Setup ----

## Parameters ----

## bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","international-agreements")
  
  source(file.path(home_folder,"code","00_master_ia.R"))

# bring in the world data from tmap ----
  
  library(tmap)
  data(World)
  world <- st_transform(World, crs = projection_crs) %>%
    filter(name != "Antarctica", na.rm = TRUE)
  
  saveRDS(world,
          file = file.path(data_clean,"world.rds"))
  
  # remove the original tmap df because clutter
  rm(World)

  class(world)
  # [1] "sf"         "data.frame"
  
  
# visualize quick
  tm_shape(world) +
    tm_polygons(alpha = 0)
  

  
  

