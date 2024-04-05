# _______________________________#
# Environment
# Clean 02: Clean Elevation Map
# 
# Stallman
# Started 2023-04-04
# Last edited: 
#________________________________#



# Startup

rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

# read in data ----

  if (!require(elevatr)) install.packages("elevatr") #for Digital Elevation Modelling
  library(elevatr)
  
  #https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html
  
  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  china <- world[world$name == "China",]

  elevation_china_zoom_07 <- get_elev_raster(china, z = 7)  
  elevation_china_zoom_04 <- get_elev_raster(china, z = 4)  
  
  # z = 12 is 82 Gigs
  # using z=7
  
  elevation_china_terra_04 <- terra::rast(elevation_china_zoom_04)
  elevation_china_terra_07 <- terra::rast(elevation_china_zoom_07)
  
  saveRDS(elevation_china_terra_07,
          file = file.path(data_clean,"shape-files","elevation_china_zoom_07.rds"))
  
  saveRDS(elevation_china_terra_04,
          file = file.path(data_clean,"shape-files","elevation_china_zoom_04.rds"))
  
  elevation_china <- readRDS(file = file.path(data_clean,"shape-files","elevation_china_zoom_07.rds"))
  
  elevation_china <- readRDS(file = file.path(data_clean,"shape-files","elevation_china_zoom_04.rds"))
  
  
   plot(elevation_china)
  
  # see 
  #https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution
  # for zoom descriptions
  
  
