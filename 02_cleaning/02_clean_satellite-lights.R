# _______________________________#
# Environment
# Clean 02: Clean Satellite Lights Extract from Rasters to Points
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
  parallel, # for parallelization
  stringr, # for string operatioons
  terra, # handle raster data
  exactextractr, # fast extractions
  sf, # vector data operations
  dplyr, # data wrangling
  R.utils, # to upnzip .gz
  archive,
  lubridate # date operations
)




# bring in data ----

# https://berenger.baospace.com/nightlights-satellite-data-free-download/

  points_to_extract_to <- readRDS("INSERT FILENAME OF DATA POINTS ONCE SNIPPED TO HYDRORIVERS SEGMENTS")

# CREATE raster stack


  current_file <- "total_precipitation"
  
  path <- file.path(data_external_raw,"ERA_5",paste0(current_file,".nc"))
  
  #era <- st_read(dns = path)
  
  era <- terra::rast(x = path)
  
  era
  
  # class       : SpatRaster 
  # dimensions  : 721, 1440, 2008  (nrow, ncol, nlyr)
  # resolution  : 0.25, 0.25  (x, y)
  # extent      : -0.125, 359.875, -90.125, 90.125  (xmin, xmax, ymin, ymax)
  # coord. ref. :  
  #   source      : download.nc 
  # varname     : tp (Total precipitation) 
  # names       : tp_ex~r=1_1, tp_ex~r=5_1, tp_ex~r=1_2, tp_ex~r=5_2, tp_ex~r=1_3, tp_ex~r=5_3, ... 
  # unit        :           m,           m,           m,           m,           m,           m, ... 
  # time        : 1940-01-01 to 2023-08-01 UTC 
  # 
  
  
  # how many layers
  dim(era)
# [1]  721 1440 2008
  
  # gives a string of all the relevant times
  t <- terra::time(era) 
  
  class(t[1])
  # [1] "POSIXct" "POSIXt" 

  
  terra::time(era) %>% head()
  
  # [1] "1940-01-01 UTC" "1940-01-01 UTC" "1940-02-01 UTC" "1940-02-01 UTC"
  # [5] "1940-03-01 UTC" "1940-03-01 UTC"
  
  min(t)
  
  min_time <- str_replace(string = min(t),
                          pattern = " UTC",
                          replacement = "")
  # [1] "1940-01-01 UTC"

  max(t)
  # [1] "2023-08-01 UTC"
  
  max_time <- str_replace(string = max(t),
                          pattern = " UTC",
                          replacement = "")
  
  dates<-  seq(min(t),max(t),by='months')
  
  length(dates) 
  # length 1004
  
  # 
  # https://gis.stackexchange.com/questions/444481/problems-extracting-era5-data-with-exact-extract-in-r
  
  # https://dominicroye.github.io/en/2018/access-to-climate-reanalysis-data-from-r/#packages
 
# subset to get the vars we want
  
  names(era) %>% head()
  # [1] "tp_expver=1_1" "tp_expver=5_1" "tp_expver=1_2" "tp_expver=5_2" "tp_expver=1_3" "tp_expver=5_3"
  
  # # https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation
  
  # "For GRIB, ERA5T data can be identified by the key expver=0005 in the GRIB header. ERA5 data is identified by the key expver=0001."
  # we want ERA5 because ERA5T is the real-time (not yet retroactively fixed) data, so the tp_expver=1_n ones
  
  all_names <- names(era)
  keep_substr <- "tp_expver=1"
  
  keep_names <- stringr::str_detect(all_names, keep_substr)
  
  era_5 <- terra::subset(era, subset = keep_names) 
  
  
  # class       : SpatRaster 
  # dimensions  : 721, 1440, 1004  (nrow, ncol, nlyr)
  # resolution  : 0.25, 0.25  (x, y)
  # extent      : -0.125, 359.875, -90.125, 90.125  (xmin, xmax, ymin, ymax)
  # coord. ref. :  
  #   source      : download.nc 
  # varname     : tp (Total precipitation) 
  # names       : tp_ex~r=1_1, tp_ex~r=1_2, tp_ex~r=1_3, tp_ex~r=1_4, tp_ex~r=1_5, tp_ex~r=1_6, ... 
  # unit        :           m,           m,           m,           m,           m,           m, ... 
  # time        : 1940-01-01 to 2023-08-01 UTC
  # 

  
  # https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-Spatialreferencesystems
  # ERA5 data is referenced in the horizontal with respect to the WGS84 ellipse (which defines the major/minor axes) 
  # and in the vertical it is referenced to the EGM96 geoid over land but over ocean it is referenced to 
  # mean sea level, with the approximation that this is assumed to be coincident with the geoid
  
  crs(era_5) <- "epsg:4326" 

# rotate so that instead of 0 to 360 the longitude is going from -180 to 180
  
 
  system.time(
    era_5 <- terra::rotate(era_5, left = TRUE)
  )
  

  
  out_path <- file.path(data_external_clean,"ERA_5","raster")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  saveRDS(era_5,
          file = file.path(data_external_clean,"ERA_5","raster",paste0(current_file,
                                                                       "_monthly_",
                                                                       min_time,"_to_",
                                                                       max_time,".rds")))
  
# # do some plots ----
  
  
  
  rotate_layer <- function(i,
                           data = era_5){
    out <- rotate(data[[i]], left = TRUE)
  }
  
  # show for a single one
  system.time(
    out_layer <- rotate_layer(layer_name = era_list[[1]])
  )
  
  plot_1 <- ggplot()+
    geom_raster(data = as.data.frame(era_5[[1]], xy=TRUE),
                aes(x = x, y=y, fill = `tp_expver=1_1`))
  
  
  plot_2 <- ggplot()+
    geom_raster(data = as.data.frame(out_layer[[1]], xy=TRUE),
                aes(x = x, y=y, fill = `tp_expver=1_1`))
  
  
  save_map(output_folder = output_maps,
           plotname = plot_1,
           filename = "to_rotate_test.png",
           width = 9,
           height = 5,
           dpi = 300)
  
  save_map(output_folder = output_maps,
           plotname = plot_2,
           filename = "rotated_test.png",
           width = 9,
           height = 5,
           dpi = 300)
  
  
