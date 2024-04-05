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
  
  source(file.path(home_folder,"code","00_startup_master.R"))
  
  
  
  ## WARNING BIG DATA ## 
  # about 3G per download, all annual data is some 130G, all monthly data is like 1.5TB
  
  if (!require(devtools)) install.packages("devtools")
  devtools::install_github("walshc/nightlights")
  devtools::install_github("JakobMie/nightlightstats")
  
  if (!require(sf)) install.packages("sf") #shapes
  if (!require(sp)) install.packages("sp") #shapes, old but we'll take it, nightlights starts as sp
  
  if (!require(tmap)) install.packages("tmap") #for visual mapping
  
  
  library(sp)
  library(nightlightstats)
  library(sf)
  library(nightlights)
  library(tmap)
  
  
  # Download, extract and load and example shapefile to work with (US counties):
  
  # 09 is CT
  dest_path <- file.path(data_temp,"tl_2015_09_cousub.zip")
  download.file("ftp://ftp2.census.gov/geo/tiger/TIGER2015/COUSUB/tl_2015_09_cousub.zip",
                destfile = dest_path)
  # unzip to a new folder
  unzip(dest_path,
        exdir = file.path(data_temp,"tl_2015_09_cousub"),
        overwrite = TRUE)
  
  
  
  shp = read_sf(file.path(data_temp,"tl_2015_09_cousub"))
  
  plot(shp[,1])
  
  # Download and extract some night lights data to a directory "night-lights":
  
  
  # using nightlightstats
  
  # create the folder
    if (!dir.exists(file.path(data_raw,"nightlightstats_noaa"))) dir.create(file.path(data_raw,"nightlightstats_noaa"), recursive = TRUE) # recursive lets you create any needed subdirectories

  my_light_location <- file.path(data_raw,"nightlightstats_noaa")
  
  # uncomment for the full-on download

  system.time(test <-
  nightlight_download(
    area_names = "world",
    time = 1993:2013,
    light_location = my_light_location,
    shapefile_location = NULL,
    shapefiles = NULL,
    download_shape = ".gpkg",
    gpkg_layer = NULL,
    admlevel = 0,
    user_coordinates = NULL,
    corrected_lights = FALSE,
    harmonized_lights = FALSE
  )
  )
  
  # user  system elapsed 
  # 0.88    0.98   96.04 
  
  
  # plot
  
  system.time(suriname_1992_plot <-
  nightlight_plot(
    area_names = "Suriname",
    time = "1992",
    shapefile_location = data_temp,
    light_location = my_light_location
  )
  )
  
  # see how long just the download takes
  # system.time( test<-   downloadNightLights(years = 2010, 
  #                     extract = FALSE,
  #                     directory = file.path(data_raw,"night-lights"))
  # )
  # 
  
  # 
  # user  system elapsed 
  # 0.48    1.02  129.47
  # about 2 mins for just the download, no extraction
  

  
  
