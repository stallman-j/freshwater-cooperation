# _______________________________#
# International Agreements
# Clean 02: Clean GADM country shapefiles and get bounding boxes
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
  
  # find bounding boxes to download
  
  data("World")
  
  # create a bbox for an "along" river
  china_russia_bbox <- c(xmin = 115,
                         ymin = 45,
                         xmax = 135,
                         ymax = 55
                         )
  
  st_bbox(china_russia_bbox,
          crs = st_crs(4326))
  
  # create a bbox for an "along" river
  ecuador_bbox <- c(xmin = -82,
                         ymin = -6,
                         xmax = -75,
                         ymax = 2
  )
  
  st_bbox(ecuador_bbox,
          crs = st_crs(4326))
  
  river_border_bbox <- c(xmin = -76.5,
                    ymin = -1,
                    xmax = -74.25,
                    ymax = .5
  )
  
  st_bbox(river_border_bbox,
          crs = st_crs(4326))
  
  
  river_through_bbox <- c(xmin = -77.5,
                         ymin = -3.25,
                         xmax = -76,
                         ymax = -2.25
  )
  
  st_bbox(river_through_bbox,
          crs = st_crs(4326))
  
  
  
  # create a bbox for a "through" river
  senegal_gambia_bbox <- c(xmin = -17.5,
                           ymin = 12.5,
                           xmax = -11,
                           ymax = 16.5)
  
  st_bbox(senegal_gambia_bbox,
          crs = st_crs(4326))
  
  # list of things to subset and crop
  
  
  
  tmap_mode("view")
  # check that these worked
  
  tm_shape(World,
           bbox = senegal_gambia_bbox) + 
    tm_polygons(c("HPI"))+
    tm_grid(labels.show = TRUE,
            labels.inside.frame = TRUE) # add lat long
  
  tm_shape(World,
           bbox = china_russia_bbox) + 
    tm_polygons(c("HPI"))+
    tm_grid(labels.show = TRUE,
            labels.inside.frame = TRUE) # add lat long
  
  
  tm_shape(World,
           bbox = ecuador_bbox) + 
    tm_polygons(c("HPI"))+
    tm_grid(labels.show = TRUE,
            labels.inside.frame = TRUE) # add lat long
  
  # zoom in for river border: ecuador peru colombia
  tm_shape(World,
           bbox = river_border_bbox) + 
    tm_polygons(c("HPI"))+
    tm_grid(labels.show = TRUE,
            labels.inside.frame = TRUE) # add lat long
  
  # zoom in for river border: ecuador peru
  tm_shape(World,
           bbox = river_through_bbox) + 
    tm_polygons(c("HPI"))+
    tm_grid(labels.show = TRUE,
            labels.inside.frame = TRUE) # add lat long
  
  
  
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
  
  
  # system.time(test <-
  # nightlight_download(
  #   area_names = "world",
  #   time = 1992,
  #   light_location = my_light_location,
  #   shapefile_location = NULL,
  #   shapefiles = NULL,
  #   download_shape = ".gpkg",
  #   gpkg_layer = NULL,
  #   admlevel = 0,
  #   user_coordinates = NULL,
  #   corrected_lights = FALSE,
  #   harmonized_lights = FALSE
  # )
  # )
  
  # user  system elapsed 
  # 0.88    0.98   96.04 
  
  
  # plot
  
  system.time(china_1992_plot <-
  nightlight_plot(
    area_names = "Ecuador",
    time = "1992",
    shapefile_location = data_temp,
    light_location = my_light_location
  )
  )
  
  # see how long just the download takes
  system.time( test<-   downloadNightLights(years = 2010, 
                      extract = FALSE,
                      directory = file.path(data_raw,"night-lights"))
  )
  
  # find what bounding box to use
  
  
  # 
  # user  system elapsed 
  # 0.48    1.02  129.47
  # about 2 mins for just the download, no extraction
  
  system.time( test<-   downloadNightLights(years = 2010, 
                                            extract = TRUE,
                                            directory = file.path(data_raw,"night-lights"))
  )
  
  path <- file.path(data_raw,"gadm-global-shapefiles","gadm_410-levels.gpkg")
  
  st_layers(path)
  
  # download the countries
  world_gadm <- st_read(dsn = path,
                        layer = "ADM_0")
  
  # 
  
  # zoom in for river border: ecuador peru

# crop the world_gadm to bounding boxes ----
  
  # turn off spherical geometry
  # see https://github.com/r-spatial/sf/issues/1902
  sf_use_s2(FALSE)
  
  gadm_make_valid <- st_make_valid(world_gadm)
  
  river_border_sf <- st_crop(gadm_make_valid, river_border_bbox)
  
  
  # user  system elapsed 
  # 58.14   49.83  266.66
  
  saveRDS(river_border_sf,
          file = file.path(data_clean,"river_border_sf.rds"))
  