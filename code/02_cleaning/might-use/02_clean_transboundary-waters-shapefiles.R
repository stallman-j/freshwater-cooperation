# _______________________________#
# Environment
# Clean 02: clean, transboundary waters shapefiles
# 
# Stallman
# Started 2023-03-16
# Last edited: 
#________________________________#


# Startup

  #rm(list = ls())
  

# packages, folders, paths ----

if (!require("rnaturalearth")) install.packages("rnaturalearth")
if (!require("terra")) install.packages("terra")
if (!require("dplyr")) install.packages("dplyr")
if (!require("rgdal")) install.packages("rgdal")
if (!require("sf")) install.packages("sf")


library(rnaturalearth)     # contains continents and country shapefiles
library(dplyr)     # data wrangling
library(terra)  # managing rasters
library(rgdal) # contains readOGR that can read a shapefile into R 
library(sf)    # vector spatial operations

# paths ----

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))
  
# Bring in data ----
  
#  https://www.earthdatascience.org/courses/earth-analytics/spatial-data-r/shapefile-structure/

  
  tfdd_basin_master_raw <- st_read(file.path(data_raw,"transboundary-waters","TFDD_SpatialData_Public202203","BasinMaster311_20220224.shp"))

  # project to our projection_crs so that it's common. projection_crs set in 00_startup_master.R
  
  tfdd_basin_master_tmp <- st_transform(tfdd_basin_master_raw, projection_crs)
  
  st_crs(tfdd_basin_master_tmp)
  
  # plot 2012 pop density
  plot(tfdd_basin_master_tmp[,8])

  tfdd_bcu_master_raw <- st_read(file.path(data_raw,"transboundary-waters","TFDD_SpatialData_Public202203","BCUMaster311_20220224.shp"))  
  
  
  # project to our projection_crs 
  tfdd_bcu_master_tmp <- st_transform(tfdd_bcu_master_raw, projection_crs)
  
  # plot 2020 pop density by country-basin
  plot(tfdd_bcu_master_tmp[,2])
  
  
  # create a folder
  
  path <- file.path(data_clean,"shape-files")
  
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  
  # save
  saveRDS(tfdd_bcu_master_tmp,
          file = file.path(path,"basin_country_shapes.rds"))
  
  
  saveRDS(tfdd_basin_master_tmp,
          file = file.path(path,"basin_shapes.rds"))
  
  
  
  