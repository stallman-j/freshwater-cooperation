# _______________________________#
# Environment
# Merge 02: Extract polygons from precipitation rasters
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

# https://mapping-in-r-workshop.ryanpeek.org/02_import_export_gpkg

min_time <- "1940-01-01"
max_time <- "2023-09-01"
current_file <- "total_precipitation"
my_fun      <- "weighted_sum"
my_weights  <- "area"

# packages ----
if (!require(sf)) install.packages("sf") #shapes
if (!require(tmap)) install.packages("tmap") #for visual mapping
if (!require(terra)) install.packages("terra") 
if (!require(lubridate)) install.packages("lubridate")
if (!require(ecmwfr)) install.packages("ecmwfr")
if (!require(exactextractr)) install.packages("exactextractr")
if (!require(tictoc)) install.packages("tictoc")

library(ecmwfr) #European Centre for Medium-Range Weather forecasts, ERA 5 data  
library(sf) # GIS vectors
library(tmap) # plotting maps
library(terra) # GIS rasters
library(exactextractr) # extracting rasters to polygons
library(tictoc) # for timing how long things take

sf_use_s2(FALSE)

# bring in data ----


my_raster <- readRDS(file = file.path(data_external_clean,"ERA_5","raster",paste0(current_file,
                                                                              "_monthly_",
                                                                              min_time,"_to_",
                                                                              max_time,".rds")))


values_varname         <- "tp_expver=1"
# requires that the raster have unique dates in the same sequence
time_vec <- time(my_raster) %>% ymd()
names(my_raster) <- time_vec

#level <- 1

for (level in 1:5){

  print(paste0("working on level ",level))
  
  print("current time is ")
  print(Sys.time())
  
  path <- file.path(data_external_clean,"GADM","global")
  
  tic("Read in polygon shapefile")
  my_polygons <- readRDS(file = file.path(path,paste0("GADM_global_ADM_",level,".rds")))
  toc()
  
## parameters ----
  #my_polygons            <- level_gadm
 
  # create an indicator for which dataset the observation is in
  my_polygons <- my_polygons %>% mutate(id = row_number())
  
  #class(st_geometry(my_polygons))
  
  # first cast everything to Multipolygon, then cast back to polygon:
  print("Casting polygons to POLYGON")
  
  tic("Casted polygons to POLYGON")
  polygons_cast <- my_polygons %>% 
                  st_cast("MULTIPOLYGON") %>% # homogenizes the type
                  st_cast("POLYGON") %>% # puts all 
                  st_make_valid() %>% # make valid geometries %>%
                  mutate(id = row_number())
  
  polygons_cast_nogeo <- polygons_cast %>% st_drop_geometry()

  toc()

# extract ----
  
  # use terra, raster or exact_extract?
  # exact_extract does better for big rasters or spatially fine
  # https://tmieno2.github.io/R-as-GIS-for-Economists/extract-speed.html
  
  # also it's faster to do all layers in a go
 
  print("Extracting polygons.")
  
 tic("Extracted polygons")
 
 # https://tidyr.tidyverse.org/articles/pivot.html
 
 # https://codes.ecmwf.int/grib/param-db/?id=228
 extract_by_stack_level <- exact_extract(x = my_raster,
                                         y = polygons_cast,
                                         progress = F,
                                         fun = my_fun,
                                         weights = my_weights
                                         )  %>%
                            mutate(id = row_number()) %>%
                            pivot_longer(cols = !id,
                                         names_to = "date",
                                         names_prefix = paste0(my_weights,"."),
                                         names_transform = ymd,
                                         values_to = paste0(values_varname,"_",my_fun,"_",my_weights))

 toc()
# merge the raster vals onto the polygons
 
 print("Merging extracted polygons to polygons df.")
 
 tic("Merged extracted polygons back to polygons df")
   gadm_rast <- left_join(polygons_cast_nogeo,extract_by_stack_level)

toc()

  path <- file.path(data_external_clean,"merged")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  # https://mapping-in-r-workshop.ryanpeek.org/02_import_export_gpkg
  
  tic("Saved polygons w/o geometry and extracted vals to an RDS")
  saveRDS(gadm_rast,
          file = file.path(data_external_clean,"merged",paste0(min_time,"_to_",
                                                               max_time,"_GADM_ADM_",level,"_",current_file,
                                                                       "_monthly_nogeo",
                                                                       ".rds")))
  toc()
  
  tic("Saved polygons with geometry to an RDS")
  saveRDS(polygons_cast,
          file = file.path(data_external_clean,"GADM","global",paste0("GADM_ADM_",level,"_as_polygons.rds")))
  toc()
  
  # write to geopackage
  tic("wrote polygons to gpkg")
  #st_write(polygons_cast,dsn = file.path(path,"era5_gadm_water.gpkg"),layer = paste0("GADM_ADM_",level,"_all_polygons"))
  #st_write(extract_by_stack_level,dsn = file.path(path,"era5_gadm_water.gpkg"),layer = paste0(min_time,"_to_",max_time,"_monthly_GADM_ADM_",level,"_no_polygons"))
  
  toc()
  
  print("finished writing to raster, current times is ")
  print(Sys.time())

  tic("Removed sfcs and rasters")
  rm(extract_by_stack_level,polygons_cast,my_polygons,path,gadm_rast,polygons_cast_nogeo)
  toc()
  
  gc()
  
}

# For level 1

# [1] "working on level 1"
# [1] "current time is "
# [1] "2023-10-14 03:36:25 EDT"
# Read in polygon shapefile: 2.89 sec elapsed
# [1] "Casting polygons to POLYGON"
# Casted polygons to POLYGON: 32.13 sec elapsed
# [1] "Extracting polygons."
# Extracted polygons: 126.72 sec elapsed
# [1] "Merging extracted polygons to polygons df."
# Writing layer `GADM_ADM_1_all_polygons' to data source 
#   `E:/data/03_clean/merged/era5_gadm_water.gpkg' using driver `GPKG'
# Writing 125661 features with 12 fields and geometry type Polygon.
# Writing layer `1940-01-01_to_2023-09-01_monthly_GADM_ADM_1_no_polygons' to data source 
# `E:/data/03_clean/merged/era5_gadm_water.gpkg' using driver `GPKG'
# Writing 126289305 features with 3 fields without geometries.
# wrote polygons to gpkg: 1122.53 sec elapsed

# [1] "finished writing to raster, current times is "
# [1] "2023-10-14 03:57:50 EDT"
# [1] "working on level 2"
# [1] "current time is "
# [1] "2023-10-14 03:57:50 EDT"
# Read in polygon shapefile: 6.21 sec elapsed
# [1] "Casting polygons to POLYGON"
# Casted polygons to POLYGON: 50.5 sec elapsed
# [1] "Extracting polygons."
# Extracted polygons: 167.14 sec elapsed
# [1] "Merging extracted polygons to polygons df."
# Writing layer `GADM_ADM_2_all_polygons' to data source 
#   `E:/data/03_clean/merged/era5_gadm_water.gpkg' using driver `GPKG'
# Writing 160960 features with 14 fields and geometry type Polygon.
# Writing layer `1940-01-01_to_2023-09-01_monthly_GADM_ADM_2_no_polygons' to data source 
# `E:/data/03_clean/merged/era5_gadm_water.gpkg' using driver `GPKG'
# Writing 161764800 features with 3 fields without geometries.
# wrote polygons to gpkg: 1442.81 sec elapsed
# [1] "finished writing to raster, current times is "
# [1] "2023-10-14 04:25:37 EDT"
# [1] "working on level 3"
# [1] "current time is "
# [1] "2023-10-14 04:25:39 EDT"
# Read in polygon shapefile: 5.8 sec elapsed
# [1] "Casting polygons to POLYGON"
# Casted polygons to POLYGON: 53.28 sec elapsed
# [1] "Extracting polygons."



  # 102.35 sec elapsed
  # 135.22 sec elapsed stupidly fast

# merge back to the sf with many polygons according to id number ----


# test out plotting ----

angola <- my_polygons %>% filter(GID_0== "AGO")
# 
ggplot(data = my_polygons[48:50,]) +
  geom_sf(aes(fill = GID_1))

names_raster  <- names(my_raster)
current_layer <- my_raster[[1]]

rast_df <- as.data.frame(current_layer, xy = TRUE) %>%
  na.omit() 


map <- ggplot() +
  geom_raster(data = rast_df,
              aes(x = x, y = y, fill = `tp_expver=1_1`)) +
  geom_sf(data = my_polygons,
          color = "gray70",
          fill = "gray99",
          alpha = 0,
          linewidth = 0.05)+
  scale_fill_viridis_c() +
  theme_void() +
  theme(
    legend.position = "bottom"
  )

save_map(output_folder = output_maps,
         plotname = map,
         filename = "raster_test_rast.png",
         width = 9,
         height = 5,
         dpi = 300)
