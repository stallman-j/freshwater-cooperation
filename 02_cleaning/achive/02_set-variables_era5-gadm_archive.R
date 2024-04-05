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


tic("Read in raster to project")
my_raster <- readRDS(file = file.path(data_external_clean,"ERA_5","raster",paste0(current_file,
                                                                              "_monthly_",
                                                                              min_time,"_to_",
                                                                              max_time,".rds")))

toc()

values_varname         <- "tp_expver=1"
# requires that the raster have unique dates in the same sequence
time_vec <- time(my_raster) %>% ymd()
names(my_raster) <- time_vec

  tic("Read in polygon shapefile")
  # don't need the merged on data for this, just need to extract to polygons
  my_polygons <- readRDS(file = file.path(data_external_clean,"EM-DAT_geocoded","GDIS_1960_2018_disasterlocations.rds")) %>%
                 filter(continent == current_continent) %>%
                 filter(disastertype == "flood" | disastertype == "drought")

  toc()
  
## parameters ----

  # create an indicator for which dataset the observation is in
  my_polygons <- my_polygons %>% mutate(id = row_number())
  
  class(st_geometry(my_polygons))
  
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
                                         values_to = paste0(values_varname,"_",my_fun,"_",my_weights)) %>%
                            filter(date >= first(time_range) & date <= last(time_range))

 toc()
 
# merge the raster vals onto the polygons
 
 print("Merging extracted polygons to polygons df.")
 
 tic("Merged extracted polygons back to polygons df")
   poly_rast <- left_join(polygons_cast_nogeo,extract_by_stack_level)

toc()

## add some variables ----
options(scipen=100, digits = 4)
  # test 

poly_rast <- poly_rast %>% 
          # uncomment to examine small samples to see things are going through right
          #filter(id==1 | id==3) %>% filter(date >= ymd("1960-01-01") & date <= ymd("1962-12-01")) %>%
        mutate(precip = `tp_expver=1_weighted_sum_area`/1000000)

poly_rast <- poly_rast %>% 
         ungroup() %>%
         group_by(id,month(date)) %>% # generate monthly average precip
         mutate(lr_monthly_avg_precip = mean(precip),
                lr_monthly_sd_precip  = sd(precip),
                monthly_zscore_precip = (precip - mean(precip))/sd(precip)) %>%
         ungroup() %>%
         group_by(id,year(date)) %>%
         mutate(current_annual_avg_precip  = mean(precip),
                current_annual_sd_precip   = sd(precip),
                annual_zscore_precip       = (precip - mean(precip))/sd(precip)) %>% 
         ungroup() %>%
         group_by(id) %>%
         mutate(lr_sd_precip = sd(precip),
                lr_mean_precip = mean(precip),
                zscore_precip  = (precip - mean(precip))/sd(precip),
                sd_deviation_precip = current_annual_sd_precip - sd(precip))
                
          

  # long-run monthly average 

# test2 %>% filter(month(date)==2 | month(date)==3) %>% filter(id==1) %>% view()
# 
#   poly_rast2 <- gadm

object.size(poly_rast) %>% format(units = "Gb")

  path <- file.path(data_external_clean,"merged")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  # https://mapping-in-r-workshop.ryanpeek.org/02_import_export_gpkg
  
  tic("Saved polygons w/o geometry and extracted vals to an RDS")
  saveRDS(poly_rast,
          file = file.path(data_external_clean,"merged",paste0(first(time_range),"_to_",
                                                               last(time_range),"_ERA5_GDIS_",current_file,
                                                                       "_monthly_nogeo",
                                                                       ".rds")))
  toc()
  
  tic("Saved polygons with geometry to an RDS")
  saveRDS(polygons_cast,
          file = file.path(data_external_clean,"merged",paste0("GDIS_polygons_cast.rds")))
  toc()
  
  
# merge on emdat disaster info ----
  
  gdis_emdat_path <- file.path(data_external_clean,"EM-DAT_geocoded","GDIS_EMDAT_1960_2018.rds")

  gdis_emdat <- readRDS(file = gdis_emdat_path)
  
  #data_test <- data[1:100,]
  
  data <-  left_join(poly_rast, gdis_emdat,
                     by = c("geo_id","date","disasterno","iso3"))  %>%
            select(iso3,disasterno,continent,date,precip,lr_monthly_avg_precip,lr_monthly_sd_precip,
                   monthly_zscore_precip,current_annual_avg_precip,current_annual_sd_precip,
                   annual_zscore_precip,lr_sd_precip,lr_mean_precip,zscore_precip,sd_deviation_precip,
                   disaster_id,disaster_type,length_disaster_years,is_disaster,deaths_dummy,
                   disaster_factor,start_ymd,end_ymd,length_disaster_months,geo_id)
                   
  
  object.size(data) %>% format(units = "Gb")
  

  tic("Saved polygons without geometry to an RDS")
  saveRDS(data,
          file = file.path(data_external_clean,"merged",paste0("GDIS_emdat_era5.rds")))
  toc()
  
  
# test out plotting ----

#angola <- my_polygons %>% filter(iso3== "AGO")
# 
ggplot(data = my_polygons[48:50,]) +
  geom_sf(aes(fill = iso3c))

names_raster  <- names(my_raster)
current_layer <- my_raster[[1]]

rast_df <- as.data.frame(current_layer, xy = TRUE) %>%
  na.omit() 


map <- ggplot() +
  geom_raster(data = rast_df,
              aes(x = x, y = y, fill = `1940-01-01`)) +
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

map

save_map(output_folder = output_maps,
         plotname = map,
         filename = "raster_test_rast.png",
         width = 9,
         height = 5,
         dpi = 300)
