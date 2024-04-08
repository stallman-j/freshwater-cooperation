# _______________________________#
# era5extractr
# 02 Clean ERA5 data
#
# Stallman
# Last edited: 2024-03-27
# Goes step-by-step through the ERA5 cleaning. For a function see clean_era5 (example at end) which (experimentally)
# should do the same thing
#________________________________#

# Startup

rm(list = ls())


# bring in the packages, folders, paths

home_folder <- file.path("P:","Projects","freshwater-cooperation")

source(file.path(home_folder,"code","00_startup.R"))

# packages ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  stringr, # for string operatioons
  terra, # handle raster data
  ggplot2,
  tictoc, # for timing things
  tmap, # for quick and dirty mapping
  tidyverse, # data wrangling
  lubridate # date operations
)

# additional help:
# in case you're getting stuck and this code is not working
# https://gis.stackexchange.com/questions/444481/problems-extracting-era5-data-with-exact-extract-in-r
# https://dominicroye.github.io/en/2018/access-to-climate-reanalysis-data-from-r/#packages



#sf_use_s2(FALSE)

# bring in data ----

# input filename comes from 01_download.R

# these are function parameters
#
input_filename <- "example_era5"
input_path     <- file.path(data_external_raw,"ERA_5")
input_filename <-  "example_era5"
input_filetype <- "nc"
keep_substrings  <- c("sp","tp") # there are multiple layer types in the example raster; this selects just the skt and tp

output_path    <- file.path(data_external_clean,"ERA_5")
output_filename <- "example_era5_clean.rds"


# here's what the function will do, but with some additional exploration

in_file <- paste0(input_filename,".",input_filetype)

era <- terra::rast(x = file.path(input_path, in_file))

all_names <- names(era)

# examine this spatial raster
era

# class       : SpatRaster
# dimensions  : 721, 1440, 12  (nrow, ncol, nlyr)
# resolution  : 0.25, 0.25  (x, y)
# extent      : -0.125, 359.875, -90.125, 90.125  (xmin, xmax, ymin, ymax)
# coord. ref. :
#   sources     : example_era5.nc:sst  (4 layers)
# example_era5.nc:sp  (4 layers)
# example_era5.nc:tp  (4 layers)
# varnames    : sst (Sea surface temperature)
# sp (Surface pressure)
# tp (Total precipitation)
# names       : sst_1, sst_2, sst_3, sst_4, sp_1, sp_2, ...
# unit        :     K,     K,     K,     K,   Pa,   Pa, ...
# time        : 1994-09-01 to 1994-09-02 01:00:00 UTC

# it has three types of layers, so let's just limit to sp and tp
keep_substr_new <- paste(keep_substrings, collapse = "|" )

keep_names <- stringr::str_detect(all_names, keep_substr_new)

era_5 <- terra::subset(era, subset = keep_names)

# now see if we've successfully gotten the layers we wanted
# class       : SpatRaster
# dimensions  : 721, 1440, 8  (nrow, ncol, nlyr)
# resolution  : 0.25, 0.25  (x, y)
# extent      : -0.125, 359.875, -90.125, 90.125  (xmin, xmax, ymin, ymax)
# coord. ref. :
#   sources     : example_era5.nc:sp  (4 layers)
# example_era5.nc:tp  (4 layers)
# varnames    : sp (Surface pressure)
# tp (Total precipitation)
# names       : sp_1, sp_2, sp_3, sp_4, tp_1, tp_2, ...
# unit        :   Pa,   Pa,   Pa,   Pa,    m,    m, ...
# time        : 1994-09-01 to 1994-09-02 01:00:00 UTC

era_5

# how many layers
dim(era)
#   [1]  721 1440   12

dim(era_5)
# 721 1440    8

# check out some of the timing characteristics
#
# gives a string of all the relevant times
t <- terra::time(era)

class(t[1])
# [1] "POSIXct" "POSIXt"


terra::time(era_5) %>% head()

# [1] "1994-09-01 00:00:00 UTC" "1994-09-01 01:00:00 UTC"
# [3] "1994-09-02 00:00:00 UTC" "1994-09-02 01:00:00 UTC"
# [5] "1994-09-01 00:00:00 UTC" "1994-09-01 01:00:00 UTC"

min(t)

min_time <- str_replace(string = min(t),
                        pattern = " UTC",
                        replacement = "")

min_time
# [1] "1994-09-01"

max(t)
# "1994-09-02 01:00:00 UTC"

max_time <- str_replace(string = max(t),
                        pattern = " UTC",
                        replacement = "")
#"1994-09-02 01:00:00"



# for some vars there are two types, retroactively fixed and then the real-time values
# # https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation

# "For GRIB, ERA5T data can be identified by the key expver=0005 in the GRIB header. ERA5 data is identified by the key expver=0001."

# we want ERA5 because ERA5T is the real-time (not yet retroactively fixed) data, so the tp_expver=1_n give the retroactively fixed data


# what is the CRS?

# https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-Spatialreferencesystems
# ERA5 data is referenced in the horizontal with respect to the WGS84 ellipse (which defines the major/minor axes)
# and in the vertical it is referenced to the EGM96 geoid over land but over ocean it is referenced to
# mean sea level, with the approximation that this is assumed to be coincident with the geoid

crs(era_5) <- "epsg:4326"

# rotate so that instead of 0 to 360 the longitude is going from -180 to 180

# this takes a while if you have lots and lots of rasters
tic("Rotated the ERA5 rasters to 0-360 instead of -180-180")
era_5 <- terra::rotate(era_5, left = TRUE)


toc()
# with the example this is short:
# Rotated the ERA5 rasters to 0-360 instead of -180-180: 0.58 sec elapsed


if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE) # recursive lets you create any needed subdirectories

saveRDS(era_5,
        file = file.path(output_path,output_filename))

# # do some plots to show what happened ----


rotate_layer <- function(i,
                         data = era_5){
  out <- terra::rotate(data[[i]], left = TRUE)
}

# show for a single one
tic("Rotated a single layer")
out_layer <- rotate_layer(i=1,
                          data = era_5)
toc()

plot_1 <- ggplot()+
  geom_raster(data = as.data.frame(era_5[[1]], xy=TRUE),
              aes(x = x, y=y, fill = `sp_1`))


plot_2 <- ggplot()+
  geom_raster(data = as.data.frame(out_layer[[1]], xy=TRUE),
              aes(x = x, y=y, fill = `sp_1`))


plot_1
plot_2

# this is one of my generated functions to save maps to the output_maps folder.

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



# let's take a look at how we would decide a bounding box if we wanted to crop these rasters right now.
# I've found the extraction algorithms fast enough that I haven't bothered

data(World)

china <- World %>% filter(name == "China")

map <- ggplot() +
  geom_sf(data = china,
          color = "gray70",
          fill = "gray99",
          alpha = 0.5,
          linewidth = .3) +
  labs(title = paste0("China"),
       caption = c("Source: R package tmap")) +
  theme_map()

map

# by inspection, we've got like 20N to 56N and 70E to 140E will give us a good bounding box

# test the function ----


tic("Cleaned ERA5 example raster")
clean_era5(input_path      = file.path(data_external_raw,"ERA_5"),
           input_filename  = "example_era5",
           input_filetype  = "nc",
           keep_substrings = c("sp"),
           output_path     = file.path(data_external_clean,"ERA_5"),
           output_filename = paste0("example_era5_clean.tif"))

toc()

#Cleaned ERA5 for 1940-2023 monthly, including rotation, for total precipitation: 156.73 sec elapsed
# takes up about 2.3G in the external data

