#' Clean ERA5 Rasters
#' @description takes as data a raster file (ideally taken from Copernicus ERA5),
#' selects a particular variable to analyze and subsets the rasters according to
#' this variable, and rotates the rasters so they can be extracted to more typical polygon data
#' @author jillianstallman
#' @param input_path file path to your raw ERA data (e.g. file.path("E:","Projects","data","01_raw",ERA_5"))
#' @param input_filename the short name that you use to distinguish the .nc file
#' @param input_filetype the type of input file; needs to be something terra::rast() can read
#' @param keep_substrings a character vector of the substrings which define which layers you would like to keep
#' @param output_path the file path to put your cleaned data. Creates it if it doesn't already exist, e.g. file.path("E:","Projects","data","03_clean","ERA_5")
#' @param output_filename the name to give the cleaned ERA5 raster. NULL defaults to:
#'  paste0(input_filename,".tif"); note this uses terra::writeRaster, not saveRDS
#'  @return just puts the correctly rotated and subsetted raster into the output_path folder
#' @examples
#' clean_era5(input_path      = file.path(data_external_raw,"ERA_5"),
#' input_filename  = "example_era5",
#' input_filetype  = "nc",
#' keep_substrings = c("skt"),
#' output_path     = file.path(data_external_clean,"ERA_5"),
#' output_filename = paste0("example_era5.tif"))
#'
#'
clean_era5 <- function(input_path,
                       input_filename = "example_era5",
                       input_filetype = "nc",
                       keep_substrings  = c("skt"), #tp_expver=1",
                       output_path,
                       output_filename = NULL
){
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    stringr, # for string operatioons
    terra, # handle raster data
    tidyverse, # data wrangling
    tictoc,
    lubridate # date operations
  )
  
  # create output folder if it doesn't exist already
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  # TO DO: examine if I can get this function without needing to drop spherical geometry
  sf_use_s2(FALSE)
  
  in_file <- paste0(input_filename,".",input_filetype)
  
  era <- terra::rast(x = file.path(input_path, in_file))
  
  all_names <- names(era)
  
  keep_substr_new <- paste(keep_substrings, collapse = "|" )
  
  keep_names <- stringr::str_detect(all_names, keep_substr_new)
  
  era_5 <- terra::subset(era, subset = keep_names)
  
  
  # what is the CRS?
  
  # https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-Spatialreferencesystems
  # ERA5 data is referenced in the horizontal with respect to the WGS84 ellipse (which defines the major/minor axes)
  # and in the vertical it is referenced to the EGM96 geoid over land but over ocean it is referenced to
  # mean sea level, with the approximation that this is assumed to be coincident with the geoid
  
  crs(era_5) <- "epsg:4326"
  
  # rotate so that instead of 0 to 360 the longitude is going from -180 to 180
  
  # this takes a while if there are lots of layers; with the 8 example raster layers it's less than a second
  era_5 <- terra::rotate(era_5, left = TRUE)
  
  
  if (is.null(output_filename)) {
    
    terra::writeRaster(era_5,
                       file = file.path(output_path,paste0(input_filename,".tif")))
    
  } else {
    
    terra::writeRaster(era_5,
                       file = file.path(output_path,output_filename))
  }
  
  
}
