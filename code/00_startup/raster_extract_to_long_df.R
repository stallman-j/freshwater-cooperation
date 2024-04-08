#' Extracts terra rasters to a vector SF
#'
#' \code{raster_extract_to_long_df} takes a terra spatial raster stack and extracts from this to either points, polygons or lines to generate for each spatial unit an extracted value for each raster layer. Outputs a long data frame where an observation is spatial-unit-by-raster-layer. This is typically going to be a spatial unit (e.g. city) by time (e.g. 2004 July) for a particular variable (e.g. sea surface temperature). Optionally casts polygon shapefile to one which is cast so that no single row contains multiple geometries. 
#' @author jillianstallman
#' @param terra_raster a terra raster. If not from 0-360 degrees longitude and -90 to 90 degrees latitude will rotate the latitude (which takes a while). Easiest to use the output of clean_era5 to generate this input raster.
#' @param vector_sf an sf object with polygons, points or lines to be extracted to
#' @param cast_vector_sf logical with default FALSE. set this to TRUE if you have a multipolygon,multipoint or multilinestring and you want to create a sf in which each row gives a single geometry unit rather than aggregating over possibly geographically disparate units (e.g. if you are aggregating temperature up to a mean annual value for a country and are using the United States, this includes Alaska and Hawaii which geographically are located far from the contiguous continental United States, which may not give you the average you wanted)
#' @param vector_cast_out_path if casting multipolygons to their component polygons, filepath to where the vector sf that gets cast is output. Default is working directory
#' @param vector_cast_out_filename if cast_vector_sf is TRUE, filename for the vector sf output. default is vector_cast.rds
#' @param save_raster_copy default FALSE, logical. Set to TRUE if you want to save a copy of the raster you bring in
#' with possibly updated coordinate reference system or rotated correctly
#' @param raster_out_path if you're saving a copy of the raster, file path to save your copy of the raster to. Default is working directory.
#' @param raster_out_filename if saving a raster copy, name of the file you want to save it as. Default is "terra_raster_rotated_and_wgs84.tif"
#' @param extracted_out_path filepath, where you want to save the vector sf with its extracted values
#' @param extracted_out_filename filename of output file from extracted raster to vector (no geometry kept)

#' @param layer_substrings character vector, giving the substrings to identify layers to keep. Default is "all," keep everything. But if you have a single layer type (e.g. surface temperature), change this because it'll become part of the column name for the long data frame.
#' @param long_df_colname character vector, column name of the panel data you're going to extract to, e.g. "precip," the final colname of which will be paste0(long_df_colname,"_",func)
#' @param layer_names_vec character vector, needs to be of the same length as the number of layers of your raster. Default NULL populates this with the output of time(terra_raster)
#' @param layer_names_title default "date", the column(i.e. variable) name that you want for the ultimate data frame that has a column with title "layer_names_title" and which has the value for each raster layer the corresponding element of layer_names_vec.
#' @param func character vector, default "mean", also e.g. "weighted_sum" for exactextractr, function to be used for aggregating, from terra (points) or exactextractr (polygons)
#' @param weights character vector, default NULL. To be used for weighting the function, see packages terra (points) or exactextractr (polygons) for options, e.g. "area"
#' @param ... additional options to be passed to terra::extract() (for points) or exactextractr::exact_extract() (for polygons/lines)
#' @examples
#' # example code
#' @export
#' @returns a pivoted-long sf 


raster_extract_to_long_df <- function(terra_raster,
                                      vector_sf,
                                      cast_vector_sf = FALSE,
                                      vector_cast_out_path  = getwd(),
                                      vector_cast_out_filename = "vector_cast.rds",
                                      save_raster_copy = FALSE,
                                      raster_out_path = getwd(),
                                      raster_out_filename = "terra_raster_rotated_and_wgs84.tif",
                                      extracted_out_path = getwd(),
                                      extracted_out_filename = "terra_raster_extracted_to_vector.rds",
                                      layer_substrings = "all",
                                      long_df_colname  = "precip",
                                      layer_names_vec = NULL,
                                      layer_names_title = "date",
                                      func = "mean", #"weighted_sum" if polygons
                                      weights = NULL, #"area" if polygons
                                      #time_interval = "months",
                                      remove_files = FALSE,
                                      ...
){
  
  # TO DO: add more explicit statement for rotating the terra_raster
  
  # create output paths
  for (out_path in c(raster_out_path,extracted_out_path,vector_cast_out_path)){
    if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  }
  
  #sf::sf_use_s2(FALSE)
  
  vector_sf    <- vector_sf %>% mutate(vector_sf_id = row_number())
  
  if (any(is.na(terra::time(terra_raster)))) stop({message("Error: Terra raster is missing some or all of its time dimension. Set with time(terra_raster) <- your_vector_of_dates")})
  if (crs(terra_raster)=="") stop({message("Error: Terra raster has no set CRS. Set with crs(terra_raster) <- your_crs_code,e.g.crs(vector_sf) <- 'epsg:4326' for WGS 84")})
  if (crs(vector_sf)=="") stop({message("Error: Vector sf file raster has no set CRS. Set with crs(vector_sf) <- your_crs_code, e.g.crs(vector_sf) <- 'epsg:4326' for WGS 84")})
  
  vector_type <- st_geometry_type(vector_sf)
  
  # if CRS is different from raster to polygon, transform to the polygon's to match the terra_raster's
  
  if(!same.crs(crs(terra_raster),crs(vector_sf))){
    
    vector_sf <- vector_sf %>% st_transform(crs = crs(terra_raster))
    
  }
  if (cast_vector_sf == FALSE) vector_cast <- vector_sf %>% mutate(vector_cast_id = vector_sf_id)
  
  if (cast_vector_sf == TRUE) {
    if ("POLYGON" %in% vector_type | "MULTIPOLYGON" %in% vector_type){
      print("vector_sf is POLYGON or MULTIPOLYGON")
      print("re-casting..")
      tictoc::tic("Casted POLYGONs to MULTIPOLYGONs")
      vector_cast <- vector_sf %>%
        st_cast("MULTIPOLYGON") %>% # homogenizes the type
        st_cast("POLYGON") %>% # puts all into the singleton type
        mutate(vector_cast_id = row_number()) %>%
        st_make_valid()
      tictoc::toc()
    } else if ("LINESTRING" %in% vector_type | "MULTILINESTRING" %in% vector_type ){
      print("vector_sf is LINESTRING or MULTILINESTRING")
      print("re-casting..")
      
      tictoc::tic("Casted LINESTRINGs to MULTILINESTRINGs")
      
      vector_cast <- vector_sf %>%
        st_cast("MULTILINESTRING") %>% # homogenizes the type
        st_cast("LINESTRING") %>% # puts all
        mutate(vector_cast_id = row_number()) %>%
        st_make_valid()
      tictoc::toc()
    } else if ("POINT" %in% vector_type | "MULTIPOINT" %in% vector_type ){
      
      print("vector_sf is POINT or MULTIPOINT")
      print("re-casting..")
      
      tictoc::tic("Casted MULTIPOINTs to MULTIPOINTs")
      
      vector_cast <- vector_sf %>%
        st_cast("MULTIPOINT") %>% # homogenizes the type
        st_cast("POINT") %>% # puts all to the single type
        mutate(vector_cast_id = row_number()) %>%
        st_make_valid()
      tictoc::toc()
    } else {
      stop({message("Error: Vector type of vector_sf is not something I know how to work with. Use a sf file of type LINESTRING, MULTILINESTRING, POLYGON, MULTIPOLYGON, POINT or MULTIPOINT")})
    }
  }
  
  
  # make sure raster extent is correct ----
  
  # ext() gives the extent of the raster as provided by the layers, not the crs() function
  
  # rotating takes a very long time
  # this says if xmin for terra_raster is 0, and xmin for vector_sf is -180, then we need to rotate
  
  if (base::round(ext(terra_raster))[1] == 0 & base::round(ext(vector_sf))[1]==-180 ) {
    print("Rotating raster from 0,360 degrees longitude to -180 to 180. Hold on, this takes a while.")
    tictoc::tic("Finished rotating terra_raster correctly")
    terra_raster <- terra::rotate(terra_raster, left = TRUE)
    tictoc::toc()
  }
  
  if (save_raster_copy==TRUE){
    saveRDS(terra_raster,
            file = file.path(raster_out_path,raster_out_filename))
  }
  
  
  
  
  # extract ----
  
  # exact_extract does better for big rasters or spatially fine
  # https://tmieno2.github.io/R-as-GIS-for-Economists/extract-speed.html
  
  # also it's faster to do all layers in one go
  
  # requires that the raster have unique dates in the same sequence
  # 
  
  if (is.null(layer_names_vec)){
    layer_names_vec <- time(terra_raster) #%>% ymd_hms()
    # get a sequence of dates
    #dates <- seq(min(time_vec),max(time_vec), by = time_interval)
    
  }
 
  tictoc::tic("Extracted all terra_raster separate layer_substrings to vector_cast")
  
  # layer_substr <- layer_substrings[1]
  
  if (length(layer_substrings)==1 | layer_substrings == "all"){ # if only one substring don't bother with the loop
    names(terra_raster) <- layer_names_vec # need to set
    
    tictoc::tic("Successfully extracted raster ")
    
    if ("POINT" %in% vector_type | "MULTIPOINT" %in% vector_type | "LINE" %in% vector_type | "MULTILINE" %in% vector_type){
      
      out_df <- terra::extract(x = terra_raster,
                               y = vector_cast,
                               method = "simple",
                               weights = FALSE,...) %>%
        dplyr::rename(vector_cast_id = ID) %>%
        tidyr::pivot_longer(cols = !vector_cast_id,
                            names_to = layer_names_title,
                            #names_prefix = paste0(weights,"."),
                            #names_transform = ymd,
                            values_to = paste0(long_df_colname,"_",func,weights))
    } else{ # only works for POLYGON or MULTIPOLYGON
      
      out_df <- exactextractr::exact_extract(x = terra_raster,
                                             y = vector_cast,
                                             progress = F,
                                             fun = func,
                                             weights = weights,...
      )  %>%
        dplyr::mutate(vector_cast_id = row_number()) %>%
        tidyr::pivot_longer(cols = !vector_cast_id,
                            names_to = layer_names_title,
                            #names_prefix = paste0(weights,"."),
                            #names_transform = ymd,
                            values_to = paste0(long_df_colname,"_",func,"_",weights))
      
    } # end else if geometry is non-point
    tictoc::toc()
    
  } else { # if multiple variables by year, then loop through the substrings that identify these variables and extract for each variable
    for (layer_substring in layer_substrings){
      
      print(paste0("using layers which include ",layer_substring))
      
      # TRUE for the indices we want to keep
      keep_indices <- stringr::str_detect(names(terra_raster), layer_substring)
      
      temp_raster <- terra::subset(terra_raster, subset = keep_indices)
      #names(temp_raster) <- time_vec
      
      
      # https://tidyr.tidyverse.org/artictoc::ticles/pivot.html
      
      # https://codes.ecmwf.int/grib/param-db/?id=228
      tictoc::tic("Successfully extracted raster ")
      temp_df <- exactextractr::exact_extract(x = temp_raster,
                                              y = vector_cast,
                                              progress = F,
                                              fun = func,
                                              weights = weights,...
      )  %>%
        dplyr::mutate(vector_cast_id = row_number()) %>%
        tidyr::pivot_longer(cols = !vector_cast_id,
                            names_to = layer_names_title,
                            #names_prefix = paste0(weights,"."),
                            #names_transform = ymd,
                            values_to = paste0(long_df_colname,"_",func,"_",weights))
      
      tictoc::toc()
      
      
      if (layer_substring==layer_substrings[1]){
        out_df <- temp_df
      } else {
        out_df <- cbind(out_df,temp_df)
      }
      rm(temp_df)
    }
    
  } # end else
  tictoc::toc()
  
  if (cast_vector_sf==TRUE){
    tictoc::tic("Saved vector cast with geometry to an RDS")
    saveRDS(vector_cast,
            file = file.path(vector_cast_out_path,vector_cast_out_filename))
    tictoc::toc()
  }
  
  
  tictoc::tic("Merged extracted sf units back to the units sf")
  out_df <- left_join(st_drop_geometry(vector_cast),out_df)
  
  tictoc::toc()
  
  tictoc::tic("Saved long data frame")
  saveRDS(out_df,
          file = file.path(extracted_out_path,extracted_out_filename))
  tictoc::toc()
  
  if (remove_files==TRUE) {
    rm(vector_cast,out_df)}
  else return(out_df)
  
  gc()
  
}
