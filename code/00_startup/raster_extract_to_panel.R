#' Extracts terra rasters to a vector SF
#'
#' \code{raster_extract_to_panel} returns polygon shapefiles with longitudinal data from rasters
#' extracted to those polygons and writes two sf files, one with the polygons shapefiles, which are
#' cast so that no single row contains multiple geometries,
#' and one with a long data frame that includes the raster extracted values
#' and an identifier that allows for linking to the sf polygons.
#' @author jillianstallman
#' @param terra_raster a terra raster, currently required to be from 0-360 degrees longitude and -90 to 90 degrees latitude, must have a time dimension given. you could use the clean_era5 function to create this raster, for example
#' @param vector_sf an sf object with polygons, points or lines to be extracted to
#' @param cast_vector_sf logical with default FALSE. set this to TRUE if you have a multipolygon,multipoint or multilinestring and you want to create a sf in which each row gives a single geometry unit rather than aggregating over possibly geographically disparate units (e.g. if you are aggregating temperature up to a mean annual value for a country and are using the United States, this includes Alaska and Hawaii which geographically are located far from the contiguous continental United States, which may not give you the average you wanted)
#' @param save_raster_copy default FALSE, logical. Set to TRUE if you want to save a copy of the raster you bring in
#' with possibly updated coordinate reference system or rotated correctly
#' @param raster_out_path if you're saving a copy of the raster, file path to save your copy of the raster to
#' @param raster_out_filename if saving a raster copy, name of the file you want to save it as
#' @param extracted_out_path filepath, where you want to save the vector sf with its extracted values
#' @param extracted_out_filename filename of output file from extracted raster to vector (no geometry kept)
#' @param vector_cast_out_path if casting multipolygons to their component polygons, filepath to where the vector sf that gets cast is output
#' @param vector_cast_out_filename if cast_vector_sf is TRUE, filename for the vector sf output
#' @param layer_substrings character vector, giving the substrings to identify layers to keep.
#' assumes each element of this will give layers which generate the same longitudinal time sequence
#' @param func character vector, default "weighted_sum," function to be used for aggregating, from terra (points) or exactextractr (polygons)
#' @param weights character vector, default "area" to be used for weighting the function, see packages terra (points) or exactextractr (polygons) for options


raster_extract_to_panel <- function(terra_raster,
                                    vector_sf,
                                    cast_vector_sf = FALSE,
                                    save_raster_copy = FALSE,
                                    raster_out_path = getwd(),
                                    raster_out_filename = "terra_raster.rds",
                                    extracted_out_path = getwd(),
                                    extracted_out_filename = "terra_raster_extracted_to_vector.rds",
                                    vector_cast_out_path  = getwd(),
                                    vector_cast_out_filename = "vector_cast.rds",
                                    layer_substrings,
                                    func = "mean", #"weighted_sum",
                                    weights = NULL, #"area",
                                    time_interval = "months",
                                    remove_files = FALSE
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
      tic("Casted POLYGONs to MULTIPOLYGONs")
      vector_cast <- vector_sf %>%
        st_cast("MULTIPOLYGON") %>% # homogenizes the type
        st_cast("POLYGON") %>% # puts all into the singleton type
        mutate(vector_cast_id = row_number()) %>%
        st_make_valid()
      toc()
    } else if ("LINESTRING" %in% vector_type | "MULTILINESTRING" %in% vector_type ){
      print("vector_sf is LINESTRING or MULTILINESTRING")
      print("re-casting..")
      
      tic("Casted LINESTRINGs to MULTILINESTRINGs")
      
      vector_cast <- vector_sf %>%
        st_cast("MULTILINESTRING") %>% # homogenizes the type
        st_cast("LINESTRING") %>% # puts all
        mutate(vector_cast_id = row_number()) %>%
        st_make_valid()
      toc()
    } else if ("POINT" %in% vector_type | "MULTIPOINT" %in% vector_type ){
      
      print("vector_sf is POINT or MULTIPOINT")
      print("re-casting..")
      
      tic("Casted MULTIPOINTs to MULTIPOINTs")
      
      vector_cast <- vector_sf %>%
        st_cast("MULTIPOINT") %>% # homogenizes the type
        st_cast("POINT") %>% # puts all to the single type
        mutate(vector_cast_id = row_number()) %>%
        st_make_valid()
      toc()
    } else {
      stop({message("Error: Vector type of vector_sf is not something I know how to work with. Use a sf file of type LINESTRING, MULTILINESTRING, POLYGON, MULTIPOLYGON, POINT or MULTIPOINT")})
    }
  }
  
  
  # manipulate the raster a bit ----
  
  # requires that the raster have unique dates in the same sequence
  time_vec <- time(terra_raster) %>% ymd()
  # get a sequence of dates
  dates <- seq(min(time_vec),max(time_vec), by = time_interval)
  
  # if the terra_raster longitude isn't the same as the vector_sf's, take for now that this basically
  # is because the terra_raster must be 0-360 degrees, while the vector_sf will be -180-180
  
  # ext() gives the extent of the raster as provided by the layers, not the crs() function
  
  # rotating takes a very long time
  # this says if xmin for terra_raster is 0, and xmin for vector_sf is -180, then we need to rotate
  
  if (base::round(ext(terra_raster))[1] == 0 & base::round(ext(vector_sf))[1]==-180 ) {
    print("Rotating raster from 0,360 degrees longitude to -180 to 180. Hold on, this takes a while.")
    tic("Finished rotating terra_raster correctly")
    terra_raster <- terra::rotate(terra_raster, left = TRUE)
    toc()
  }
  
  if (save_raster_copy==TRUE){
    saveRDS(terra_raster,
            file = file.path(raster_out_path,raster_out_filename))
  }
  
  
  
  
  # extract ----
  
  # exact_extract does better for big rasters or spatially fine
  # https://tmieno2.github.io/R-as-GIS-for-Economists/extract-speed.html
  
  # also it's faster to do all layers in one go
  
  
  tic("Extracted all terra_raster separate layer_substrings to vector_cast")
  
  # layer_substr <- layer_substrings[1]
  
  if (length(layer_substrings)==1){ # if only one substring don't bother with the loop
    names(terra_raster) <- time_vec # need to set
    
    tic("Successfully extracted raster ")
    
    if ("POINT" %in% vector_type | "MULTIPOINT" %in% vector_type ){
      
      out_df <- terra::extract(x = terra_raster,
                               y = vector_cast,
                               method = "simple",
                               weights = FALSE)%>%
        mutate(vector_cast_id = row_number()) %>%
        pivot_longer(cols = !vector_cast_id,
                     names_to = "date",
                     #names_prefix = paste0(weights,"."),
                     names_transform = ymd,
                     values_to = paste0(layer_substrings,"_",func,weights))
    } else{
      
      out_df <- exact_extract(x = terra_raster,
                              y = vector_cast,
                              progress = F,
                              fun = func,
                              weights = weights
      )  %>%
        mutate(vector_cast_id = row_number()) %>%
        pivot_longer(cols = !vector_cast_id,
                     names_to = "date",
                     names_prefix = paste0(weights,"."),
                     names_transform = ymd,
                     values_to = paste0(layer_substrings,"_",func,weights))
      
    } # end else if geometry is non-point
    toc()
    
  } else { # if multiple variables by year, then loop through the substrings that identify these variables and extract for each variable
    for (layer_substr in layer_substrings){
      
      print(paste0("using layer ",layer_substr))
      
      # TRUE for the indices we want to keep
      keep_indices <- stringr::str_detect(names(terra_raster), layer_substr)
      
      
      temp_raster <- terra::subset(terra_raster, subset = keep_indices)
      names(temp_raster) <- time_vec
      
      
      # https://tidyr.tidyverse.org/articles/pivot.html
      
      # https://codes.ecmwf.int/grib/param-db/?id=228
      tic("Successfully extracted raster ")
      temp_df <- exact_extract(x = temp_raster,
                               y = vector_cast,
                               progress = F,
                               fun = func,
                               weights = weights,
                               ...
      )  %>%
        mutate(vector_cast_id = row_number()) %>%
        pivot_longer(cols = !vector_cast_id,
                     names_to = "date",
                     names_prefix = paste0(weights,"."),
                     names_transform = ymd,
                     values_to = paste0(layer_substr,"_",func,"_",weights))
      
      toc()
      
      
      if (layer_substr==layer_substrings[1]){
        out_df <- temp_df
      } else {
        out_df <- cbind(out_df,temp_df)
      }
      rm(temp_df)
    }
    
  } # end else
  toc()
  
  
  tic("Saved vector cast with geometry to an RDS")
  saveRDS(vector_cast,
          file = file.path(vector_cast_out_path,vector_cast_out_filename))
  toc()
  
  
  
  tic("Merged extracted polygons back to polygons df")
  out_df <- left_join(st_drop_geometry(vector_cast),out_df)
  
  toc()
  
  tic("Saved long panel df ")
  saveRDS(out_df,
          file = file.path(extracted_out_path,extracted_out_filename))
  toc()
  
  if (remove_files==TRUE) {
    rm(vector_cast,out_df)}
  else return(out_df)
  
  gc()
  
}
