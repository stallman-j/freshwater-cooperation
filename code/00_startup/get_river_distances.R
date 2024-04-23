# requires the following data in environment: hydro_rivers
#' @param main_river hydrorivers ID
#' @param data_clean_path the place to put cleaned data
#' @param points_data_path where the points data lives
#' @param points_leading_string header for the points data, assumes the points data name is of form paste0(points_leading_string,main_river,"_river_points.rds")
#' @param river_network_path where the river network lives
#' @param river_network_missing_path where to log that the river network is currently missing
#' @param checked_river_path where to log that the river distances have been obtained
#' @export

get_river_distances <- function(main_river,
                                max_current_points = 1000, # the max is 1886 for triads and below; biggest 30-40 are above 200
                                points_data_path = file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","river-points"), 
                                #file.path("E:","data","02_temp","merged","DHS_HydroSHEDS","river-points"), # for just DHS
                                points_leading_string  = "DHS_GLOW_MAIN_RIV_", #"DHS_MAIN_RIV_", # for just DHS      
                                river_network_path = file.path("E:","data","03_clean","HydroSHEDS","river_networks"),
                                ID_varname = "ID", #"DHSID", # else "ID" for GLOW
                                checked_river_path = file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","checked-main-rivers","dhs-glow-distances"),
                                #file.path("E:","data","02_temp","merged","DHS_HydroSHEDS","checked-main-rivers","dhs-river-distances"),
                                river_network_missing_path =  file.path("E:","data","02_temp","HydroSHEDS","river-network-missing"),
                                dyad_distances_path =         file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","dyad-distances"),
                                #file.path("E:","data","02_temp","merged","DHS_HydroSHEDS","dyad-distances"),
                                distance_matrices_path =      file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","distance-matrices"),
                                distance_matrices_flat_path = file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","distance-matrices_flat")
                                #file.path("E:","data","02_temp","merged","DHS_HydroSHEDS","distance-matrices_flat")
                                  
){
  
  tryCatch( {
    
    points_filename_string <- paste0(points_leading_string,main_river,"_river_points.rds")
    
    paths_to_create <- c(checked_river_path,
                         river_network_missing_path,
                         dyad_distances_path,
                         distance_matrices_path,
                         distance_matrices_flat_path,
                         river_network_missing_path
    )
    
    for (path in paths_to_create){
      
      if (!dir.exists(path)) dir.create(path, recursive = TRUE)
      
    }
    
    # run through conditions that should stop the function 
    # 
    # Stop if riverr network directory doesn't exist
    if (!dir.exists(river_network_path)) {
      
      stop(paste0("River network directory in ",river_network_path," doesn't exist. You either need to create the river networks or check that you wrote the path correctly."))
      
    }
    
    # if the current river network doesn't exist, create output that says the network is missing, and stop function
    if (!file.exists(file = file.path(river_network_path,paste0("MAIN_RIV_",main_river,"_cleaned_river_network.rds")))){
      
      saveRDS(main_river,file.path(river_network_missing_path,paste0("MAIN_RIV_",main_river,"_network_doesnt_exist.rds")))
      
      stop(paste0("Current river network for main_river ",main_river," doesn't exist. Create it first and come back."))
      
    } 
    # Stop if the river has already been checked, don't check it again
    if (file.exists(file = file.path(checked_river_path,paste0("MAIN_RIV_",main_river,"_already_checked.rds")))){
      
      stop(paste0("The distances you wanted for main_river ",main_river," have already been calculated. No need to go through this again."))
      
    }
    
    # Stop if the river has already been checked, don't check it again
    if (!file.exists(file = file.path(points_data_path,points_filename_string))){
      
      stop(paste0("The closest points to ",main_river," have not yet been calculated. Go through a get_river_points function first to get these points."))
      
    }
    
    # bring in river network
    
    current_river_network <- readRDS(file = file.path(river_network_path,paste0("MAIN_RIV_",main_river,"_cleaned_river_network.rds")))
    
    current_points        <- readRDS(file = file.path(points_data_path,points_filename_string))
    
    if (nrow(current_points)>max_current_points){
      
      stop(paste0("There are ",nrow(current_points)," rows in the current points data - too many to bother with for now."))
      
    }
    
    # rename the ID so we can keep it
    names(current_points)[names(current_points) == ID_varname] <- "ID"
    
    points_on_river <- xy2segvert(x = current_points$X,
                                  y = current_points$Y,
                                  rivers = current_river_network)
    
    # distance_mat <- upstreammat(seg = points_on_river$seg,
    #                             vert = points_on_river$vert,
    #                             ID = current_points$ID,
    #                             rivers = current_river_network,
    #                             flowconnected = TRUE)
    
    distance_mat <- upstreammat_upper_triangle(seg = points_on_river$seg,
                                                 vert = points_on_river$vert,
                                                 ID = current_points$ID,
                                                 rivers = current_river_network,
                                                 flowconnected = TRUE)
    
    #own_ID_names  <- rownames(distance_mat)
    #pair_ID_names <- colnames(distance_mat)
    
    # borrowed from dist2list from vmikk/metagMisc on Github
    
    dat <- as.data.frame(distance_mat)
    
    rownames(dat) <- rownames(distance_mat)
    value         <- stack(dat)$values
    rnames        <- rownames(dat)
    namecol       <- expand.grid(rnames,rnames)
    colnames(namecol) <- c("row","col")
    
    # (i,j) positive value if 2nd location is upstream of the first; negative if downstream
    # positive value if col upstream of row 
    
    dyad_distances <- data.frame(namecol, value)%>%
      rename(downstream = row,
             upstream   = col,
             distance_m = value)
    
    dyad_upstream  <- dyad_distances %>%
      filter(value >= 0) 
    
   
      saveRDS(object = dyad_upstream,
              file =file.path(dyad_distances_path,paste0(points_leading_string,main_river,"_dyad_distances.rds")))
    
    saveRDS(object = dyad_distances,
            file = file.path(distance_matrices_flat_path,paste0(points_leading_string,main_river,"_distances_matrix_flat.rds")))
    
    saveRDS(object = distance_mat,
            file = file.path(distance_matrices_path,paste0(points_leading_string,main_river,"_distances_matrix.rds")))
    
    
    saveRDS(main_river,
            file.path(checked_river_path,paste0("MAIN_RIV_",main_river,"_already_checked.rds")))
  }, # main part of the function for tryCatch

  error = function(e) {
    # code executed in event of an error
    return("error") },
  warning = function(w) {
    # code executed in event of a warning
    return("warning")
  }


  ) # end tryCatch
} # end function
