# requires the following data in environment: hydro_rivers
#' @param main_river hydrorivers ID
#' @param data_clean_path the place to put cleaned data
#' @param points_data_path where the points data lives
#' @param points_leading_string header for the points data, assumes the points data name is of form paste0(points_leading_string,main_river,"_river_points.rds")
#' @param river_network_path where the river network lives
#' @param river_network_missing_path where to log that the river network is currently missing
#' @param checked_river_path where to log that the river distances have been obtained
#' @param river_network_missing_path path to log that the river network is missing
#' @param town_measurement_distances_path place to put the distance matrices for distances between a town and a river width measurement location
#' @param town_town_distances_path file path of where to put the distance matrices for (on-river) distances between towns
#' @export

get_river_distances <- function(main_river,# 10865554 has 10 towns and 8 width measurements, good for an example
                                max_current_points = 1000, # the max is 1886 for triads and below; biggest 30-40 are above 200
                                points_data_path = file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","river-points"), 
                                points_leading_string  = "DHS_GLOW_MAIN_RIV_", #"DHS_MAIN_RIV_", # for just DHS      
                                river_network_path = file.path("E:","data","03_clean","HydroSHEDS","river_networks"),
                                checked_river_path = file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","checked-main-rivers","dhs-glow-distances"),
                                river_network_missing_path =  file.path("E:","data","02_temp","HydroSHEDS","river-network-missing"),
                                town_measurement_distances_path =         file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","town-measurement-distances"),
                                town_town_distances_path =      file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","town-town-distances")
                                  
){
  
  tryCatch( {
    
    points_filename_string <- paste0(points_leading_string,main_river,"_river_points.rds")
    
    paths_to_create <- c(checked_river_path,
                         river_network_missing_path,
                         town_measurement_distances_path,
                         town_town_distances_path,
                         river_network_missing_path
    )
    
    for (path in paths_to_create){
      
      if (!dir.exists(path)) dir.create(path, recursive = TRUE)
      
    }
    
    # run through conditions that should stop the function 
    # 
    # Stop if river network directory doesn't exist
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
    
  # 1) block out the current points which are type DHS_town
  # 2) for each of these, calculate the upstream distance to all other points
    
  # plot to test 
  # main_riv <- 10865554 # has 8 measurement obs and 10 towns
  # main_riv <- 11527323 # has 216 total obs
  
    towns_points <- current_points %>%
                    dplyr::filter(type == "DHS_town")
    
    measurement_points <- current_points %>%
                          dplyr::filter(type == "GLOW")

    
    # get the segment and vertex of the river network for all the towns
    towns_points_on_river <- riverdist::xy2segvert(x = towns_points$X,
                                                   y = towns_points$Y,
                                                   rivers = current_river_network)
    
    # for each town, calculate the upstream distance to 
    measurement_points_on_river <- riverdist::xy2segvert(x = measurement_points$X,
                                  y = measurement_points$Y,
                                  rivers = current_river_network)
    
    # plot to see that everything makes sense
    
    # m <- 1
    # t <- 1
    # 
    # # use plots to check that measurements are going all right
    # plot(current_river_network)
    # #showends(seg = 29, rivers = current_river_network)
    # #points(towns_points$X, towns_points$Y, pch = 2, col = "red")
    # riverpoints(seg = towns_points_on_river$seg,vert = towns_points_on_river$vert, rivers = current_river_network, col = "red", pch = 17)
    # #points(measurement_points$X, measurement_points$Y, pch = 0, col = "blue")
    # riverpoints(seg = measurement_points_on_river$seg,vert = measurement_points_on_river$vert, rivers = current_river_network, col = "blue", pch = 15)
    # 
    # 
    # riverpoints(seg = towns_points_on_river[t,]$seg,vert = towns_points_on_river[t,]$vert, rivers = current_river_network, col = "black",pch = 17)
    # riverpoints(seg = measurement_points_on_river[m,]$seg,vert = measurement_points_on_river[m,]$vert, rivers = current_river_network, col = "black", pch = 15)
    # 
    # 
    # https://cran.r-project.org/web/packages/riverdist/vignettes/riverdist_vignette.html
    #  In the flow-connected case, upstream() returns the network distance as positive if the second location is upstream of the first, and negative if downstream
    
    # for each town, get the distance from each measurement point
    
    
    if (nrow(measurement_points)>0 & nrow(towns_points)>0) {
    tic("Got towns-measurements distances")
    for (t in 1:nrow(towns_points_on_river)) {
      
      
      dists_temp <- data.frame(town_ID = towns_points$ID[t],
                               measurement_ID = measurement_points$ID,
                               distance = NA,
                               MAIN_RIV = main_river)
      

      for (m in 1:nrow(measurement_points_on_river)){
        
    dists_temp[m,"distance"] <- upstream(startseg = towns_points_on_river[t,]$seg, # row t, segment column
                     endseg   = measurement_points_on_river[m,]$seg, # row m, segment column
                     startvert = towns_points_on_river[t,]$vert,
                     endvert   = measurement_points_on_river[m,]$vert,
                     rivers = current_river_network,
                     flowconnected = TRUE)
    
    
    } # end for loop over towns t
    
      if (t==1) {
    dists_df <- dists_temp
      } else {
        dists_df <- rbind(dists_df,dists_temp)
      }
    } # end forloop over measurement points m
    
      saveRDS(object = dists_df,
              file =file.path(town_measurement_distances_path,paste0(points_leading_string,main_river,"_town-measurement-distances.rds")))
    
      toc()
} # end if-else statement that only asks for measurement-town distances if there are both measurement and towns 
      
# get distances for town to town

      # https://cran.r-project.org/web/packages/riverdist/vignettes/riverdist_vignette.html
      #  In the flow-connected case, upstream() returns the network distance as positive if the second location is upstream of the first, and negative if downstream
      
    # make a toy version
    
    # towns_points_test <- towns_points[1:3,]
    # towns_points_on_river_test <- riverdist::xy2segvert(x = towns_points_test$X,
    #                                                      y = towns_points_test$Y,
    #                                                      rivers = current_river_network)  
    
    # testing plots
    # t <- 1
    # m <- 2
    # 
    # plot(current_river_network)
    # #showends(seg = 29, rivers = current_river_network)
    # #points(towns_points$X, towns_points$Y, pch = 2, col = "red")
    # riverpoints(seg = towns_points_on_river$seg,vert = towns_points_on_river$vert, rivers = current_river_network, col = "red", pch = 17)
    # #points(measurement_points$X, measurement_points$Y, pch = 0, col = "blue")
    # riverpoints(seg = measurement_points_on_river$seg,vert = measurement_points_on_river$vert, rivers = current_river_network, col = "blue", pch = 15)
    # 
    # 
    # riverpoints(seg = towns_points_on_river[t,]$seg,vert = towns_points_on_river[t,]$vert, rivers = current_river_network, col = "black",pch = 17)
    # riverpoints(seg = measurement_points_on_river[m,]$seg,vert = measurement_points_on_river[m,]$vert, rivers = current_river_network, col = "black", pch = 15)

    
      # for each town, get the distance from each measurement point
    
    if (nrow(towns_points)>0) {
      
      tic("Got distances for T squared divided by 2")
      for (t in 1:nrow(towns_points_on_river)) {
        
        
        dists_temp <- data.frame(town_a_ID = towns_points$ID[t],
                                 town_b_ID = towns_points$ID,
                                 distance = NA,
                                 MAIN_RIV = main_river)
        
        
        for (m in 1:nrow(towns_points_on_river)){
          
        if (m==t){ # if it's the same town, distance will be zero

          dists_temp[m,"distance"] <- 0
        }
          if (m < t) { # just delete the lower diagonal observations ex post
            dists_temp[m,"distance"] <- "to_delete"
          }
          
          if (m > t) { # this will give the "upper triangle" of a distance matrix if t is rows and m columns
          dists_temp[m,"distance"] <- upstream(startseg = towns_points_on_river[t,]$seg, # row t, segment column
                                               endseg   = towns_points_on_river[m,]$seg, # row m, segment column
                                               startvert = towns_points_on_river[t,]$vert,
                                               endvert   = towns_points_on_river[m,]$vert,
                                               rivers = current_river_network,
                                               flowconnected = TRUE)
          
          } # end if-else if m > t then take distance
      
        } # end for loop over towns t
        
        if (t==1) {
          dists_df <- dists_temp
        } else {
          dists_df <- rbind(dists_df,dists_temp)
        }
      } # end forloop over measurement points m
      
    # NOTE: This also removes the NA obs (which are where the obs are not flow connected)
    dists_df <- dists_df %>%
                dplyr::filter(distance != "to_delete") %>%
                dplyr::mutate(distance = as.numeric(distance))
    
    toc()
    } # end if nrow(towns_points)>0 then take town distances
    
    # There are cases where this is faster to just go through all the town-town distances, esp. with small river networks
    # 
    # tic("Got distances for T squared")
    # for (t in 1:nrow(towns_points_on_river)) {
    #   
    #   
    #   dists_temp <- data.frame(town_a_ID = towns_points$ID[t],
    #                            town_b_ID = towns_points$ID,
    #                            distance = NA,
    #                            MAIN_RIV = main_river)
    #   
    #   
    #   for (m in 1:nrow(towns_points_on_river)){
    #     
    # 
    #       dists_temp[m,"distance"] <- upstream(startseg = towns_points_on_river[t,]$seg, # row t, segment column
    #                                            endseg   = towns_points_on_river[m,]$seg, # row m, segment column
    #                                            startvert = towns_points_on_river[t,]$vert,
    #                                            endvert   = towns_points_on_river[m,]$vert,
    #                                            rivers = current_river_network,
    #                                            flowconnected = TRUE)
    #     
    #     
    #   } # end for loop over towns t
    #   
    #   if (t==1) {
    #     dists_df <- dists_temp
    #   } else {
    #     dists_df <- rbind(dists_df,dists_temp)
    #   }
    # } # end forloop over measurement points m
    # 
    # 
    # toc()
    
    
    saveRDS(object = dists_df,
            file = file.path(town_town_distances_path,paste0(points_leading_string,main_river,"_town-town-distances.rds")))
    
    
    
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
