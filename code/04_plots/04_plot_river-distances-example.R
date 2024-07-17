# _______________________________#
# Freshwater Cooperation
# Merge 02 b: Get River Distances
# 
# Stallman
# Started: 2024-07-15
# Last edited: 2024-07-15
#________________________________#



# Startup

  rm(list = ls())
  
  # https://cran.r-project.org/web/packages/riverdist/vignettes/riverdist_vignette.html
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    #stringr, # string operations
    countrycode, # country naming conversions
    sf, # vector spatial geometry
    riverdist, # calculating river distances
    tidyverse,
    stringr, # for string calculations
    dplyr,
    tictoc # timing 
  )


# bring in the packages, folders, paths ----

  home_folder <- file.path("P:","Projects","freshwater-cooperation")
  source(file.path(home_folder,"code","00_startup_master.R"))
  source(file.path(code_startup_project_specific,"get_river_distances.R")) # function for getting along-river distances

# paths ----
  points_data_path = file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","river-points")
  points_leading_string  = "DHS_GLOW_MAIN_RIV_"     
  river_network_path = file.path("E:","data","03_clean","HydroSHEDS","river_networks")
  checked_river_path = file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","checked-main-rivers","dhs-glow-distances")
  river_network_missing_path =  file.path("E:","data","02_temp","HydroSHEDS","river-network-missing")
  town_measurement_distances_path =         file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","town-measurement-distances")
  town_town_distances_path =      file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","town-town-distances")
  error_message_path              = file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","river-distances-errors")
  
# plot a river with just towns  ----

  # rivers of one town with width observations to the N and S:
  one_town_with_width <- c(11415732)
  
  main_river <- 11175613
  
  # bring in river network
  points_filename_string <- paste0(points_leading_string,main_river,"_river_points.rds")
  
  current_river_network <- readRDS(file = file.path(river_network_path,paste0("MAIN_RIV_",main_river,"_cleaned_river_network.rds")))
  
  current_points        <- readRDS(file = file.path(points_data_path,points_filename_string))
  
  towns_points <- current_points %>%
    dplyr::filter(type == "DHS_town")

  
  # #?riverdist::plot.rivernetwork
  plot(current_river_network, 
       segmentnum = FALSE,
       color = FALSE,
       linecol = "black")
  points(towns_points$X, towns_points$Y, pch = 2, col = "red")

  town_labels <- stringr::str_sub(towns_points$ID, start = -4)
  text(towns_points$X, towns_points$Y, labels = town_labels, col = "red")

  

# with both measurements and towns ----

  one_town_with_width <- c(11415732,10858034,10985870,1040750,11136381,11330752,11261663,11264882,11301586,11265161,11526875,10877437,11424526,11473423,11050157,10823409,11527323,11526992,11523760,11323986,11525623,11526486,11524662)
  two_towns_with_width <- c(11159794,10539005,11526898,11444434,11527409,11268453,10812849,11198530,11450721,11385893,11358588,11504845,11367659,10891584,11173909,11372098,)
  
  df <- readRDS(file = file.path(data_external_temp, "merged","DHS_GLOW_HydroSHEDS","towns_measurement_counts.rds"))
  
  
  # looked through one-town rivers
  # 
  for (riv in df$MAIN_RIV) {
  
    # subset the df to get the n_towns and n_measurements of this particular river
  
    current_n_towns <- df[df$MAIN_RIV==riv,]$n_towns
    current_n_measurements <- df[df$MAIN_RIV==riv,]$n_measurements
    
    if (current_n_towns > 1 & current_n_measurements > 0){
  main_river <- riv #11297282

  # bring in river network
  points_filename_string <- paste0(points_leading_string,main_river,"_river_points.rds")
  
  current_river_network <- readRDS(file = file.path(river_network_path,paste0("MAIN_RIV_",main_river,"_cleaned_river_network.rds")))
  
  current_points        <- readRDS(file = file.path(points_data_path,points_filename_string))
  towns_points <- current_points %>%
    dplyr::filter(type == "DHS_town")
  
  measurement_points <- current_points %>%
    dplyr::filter(type == "GLOW")
  

  #?riverdist::plot.rivernetwork
  plot(current_river_network, 
       #segmentnum = FALSE,
       color = FALSE,
       linecol = "black")
  # #showends(seg = 29, rivers = current_river_network)
  points(towns_points$X, towns_points$Y, pch = 2, col = "red")
  points(measurement_points$X, measurement_points$Y, pch = 0, col = "blue")
  # riverpoints(seg = towns_points_on_river$seg,vert = towns_points_on_river$vert, rivers = current_river_network, col = "red", pch = 17)
  town_labels <- stringr::str_sub(towns_points$ID, start = -4)
  
  
  measurement_labels <- stringr::str_sub(measurement_points$ID, start = -4)
  # 
  text(measurement_points$X, measurement_points$Y, labels = measurement_labels, col = "blue")
  text(towns_points$X, towns_points$Y, labels = town_labels, col = "red")
  
  

  title(paste0("River ",main_river))
  
  print(paste("Current River is",riv,"which has",current_n_towns,"towns and",current_n_measurements,"measurements."))
  
  question1 <- readline("Would you like to continue looking at plots? (Y/N) \n")
  if(regexpr(question1, 'y', ignore.case = TRUE) == 1){
    continue = TRUE
    next
  } else if (regexpr(question1, 'n', ignore.case = TRUE) == 1){
      break  
    
  }

    }
    
  } # end FOR loop over main_riv
