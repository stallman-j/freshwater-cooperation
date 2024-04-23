# _______________________________#
# Environment
# Clean 02: Get River Distances
# 
# Stallman
# Started: 2023-10-23
# Last edited: 2024-04-08
#________________________________#



# Startup

  rm(list = ls())

  # https://cran.r-project.org/web/packages/riverdist/vignettes/riverdist_vignette.html

# bring in the packages, folders, paths ----
  
  home_folder <- file.path("P:","Projects","freshwater-cooperation")
  source(file.path(home_folder,"code","00_startup_master.R"))
  source(file.path(code_startup_project_specific,"get_river_distances.R")) # function for getting along-river distances
  source(file.path(code_startup_project_specific,"upstreammat_upper_triangle.R")) # function for taking the upper triangle of distance matrices rather than the entire thing
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    #stringr, # string operations
    countrycode, # country naming conversions
    sf, # vector spatial geometry
    ggrepel, # for jittering text in a plot
    riverdist, # calculating river distances
    rdhs, # getting DHS data
    elevatr, # for getting elevation from points
    RColorBrewer, # for getting gradients and colors
    parallel, # for parallelizing operations
    tidyverse,
    dplyr,
    tictoc # timing # more ability to customize to output to latex. use with kableExtra to output tables
    # to console, latex, Rmarkdown, html etc.
  )


  n_cores          <- 18 #detectCores(logical = TRUE) - 2

  
  # bring in data ----
  
  # path <- file.path(data_external_raw,"HydroSHEDS","HydroRIVERS_v10_af.gdb","HydroRIVERS_v10_af.gdb")
  # 
  # system.time(
  #   hydro_rivers <- st_read(dsn = path) %>% 
  #     st_transform(crs = equal_area_crs) %>% 
  #     rename(geometry = Shape) # need this to use riverdist package
  # )
  # # get all the main rivers and convert to a vector to lapply through
  # main_rivers_all <- hydro_rivers %>% st_drop_geometry() %>% select(MAIN_RIV) %>%
  #   unique() %>% as.vector() %>% .[[1]]
  # 
  # main_rivers_sizes <- hydro_rivers %>%
  #   st_drop_geometry() %>%
  #   group_by(MAIN_RIV) %>%
  #   summarise(n_segments = n()) %>%
  #   ungroup()
  # 
  # 
  # main_rivers_small <- hydro_rivers %>%
  #                           st_drop_geometry() %>%
  #                           group_by(MAIN_RIV) %>%
  #                           summarise(n_segments = n()) %>%
  #                           ungroup() %>%
  #                           filter(n_segments <= 200 & n_segments >=30) %>%
  #                           arrange(n_segments) %>%
  #                           select(MAIN_RIV) %>% unique() %>% as.vector() %>% .[[1]]
  # 
  
# select main rivers to go over ----
  
  singletons <- readRDS(file = file.path(data_external_clean,"merged","DHS_ERA5_HydroSHEDS","singletons_node-level.rds"))
  dyads      <- readRDS(file = file.path(data_external_clean,"merged","DHS_ERA5_HydroSHEDS","dyads_node-level.rds"))
  triads     <- readRDS(file = file.path(data_external_clean,"merged","DHS_ERA5_HydroSHEDS","triads_node-level.rds"))
  under_100  <-  readRDS(file = file.path(data_external_clean,"merged","DHS_ERA5_HydroSHEDS","river-distances_under-100-towns.rds"))
  
  main_rivers_singletons <- singletons %>% select(MAIN_RIV) %>% unique() %>% as.vector()%>% .[[1]]
  main_rivers_dyads <- dyads %>% select(MAIN_RIV) %>% unique() %>% as.vector() %>% .[[1]]
  main_rivers_triads <- triads %>% select(MAIN_RIV) %>% unique() %>% as.vector() %>% .[[1]]
  main_rivers_under_100 <- under_100 %>% select(MAIN_RIV) %>% unique() %>% as.vector() %>% .[[1]]
  


# get the DHS locations placed on river networks ----

  # examples and checking the function works ----
  
  # go randomly through the rivers and pick one out. If it's been picked out it'll tell you
  # 
  # # 11139531 has a DHS four rows; all at 0 distance 
  # 10856281 # has two 0 and 2 NA
  # 11123192: has two zeros, and a positive and a negative
  # 
  # 
  main_river <- 11139531 # a river already checked 
  main_river <- 10856281 # a long river, river network should be missing
  main_river <- 11123192
  erroneous_river_network_path <- file.path("E:","data","03_clean","HydroSHEDS","river-river")
  erroneous_hydro_rivers_units_path = file.path("E:","data","02_temp","HydroSHEDS","shape-files")
  
  #main_river <- sample(main_rivers_all, size = 1)
  
  #hydro_segments <- hydro_rivers %>% filter(MAIN_RIV==main_river)
  
  points_data_path = file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","river-points")
  filenames <- list.files(points_data_path)
  
  df = matrix(ncol = 2,
               nrow = length(n_files)) %>% as.data.frame()
  
  names(df) <- c("MAIN_RIV","n_rows")
  
  
  get_nrow <- function(filename) {
    
    current_points <- readRDS(file = file.path(points_data_path,filename))
    
    n_row <- nrow(current_points)
    
    return(n_row)
  }
  
  
  
  out_vec <- sapply(filenames, get_nrow)
  
  
out <- get_river_distances(main_river=main_river,
                              river_network_path      = file.path("E:","data","03_clean","HydroSHEDS","river_networks"),
                              hydro_rivers_units_path = file.path("E:","data","03_clean","HydroSHEDS","shape-files"))



get_river_distances(main_river=11524662) # should give an error, this is the one with 1886 current points

system.time(
get_river_distances(main_river=11344899) # this has 323 current points
)

get_river_distances(main_river=11527323) # this has 261 current points


# check the plotting 
# 
# MAIN_RIV 10865554 is a good one to test plotting
current_river_network <- readRDS(file = file.path("E:","data","03_clean","HydroSHEDS","river_networks",paste0("MAIN_RIV_10865554_cleaned_river_network.rds")))
dev.off()
plot(current_river_network)


# parallelize ----

tic(paste0("Get DHS and GLOW Intersections for triads"))


  # 12 cores uses 85-100% CPU and 50-60GB RAM, about right
  # running this on 8 cores over all the main_rivers_all (16352 rivers in Africa) used up 50-60% CPU and hovered around 27-32 GB of RAM

cl <- makeCluster(n_cores) # n_cores # runs low on RAM if hydro_rivers gets sent to too many places, try building up
clusterEvalQ(cl,library(sf)) # send these separately, clusterEvalQ(cl, fun) is the call format
clusterEvalQ(cl,library(riverdist))
clusterEvalQ(cl, library(dplyr))
clusterEvalQ(cl, library(countrycode))
#clusterEvalQ(cl, library(ggplot2))


clusterExport(cl, c("get_river_distances","upstreammat_upper_triangle"))
#clusterExport(cl, c("dhs_data"))
#clusterExport(cl, c("max_distance_to_snap"))
#clusterExport(cl, c("equal_area_crs"))


# for everywhere
#parLapply(cl, main_rivers_all, get_dhs_river_distances)

tic("Got distances for singletons")
parLapply(cl, main_rivers_singletons, get_river_distances)
toc()

tic("Got distances for dyads") 
parLapply(cl, main_rivers_dyads, get_river_distances)
toc()

tic("Got distances for triads")
parLapply(cl, main_rivers_triads, get_river_distances)
toc()

tic("Got distances for n_Towns < 100")
parLapply(cl, main_rivers_under_100, get_river_distances, max_current_points = 500)
toc()

# Got distances for n_Towns < 100: 8687.74 sec elapsed if stopped when max_current_points == 500


tic("Got distances for n_Towns < 100 and max_current_points == 1000 (| done for <500)")
parLapply(cl, main_rivers_under_100, get_river_distances, max_current_points = 1000)
toc()

# Got distances for n_Towns < 100: 58180.95 sec elapsed


tic("Got distances for n_Towns < 100 and max_current_points == 2000 (| done for <1000)")
parLapply(cl, main_rivers_under_100, get_river_distances, max_current_points = 2000)
toc()

#896556.36 sec elapsed;  10.37681 days


# 

# this is for all the DHS distances ----

# parLapply(cl,
#           main_rivers_all,
#           get_river_distances,
#           points_data_path = file.path("E:","data","02_temp","merged","DHS_HydroSHEDS","river-points"), # for just DHS
#           points_leading_string  = "DHS_MAIN_RIV_", # for just DHS      
#           river_network_path = file.path("E:","data","03_clean","HydroSHEDS","river_networks"),
#           ID_varname = "DHSID", # else "ID" for GLOW
#           checked_river_path = file.path("E:","data","02_temp","merged","DHS_HydroSHEDS","checked-main-rivers","dhs-river-distances"),
#           river_network_missing_path = file.path("E:","data","02_temp","HydroSHEDS","river-network-missing"),
#           dyad_distances_path = file.path("E:","data","02_temp","merged","DHS_HydroSHEDS","dyad-distances"),
#           distance_matrices_path = file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","distance-matrices"),
#           distance_matrices_flat_path = file.path("E:","data","02_temp","merged","DHS_HydroSHEDS","distance-matrices_flat"))

gc()
toc()

stopCluster(cl)

