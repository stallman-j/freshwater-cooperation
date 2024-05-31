# _______________________________#
# Environment
# Merge 02 b: Get River Distances
# 
# Stallman
# Started: 2023-10-23
# Last edited: 2024-05-31
#________________________________#



# Startup

  rm(list = ls())

  # https://cran.r-project.org/web/packages/riverdist/vignettes/riverdist_vignette.html

# bring in the packages, folders, paths ----
  
  home_folder <- file.path("P:","Projects","freshwater-cooperation")
  source(file.path(home_folder,"code","00_startup_master.R"))
  source(file.path(code_startup_project_specific,"get_river_distances.R")) # function for getting along-river distances

  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    #stringr, # string operations
    countrycode, # country naming conversions
    sf, # vector spatial geometry
    riverdist, # calculating river distances
    parallel, # for parallelizing operations
    tidyverse,
    dplyr,
    tictoc # timing 
  )


  n_cores          <- 18 #detectCores(logical = TRUE) - 2

  
  # bring in data ----
  
  # if we want to examine which rivers to go over
  path <- file.path(data_external_raw,"HydroSHEDS","HydroRIVERS_v10_af.gdb","HydroRIVERS_v10_af.gdb")

  system.time(
    hydro_rivers <- st_read(dsn = path) %>%
      st_transform(crs = equal_area_crs) %>%
      rename(geometry = Shape) # need this to use riverdist package
  )
  # get all the main rivers and convert to a vector to lapply through
  main_rivers_all <- hydro_rivers %>% st_drop_geometry() %>% select(MAIN_RIV) %>%
    unique() %>% as.vector() %>% .[[1]]

  main_rivers_sizes <- hydro_rivers %>%
    st_drop_geometry() %>%
    group_by(MAIN_RIV) %>%
    summarise(n_segments = n()) %>%
    ungroup()


  main_rivers_small <- hydro_rivers %>%
                            st_drop_geometry() %>%
                            group_by(MAIN_RIV) %>%
                            summarise(n_segments = n()) %>%
                            ungroup() %>%
                            filter(n_segments <= 30 & n_segments >=1) %>%
                            arrange(n_segments) %>%
                            select(MAIN_RIV) %>% unique() %>% as.vector() %>% .[[1]]


# test a changed distance function
  
  # some examples of rivers
  #main_river <- 10865554
  #main_river <- 11139531  
  #main_river <- 10856281 # a super long river, river network should be missing
 
  main_river <- 11123192
  
  #erroneous_river_network_path <- file.path("E:","data","03_clean","HydroSHEDS","river-river")
  #erroneous_hydro_rivers_units_path = file.path("E:","data","02_temp","HydroSHEDS","shape-files")
  

  get_river_distances(main_river=11524662) # should give an error, this is the one with 1886 current points
  
  tic("Got a single river distance fully")
  get_river_distances(main_river=11344899) # this has 323 current points
  toc()
  # Got a single river distance fully: 1.66 sec elapsed
  
  get_river_distances(main_river=11527323) # this has 261 current points


# to check the plotting in detail, look in the get_river_distances function
# 
# MAIN_RIV 10865554 is a good one to test plotting
# current_river_network <- readRDS(file = file.path("E:","data","03_clean","HydroSHEDS","river_networks",paste0("MAIN_RIV_10865554_cleaned_river_network.rds")))
# dev.off()
# plot(current_river_network)


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


clusterExport(cl, c("get_river_distances"))


# for everywhere
#parLapply(cl, main_rivers_all, get_dhs_river_distances)


tic("Got distances for rivers of < 30 segments")
parLapply(cl, main_rivers_small, get_river_distances, max_current_points = 1000)
toc()


gc()
toc()

stopCluster(cl)

