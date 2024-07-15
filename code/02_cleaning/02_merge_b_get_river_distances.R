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


  # 18 cores put CPU at 100% try going lower
  # 14 cores put CPU around 95%, that's about right
  n_cores          <- 14 #detectCores(logical = TRUE) - 2

  
  # get all the rivers for which river points exist by looking at the filenames of river points
  # outputted from the get_river_points function
  

  path <- file.path(data_external_temp,"merged","DHS_GLOW_HydroSHEDS","river-points")
  

  main_rivers_list <- stringr::str_remove(list.files(path),"DHS_GLOW_MAIN_RIV_") %>%
                       stringr::str_remove("_river_points.rds")
  
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
  get_river_distances(main_river=11276981 ) 
  toc()
  
  tic("Got a single river distance fully")
  get_river_distances(main_river=main_rivers_small[100] ) 
  toc()
  # Got a single river distance fully: 1.66 sec elapsed
  
  get_river_distances(main_river=10009216) 


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
clusterEvalQ(cl, library(tictoc))

#clusterEvalQ(cl, library(ggplot2))


clusterExport(cl, c("get_river_distances"))


# for everywhere
#parLapply(cl, main_rivers_all, get_dhs_river_distances)


tic("Got distances for rivers of < 1000 points which have river points calculated already")
parLapply(cl, main_rivers_list, get_river_distances, max_current_points = 1000)
toc()


tic("Got distances for rivers of < 2000 & > 1000 points which have river points calculated already")
parLapply(cl, main_rivers_list, get_river_distances, max_current_points = 2000)
toc()

tic("Got distances for rivers of < 3000 & > 2000 points which have river points calculated already")
parLapply(cl, main_rivers_list, get_river_distances, max_current_points = 3000)
toc()

tic("Got distances for rivers of < 4000 & > 3000 points which have river points calculated already")
parLapply(cl, main_rivers_list, get_river_distances, max_current_points = 4000)
toc()

gc()
toc()

stopCluster(cl)

