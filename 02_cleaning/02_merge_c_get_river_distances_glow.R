# _______________________________#
# Environment
# Clean 02: Merge DHS and GPS Data
# 
# Stallman
# Started: 2023-10-23
# Last edited: 2023-11-13
# Edit: added get elevation to merge function merge_dhs_gps
#________________________________#



# Startup

  rm(list = ls())

  # https://cran.r-project.org/web/packages/riverdist/vignettes/riverdist_vignette.html

# bring in the packages, folders, paths ----
  
  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))
  #source(file.path(code_startup_general,"merge_dhs_gps.R")) # function for merging dhs and gps data
  source(file.path("P:","Projects","coding","r-code","00_startup","get_river_distances"))
  
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
  
  #remove.packages("stringr")
  #install.packages("stringr")
  
  level <- 1
  dhs_gps_filename <- "UGGE7AFL" # # UGGE7AFL and #UGGC7BFL
  
  country          <- "UG"
  n_cores          <- 18 #detectCores(logical = TRUE) - 2
  #units_name       <- c("Uganda")
  equal_area_crs   <- "ESRI:102022"
  max_distance_to_snap <- 10000 # max distance at which to snap to a river; 10km b/c that's max perturbation
  surveyyear_start <- 2018 %>% as.character()
  surveyyear_end   <- 2020 %>% as.character()
  long_river_threshold <- 100
# bring in hydrorivers ----
  
  dhs_data <- readRDS(file = file.path(data_external_temp,"DHS","GPS","merged","all_Africa_DHS_GPS.rds")) %>%
              st_transform(crs = equal_area_crs)
  
  
  countries <- dhs_data$DHSCC %>% unique()
  
  
  gadm_in_path <- file.path(data_external_clean,"GADM","global")
  
  gadm_in_filename <- paste0("GADM_global_ADM_",level,".rds")
  tic("Bringing in GADM data")
  gadm_data  <- readRDS(file.path(gadm_in_path,gadm_in_filename)) %>%
    st_transform(crs = equal_area_crs)
  
  toc()
  gadm_data$continent <- countrycode(gadm_data$GID_0,
                                     origin = "iso3c",
                                     destination = "continent")
  
  
  
  path <- file.path(data_external_raw,"HydroSHEDS","HydroRIVERS_v10_af.gdb","HydroRIVERS_v10_af.gdb")
  
  system.time(
    hydro_rivers <- st_read(dsn = path) %>% 
      st_transform(crs = equal_area_crs) %>% 
      rename(geometry = Shape) # need this to use riverdist package
  )
  
  # without st_transform
  # user  system elapsed 
  # 1.83    1.47   31.17
  # 
  # with st_transform
  # user  system elapsed 
  # 7.39    2.53   40.36

  

# bring in data ----
  
  names(hydro_rivers)
  # [1] "HYRIV_ID"     "NEXT_DOWN"    "MAIN_RIV"     "LENGTH_KM"    "DIST_DN_KM"  
  # [6] "DIST_UP_KM"   "CATCH_SKM"    "UPLAND_SKM"   "ENDORHEIC"    "DIS_AV_CMS"  
  # [11] "ORD_STRA"     "ORD_CLAS"     "ORD_FLOW"     "HYBAS_L12"    "Shape_Length"
  # [16] "geometry" 
  
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
                            filter(n_segments <= 200 & n_segments >=30) %>%
                            arrange(n_segments) %>%
                            select(MAIN_RIV) %>% unique() %>% as.vector() %>% .[[1]]
  



  # https://ryanpeek.org/2017-11-21-mapping-with-sf-part-3/

  # get_dhs_river_distances ----
  
  out_path      = file.path(data_external_temp,"DHS","GPS","merged")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)
  
  
  # requires in the environment:
  # gadm_data
  # hydro_rivers
  # dhs_data

    

  
  
  # examples and checking the function works ----
  
  # go randomly through the rivers and pick one out. If it's been picked out it'll tell you
  # 
  main_river <- 10324907 # a river already checked 
  main_river <- 10071782 # a long river, river rnetwork should be missing
  main_river <- 10111017
  erroneous_river_network_path <- file.path("E:","data","03_clean","HydroSHEDS","river-river")
  erroneous_hydro_rivers_units_path = file.path("E:","data","02_temp","HydroSHEDS","shape-files")
  
  main_river <- sample(main_rivers_all, size = 1)
  
  hydro_segments <- hydro_rivers %>% filter(MAIN_RIV==main_river)
  
out <- get_dhs_river_distances(main_river=main_river,
                              river_network_path      = file.path("E:","data","03_clean","HydroSHEDS","river_networks"),
                              hydro_rivers_units_path = file.path("E:","data","03_clean","HydroSHEDS","shape-files"))


# check the plotting 
# 
# MAIN_RIV 10865554 is a good one to test plotting
current_river_network <- readRDS(file = file.path("E:","data","03_clean","HydroSHEDS","river_networks",paste0("MAIN_RIV_10865554_cleaned_river_network.rds")))
dev.off()
plot(current_river_network)

# if you run through the function manually you'll get these
# # current_points and points_on_river
# points(current_points$X,
#        current_points$Y,
#        pch = 16,
#        col = "red")
# 
# riverpoints(seg = points_on_river$seg,
#             vert = points_on_river$vert,
#             rivers = current_river_network,
#             pch = 15,
#             col = "blue")

  # [1] "MAIN_RIV 10508677 has 3388 segments."
  # Checking which countries intersect with the river: 4.44 sec elapsed
  # [1] "Current river network for main_river10508677 already exists. Reading it in"
  # getting closest points: 0.34 sec elapsed
  # Calculating distances for current river points: 15540.14 sec elapsed
  #   


# parallelize ----




tic(paste0("Get DHS intersections with all rivers"))

  # 20 cores put CPU at 100% and memory at 75 even with rivers of length 20 or less; that's too many cores
  # 18 cores: CPU was at 60-70%, RAM around 60G for small rivers (<20 segments)
  # 16 cores puts CPU close to 100% and memory at about 80, probably not the best
  # 14 cores used 100% cpu with other processes, try 12
  # 12 cores uses 85-100% CPU and 50-60GB RAM, about right
  # running this on 8 cores over all the main_rivers_all (16352 rivers in Africa) used up 50-60% CPU and hovered around 27-32 GB of RAM

cl <- makeCluster(n_cores) # n_cores # runs low on RAM if hydro_rivers gets sent to too many places, try building up
clusterEvalQ(cl,library(sf)) # send these separately, clusterEvalQ(cl, fun) is the call format
clusterEvalQ(cl,library(riverdist))
clusterEvalQ(cl, library(dplyr))
clusterEvalQ(cl, library(countrycode))
clusterEvalQ(cl, library(ggplot2))


clusterExport(cl, c("hydro_rivers","gadm_data")) # this is a big export b/c the hydro_rivers is huge
clusterExport(cl, c("dhs_data"))
#clusterExport(cl, c("max_distance_to_snap"))
#clusterExport(cl, c("equal_area_crs"))


# for a limited single country
#parLapply(cl, main_rivers_limited, get_dhs_river_distances)

# for everywhere
#parLapply(cl, main_rivers_all, get_dhs_river_distances)

parLapply(cl, main_rivers_small, get_dhs_river_distances)




gc()
toc()

stopCluster(cl)

