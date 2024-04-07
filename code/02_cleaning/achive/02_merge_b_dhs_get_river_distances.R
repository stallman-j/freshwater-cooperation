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
  n_cores          <- 12 #detectCores(logical = TRUE) - 2
  #units_name       <- c("Uganda")
  equal_area_crs   <- "ESRI:102022"
  max_distance_to_snap <- 10000 # max distance at which to snap to a river; 10km b/c that's max perturbation
  surveyyear_start <- 2018 %>% as.character()
  surveyyear_end   <- 2020 %>% as.character()
  long_river_threshold <- 4000 
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
  
  
  
  path <- file.path(data_raw,"hydroSHEDS","HydroRIVERS_v10_af.gdb","HydroRIVERS_v10_af.gdb")
  
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
  
  # bring in a hydrorivers that's a lot smaller
  
  path <- file.path(data_external_clean,"HydroSHEDS","shape-files","SEN_hydrorivers.rds")
  
  system.time(
    hydro_rivers_limited <- readRDS(file = path)
  )
  
  main_rivers_limited <- hydro_rivers_limited %>% st_drop_geometry() %>% select(MAIN_RIV) %>%
    unique() %>% as.vector() %>% .[[1]]
  


# bring in data ----
  
  names(hydro_rivers)
  # [1] "HYRIV_ID"     "NEXT_DOWN"    "MAIN_RIV"     "LENGTH_KM"    "DIST_DN_KM"  
  # [6] "DIST_UP_KM"   "CATCH_SKM"    "UPLAND_SKM"   "ENDORHEIC"    "DIS_AV_CMS"  
  # [11] "ORD_STRA"     "ORD_CLAS"     "ORD_FLOW"     "HYBAS_L12"    "Shape_Length"
  # [16] "geometry" 
  
  # get all the main rivers and convert to a vector to lapply through
  main_rivers_all <- hydro_rivers %>% st_drop_geometry() %>% select(MAIN_RIV) %>%
    unique() %>% as.vector() %>% .[[1]]
  
  
  

# get the DHS locations placed on river networks ----
  
  # https://ryanpeek.org/2017-11-21-mapping-with-sf-part-3/

  # get_dhs_river_distances ----
  
  out_path      = file.path(data_external_temp,"DHS","GPS","merged")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)
  
  
  # requires in the environment:
  # gadm_data
  # hydro_rivers
  # dhs_data
  # max_distance_to_snap
  # 
  get_dhs_river_distances <- function(main_river,
                                          data_clean_path = file.path("E:","data","03_clean"),
                                          data_temp_path  = file.path("E:","data","02_temp"),
                                          river_network_path = file.path("E:","data","03_clean","HydroSHEDS","river_networks"),
                                          hydro_rivers_units_path = file.path("E:","data","03_clean","HydroSHEDS","shape-files"),
                                          gadm_union_shapefile_path = file.path("E:","data","03_clean","shape-files","GADM"),
                                          max_distance_to_snap = 10000,
                                          equal_area_crs = "ESRI:102022" # Albert Conic equal-area for Africa
                                      ){
    
    tryCatch( {
    # create output folders
    checked_river_path         <- file.path(data_temp_path,"merged","DHS_HydroSHEDS","checked-main-rivers","dhs-clusters")
    river_network_missing_path <- file.path(data_temp_path,"HydroSHEDS","river-network-missing")
    dyad_distances_path        <- file.path(data_temp_path,"merged","DHS_HydroSHEDS","dyad-distances")
    distance_matrices_path     <- file.path(data_temp_path,"merged","DHS_HydroSHEDS","distance-matices")
    hydro_rivers_units_missing_path <- file.path(data_temp_path,"HydroSHEDS","hydro_rivers_units_missing")
    
    paths_to_create <- c(checked_river_path,
                         river_network_missing_path,
                         dyad_distances_path,
                         distance_matrices_path,
                         gadm_union_shapefile_path
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
    
    # Stop and exit if there's an incorrectly specified path for the country-specific hydrorivers units, exit
    if (!dir.exists(hydro_rivers_units_path)) {
      
      stop(paste0("Path ",hydro_rivers_units_path," doesn't exist. You either need to create the country-specific river shapefiles or check that you wrote the path correctly."))
      
    }
    
    # if the current river network doesn't exist, create output that says the network is missing, and stop function
    if (!file.exists(file = file.path(data_clean_path,"HydroSHEDS","river_networks",paste0("MAIN_RIV_",main_river,"_cleaned_river_network.rds")))){
      
      saveRDS(main_river,file.path(river_network_missing_path,paste0("MAIN_RIV_",main_river,"_network_doesnt_exist.rds")))
      
      stop(paste0("Current river network for main_river ",main_river," doesn't exist. Create it first and come back."))
      
      
    } 
    # Stop if the river has already been checked, don't check it again
    if (file.exists(file = file.path(checked_river_path,paste0("MAIN_RIV_",main_river,"_already_checked.rds")))){
      
      stop(paste0("DHS cluster distances for main_river ",main_river," have already been calculated. No need to go through this again."))
      
    }
    
      # bring in river network
        
        current_river_network <- readRDS(file = file.path(river_network_path,paste0("MAIN_RIV_",main_river,"_cleaned_river_network.rds")))
        
        single_river <- hydro_rivers %>%
          dplyr::filter(MAIN_RIV == main_river) %>%
          dplyr::mutate(width = 1/as.numeric(ORD_CLAS))


        mouth_stretch <- single_river %>% filter(NEXT_DOWN ==0)
        # get polygon boundary of river network

        # a little extra distance around the river than what the maximum snap will be
        polygon_boundary <- single_river %>% st_buffer(10100) %>% st_convex_hull() %>% st_union()

        #     plot(st_geometry(polygon_boundary))

        # check which countries intersect
        intersecting_indices <- lengths(st_intersects(gadm_data, polygon_boundary))>0

        # get the iso3c of the countries involved
        intersecting_countries <- gadm_data[intersecting_indices,] %>% select(GID_0) %>%
          st_drop_geometry() %>% unique() %>% .[,1]

        # restrict GADM data to just those countries
        river_countries <- gadm_data %>%
          filter(GID_0 %in% intersecting_countries & continent == "Africa")

        countries <- unique(river_countries$GID_0)

        countries_DHS    <- countrycode(countries,
                                        origin = "iso3c",
                                        destination = "dhs")

        dhs_river_countries <- dhs_data %>%
          filter(DHSCC %in% countries_DHS)
        
        if (nrow(dhs_river_countries)==0){
          saveRDS(main_river,
                  file.path(checked_river_path,paste0("MAIN_RIV_",main_river,"_already_checked.rds")))
          
          stop(paste0("No DHS datasets intersect with MAIN_RIV ",main_river,"."))
        }
          
              river_polygons <- gadm_data %>% 
                filter(GID_0 %in% intersecting_countries & continent == "Africa")%>%
                group_by(GID_0) %>%
                summarize(geometry = st_union(geom)) %>%
                st_make_valid()
              
              
              #plot(st_geometry(river_polygons))
              
              
              # check if gadm shapefile that's the union of relevant countries exists. If it exists, read it in. If not, create it.
              
              if (file.exists(file.path(gadm_union_shapefile_path,paste0(paste(countries,collapse="_"),"_union_shapefile.rds")))) {
                
                units <- readRDS(file = file.path(gadm_union_shapefile_path,paste0(paste(countries,collapse="_"),"_union_shapefile.rds")))
                
              } else {
                units <- gadm_data %>% filter(GID_0 %in% countries) %>% st_union() %>% st_make_valid() 
                
                saveRDS(units,
                        file = file.path(gadm_union_shapefile_path,paste0(paste(countries,collapse="_"),"_union_shapefile.rds")))
              } # end ifelse over if the GADM file exists then bring it in, otherwise create it
              
              
              if (!file.exists( file.path(hydro_rivers_units_path,paste0(paste(countries,collapse = "_"),"_hydrorivers.rds")))) {
                
                # flag that the units are missing
                saveRDS(c(countries),
                        file.path(hydro_rivers_units_missing_path,paste0(paste(countries,collapse = "_"),"hydro_rivers_units_missing.rds")))
                
                stop(paste0("HydroRIVERS file for countries ",paste(countries,collapse = " "), "is missing. Create elsewhere first (it takes a while) then come back."))
                
              } 
              
                
                hydro_rivers_units <- readRDS(file = file.path(hydro_rivers_units_path,paste0(paste(countries,collapse = "_"),"_hydrorivers.rds")))
                
                
                
                # get closest points of the DHS to the rivers
                
                # this is a fast operation
                nearest_indices <- st_nearest_feature(dhs_data,hydro_rivers_units)
                
                # this is also fast
                closest_points <- dhs_data %>%
                  mutate(my_linestring = st_nearest_points(dhs_data,hydro_rivers_units[nearest_indices,], pairwise = TRUE),
                         closest_point = st_cast(my_linestring, 'POINT')[seq(2, nrow(.)*2, 2)],
                         distance_to_river      = st_distance(dhs_data, hydro_rivers_units[nearest_indices,], by_element = TRUE),
                         snapped_point_cond = st_sfc(ifelse(as.numeric(distance_to_river) <= max_distance_to_snap, st_geometry(closest_point),geometry),crs = st_crs(equal_area_crs))
                  ) %>%
                  cbind(st_drop_geometry(hydro_rivers_units[nearest_indices,]))
                
                # add in X Y coordinates in projected form
                closest_points$X <- st_coordinates(closest_points)[,1]
                closest_points$Y <- st_coordinates(closest_points)[,2]
                
                current_points <- closest_points %>% filter(MAIN_RIV == main_river)
                
              
              # save dyad distances
              
              if (nrow(current_points)==0) {
                saveRDS(main_river,
                        file.path(checked_river_path,paste0("MAIN_RIV_",main_river,"_already_checked.rds")))
                
                stop(paste0("No points in DHS data match with MAIN_RIV ",main_river))
                
                
              }
                
                points_on_river <- xy2segvert(x = current_points$X,
                                              y = current_points$Y,
                                              rivers = current_river_network)
                
                distance_mat <- upstreammat(seg = points_on_river$seg,
                                            vert = points_on_river$vert,
                                            ID = current_points$DHSID,
                                            rivers = current_river_network,
                                            flowconnected = TRUE)
                
                # Calculating distances for current river points: for  14376.14 sec elapsed for 3448 segments
                # Calculating distances for current river points: 640.58 sec elapsed for 1448 segments
                
                DHS_own_ID_names  <- rownames(distance_mat)
                DHS_pair_ID_names <- colnames(distance_mat)
                
                # borrowed from dist2list from vmikk/metagMisc on Github
                
                dat <- as.data.frame(distance_mat)
                
                rownames(dat) <- rownames(distance_mat)
                value         <- stack(dat)$values
                rnames        <- rownames(dat)
                namecol       <- expand.grid(rnames,rnames)
                colnames(namecol) <- c("row","col")
                
                # (i,j) positive value if 2nd location is upstream of the first; negative if downstream
                # positive value if col upstream of row 
                
                dyad_distances <- data.frame(namecol, value)
                
                dyad_upstream  <- dyad_distances %>%
                  filter(value >= 0) %>%
                  rename(downstream = row,
                         upstream   = col,
                         distance_m = value)
                
                out_path <- 
                saveRDS(object = dyad_upstream,
                        file =file.path(dyad_distances_path,paste0("DHS_MAIN_RIV_",main_river,"_dyad_distances.rds")))
                
                saveRDS(object = dyad_distances,
                        file = file.path(distance_matrices_path,paste0("DHS_MAIN_RIV_",main_river,"_distances_matrix.rds")))
                
                saveRDS(main_river,
                        file.path(checked_river_path,paste0("MAIN_RIV_",main_river,"_already_checked.rds")))
}, # main part of the function for tryCatch

error = function(e) {
  # code executed in event of an error
  return(NA) },
warning = function(w) {
  # code executed in event of a warning
  return(NA)
}


    ) # end tryCatch
  } # end function
  

  
  
  # examples and checking the function works ----
  
  # go randomly through the rivers and pick one out. If it's been picked out it'll tell you
  # 
  main_river <- 10324907 # a river already checked 
  main_river <- 10071782 # a long river, river rnetwork should be missing
  erroneous_river_network_path <- file.path("E:","data","03_clean","HydroSHEDS","river-river")
  erroneous_hydro_rivers_units_path = file.path("E:","data","02_temp","HydroSHEDS","shape-files")
  
  main_river <- sample(main_rivers_all, size = 1)
  
  hydro_segments <- hydro_rivers %>% filter(MAIN_RIV==main_river)
  
out <- get_dhs_river_distances(main_river=10428478,
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
parLapply(cl, main_rivers_all, get_dhs_river_distances)

stopCluster(cl)


gc()
toc()



