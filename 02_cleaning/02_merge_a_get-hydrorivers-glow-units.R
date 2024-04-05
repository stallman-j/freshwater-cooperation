# _______________________________#
# Environment
# Clean 02: Merge DHS and GPS Data
# 
# Stallman
# Started: 2023-10-23
# Last edited: 2023-11-13
# Edit: added get elevation to merge function merge_dhs_gps
#________________________________#

## THIS IS DONE! YAY
## 2024-01-31 CHECKED
## 2024-02-11 added GLOW units 
# Startup

  rm(list = ls())

  # https://cran.r-project.org/web/packages/riverdist/vignettes/riverdist_vignette.html

# bring in the packages, folders, paths ----
  
  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))
  #source(file.path(code_startup_general,"merge_dhs_gps.R")) # function for merging dhs and gps data

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    stringr, # string operations
    countrycode, # country naming conversions
    sf, # vector spatial geometry
    ggrepel, # for jittering text in a plot
    riverdist, # calculating river distances
    rdhs, # getting DHS data
    elevatr, # for getting elevation from points
    RColorBrewer, # for getting gradients and colors
    parallel, # for parallelizing operations
    tidyverse,
    tictoc # timing # more ability to customize to output to latex. use with kableExtra to output tables
    # to console, latex, Rmarkdown, html etc.
  )

  level <- 1
  dhs_gps_filename <- "UGGE7AFL" # # UGGE7AFL and #UGGC7BFL
  
  country          <- "UG"
  n_cores          <- 23 #detectCores(logical = TRUE) - 2
  #units_name       <- c("Uganda")
  equal_area_crs   <- "ESRI:102022"
  max_distance_to_snap <- 10000 # max distance at which to snap to a river, really high at this point; 10km b/c that's max perturbation
  long_river_threshold <- 4000 

# bring in hydrorivers ----
  
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
  
  system.time(
    river_locations <- readRDS(file = file.path(data_external_temp,"GLOW_global-long-term-river-width",
                                                "africa_river_width_locations_equal_area.rds"))
    
  )
  
  # without st_transform
  # user  system elapsed 
  # 1.83    1.47   31.17
  # 
  # with st_transform
  # user  system elapsed 
  # 7.39    2.53   40.36
  
# for all rivers, find which countries they cross ----
  
  # get all the main rivers and convert to a vector to loop through
  main_rivers_all <- hydro_rivers %>% st_drop_geometry() %>% select(MAIN_RIV) %>%
    unique() %>% as.vector() %>% .[[1]]
  
# for each main river, find which countries it intersects, and make a file that says to create the file ----

# this is very slow, would need to paralellize... or can just run overnight!
  
  countries_path <- file.path(data_external_clean,"HydroSHEDS","countries-sharing-a-river")
  
  if (!dir.exists(countries_path)) dir.create(countries_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  
get_hydro_river_countries <- function(main_river,
                                      data_clean_path = file.path("E:","data","03_clean")){
  
  single_river <- hydro_rivers %>%
    dplyr::filter(MAIN_RIV == main_river) %>%
    dplyr::mutate(width = 1/as.numeric(ORD_CLAS))
  
  polygon_boundary <- single_river %>% st_buffer(10100) %>% st_convex_hull() %>% st_union()
  
  intersecting_indices <- lengths(st_intersects(gadm_data, polygon_boundary))>0
  
  # get the iso3c of the countries involved
  intersecting_countries <- gadm_data[intersecting_indices,] %>% select(GID_0) %>%
    st_drop_geometry() %>% unique() %>% .[,1]
  
  # restrict GADM data to just those countries
  river_countries <- gadm_data %>%
    filter(GID_0 %in% intersecting_countries & continent == "Africa")
  
  countries <- unique(river_countries$GID_0)
  
  if (!file.exists( file.path(data_clean_path,"HydroSHEDS","countries-sharing-a-river",paste0(paste(countries,collapse = "_"),"_has-common-river.rds")))) {
    
    # flag that the units are missing
    saveRDS(c(countries),
            file.path(data_clean_path,"HydroSHEDS","countries-sharing-a-river",paste0(paste(countries,collapse = "_"),"_has-common-river.rds")))
  
  }
  
  rm(single_river)
  }
  
  
# parallelize ----

tic(paste0("Get the country combinations which have common rivers"))

# 16 cores puts CPU close to 100% and memory at about 80, probably not the best
# 
cl <- makeCluster(n_cores) # n_cores # runs low on RAM if hydro_rivers gets sent to too many places, try building up
clusterEvalQ(cl,library(sf)) # send these separately, clusterEvalQ(cl, fun) is the call format
clusterEvalQ(cl, library(dplyr))
clusterExport(cl, c("hydro_rivers","gadm_data")) # this is a big export b/c the hydro_rivers is huge

parLapply(cl, main_rivers_all, get_hydro_river_countries)

stopCluster(cl)

# running this on 8 cores over all the main_rivers_all (16352 rivers in Africa) used up 50-60% CPU and hovered around 27-32 GB of RAM
# probably could go up to 12 or 14 if I wanted to throttle other functions on the computer; but this amount allows for other 
# programs to run pretty uninterrupted.

gc()
toc()

#Get the country combinations which have common rivers: 15928.05 sec elapsed (over 4 hours)
# this is all 16k rivers

  
# get all the countries in a list ----
  
  
  countries_filenames <- list.files(path = file.path(data_external_clean,"HydroSHEDS","countries-sharing-a-river"),
                                    pattern = "_has-common-river")
  
 
    countries_rds <- stringr::str_remove(countries_filenames,
                                             pattern = "_has-common-river.rds")
    
    
    countries <- strsplit(countries_rds,
                          split = "_")
    
    
    country_groups <- countries[-c(1)] # the first was having something odd with it
  
# get hydrorivers split into countries ----
  
  river_countries <- gadm_data %>% 
    filter(continent == "Africa")
  
  countries_single <- unique(river_countries$GID_0)  # all of these needed, do for (countries in countries_single)
  
  out_path <- file.path(data_external_clean,"HydroSHEDS","shape-files")
  
  # get the cluster started
  # using 16 cores runs this at about 85% CPU and 18-19Gb of RAM
  
  cl <- makeCluster(n_cores)
  clusterEvalQ(cl,library(sf))


  for (i in 1:length(country_groups)){
    
    countries <- country_groups[[i]]
  
    if (!file.exists( file.path(data_external_clean,"HydroSHEDS","shape-files",paste0(paste(countries,collapse = "_"),"_hydrorivers.rds")))){
      
  units <- gadm_data %>% filter(GID_0 %in% countries) %>% st_union() %>% st_make_valid() 
  
  clusterExport(cl, c("units"))
  
  split_list <- 
    get_parallel_splits(thing_to_split = hydro_rivers, 
                        n_cores = n_cores)
  

  # do the split
  split_results <-  parLapply(cl, split_list, function(x) st_intersection(x,units))
  
  
  hydro_rivers_units <- do.call("rbind", split_results) %>%
    mutate(width = 1/as.numeric(ORD_CLAS))
  
  
  saveRDS(object = hydro_rivers_units,
          file = file.path(data_external_clean,"HydroSHEDS","shape-files",paste0(paste(countries,collapse = "_"),"_hydrorivers.rds")))
  
  rm(split_list,split_results,units)
  gc()
    } # end ifelse if the country hydro_rivers_units already exists 
  } # end for loop over countries
  
  stopCluster(cl)
  

  
  
  
# do the same for GLOW ----

  out_path <- file.path(data_external_temp,"GLOW_global-long-term-river-width","shape-files")
  
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  

 cl <- makeCluster(n_cores)
  clusterEvalQ(cl,library(sf))
  

  for (i in 1:length(country_groups)){
    
    countries <- country_groups[[i]]
    
    if (!file.exists( file.path(data_external_temp,"GLOW_global-long-term-river-width","shape-files",paste0(paste(countries,collapse = "_"),"_glow.rds")))){
      
      units <- gadm_data %>% filter(GID_0 %in% countries) %>% st_union() %>% st_make_valid() 
      
      clusterExport(cl, c("units"))
      
      split_list <- 
        get_parallel_splits(thing_to_split = river_locations, 
                            n_cores = n_cores)
      
      
      # do the split
      split_results <-  parLapply(cl, split_list, function(x) st_intersection(x,units))
      
      
      river_locations_units <- do.call("rbind", split_results) #%>%
       # mutate(width = 1/as.numeric(ORD_CLAS))
      
      
      saveRDS(object = river_locations_units,
              file = file.path(data_external_temp,"GLOW_global-long-term-river-width","shape-files",paste0(paste(countries,collapse = "_"),"_glow.rds")))
      
      rm(split_list,split_results,units)
      gc()
    } # end ifelse if the country hydro_rivers_units already exists 
  } # end for loop over countries
  
  stopCluster(cl)
  
  
