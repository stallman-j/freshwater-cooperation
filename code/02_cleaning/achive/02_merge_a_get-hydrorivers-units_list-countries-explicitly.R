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
  n_cores          <- 14 #detectCores(logical = TRUE) - 2
  #units_name       <- c("Uganda")
  equal_area_crs   <- "ESRI:102022"
  max_distance_to_snap <- 10000 # max distance at which to snap to a river, really high at this point; 10km b/c that's max perturbation
  surveyyear_start <- 2018 %>% as.character()
  surveyyear_end   <- 2020 %>% as.character()
  long_river_threshold <- 4000 
  period_length    <- 60 # what months window is used for the mortality data
  
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
  
# for all rivers, find which countries they cross ----
  # get all the main rivers and convert to a vector to loop through
  main_rivers_all <- hydro_rivers %>% st_drop_geometry() %>% select(MAIN_RIV) %>%
    unique() %>% as.vector() %>% .[[1]]
  
  # find 
  
  
# get hydrorivers split into countries ----
  
  river_countries <- gadm_data %>% 
    filter(continent == "Africa")
  
  countries_single <- unique(river_countries$GID_0)  # all of these needed, do for (countries in countries_single)
  
  # getting these through the folder file.path(data_external_clean,"HydroSHEDS","hydro_rivers_units_missing")
  country_groups <- list( c("GIN","GMB","GNB","SEN"),
                          c("ETH","SOM"),
                          c("ESH","MAR"),
                          c("DZA","LBY","NER"),
                          c("EGY","SDN"),
                          c("KEN","SOM"),
                          c("MOZ","SWZ","ZAF"),
                          c("LBY","SDN","TCD"),
                          c("LBY","SDN"),
                          c("LBY","TCD"),
                          c("ESH","MAR","MRT"),
                          c("LBY","NER"),
                          c("KEN","TZA"),
                          c("MLI","NER"))
  
  full_country_groups <- list(c("GMB","GNB","SEN"),
                              c("GIN","GMB","GNB","SEN"),
                              c("ETH","SOM"),
                              c("DZA","ESH","MAR","MRT"),
                          c("ESH","MRT"),
                          c("AGO","NAM"),
                          c("DZA","NER"),
                          c("DZA","MLI"),
                          c("DZA","MRT"),
                          c("DZA","MAR"),
                          c("MLI","MRT"),
                          c("GIN","GNB"),
                          c("DZA","ESH","MRT"),
                          c("DJI","ETH","SOM"),
                          c("DJI","ETH"),
                          c("LBY","TCD"),
                          c("GAB","GNQ"),
                          c("GIN","GMB","SEN"),
                          c("GIN","GMB"),
                          c("GIN","GNB","SEN"),
                          c("GIN","MLI","MRT","SEN"),
                          c("GNB","SEN"),
                          c("GMB","SEN"),
                          c("MRT","SEN")
                          )
  
  out_path <- file.path(data_external_clean,"HydroSHEDS","shape-files")
  
  # get the cluster started
  cl <- makeCluster(n_cores)
  clusterEvalQ(cl,library(sf))
  #clusterExport(cl, c("hydro_rivers","gadm_data")) # this is a big export b/c the hydro_rivers is huge
  
  # for single countries
  # for (i in 1:length(countries_single)){}
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
  
