# _______________________________#
# Environment
# Clean 02: Merge DHS and GPS Data
# 
# Stallman
# Started: 2023-10-23
# Last edited: 2023-11-13
# Edit: added get elevation to merge function merge_dhs_gps
# Last edited: 2023-12-09
# Edit: made it just getting the river network given having dhs_child-mortality data
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
    tictoc # timing # more ability to customize to output to latex. use with kableExtra to output tables
    # to console, latex, Rmarkdown, html etc.
  )

  level <- 1
  dhs_gps_filename <- "UGGE7AFL" # # UGGE7AFL and #UGGC7BFL
  
  country          <- "UG"
  n_cores          <- 18 #detectCores(logical = TRUE) - 2
  #units_name       <- c("Uganda")
  equal_area_crs   <- "ESRI:102022"
  max_distance_to_snap <- 10000 # max distance at which to snap to a river; 10km b/c that's max perturbation
  surveyyear_start <- 2018 %>% as.character()
  surveyyear_end   <- 2020 %>% as.character()
  long_river_threshold <- 4000 # original was 4000 
  period_length    <- 60 # what months window is used for the mortality data
# bring in hydrorivers ----
  
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

  names(hydro_rivers)
  # [1] "HYRIV_ID"     "NEXT_DOWN"    "MAIN_RIV"     "LENGTH_KM"    "DIST_DN_KM"  
  # [6] "DIST_UP_KM"   "CATCH_SKM"    "UPLAND_SKM"   "ENDORHEIC"    "DIS_AV_CMS"  
  # [11] "ORD_STRA"     "ORD_CLAS"     "ORD_FLOW"     "HYBAS_L12"    "Shape_Length"
  # [16] "geometry" 
  
  main_river <- 10005351
  
  # # some big rivers
  # 10071782
  # 10428478
  
  temp <- hydro_rivers %>% filter(MAIN_RIV==10071782)
  
  
  # country codes 
  #  https://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10136
  
  
  countries_DHS <- readRDS(
    file= file.path(data_external_clean,"DHS","datasets-for-selection",
                    paste0("countries_DHS.rds")))
  

  gadm_in_path <- file.path(data_external_clean,"GADM","global")
  
  gadm_in_filename <- paste0("GADM_global_ADM_",level,".rds")
  tic("Bringing in GADM data")
  gadm_data  <- readRDS(file.path(gadm_in_path,gadm_in_filename)) %>%
    st_transform(crs = equal_area_crs)
    
  toc()
  gadm_data$continent <- countrycode(gadm_data$GID_0,
                                     origin = "iso3c",
                                     destination = "continent")

  # a little circular since this file got created within the loop below but whatevs for now
  #out_path <- file.path(data_external_temp,"shape-files","hydroRIVERS")
  #hydro_rivers_units <- readRDS(file = file.path(out_path,paste0("SEN_hydrorivers.rds")))
  
  #main_rivers_senegal <- hydro_rivers_units$MAIN_RIV %>% unique() %>% .[-c(1)] # got this from restricting units to Senegal later on
  
  # get all the main rivers and convert to a vector to loop through
  main_rivers_all <- hydro_rivers %>% 
                    st_drop_geometry() %>%
                     dplyr::select(MAIN_RIV) %>%
                    unique() %>% as.vector() %>% .[[1]]
  
  
  # main_rivers_sizes <- hydro_rivers %>%
  #                    st_drop_geometry() %>%
  #                    group_by(MAIN_RIV) %>%
  #                    summarise(n_segments = n())
  #                    
  #                 
  #   out_path <- file.path(data_external_clean,"HydroSHEDS","summary-information")
  # if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)
  # 
  # saveRDS(main_rivers_sizes,file = file.path(out_path,paste0("rivers_and_segment_size.rds")))
  # 
  # main_rivers_medium_big <- hydro_rivers %>%
  #                           st_drop_geometry() %>%
  #                           group_by(MAIN_RIV) %>%
  #                           summarise(n_segments = n()) %>%
  #                           ungroup() %>%
  #                           filter(n_segments <= 12000 & n_segments >= 4000) %>%
  #                           arrange(n_segments) %>%
  #                           as.data.frame()
                            
  
# find which rivers are missing ----
# 
  missing_rivers_path <- file.path(data_external_temp,"HydroSHEDS","river-network-missing")
  missing_river_files <- list.files(missing_rivers_path)
  
  missing_rivers <- str_extract(missing_river_files, "[^MAIN_RIV_]+")
  
  # started 2023-12-06 at around 16:20pm
  
# parallelize making the river network ----
  
  ## create get river network function ----
  out_path <- file.path(data_external_clean,"HydroSHEDS","long_rivers")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  out_path <- file.path(data_external_clean,"HydroSHEDS","river_networks")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  
  get_river_network <- function(current_main_river,
                                out_path = file.path("E:","data","03_clean","HydroSHEDS"),
                                long_rivers_path = file.path("E:","data","02_temp","HydroSHEDS","long-rivers"),
                                long_river_threshold = 4000
                                #hydro_rivers = hydro_rivers # DO NOT Uncomment this - including it caused a recursive error
                                ){
  
    if (!dir.exists(long_rivers_path)) dir.create(long_rivers_path, recursive = TRUE) 
    
    if (file.exists(file = file.path(out_path,"river_networks",paste0("MAIN_RIV_",current_main_river,"_cleaned_river_network.rds")))){
      
      print(paste0("Current river network for main_river ",current_main_river," already exists. No need to create."))
      
    } else {
    single_river <- hydro_rivers %>%
    dplyr::filter(MAIN_RIV==current_main_river)%>%
    dplyr::mutate(width = 1/as.numeric(ORD_CLAS)) 
  
    if (nrow(single_river)>long_river_threshold){
      
  saveRDS(current_main_river,file = file.path(long_rivers_path,paste0("long_river_MAIN_RIV_",current_main_river,".rds")))
    } else {
      
  mouth_segment <- which(single_river$NEXT_DOWN == 0)

  current_river_network <- line2network(sf = single_river)
  
  current_river_network$mouth$mouth.seg <- mouth_segment
  
  # get mouth vertex 
  # how many vertices are there on this segment of the river
  len <- dim(current_river_network$lines[[mouth_segment]])[1]
  
  # come back to this one, use showends() function to look at which vertex should be set as the final one
  #showends(seg = mouth_segment, rivers = current_river_network)
  
  current_river_network$mouth$mouth.vert<- len
  
  # try cleaning up the river network
  
  # tic(paste0("cleaning up river network",main_river," with ",nrow(single_river)," segments"))
  # # started around 6:26pm 2023-12-05
  # #current_river_network_fix <- cleanup(current_river_network)
  # 
  # #current_river_network <- current_river_network_fix
  # 
  # 
  # toc()
  
  buildsegroutes(current_river_network,
                 lookup = TRUE,
                 verbose = FALSE)
  
  saveRDS(current_river_network, file = file.path(out_path,"river_networks",paste0("MAIN_RIV_",current_main_river,"_cleaned_river_network.rds")))
  
  rm(current_river_network)
  
  gc()
    } # end if-else that if river is really big we'll deal with it later
    } # end if-else that if river network already exists we won't create it
  }
  
  # run as an example
  # 
  # # some big rivers
  # 10071782
  # 10428478

  get_river_network(10428478)
  
  get_river_network(10746900)
  
  get_river_network(10071782,
                    long_river_threshold = 13000)
  
  # run some big rivers
  # for (i in 1:length(main_rivers_medium_big[1,1])) {
  #   
  #   tic(paste0("Got river network for river ", main_rivers_medium_big[i,1] , ", a river of " ,main_rivers_medium_big[i,2]," segments."))
  #   get_river_network(main_rivers_medium_big[i,1])
  #   
  #   toc()
  #   
  #   
  # }


  ## run getting the river network function ----
  
  tic(paste0("Get river networks for all the HydroRIVERS"))

  cl <- makeCluster(n_cores) # n_cores
  clusterEvalQ(cl,library(sf)) # send these separately, clusterEvalQ(cl, fun) is the call format
  clusterEvalQ(cl,library(riverdist))
  clusterEvalQ(cl, library(dplyr))
  clusterExport(cl, c("hydro_rivers")) # this is a big export b/c the hydro_rivers is huge
  
  #parLapply(cl, main_rivers_all, get_river_network)
  
  #parLapply(cl, main_rivers_medium_big[1:n_cores,1], get_river_network)
  
  parLapply(cl, missing_rivers, get_river_network)
  
  gc()
  toc()
  
  stopCluster(cl)
  
  # running this on 8 cores over all the main_rivers_all (16352 rivers in Africa) used up 50-60% CPU and hovered around 27-32 GB of RAM
  # probably could go up to 12 or 14 if I wanted to throttle other functions on the computer; but this amount allows for other 
  # programs to run pretty uninterrupted.
  

  
  # ran through for all rivers (minus some 2000 that were done earlier as tests as well as about 100 very large rivers)
  # Get river networks for all the HydroRIVERS: 160043.16 sec elapsed
  # 44.45643 hours
  
  #Get river networks for all the HydroRIVERS: 137531.04 sec elapsed # over missing rivers
  #
  
  # Get river networks for all the HydroRIVERS: 321422.05 sec (89 hours) elapsed for 8 rivers of length:
  # 1
  # 11110394
  # 4045
  # 2
  # 10586083
  # 4074
  # 3
  # 11385960
  # 4083
  # 4
  # 10262369
  # 4110
  # 5
  # 10091109
  # 4357
  # 6
  # 10857729
  # 4417
  # 7
  # 10848636
  # 4450
  # 8
  # 10101070
  # 4472
