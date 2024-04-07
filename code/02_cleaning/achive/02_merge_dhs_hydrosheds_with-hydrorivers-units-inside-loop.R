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
    tictoc # timing # more ability to customize to output to latex. use with kableExtra to output tables
    # to console, latex, Rmarkdown, html etc.
  )

  level <- 1
  dhs_gps_filename <- "UGGE7AFL" # # UGGE7AFL and #UGGC7BFL
  
  country          <- "UG"
  n_cores          <- 8 #detectCores(logical = TRUE) - 2
  #units_name       <- c("Uganda")
  equal_area_crs   <- "ESRI:102022"
  max_distance_to_snap <- 10000 # max distance at which to snap to a river, really high at this point; 10km b/c that's max perturbation
  surveyyear_start <- 2018 %>% as.character()
  surveyyear_end   <- 2020 %>% as.character()
  long_river_threshold <- 4000 
  period_length    <- 60 # what months window is used for the mortality data
# bring in hydrorivers ----
  
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

  names(hydro_rivers)
  # [1] "HYRIV_ID"     "NEXT_DOWN"    "MAIN_RIV"     "LENGTH_KM"    "DIST_DN_KM"  
  # [6] "DIST_UP_KM"   "CATCH_SKM"    "UPLAND_SKM"   "ENDORHEIC"    "DIS_AV_CMS"  
  # [11] "ORD_STRA"     "ORD_CLAS"     "ORD_FLOW"     "HYBAS_L12"    "Shape_Length"
  # [16] "geometry" 
  
  # country codes 
  #  https://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10136
  
  
  dhs_mortality_data <- readRDS(file = file.path(data_external_clean,"merged",
                          paste0("Africa_all_years_DHS_",period_length,"_months_window_child_mortality_with_GPS.rds"))) %>%
                        st_as_sf(crs = 4326) %>%
                        st_transform(crs = equal_area_crs)
  
  countries <- dhs_mortality_data$DHSCC %>% unique()
  

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
  out_path <- file.path(data_external_temp,"shape-files","hydroRIVERS")
  hydro_rivers_units <- readRDS(file = file.path(out_path,paste0("SEN_hydrorivers.rds")))
  
  main_rivers_senegal <- hydro_rivers_units$MAIN_RIV %>% unique() %>% .[-c(1)] # got this from restricting units to Senegal later on
  
  # get all the main rivers and convert to a vector to loop through
  main_rivers_all <- hydro_rivers %>% st_drop_geometry() %>% select(MAIN_RIV) %>%
                     filter(!MAIN_RIV %in% main_rivers_senegal) %>% 
                    unique() %>% as.vector() %>% .[[1]]
                  
  # need to keep track of which rivers have been checked
  main_rivers_df <- hydro_rivers %>% st_drop_geometry() %>% select(MAIN_RIV) %>%
    unique()
  
  main_rivers_df <- main_rivers_df %>%
                    mutate(checked = 0) %>%
                    mutate(checked = ifelse(row_number()< 67, 1, 0))

  #saveRDS(main_rivers_df, file = file.path(data_external_clean,"HydroSHEDS","main_rivers_checking_df.rds"))
  
  # this is the most updated
  main_rivers_df <- readRDS(file = file.path(data_external_clean,"HydroSHEDS","main_rivers_checking_df.rds") )
  #main_river <- 11395641

  # started 2023-12-06 at around 16:20pm
  
# parallelize making the river network ----
  
  ## create get river network function ----
  out_path <- file.path(data_external_clean,"HydroSHEDS","long_rivers")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  out_path <- file.path(data_external_clean,"HydroSHEDS","river_networks")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  
  get_river_network <- function(main_river,
                                out_path = file.path("E:","data","03_clean","HydroSHEDS"),
                                long_river_threshold = 4000
                                ){
  
    if (file.exists(file = file.path(out_path,"river_networks",paste0("MAIN_RIV_",main_river,"_cleaned_river_network.rds")))){
      
      print(paste0("Current river network for main_river ",main_river," already exists. No need to create."))
      
    } else {
    single_river <- hydro_rivers %>%
    filter(MAIN_RIV==main_river)%>%
    mutate(width = 1/as.numeric(ORD_CLAS)) 
  
    if (nrow(single_river)>long_river_threshold){
      
  saveRDS(main_river,file = file.path(out_path,"long_rivers",paste0("long_river_MAIN_RIV_",main_river,".rds")))
    } else {
      
  source_segment <- which(single_river$NEXT_DOWN == 0)

  current_river_network <- line2network(sf = single_river)
  
  current_river_network$mouth$mouth.seg <- source_segment
  current_river_network$mouth$mouth.vert<- 1
  
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
  
  saveRDS(current_river_network, file = file.path(out_path,"river_networks",paste0("MAIN_RIV_",main_river,"_cleaned_river_network.rds")))
  
  rm(current_river_network)
  
  gc()
    } # end if-else that if river is really big we'll deal with it later
    } # end if-else that if river network already exists we won't create it
  }
  
  # run as an example
  get_river_network(10538109)
  

  ## run getting the river network function ----
  
  tic(paste0("Get river networks for all the HydroRIVERS"))

  cl <- makeCluster(8) # n_cores
  clusterEvalQ(cl,library(sf)) # send these separately, clusterEvalQ(cl, fun) is the call format
  clusterEvalQ(cl,library(riverdist))
  clusterEvalQ(cl, library(dplyr))
  clusterExport(cl, c("hydro_rivers")) # this is a big export b/c the hydro_rivers is huge
  
  
  parLapply(cl, main_rivers_all, get_river_network)
  
  stopCluster(cl)
  
  # running this on 8 cores over all the main_rivers_all (16352 rivers in Africa) used up 50-60% CPU and hovered around 27-32 GB of RAM
  # probably could go up to 12 or 14 if I wanted to throttle other functions on the computer; but this amount allows for other 
  # programs to run pretty uninterrupted.
  
  gc()
  toc()
  
# get the DHS locations placed on river networks ----
  
  # initialize a counter to see where we're at
  i <- 1
  
  #j <- 1
  # https://ryanpeek.org/2017-11-21-mapping-with-sf-part-3/
  
  for (j in 1:length(main_rivers_all)){
  
    main_river <- main_rivers_all[j]
    
    print(paste0("Working on MAIN_RIV ",main_river, "."))
    
    if (main_rivers_df[j,2]==1){
      print("Already checked, move onto the next iteration")
      i <- i+1
    } else {
      
    
    
    print(paste0("This is iteration " ,i, " of ",length(main_rivers_all)))
    
    # if the current river network doesn't exist, end
    if (!file.exists(file = file.path(data_external_clean,"HydroSHEDS","river_networks",paste0("MAIN_RIV_",main_river,"_cleaned_river_network.rds")))){
      
      print(paste0("Current river network for main_river ",main_river," doesn't exist. Create it with another function."))
      
    } else { # otherwise read in the network
      
      current_river_network <- readRDS(file = file.path(data_external_clean,"HydroSHEDS","river_networks",paste0("MAIN_RIV_",main_river,"_cleaned_river_network.rds")))
      
    # get the external boundary of the river 
      
      single_river <- hydro_rivers %>%
        filter(MAIN_RIV==main_river)%>%
        mutate(width = 1/as.numeric(ORD_CLAS))
      
    polygon_boundary <- single_river %>% st_buffer(20000) %>% st_convex_hull() %>% st_union() 
    
    # find which countries intersect with this
    
    plot(st_geometry(polygon_boundary))
    
    # https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwjq3Yff_fiCAxWukYkEHSIIC4MQFnoECA0QAw&url=http%3A%2F%2Fpostgis.net%2Fworkshops%2Fpostgis-intro%2Fspatial_relationships.html&usg=AOvVaw2O1JV4HykW5lrmGgVLjGZH&opi=89978449
    # st_intersects gives if any points in common
    
    tic("Checking which countries intersect with the river")
    intersecting_indices <- lengths(st_intersects(gadm_data, polygon_boundary))>0
    
    toc()
    
    # get the iso3c of the countries involved
    intersecting_countries <- gadm_data[intersecting_indices,] %>% select(GID_0) %>%
                              st_drop_geometry() %>% unique() %>% .[,1]
    
    river_countries <- gadm_data %>% 
      filter(GID_0 %in% intersecting_countries & continent == "Africa")
             
    countries <- unique(river_countries$GID_0) 
    
    
    
    countries_DHS    <- countrycode(countries,
                                    origin = "iso3c",
                                    destination = "dhs")
    
    dhs_mortality_data_river_countries <- dhs_mortality_data %>%
                                   filter(DHSCC %in% countries_DHS)
    
    if (nrow(dhs_mortality_data_river_countries)==0){
      print(paste0("No DHS datasets intersect with ",main_river))
      
      # log the river as checked, and save
      main_rivers_df[j,2] <- 1
      saveRDS(main_rivers_df, file = file.path(data_external_clean,"HydroSHEDS","main_rivers_checking_df.rds"))
      
      
      i <- i+1
    } else {
      
    years <- unique(dhs_mortality_data_river_countries$end_year)
    
    for (year in years){
      
      print(paste0("Working on DHS for ",year, "of years ",paste(years, collapse = " ")," in countries ", paste(countries, collapse = " ")))
      
      dhs_mortality_river_single_year <- dhs_mortality_data_river_countries %>%
                                         filter(end_year == year)
             
    
    if (nrow(dhs_mortality_river_single_year)==0){
      print(paste0("No DHS datasets for ",paste(countries,collapse = " ")," in year ",year))

      } else{
      
      
    river_polygons <- gadm_data %>% 
      filter(GID_0 %in% intersecting_countries & continent == "Africa")%>%
                      group_by(GID_0) %>%
                      summarize(geometry = st_union(geom)) %>%
                      st_make_valid()
                       
    
    plot(st_geometry(river_polygons))
    # for 20441 obs, took 7.62 seconds
    
# get hydrorivers units ----
    
    units <- gadm_data %>% filter(GID_0 %in% countries) %>% st_union() %>% st_make_valid() 
    
    out_path <- file.path(data_external_temp,"shape-files","GADM")
    if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
    
    saveRDS(units,
            file = file.path(data_external_temp,"shape-files","GADM",paste0(paste(countries,collapse="_"),"_union_shapefile.rds")))
    
    out_path <- file.path(data_external_temp,"shape-files","hydroRIVERS")
    if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
    
    if (file.exists(file.path(out_path,paste0(paste(countries,collapse = "_"),"_hydrorivers.rds")))) {

      hydro_rivers_units <- readRDS(file = file.path(out_path,paste0(paste(countries,collapse = "_"),"_hydrorivers.rds")))
      
    } else { # if hydro_rivers_units doesn't exist, create it
      
    tic(paste0("Split hydroRIVERS into our countries ",paste(countries,collapse = "_")))
    split_list <- 
      get_parallel_splits(thing_to_split = hydro_rivers, 
                          n_cores = n_cores)
    
    cl <- makeCluster(n_cores)
    clusterEvalQ(cl,library(sf))
    clusterExport(cl, c("units"))
    
    
    split_results <-  parLapply(cl, split_list, function(x) st_intersection(x,units))
    
    stopCluster(cl)
    
    hydro_rivers_units <- do.call("rbind", split_results) %>%
      mutate(width = 1/as.numeric(ORD_CLAS))
    

    saveRDS(object = hydro_rivers_units,
            file = file.path(out_path,paste0(paste(countries,collapse = "_"),"_hydrorivers.rds")))
    
    rm(split_list,split_results)
    gc()
    toc()
    } # end if file hydro_rivers_units exists, bring it in; if not create it
    
    # this is a fast operation
    nearest_indices <- st_nearest_feature(dhs_mortality_river_single_year,hydro_rivers_units)
    
    # this is also fast
    tic("getting closest points")
    closest_points <- dhs_mortality_river_single_year %>%
      mutate(my_linestring = st_nearest_points(dhs_mortality_river_single_year,hydro_rivers_units[nearest_indices,], pairwise = TRUE),
             closest_point = st_cast(my_linestring, 'POINT')[seq(2, nrow(.)*2, 2)],
             distance_to_river      = st_distance(dhs_mortality_river_single_year, hydro_rivers_units[nearest_indices,], by_element = TRUE),
             snapped_point_cond = st_sfc(ifelse(as.numeric(distance_to_river) <= max_distance_to_snap, st_geometry(closest_point),geometry),crs = st_crs(equal_area_crs))
      ) %>%
      cbind(st_drop_geometry(hydro_rivers_units[nearest_indices,]))
    
    toc()
    # add in X Y coordinates in projected form
    closest_points$X <- st_coordinates(closest_points)[,1]
    closest_points$Y <- st_coordinates(closest_points)[,2]

    current_points <- closest_points %>% filter(MAIN_RIV == main_river)
    
# calculate distances ----
      
    if (nrow(current_points)==0) {
      print(paste0("No points in current DHS in year ", year, " match with the current choice of river ID #,",main_river, ". Keep looping."))
      } else{
        
      points_on_river <- xy2segvert(x = current_points$X,
                                    y = current_points$Y,
                                    rivers = current_river_network)
      
      
      # takes a really long time to run for a really large river network
      tic("Calculating distances for current river points")
      distance_mat <- upstreammat(seg = points_on_river$seg,
                            vert = points_on_river$vert,
                            ID = current_points$DHSID,
                            rivers = current_river_network,
                            flowconnected = TRUE)
      
      
      toc()
      
      # Calculating distances for current river points: for  14376.14 sec elapsed for 3448 segments
      # Calculating distances for current river points: 640.58 sec elapsed for 1448 segments

      # for 705 obs on a river network with 20441 rows takes 
      out_path <- file.path(data_external_temp,"merged")
      if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
      
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
                        filter(value > 0) %>%
                        rename(downstream = row,
                               upstream   = col,
                               distance_m = value)
      
      
      out_path <- file.path(data_external_temp,"merged","hydroRIVERS_DHS")
      if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
      
      saveRDS(object = dyad_upstream,
              file = file.path(out_path,paste0("DHS_",surveyyear_start,"_to_",surveyyear_end,"_",paste(countries,collapse="_"),"_MAIN_RIV_",main_river,"_dyad_distances.rds")))
      
      saveRDS(object = dyad_distances,
              file = file.path(out_path,paste0("DHS_",surveyyear_start,"_to_",surveyyear_end,"_",paste(countries,collapse="_"),"_MAIN_RIV_",main_river,"_distances_matrix.rds")))
      
      print(paste0("Saved distance matrices at",out_path,paste0("DHS_",surveyyear_start,"_to_",surveyyear_end,"_",paste(countries,collapse="_"),"_MAIN_RIV_",main_river,"_dyad_distances.rds")))
      
      print("Preparing to plot")
      
# plot current river, and current river in the countries ----
      
      #for main_river == 10508130 this is a good map to make
      map <- ggplot() +
        # geom_sf(data = units,
        #         color = "gray70",
        #         fill = "gray99",
        #         alpha = 0.5,
        #         linewidth = .3)+
        geom_sf(data = single_river,
                color = yale_medblue,
                linewidth = single_river$width*2,
                alpha = single_river$width*2) +
        geom_sf(data = current_points)+
        geom_sf(data = source_stretch,
                color = 'green',
                linewidth = 1.5)+
        labs(title = paste0("Example: River Distance Calculations,", paste(countries,collapse = " ")," ", year," DHS"),
             caption = c(paste0("Reported cluster in black; buffer of 5 km around cluster in dashed red; river source in green
                             Labels give (Country+DHS cluster number, segment vertex number)
                             Data from DHS (",year,")", "GADM (2022), AWS (2023), HydroRIVERS (2023).")),
             colour = "Elevation (m)")+
        geom_sf(data = source_stretch %>% st_buffer(5000), col = 'green', fill = NA, linewidth = 1.5)+
        geom_sf(data = current_points %>% st_buffer(5000), col = 'red', fill = NA, linewidth = .5,linetype = "dashed")+
        ggrepel::geom_text_repel(data = current_points,
                                 aes(label = paste0(current_points$DHSCC,current_points$DHSCLUST),#,",",points_on_river$vert),
                                     geometry = geometry),
                                 stat = "sf_coordinates",
                                 max.overlaps = 20,
                                 col = "black"#row_number(test_rivers))
        )+
        xlab("") + ylab("")+
        theme_map(legend_position = "none")

      map


      out_path <- file.path(output_maps,"HydroSHEDS")
      if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories


      save_map(output_folder = out_path,
               plotname = map,
               filename = paste0(paste(countries,collapse = "_"),"_MAIN_RIV_",main_river,"_",year,"_river_distances.png"),
               width = 8,
               height = 7,
               dpi  = 300)
      
      
      
      #for main_river == 10508130 this is a good map to make
      map <- ggplot() +
        geom_sf(data = units,
                color = "gray70",
                fill = "gray99",
                alpha = 0.5,
                linewidth = .3)+
        geom_sf(data = single_river,
                color = yale_medblue,
                linewidth = single_river$width*2,
                alpha = single_river$width*2) +
        geom_sf(data = current_points)+
        geom_sf(data = source_stretch,
                color = 'green',
                linewidth = 1.5)+
        labs(title = paste0("Example: River Distance Calculations,", paste(countries,collapse = " ")," ",year," DHSs"),
             caption = c(paste0("Reported cluster in black; buffer of 5 km around cluster in dashed red; river source in green
                             Labels give (Country+DHS cluster number, segment vertex number)
                             Data from DHS (",year,")", "GADM (2022), AWS (2023), HydroRIVERS (2023).")),
             colour = "Elevation (m)")+
        geom_sf(data = source_stretch %>% st_buffer(5000), col = 'green', fill = NA, linewidth = 1.5)+
        geom_sf(data = current_points %>% st_buffer(5000), col = 'red', fill = NA, linewidth = .5,linetype = "dashed")+
        ggrepel::geom_text_repel(data = current_points,
                                 aes(label = paste0(current_points$DHSCC,current_points$DHSCLUST),#,",",points_on_river$vert),
                                     geometry = geometry),
                                 stat = "sf_coordinates",
                                 max.overlaps = 20,
                                 col = "black"#row_number(test_rivers))
        )+
        xlab("") + ylab("")+
        theme_map(legend_position = "none")
      
      map
      
      
      out_path <- file.path(output_maps,"HydroSHEDS")
      if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
      
      
      save_map(output_folder = out_path,
               plotname = map,
               filename = paste0(paste(countries,collapse = "_"),"_MAIN_RIV_full_",main_river,"_",year,"_river_distances.png"),
               width = 8,
               height = 7,
               dpi  = 300)
      
      print("Finished plotting, removing river network and river data and doing garbage cleanup")
      
      rm(current_river_network,single_river)
      gc()
      
      i <- i+1
      
      } # end ifelse statement that checks if we've already checked the df and moves on if not 
      } # end loop over years
      } # end if-else statement for if there aren't any DHS datasets for the river country of interest during the time of interest
    } # end if-else statement if there aren't any DHS datasets for the MAIN_RIV of interest

    } # end if-else statement looping over current points
    }  # end if-else for if the current river network doesn't exist then exit
  } # end loop over main_rivers
  
    
  
  # [1] "MAIN_RIV 10508677 has 3388 segments."
  # Checking which countries intersect with the river: 4.44 sec elapsed
  # [1] "Current river network for main_river10508677 already exists. Reading it in"
  # getting closest points: 0.34 sec elapsed
  # Calculating distances for current river points: 15540.14 sec elapsed
  #   

 
# extra plots ----


  cols <- rev(brewer.pal(11, 'RdYlGn'))
  

    map <- ggplot(data = units) +
      geom_sf(color = "gray70",
              fill = "gray99",
              alpha = 0.5,
              linewidth = .3) +
      geom_sf(data = hydro_rivers_units,
              alpha = hydro_rivers_units$width,
              color = yale_medblue,
              linewidth = hydro_rivers_units$width) +
      geom_sf(data = dhs_data)+
      labs(title = paste0("Survey Clusters and Rivers, Uganda 2016 DHS"),
           caption = c("Data from DHS (2016), GADM (2022), AWS (2023), HydroRIVERS (2023)"),
           colour = "Elevation (m)")+
      coord_sf(datum = st_crs(equal_area_crs))+
      xlab("") + ylab("")+
      theme_map(legend_position = c(0.9,0.3))  +
      scale_colour_gradientn(colours = cols)

    map

    out_path <- file.path(output_maps,"DHS")
    if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
    
    save_map(output_folder = out_path,
             plotname = map,
             filename = "rivers_dhs_uganda.png",
             width = 8,
             height = 10,
             dpi  = 300)
  

  # 
  
   map <- ggplot() +
     # geom_sf(data = units,
     #         color = "gray70",
     #         fill = "gray99",
     #         alpha = 0.5,
     #         linewidth = .3)+
    geom_sf(data = single_river,
            color = yale_medblue,
            linewidth = single_river$width*2,
            alpha = single_river$width*2) +
    geom_sf(data = river_stretches,
            color = "red",
            linewidth = river_stretches$width*2,
            alpha = river_stretches$width*2)+
     geom_sf(data = test_points)+
     geom_sf(data = source_stretch,
             color = 'green',
             linewidth = 1.5)+
     labs(title = paste0("Example: River Distance Calculations, Uganda 2016 DHS"),
          caption = c(paste0("River segments with DHS cluster in red; original cluster in black; source in green \n
                             Coordinates give (DHS Cluster Number, River Vertex Number) \n
                             Data from DHS (2016), GADM (2022), AWS (2023), HydroRIVERS (2023).")),
          colour = "Elevation (m)")+
     geom_sf(data = source_stretch %>% st_buffer(5000), col = 'green', fill = NA, linewidth = 1.5)+
     geom_sf(data = river_stretches %>% st_buffer(5000), col = 'red', fill = NA, linewidth = 1.5)+
     # geom_sf_text(data = test_points,
     #              aes(label = test_points$DHSCLUST),
     #              col = "black"#row_number(test_rivers))
     # )+
     ggrepel::geom_text_repel(data = test_points,
                  aes(label = paste0(test_points$DHSCLUST,",",points_on_river$vert),
                      geometry = geometry),
                  stat = "sf_coordinates",
                  col = "black"#row_number(test_rivers))
     )+
     xlab("") + ylab("")+
     theme_map(legend_position = "none")
     
   map
   
  
   out_path <- file.path(output_maps,"HydroSHEDS")
   if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
   
   
   save_map(output_folder = out_path,
            plotname = map,
            filename = paste0(units_name,"_example_river_distances.png"),
            width = 8,
            height = 10,
            dpi  = 300)
  


  

  map <- ggplot(data = units) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    geom_sf(data = hydro_rivers_units,
            alpha = hydro_rivers_units$width,
            color = yale_medblue,
            linewidth = hydro_rivers_units$width) +
    geom_sf(data = closest_points$snapped_point_cond, col = 'red')+
    geom_sf(data = dhs_data %>% st_buffer(max_distance_to_snap), col = 'black', fill = NA, linetype = 'dashed')+
    #geom_sf(data = closest_points %>% filter(as.numeric(distance)<=max_distance_to_snap) %>% .$my_linestring, linetype = 'dotted')+
    geom_sf(data = closest_points %>% filter(as.numeric(distance)<=max_distance_to_snap), shape = 1)+
    geom_sf(data = closest_points)+
    labs(title = paste0("Survey Clusters and Snapping to River Segments, Uganda 2016 DHS"),
         caption = c(paste0("Snapped points in red; original cluster in black; buffer of ",max_distance_to_snap/1000," km around DHS clusters. \nData from DHS (2016), GADM (2022), AWS (2023), HydroRIVERS (2023).")),
         colour = "Elevation (m)")+
    xlab("") + ylab("")+
    theme_map(#legend_position = "none",
              axis_text_x = element_blank(),
              axis_text_y = element_blank()) # legend_position = c(0.9,0.3))  +
   # scale_colour_gradientn(colours = cols)
  
  map
  
  
  out_path <- file.path(output_maps,"DHS")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  
  save_map(output_folder = out_path,
           plotname = map,
           filename = paste0(dhs_gps_filenames,"_with_rivers.png"),
           width = 8,
           height = 10,
           dpi  = 300)
  
  
  #Calculating distances, snapped point, and 
  #buffers for all 685 clusters in UGGE7AFL and all 11763 river segments in the country polygon.: 0.94 sec elapsed
  
# plot of entire hydronetwork over multiple countries ----
  
  # just do the plot with the DHS the first time we go through the rivers
    
    tic("Plotting rivers")
    map <- ggplot(data = units) +
      geom_sf(color = "gray70",
              fill = "gray99",
              alpha = 0.5,
              linewidth = .3) +
      geom_sf(data = hydro_rivers_units,
              alpha = hydro_rivers_units$width,
              color = yale_medblue,
              linewidth = hydro_rivers_units$width) +
      geom_sf(data = closest_points$snapped_point_cond, col = 'red')+
      geom_sf(data = dhs_data %>% st_buffer(max_distance_to_snap), col = 'black', fill = NA, linetype = 'dashed')+
      #geom_sf(data = closest_points %>% filter(as.numeric(distance)<=max_distance_to_snap) %>% .$my_linestring, linetype = 'dotted')+
      geom_sf(data = closest_points %>% filter(as.numeric(distance_to_river)<=max_distance_to_snap), shape = 1)+
      geom_sf(data = closest_points)+
      labs(title = paste0("Survey Clusters and Snapping to River Segments ",paste(countries,collapse = " "), ", DHS ",surveyyear_start," to ",surveyyear_end),
           caption = c(paste0("Snapped points in red; original cluster in black; buffer of ",max_distance_to_snap/1000," km around DHS clusters.
                              Data from DHS (",surveyyear_start," - ",surveyyear_end, "), GADM (2022), HydroRIVERS (2023).")),
           colour = "Elevation (m)")+
      xlab("") + ylab("")+
      theme_map(#legend_position = "none",
        axis_text_x = element_blank(),
        axis_text_y = element_blank()) # legend_position = c(0.9,0.3))  +
    # scale_colour_gradientn(colours = cols)
    
    #map
    
    
    out_path <- file.path(output_maps,"DHS")
    if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
    
    
    save_map(output_folder = out_path,
             plotname = map,
             filename = paste0(paste(dhs_gps_filenames,collapse="_"),"_with_rivers.png"),
             width = 8,
             height = 7,
             dpi  = 300)
    
    toc()