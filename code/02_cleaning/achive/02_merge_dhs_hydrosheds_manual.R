# _______________________________#
# Environment
# Clean 02: Merge DHS and GPS Data
# 
# Stallman
# Started: 2023-10-23
# Last edited: 2023-11-13
# Edit: added get elevation to merge function merge_dhs_gps
#________________________________#


# https://dhsprogram.com/data/Guide-to-DHS-Statistics/Adult_Mortality_Rates.htm

# Startup

  rm(list = ls())


# bring in the packages, folders, paths ----
  
  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))
  source(file.path(code_startup_general,"merge_dhs_gps.R")) # function for merging dhs and gps data

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

  level <- 2
  dhs_gps_filename <- "UGGE7AFL"
  country          <- "UG"
  n_cores          <- detectCores(logical = TRUE) - 2
  units_name       <- c("Uganda")
  equal_area_crs   <- "ESRI:102022"
  max_distance_to_snap <- 10000 # max distance at which to snap to a river, really high at this point; 10km b/c that's max perturbation
  
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
  # 
  
  names(hydro_rivers)
  # [1] "HYRIV_ID"     "NEXT_DOWN"    "MAIN_RIV"     "LENGTH_KM"    "DIST_DN_KM"   "DIST_UP_KM"   "CATCH_SKM"   
  # [8] "UPLAND_SKM"   "ENDORHEIC"    "DIS_AV_CMS"   "ORD_STRA"     "ORD_CLAS"     "ORD_FLOW"     "HYBAS_L12"   
  # [15] "Shape_Length" "Shape" 
  
 dhs_data <- readRDS(file.path(data_external_temp,"DHS","GPS",paste0(dhs_gps_filename,"_GADM_ADM_",level,".rds"))) %>%
              filter(!is.na(GID_0)) %>% # take out the ones that couldn't merge onto the country level
             st_transform(crs = equal_area_crs) 
 
 
 # bring in the country-level data
 in_path <- file.path(data_external_clean,"GADM","global")
 gadm_data  <- readRDS(file.path(in_path,paste0("GADM_global_ADM_1.rds"))) %>%
               st_transform(crs = equal_area_crs)
 
 
# start looping here ----
 GID_0_val    <- countrycode(country,
                             origin = "dhs",
                             destination = "iso3c")
 
 gadm_filtered <- gadm_data %>% filter(GID_0 == GID_0_val)
 
 gadm_union <- gadm_filtered %>% st_union() %>% st_make_valid() 
 
 units <- gadm_union 

  plot(st_geometry(units))
  
  st_crs(units)
  
  out_path <- file.path(data_external_temp,"shape-files","GADM")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  saveRDS(units,
          file = file.path(data_external_temp,"shape-files","GADM",paste0(units_name,"_.rds")))
  
  units <- readRDS(file =file.path(data_external_temp,"shape-files","GADM",paste0(units_name,"_.rds")) )
  
  tic(paste0("Split hydroRIVERS into our choice of country",units_name))
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
  
  
  
  out_path <- file.path(data_external_temp,"shape-files","hydroRIVERS")
  
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  saveRDS(object = hydro_rivers_units,
          file = file.path(out_path,paste0(units_name,"_hydrorivers.rds")))
  toc()
  
  # 65.16 sec elapsed for the Zambezi river basin; 19 sec for just Malawi
  # 20.5 sec elapsed for Uganda
  
  # 
  # 31.06 sec elapsed
  
  in_path <- file.path(data_external_temp,"shape-files","hydroRIVERS")
  
  hydro_rivers_units <- readRDS(file = file.path(in_path,paste0(units_name,"_hydrorivers.rds")))
  
# Assign nearest river stretch to points ----


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
  

# snap points to nearest river segments ----
  
  set.seed(16)
    
  example_distance_to_snap <- 60000 # make big buffers
  
  test_clusters <- dhs_data[sample(1:nrow(dhs_data), size = 15, replace = FALSE),] # %>% filter(LATNUM>2.5 & LONGNUM > 33) #
                    #
  test_rivers   <- hydro_rivers_units %>% slice_max(Shape_Length, n=20) # [sample(1:nrow(hydro_rivers_units), size = 20, replace = FALSE),]
  

  #  https://yutani.rbind.io/post/geom-sf-text-and-geom-sf-label-are-coming/

  
  #https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf
  
  # https://gis.stackexchange.com/questions/349955/getting-a-new-column-with-distance-to-the-nearest-feature-in-r
  
  # st_nearest_feature(a,b)
  # gives the index (row number) of the nearest feature in b to each feature in a
  nearest_indices <- st_nearest_feature(test_clusters,test_rivers)
  
  # use st_distance to get element-wise distances between each element of a and corresponding element of b
  
  closest_points <- test_clusters %>%
                    mutate(my_linestring = st_nearest_points(test_clusters,test_rivers[nearest_indices,], pairwise = TRUE),
                           closest_point = st_cast(my_linestring, 'POINT')[seq(2, nrow(.)*2, 2)],
                           distance      = st_distance(test_clusters, test_rivers[nearest_indices,], by_element = TRUE),
                           snapped_point_cond = st_sfc(ifelse(as.numeric(distance) <= example_distance_to_snap, st_geometry(closest_point),geometry),crs = st_crs(equal_area_crs))
                    ) %>%
                    cbind(test_clusters, st_drop_geometry(test_rivers)[nearest_indices,]) # join on river data


  
  map <- ggplot(data = units) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    geom_sf(data = test_rivers %>% st_buffer(example_distance_to_snap), col = 'red', fill = NA)+
    geom_sf(data = test_rivers,
            alpha = test_rivers$width,
            color = yale_medblue,
            linewidth = test_rivers$width) +
    # geom_sf_text(data = test_rivers,
    #              aes(label = test_rivers$HYRIV_ID),
    #              col = yale_medblue#row_number(test_rivers))
    #              )+
    geom_sf(data = closest_points$snapped_point_cond, shape = 1, col ='blue')+
                   #colour = test_clusters$elevation) +
    geom_sf(data = closest_points %>% filter(as.numeric(distance)<=example_distance_to_snap) %>% .$my_linestring, linetype = 'dotted')+
    geom_sf(data = closest_points %>% filter(as.numeric(distance)<=example_distance_to_snap), shape = 1)+
   geom_sf_text(data = test_clusters,
                aes(label = paste0("DHS_",DHSCLUST)))+
    geom_sf(data = closest_points)+
    labs(title = paste0("Example: Survey Clusters and Snapping to River Segments, Uganda 2016 DHS"),
         caption = c(paste0("Data from DHS (2016), GADM (2022), AWS (2023), HydroRIVERS (2023), example buffer of ",example_distance_to_snap/1000," km.")),
         colour = "Elevation (m)")+
    xlab("") + ylab("")+
    theme_map(legend_position = "none") +# legend_position = c(0.9,0.3))  +
    scale_colour_gradientn(colours = cols)
  
  map
  
  out_path <- file.path(output_maps,"DHS")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  
  save_map(output_folder = out_path,
           plotname = map,
           filename = paste0(units_name,"_test_clusters_rivers.png"),
           width = 8,
           height = 10,
           dpi  = 300)
  

  
# run through for all Uganda ----
  
  # st_nearest_feature(a,b)
  # gives the index (row number) of the nearest feature in b to each feature in a
  tic(paste0("Getting nearest river segment for all ",nrow(dhs_data), " clusters in ",dhs_gps_filename,
              " and all ",nrow(hydro_rivers_units)," river segments in the country polygon."))
  
  nearest_indices <- st_nearest_feature(dhs_data,hydro_rivers_units)
  
  toc()
  
  # .83 seconds, no issue
  
  # use st_distance to get element-wise distances between each element of a and corresponding element of b
  
  tic(paste0("Calculating distances, snapped point, and buffers for all ",nrow(dhs_data), " clusters in ",dhs_gps_filename,
             " and all ",nrow(hydro_rivers_units)," river segments in the country polygon."))
  
  closest_points <- dhs_data %>%
    mutate(my_linestring = st_nearest_points(dhs_data,hydro_rivers_units[nearest_indices,], pairwise = TRUE),
           closest_point = st_cast(my_linestring, 'POINT')[seq(2, nrow(.)*2, 2)],
           distance_to_river      = st_distance(dhs_data, hydro_rivers_units[nearest_indices,], by_element = TRUE),
           snapped_point_cond = st_sfc(ifelse(as.numeric(distance) <= max_distance_to_snap, st_geometry(closest_point),geometry),crs = st_crs(equal_area_crs))
    ) %>%
    cbind(st_drop_geometry(hydro_rivers_units[nearest_indices,]))
  
  # add in X Y coordinates in projected form
  closest_points$X <- st_coordinates(closest_points)[,1]
  closest_points$Y <- st_coordinates(closest_points)[,2]
  
  out_path <- file.path(data_external_clean,"DHS")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  
  saveRDS(object = closest_points,
          file = file.path(out_path,paste0(dhs_gps_filename,"_snapped_to_rivers.rds")))
  
  toc()
  # Calculating distances, snapped point, and buffers for all 685 clusters in UGGE7AFL and all 11763 river segments in the country polygon.: 1.53 sec elapsed
  
  closest_points <- readRDS( file = file.path(out_path,paste0(dhs_gps_filename,"_snapped_to_rivers.rds")))
  
# for each main river, calculate distances along the rivers ----
  
  # builds heavily on this guy
  # https://ryanpeek.org/2017-11-21-mapping-with-sf-part-3/
  
  # 10904406
  # 10047863
  
  main_river <- 10904406
  
  single_river <- hydro_rivers %>%
                filter(MAIN_RIV==main_river)%>%
    mutate(width = 1/as.numeric(ORD_CLAS)) %>%
    rename(geometry = Shape)
               
  # if we need an external boundary
  # polygon_boundary <- single_river %>% st_buffer(20000) %>% st_convex_hull() %>% st_union() 

  # find which is the rownumber of the source, we'll need this in the cleanup() function
  source_seg <- which(single_river$NEXT_DOWN == 0)
  
  test_points <- closest_points %>% filter(MAIN_RIV == main_river)
  
  
  
  source_stretch <- single_river %>% filter(NEXT_DOWN ==0)
  
  
  test_rn <- line2network(sf = single_river)
  
  test_rn$mouth$mouth.seg <- source_seg
  test_rn$mouth$mouth.vert<- 1
  

  points_on_river <- xy2segvert(x = test_points$X,
                                y = test_points$Y,
                                rivers = test_rn)
  
  
  
  riverdirection(startseg = 1548, endseg = 1516, startvert = 11, endvert = 2,
                 rivers = test_rn,
                 flowconnected = TRUE)
  
  
  # testing direction distances 
  data(Gulk, fakefish)
  # Mouth must be specified
  Gulk$mouth$mouth.seg <- 1
  Gulk$mouth$mouth.vert <- 1
  
  logi1 <- (fakefish$flight.date==as.Date("2015-11-25"))
  upstreammat(seg=fakefish$seg, vert=fakefish$vert, rivers=Gulk, logical=logi1)
  
  my_mat <- upstreammat(seg = points_on_river$seg,
                        vert = points_on_river$vert,
                        ID = test_points$DHSCLUST,
                        rivers = test_rn,
                        flowconnected = TRUE)
  
  # results (i,j) matrix, (i,j)th element >0 if j is upstream of i, else <0
  # flowconnected = TRUE returns distances if input locations are flow connected, else NA
  
  rn_cleaned <- cleanup(rivers = test_rn)
  # n to dissolve; then 69 
  
  # zoom to the river segments we want
  zoomtoseg(seg = 65:75, rivers = test_rn)
  
  # 
  # topologydots(rivers = test_rn)
  # 
  # rn_fixed <- cleanup(test_rn)
  # 
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
  

# Mapped ----
  

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
           filename = paste0(dhs_gps_filename,"_with_rivers.png"),
           width = 8,
           height = 10,
           dpi  = 300)
  
  
  #Calculating distances, snapped point, and 
  #buffers for all 685 clusters in UGGE7AFL and all 11763 river segments in the country polygon.: 0.94 sec elapsed
  
  
# run through all countries and DHSs ----
  # THIS IS USED FROM THE OLD MERGE FILE, NOTHING REALLY DONE YET
    
    # 
    # level <- 1
    
    for (level in 1:2){
      tic(paste0("Running through all countries at ADM level ", level," to merge with GPS data"))
      
      for (country in countries) {
        
        
        continent <- countrycode(country,
                                 origin = "dhs",
                                 destination = "continent")
        
        if (continent == "Africa"){
          
          GID_0_val    <- countrycode(country,
                                      origin = "dhs",
                                      destination = "iso3c")
          
          ## Steps: 
          ## 1) Assign nearest river stretch to the point
          ## 2) Assign a value of how far away that river stretch is from the point
          
          
          tic("restricting polygons to current country and getting union for river intersection")
          country_cast <- vector_cast %>% filter(GID_0 == GID_0_val)
          print(paste0("starting on country ",GID_0_val," and time is ",Sys.time()))
          units <- st_union(country_cast) %>% st_transform(crs = st_crs(hydro_rivers))
          
          dhs_gps_filename_rds <- paste0(dhs_gps_filename,".rds")
          
          dhs_gps_in <- file.path(data_external_temp,"DHS","GPS",dhs_gps_filename_rds)
          
          
          desired_datasets = gps_datasets %>% filter(DHS_CountryCode==country)
          dhs_gps_filename = stringr::str_extract(desired_datasets$FileName,"[^.]+")
          
          
          
          toc()
          
          
        } else print("Country's not in Africa, do another time, hydrorivers is using Africa")
        
      }
      toc()
    }
  