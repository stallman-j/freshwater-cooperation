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
  n_cores          <- 4 #detectCores(logical = TRUE) - 2
  #units_name       <- c("Uganda")
  equal_area_crs   <- "ESRI:102022"
  max_distance_to_snap <- 10000 # max distance at which to snap to a river, really high at this point; 10km b/c that's max perturbation
  surveyyear_start <- 2018 %>% as.character()
  surveyyear_end   <- 2020 %>% as.character()
  long_river_threshold <- 4000 
  period_length    <- 60 # what months window is used for the mortality data
# bring in hydrorivers ----
  
  
  # dhs_mortality_data <- readRDS(file = file.path(data_external_clean,"merged",
  #                                                paste0("Africa_all_years_DHS_",period_length,"_months_window_child_mortality_with_GPS.rds"))) %>%
  #   st_as_sf(crs = 4326) %>%
  #   st_transform(crs = equal_area_crs)
  # 
  # countries <- dhs_mortality_data$DHSCC %>% unique()
  # 
  # 
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
  
# get hydrorivers split into countries ----
  
  river_countries <- gadm_data %>% 
    filter(continent == "Africa")
  
  countries_single <- unique(river_countries$GID_0)  # all of these needed, do for (countries in countries_single)
  
  # getting these through the folder file.path(data_external_clean,"HydroSHEDS","hydro_rivers_units_missing")
  country_groups <- list( c("ESH","MRT"),
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
  

# bring in data ----
  
  names(hydro_rivers)
  # [1] "HYRIV_ID"     "NEXT_DOWN"    "MAIN_RIV"     "LENGTH_KM"    "DIST_DN_KM"  
  # [6] "DIST_UP_KM"   "CATCH_SKM"    "UPLAND_SKM"   "ENDORHEIC"    "DIS_AV_CMS"  
  # [11] "ORD_STRA"     "ORD_CLAS"     "ORD_FLOW"     "HYBAS_L12"    "Shape_Length"
  # [16] "geometry" 
  
  # country codes 
  #  https://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10136
  

  # a little circular since this file got created within the loop below but whatevs for now
  out_path <- file.path(data_external_temp,"shape-files","hydroRIVERS")
  hydro_rivers_units <- readRDS(file = file.path(out_path,paste0("SEN_hydrorivers.rds")))
  
  main_rivers_senegal <- hydro_rivers_units$MAIN_RIV %>% unique() %>% .[-c(1)] # got this from restricting units to Senegal later on
  
  # get all the main rivers and convert to a vector to loop through
  main_rivers_all <- hydro_rivers %>% st_drop_geometry() %>% select(MAIN_RIV) %>%
                     filter(!MAIN_RIV %in% main_rivers_senegal) %>% 
                    unique() %>% as.vector() %>% .[[1]]
                  
  

# this was based off an old version ----
  
  # need to keep track of which rivers have been checked
  # main_rivers_df <- hydro_rivers %>% st_drop_geometry() %>% select(MAIN_RIV) %>%
  #   unique()
  # 
  # main_rivers_df <- main_rivers_df %>%
  #   mutate(checked = 0) %>%
  #   mutate(checked = ifelse(row_number()< 67, 1, 0))
  # 
  # 
  # main_rivers_df <- readRDS(file = file.path(data_external_clean,"HydroSHEDS","main_rivers_checking_df.rds") )
  # 
  # 
  # out_path <- file.path(data_external_clean,"HydroSHEDS","checked_main_rivers")
  # if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  # 
  # i<- 1
  # for (i in 1:nrow(main_rivers_df)){
  #   
  #   main_river <- main_rivers_df[i,1]
  #   
  #   checked_already <- main_rivers_df[i,2]
  #   
  #   if (checked_already == 1){
  #     
  #     saveRDS(main_river, file.path(out_path, paste0("MAIN_RIV_",main_river,"_already_checked.rds")))
  #   }
  # 
  #     
  # }

# get the DHS locations placed on river networks ----
  
  # initialize a counter to see where we're at
  i <- 1
  
  #j <- 1
  # https://ryanpeek.org/2017-11-21-mapping-with-sf-part-3/
  
  # export: sf
  # export: riverdist
  # export: ggplot2
  
  # export: hydro_rivers
  # export: gadm_data
  
  # get_dhs_river_intersections ----
  get_dhs_river_intersections <- function(main_river,
                                          output_maps  = file.path("P:","Projects","environment","output","03_maps"),
                                          data_clean_path = file.path("E:","data","03_clean"),
                                          hydro_rivers = hydro_rivers,
                                          gadm_data    = gadm_data){
    
  
    # functions ----
    
    theme_map <- function(legend_text_size = 8,
                          legend_title_size = 10,
                          legend_position = c(0.2,0.3), # first term is LR, second up-down. "none" for no legend
                          axis_title_x = element_text(color = "black"), # element_blank() # to remove
                          axis_title_y = element_text(color = "black"), # element_blank() # to remove
                          axis_text_x  = element_text(color = "darkgrey"), # element_blank() # to remove
                          axis_text_y  = element_text(color = "darkgrey"), # element_blank() # to remove
                          ...) {
      theme_minimal() +
        theme(
          text = element_text(color = "#22211d"),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.text.x = axis_text_x,
          axis.text.y = axis_text_y,
          axis.ticks = element_blank(),
          axis.ticks.length = unit(0, "pt"), #length of tick marks
          #axis.ticks.x = element_blank(),
          axis.title.x = axis_title_x,
          axis.title.y = axis_title_y,
          
          # Background Panels
          # panel.grid.minor = element_line(color = "#ebebe5", linewidth = 0.2),
          panel.grid.major = element_blank(), #element_line(color = "#ebebe5", linewidth = 0.2),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA), 
          panel.background = element_rect(fill = "white", color = NA), 
          panel.border = element_blank(),
          #plot.caption = element_blank(), 
          #element_text(face = "italic", linewidth = 6,
          #lineheight = 0.4),
          # Legends
          legend.background = element_rect(fill = "white", color = "#ebebe5", linewidth = 0.3),
          legend.position = legend_position, # put inside the plot
          legend.key.width = unit(.8, 'cm'), # legend box width,
          legend.key.height = unit(.8,'cm'), # legend box height
          #legend.text = element_text(linewidth = legend_text_size),
          #legend.title = element_text(linewidth = legend_title_size),
          plot.margin = unit(c(0,0,0,0), "mm"), # T R BL
          ...
        )
      # if the points on the legend are way too big 
    }
    
  
    
    save_map <- function(output_folder = output_maps,
                         plotname,
                         filename,
                         width = 9,
                         height = 5,
                         dpi    = 300)  {
      
      # create the output folder if it doesn't exist already
      if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE) # recursive lets you create any needed subdirectories
      
      
      ggsave(filename = file.path(output_folder,filename),
             plot = plotname,
             device = png,
             width = width,
             height = height,
             units = c("in"),
             dpi   = dpi)
    }
    
  

    # add a few paths ----
    # path to put flags if river network doesn't exist
    paths <- c(file.path(data_clean_path,"HydroSHEDS","river_network_missing"), # for putting flags where rivers are DNE (do not e)
               file.path(data_clean_path,"HydroSHEDS","checked_main_rivers"), # for putting flags over which rivers have been checked
               file.path(data_clean_path,"shape-files","GADM"), # for putting shapefiles of the subsets of countries which intersect a particular river
               file.path(data_clean_path,"shape-files","HydroSHEDS"), # for putting country_hydro_rivers_units files
               file.path(data_clean_path,"HydroSHEDS","hydro_rivers_units_missing"),
               file.path(data_clean_path,"merged","DHS_HydroSHEDS"), # for outputting distance matrices
               file.path(output_maps,"HydroSHEDS") # output folder for mapping
               )
    
    
    for (path in paths){
    if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  }

    
  # check if river has already been checked ----
    
    # if the river has already been checked, don't check it again
    if (file.exists(file = file.path(data_clean_path,"HydroSHEDS","checked_main_rivers",paste0("MAIN_RIV_",main_river,"_already_checked.rds")))){
      
      print(paste0("Current river network for main_river ",main_river," has already been checked. No need to go through this again."))
      #stop({message(paste0("Warning: ",main_river," has already been checked"))})

      
    } else {
      
    # bring in current river network ----
      
      # if the current river network doesn't exist, end and note that there's a current river network that doesn't exist
      if (!file.exists(file = file.path(data_clean_path,"HydroSHEDS","river_networks",paste0("MAIN_RIV_",main_river,"_cleaned_river_network.rds")))){
          
        print(paste0("Current river network for main_river ",main_river," doesn't exist. Create it with a different function."))
        

        saveRDS(main_river,file.path(data_clean_path,"HydroSHEDS","river_network_missing",paste0("MAIN_RIV_",main_river,"_network_doesnt_exist.rds")))
        
        #stop({message(paste0("Error: ",main_river," does not have a current river network created"))})
        
      } else {
        
  # bring in river network ----
    
  current_river_network <- readRDS(file = file.path(data_clean_path,"HydroSHEDS","river_networks",paste0("MAIN_RIV_",main_river,"_cleaned_river_network.rds")))
        
  single_river <- hydro_rivers %>%
    dplyr::filter(MAIN_RIV==main_river)%>%
    dplyr::mutate(width = 1/as.numeric(ORD_CLAS))
  
  # get polygon boundary of river network ----
  
  polygon_boundary <- single_river %>% st_buffer(20000) %>% st_convex_hull() %>% st_union() 
        
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
  
  dhs_mortality_data_river_countries <- dhs_mortality_data %>%
    filter(DHSCC %in% countries_DHS)
      
    # see if there's DHS mortality data for these countries ----
      
  if (nrow(dhs_mortality_data_river_countries)==0){
    print(paste0("No DHS datasets intersect with MAIN_RIV ",main_river,". Ending function here."))

    
    saveRDS(main_river,
            file.path(data_clean_path,"HydroSHEDS","checked_main_rivers",paste0("MAIN_RIV_",main_river,"_already_checked.rds")))
    
    #stop({message(paste0("MAIN_RIV ",main_river," has no DHS datasets intersecting with it."))})
    

  } else {
    
    # loop over years to see if there's an intersection ----
      
    years <- unique(dhs_mortality_data_river_countries$end_year)
    
    
    for (year in years){
      
      #print(paste0("Working on DHS for ",year, " of years ",paste(years, collapse = " ")," in countries ", paste(countries, collapse = " ")))
      
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
        
        
        #plot(st_geometry(river_polygons))

        
    # check if hydrorivers_units exists. If not exit and flag that the units need to be made ----
      
        if (file.exists(file.path(data_clean_path,"shape-files","GADM",paste0(paste(countries,collapse="_"),"_union_shapefile.rds")))) {
          
          units <- readRDS(file = file.path(data_clean_path,"shape-files","GADM",paste0(paste(countries,collapse="_"),"_union_shapefile.rds")))
          
          } else {
        units <- gadm_data %>% filter(GID_0 %in% countries) %>% st_union() %>% st_make_valid() 
        
        saveRDS(units,
                file = file.path(data_clean_path,"shape-files","GADM",paste0(paste(countries,collapse="_"),"_union_shapefile.rds")))
        } # end ifelse over if the GADM file exists then bring it in, otherwise create it
        
        if (!file.exists( file.path(data_clean_path,"HydroSHEDS","shape-files",paste0(paste(countries,collapse = "_"),"_hydrorivers.rds")))) {
          
          # flag that the units are missing
          saveRDS(c(countries),
                  file.path(data_clean_path,"HydroSHEDS","hydro_rivers_units_missing",paste0(paste(countries,collapse = "_"),"hydro_rivers_units_missing.rds")))
          
          
        } else {
          
          hydro_rivers_units <- readRDS(file = file.path(data_clean_path,"HydroSHEDS","shape-files",paste0(paste(countries,collapse = "_"),"_hydrorivers.rds")))
          
        
        
    # get closest points ----
          
    # this is a fast operation
    nearest_indices <- st_nearest_feature(dhs_mortality_river_single_year,hydro_rivers_units)
    
    # this is also fast
    closest_points <- dhs_mortality_river_single_year %>%
      mutate(my_linestring = st_nearest_points(dhs_mortality_river_single_year,hydro_rivers_units[nearest_indices,], pairwise = TRUE),
             closest_point = st_cast(my_linestring, 'POINT')[seq(2, nrow(.)*2, 2)],
             distance_to_river      = st_distance(dhs_mortality_river_single_year, hydro_rivers_units[nearest_indices,], by_element = TRUE),
             snapped_point_cond = st_sfc(ifelse(as.numeric(distance_to_river) <= max_distance_to_snap, st_geometry(closest_point),geometry),crs = st_crs(equal_area_crs))
      ) %>%
      cbind(st_drop_geometry(hydro_rivers_units[nearest_indices,]))
    
    # add in X Y coordinates in projected form
    closest_points$X <- st_coordinates(closest_points)[,1]
    closest_points$Y <- st_coordinates(closest_points)[,2]
    
    current_points <- closest_points %>% filter(MAIN_RIV == main_river)
      
    # save dyad distances ----
    
    if (nrow(current_points)==0) {
      print(paste0("No points in current DHS in year ", year, " match with the current choice of river ID #,",main_river, ". Keep looping."))
    } else{
      
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
        filter(value > 0) %>%
        rename(downstream = row,
               upstream   = col,
               distance_m = value)
      
      saveRDS(object = dyad_upstream,
              file =file.path(data_clean_path,"merged","DHS_HydroSHEDS",paste0("DHS_",year,"_",paste(countries,collapse="_"),"_MAIN_RIV_",main_river,"_dyad_distances.rds")))
      
      saveRDS(object = dyad_distances,
              file = file.path(data_clean_path,"merged","DHS_HydroSHEDS",paste0("DHS_",year,"_",paste(countries,collapse="_"),"_MAIN_RIV_",main_river,"_distances_matrix.rds")))
      
    # plot ----
        
      #for main_river == 10508130 this is a good map to make
      map <- ggplot() +
        # geom_sf(data = units,
        #         color = "gray70",
        #         fill = "gray99",
        #         alpha = 0.5,
        #         linewidth = .3)+
        geom_sf(data = single_river,
                color = "#286dc0",
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
      
     # map

      
      save_map(output_folder = file.path(output_maps,"HydroSHEDS"),
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
                color = "#286dc0",
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
      
      #map
      
      
      save_map(output_folder = file.path(output_maps,"HydroSHEDS"),
               plotname = map,
               filename = paste0(paste(countries,collapse = "_"),"_MAIN_RIV_full_",main_river,"_",year,"_river_distances.png"),
               width = 8,
               height = 7,
               dpi  = 300)
      
      
      saveRDS(main_river,
              file.path(data_clean_path,"HydroSHEDS","checked_main_rivers",paste0("MAIN_RIV_",main_river,"_already_checked.rds")))
      
 
  
  
 } # end ifelse statement for if no DHS points match with current river ID
        } # end ifelse statement for stop if hydro_rivers_units is missing
      } # end ifelse statement for if no DHS datasets in current year
      } # end loop over years
  } # end if-else statement if there aren't any DHS datasets for the MAIN_RIV of interest
      } # end ifelse if current_river network doesn't already exist, exit
    } # end ifelse if main_river has already been checked, don't check it again
    
  } # end function
  
  
  
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
    
