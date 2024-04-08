#' Get Closest Points on a River
#' @description from points_data which has lat, long, an id variable, and country assigned, as well as a main_river which defines a river network, get the lat-long coordinates of the nearest point on the river network to all points in the points_data data frame
#' @param main_river a specification in hydro rivers of a MAIN_RIV
#' @param points_data sf data frame with the points we would like to snap to the river. needs to have at least four variables, below:
#' @param lat_varname column name (character vector) of the latitude coordinate
#' @param long_varname character vec of column name of the longitude coordinate
#' @param id_varname character vector of the point ID name
#' @param country_varname character vector of the ISO3c country name for each point
#' @param data_clean_path location to place the cleaned data
#' @param data_temp_path path of the temp data on which a number of inputs to this function live
#' @param river_network_path
#' @param hydro_rivers_units_path
#' @param glow_countries_units_path
#' @param gadm_union_shapefile_path
#' @param max_distance_to_snap
#' @param equal_area_crs 
#' @export
#' 
get_river_points_safe <- function(main_river,
                             points_data,
                             lat_varname = "lat",
                             long_varname = "lon",
                             id_varname   = "ID",
                             country_varname = "country_iso3c",
                                       data_clean_path = file.path("E:","data","03_clean"),
                                       data_temp_path  = file.path("E:","data","02_temp"),
                                       river_network_path = file.path("E:","data","03_clean","HydroSHEDS","river_networks"),
                                       hydro_rivers_units_path = file.path("E:","data","03_clean","HydroSHEDS","shape-files"),
                                       glow_countries_units_path = file.path("E:","data","02_temp","GLOW_global-long-term-river-width","shape-files"),
                                       gadm_union_shapefile_path = file.path("E:","data","03_clean","shape-files","GADM"),
                                       max_distance_to_snap = 10100,
                                       equal_area_crs = "ESRI:102022"
){
  
  tryCatch( {
    # create output folders
    checked_river_path         <- file.path(data_temp_path,"merged","DHS_GLOW_HydroSHEDS","checked-main-rivers","dhs-glow-river-points")
    river_network_missing_path <- file.path(data_temp_path,"HydroSHEDS","river-network-missing")
    river_points_path        <- file.path(data_temp_path,"merged","DHS_GLOW_HydroSHEDS","river-points")
    hydro_rivers_units_missing_path <- file.path(data_temp_path,"HydroSHEDS","hydro_rivers_units_missing")
    glow_countries_units_missing_path  <- file.path(data_temp_path,"GLOW_global-long-term-river-width","glow_units_missing")
    
    paths_to_create <- c(checked_river_path,
                         river_network_missing_path,
                         river_points_path,
                         hydro_rivers_units_missing_path,
                         glow_countries_units_missing_path
    )
    
    for (path in paths_to_create){
      
      if (!dir.exists(path)) dir.create(path, recursive = TRUE)
      
    }
    
    # create temp variables for the points data 
    # create some variables to manipulate; this is easier in base R than dplyr?
    points_data[["lat"]]          <- points_data[[lat_varname]]
    points_data[["lon"]]          <- points_data[[long_varname]]
    points_data[["ID"]]            <- points_data[[id_varname]]
    points_data[["country_iso3c"]] <- points_data[[country_varname]]
    
    
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
      
      stop(paste0("These river points for main_river ",main_river," have already been calculated. No need to go through this again."))
      
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

    points_data_restricted <- points_data %>%
      filter(country_iso3c %in% countries) %>%
      select(ID,lat,lon,geometry,type)%>%
      st_transform(crs = st_crs(equal_area_crs))
    
    
    
    if (nrow(points_data_restricted)==0){
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
    
    
    if (!file.exists( file.path(glow_countries_units_path,paste0(paste(countries,collapse = "_"),"_glow.rds")))) {
      
      # flag that the units are missing
      saveRDS(c(countries),
              file.path(glow_countries_units_missing_path,paste0(paste(countries,collapse = "_"),"glow_rivers_units_missing.rds")))
      
      stop(paste0("GLOW file for countries ",paste(countries,collapse = " "), "is missing. Create elsewhere first (it takes a while) then come back."))
      
    } 
    
    
    glow_country_units <- readRDS(file = file.path(glow_countries_units_path,paste0(paste(countries,collapse = "_"),"_glow.rds"))) %>%
                          dplyr::mutate(type = "GLOW") %>%
                          st_transform(crs = st_crs(equal_area_crs))
    
    points_data_full <- rbind(glow_country_units,points_data_restricted)
    
    # get closest points of the DHS to the rivers
    
    # this is a fast operation
    nearest_indices <- st_nearest_feature(points_data_full,hydro_rivers_units)
    
    # this is also fast
    closest_points <- points_data_full %>%
      mutate(my_linestring = sf::st_nearest_points(points_data_full,hydro_rivers_units[nearest_indices,], pairwise = TRUE),
             closest_point = sf::st_cast(my_linestring, 'POINT')[seq(2, nrow(.)*2, 2)],
             distance_to_river      = sf::st_distance(points_data_full, hydro_rivers_units[nearest_indices,], by_element = TRUE),
             snapped_point_cond = sf::st_sfc(ifelse(as.numeric(distance_to_river) <= max_distance_to_snap, sf::st_geometry(closest_point),geometry),crs = st_crs(equal_area_crs))
      ) %>%
      cbind(sf::st_drop_geometry(hydro_rivers_units[nearest_indices,]))
    
    # add in X Y coordinates in projected form
    closest_points$X <- sf::st_coordinates(closest_points)[,1]
    closest_points$Y <- sf::st_coordinates(closest_points)[,2]
    
    current_points <- closest_points %>% filter(MAIN_RIV == main_river)
    
    
    # save 
    
    if (nrow(current_points)==0) {
      saveRDS(main_river,
              file.path(checked_river_path,paste0("MAIN_RIV_",main_river,"_already_checked.rds")))
      
      stop(paste0("No points in points data match with MAIN_RIV ",main_river))
      
      
    }
    
    saveRDS(object = current_points,
            file =file.path(river_points_path,paste0("DHS_GLOW_MAIN_RIV_",main_river,"_river_points.rds")))
    
    saveRDS(main_river,
            file.path(checked_river_path,paste0("MAIN_RIV_",main_river,"_already_checked.rds")))
    
  }, # main part of the function for tryCatch
  
  error = function(e) {
    # code executed in event of an error
    return(0) },
  warning = function(w) {
    # code executed in event of a warning
    return(1)
  }
  
  
  ) # end tryCatch
} # end function



get_river_points <- function(main_river,
                                  points_data,
                                  lat_varname = "lat",
                                  long_varname = "lon",
                                  id_varname   = "ID",
                                  country_varname = "country_iso3c",
                                  data_clean_path = file.path("E:","data","03_clean"),
                                  data_temp_path  = file.path("E:","data","02_temp"),
                                  river_network_path = file.path("E:","data","03_clean","HydroSHEDS","river_networks"),
                                  hydro_rivers_units_path = file.path("E:","data","03_clean","HydroSHEDS","shape-files"),
                                  glow_countries_units_path = file.path("E:","data","02_temp","GLOW_global-long-term-river-width","shape-files"),
                                  gadm_union_shapefile_path = file.path("E:","data","03_clean","shape-files","GADM"),
                                  max_distance_to_snap = 10100,
                                  equal_area_crs = "ESRI:102022"
){
  

    # create output folders
    checked_river_path         <- file.path(data_temp_path,"merged","DHS_GLOW_HydroSHEDS","checked-main-rivers","dhs-glow-river-points")
    river_network_missing_path <- file.path(data_temp_path,"HydroSHEDS","river-network-missing")
    river_points_path        <- file.path(data_temp_path,"merged","DHS_GLOW_HydroSHEDS","river-points")
    hydro_rivers_units_missing_path <- file.path(data_temp_path,"HydroSHEDS","hydro_rivers_units_missing")
    glow_countries_units_missing_path  <- file.path(data_temp_path,"GLOW_global-long-term-river-width","glow_units_missing")
    
    paths_to_create <- c(checked_river_path,
                         river_network_missing_path,
                         river_points_path,
                         hydro_rivers_units_missing_path,
                         glow_countries_units_missing_path
    )
    
    for (path in paths_to_create){
      
      if (!dir.exists(path)) dir.create(path, recursive = TRUE)
      
    }
    
    # create temp variables for the points data 
    # create some variables to manipulate; this is easier in base R than dplyr?
    points_data[["lat"]]          <- points_data[[lat_varname]]
    points_data[["lon"]]          <- points_data[[long_varname]]
    points_data[["ID"]]            <- points_data[[id_varname]]
    points_data[["country_iso3c"]] <- points_data[[country_varname]]
    
    
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
      
      stop(paste0("These river points for main_river ",main_river," have already been calculated. No need to go through this again."))
      
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
    
    points_data_restricted <- points_data %>%
      filter(country_iso3c %in% countries) %>%
      select(ID,lat,lon,geometry,type)%>%
      st_transform(crs = st_crs(equal_area_crs))
    
    
    
    if (nrow(points_data_restricted)==0){
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
    
    
    if (!file.exists( file.path(glow_countries_units_path,paste0(paste(countries,collapse = "_"),"_glow.rds")))) {
      
      # flag that the units are missing
      saveRDS(c(countries),
              file.path(glow_countries_units_missing_path,paste0(paste(countries,collapse = "_"),"glow_rivers_units_missing.rds")))
      
      stop(paste0("GLOW file for countries ",paste(countries,collapse = " "), "is missing. Create elsewhere first (it takes a while) then come back."))
      
    } 
    
    
    glow_country_units <- readRDS(file = file.path(glow_countries_units_path,paste0(paste(countries,collapse = "_"),"_glow.rds"))) %>%
      dplyr::mutate(type = "GLOW")
    
    points_data_full <- rbind(glow_country_units,points_data_restricted)
    
    # get closest points of the DHS to the rivers
    
    # this is a fast operation
    nearest_indices <- st_nearest_feature(points_data_full,hydro_rivers_units)
    
    # this is also fast
    closest_points <- points_data_full %>%
      mutate(my_linestring = sf::st_nearest_points(points_data_full,hydro_rivers_units[nearest_indices,], pairwise = TRUE),
             closest_point = sf::st_cast(my_linestring, 'POINT')[seq(2, nrow(.)*2, 2)],
             distance_to_river      = sf::st_distance(points_data_full, hydro_rivers_units[nearest_indices,], by_element = TRUE),
             snapped_point_cond = sf::st_sfc(ifelse(as.numeric(distance_to_river) <= max_distance_to_snap, sf::st_geometry(closest_point),geometry),crs = st_crs(equal_area_crs))
      ) %>%
      cbind(sf::st_drop_geometry(hydro_rivers_units[nearest_indices,]))
    
    # add in X Y coordinates in projected form
    closest_points$X <- sf::st_coordinates(closest_points)[,1]
    closest_points$Y <- sf::st_coordinates(closest_points)[,2]
    
    current_points <- closest_points %>% filter(MAIN_RIV == main_river)
    
    
    # save 
    
    if (nrow(current_points)==0) {
      saveRDS(main_river,
              file.path(checked_river_path,paste0("MAIN_RIV_",main_river,"_already_checked.rds")))
      
      stop(paste0("No points in points data match with MAIN_RIV ",main_river))
      
      
    }
    
    saveRDS(object = current_points,
            file =file.path(river_points_path,paste0("DHS_GLOW_MAIN_RIV_",main_river,"_river_points.rds")))
    
    saveRDS(main_river,
            file.path(checked_river_path,paste0("MAIN_RIV_",main_river,"_already_checked.rds")))
    

} # end function
