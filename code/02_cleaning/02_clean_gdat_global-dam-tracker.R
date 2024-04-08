# _______________________________#
# Environment
# Clean 02: Clean GADM country shapefiles
# 
# Stallman
# Started 2023-10-11
# Last edited: 
#________________________________#



# Startup
  
  rm(list = ls())
  
  
  # bring in the packages, folders, paths
  
  home_folder <- file.path("P:","Projects","freshwater-cooperation")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

# packages ----
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    sf,
    tmap,
    countrycode
  )

# bring in data ----
  
  path <- file.path(data_external_raw,"GDAT_global-dam-tracker","GDAT_data_v1","GDAT_data_v1","data")
  
  st_layers(path)
  
  # turn off spherical geometry
  # see https://github.com/r-spatial/sf/issues/1902
  #sf_use_s2(FALSE)
  
  out_path <- file.path(data_external_clean,"GDAT_global-dam-tracker")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  
  system.time(
  world <- readRDS(file.path(data_external_clean,"world.rds")) 
  )
  
  africa <- world %>% filter(continent == "Africa")
  
  plot(st_geometry(africa))
  
  
  dams <- st_read(dsn = path, 
                        layer = "GDAT_v1_dams") %>%
          st_make_valid() %>%
    dplyr::rename(ID = Feature_ID,
                  lat = Lat,
                  lon = Long
    ) %>%
    dplyr::mutate(country_iso3c = countrycode::countrycode(Admin0,
                                                           origin = "country.name",
                                                           destination = "iso3c"))
  
  
  
  names(dams)
  
  dams_africa <- st_intersection(dams,africa)
  
  catchments <- st_read(dsn = path, 
                  layer = "GDAT_v1_catchments") %>%
    st_make_valid()
  
  
  catchments_africa <- st_intersection(catchments,africa)
  
  names(catchments)
  

  saveRDS(catchments_africa,
          file = file.path(out_path,paste0("GDAT_catchments-africa.rds")))
  
  saveRDS(dams_africa,
          file = file.path(out_path,paste0("GDAT_dams-africa.rds")))
  
  saveRDS(catchments,
          file = file.path(out_path,paste0("GDAT_catchments.rds")))
  
  saveRDS(dams,
          file = file.path(out_path,paste0("GDAT_dams.rds")))
  
  dams_africa_restricted <- dams_africa %>%
    select(ID,lat,lon,country_iso3c)
  
  saveRDS(dams_africa_restricted,
          file = file.path(data_external_clean,"GDAT_global-dam-tracker",paste0("GDAT_dams-africa_for_river_points.rds")))
  
  
# make a plot ----

  

  system.time(
    world <- readRDS(file.path(data_external_clean,"world.rds")) 
  )
  
  africa <- world %>% filter(continent == "Africa")
  
  dams_africa <- readRDS(file.path(data_external_clean,"GDAT_global-dam-tracker","GDAT_dams-af"))
  
  map <- ggplot(data = africa) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    # geom_sf(data = catchments_africa,
    #         fill = yale_lblue,
    #         alpha = .3
    #         ) +
    geom_sf(data = dams_africa,
            color = "brown",
            shape = 17) +
    labs(title = paste0("Dam Locations in Africa"),
         caption = c("Data from GDAT (2023)")) +
    theme_map()
  
  map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = paste0("gdat_dams_in_africa.png"),
           width = 8,
           height = 9,
           dpi  = 300)
  
