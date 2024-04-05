# _______________________________#
# International Agreements
# Clean 02: Clean Geodar global dams and catchment areas
# 
# Stallman
# Started 2023-04-04
# Last edited: 
#________________________________#



# Startup

rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

# read in data ----
  

  
  path <- file.path(data_raw,"GeoDAR-TopoCat_drainage-topology-catchment-database","GeoDAR_TopoCat","GeoDAR_TopoCat_v11_10.gdb")
  
  # view available layers
  st_layers(path)
  
  # download the countries
  world_catchments <- st_read(dsn = path,
                        layer = "Catchments")

  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  path <- file.path(data_raw,"GeoDAR_georeferenced-global-dams-reservoirs","GeoDAR_v10_v11","GeoDAR_v10_v11")
  st_layers(path)
  
  world_dams <- st_read(dsn = path,
                        layer = "GeoDAR_v11_dams")
  
  
  world_reservoirs <- st_read(dsn = path,
                             layer = "GeoDAR_v11_reservoirs")
  
  sf_use_s2(FALSE) # removes spherical geometry
  
  world <- st_transform(world,projection_crs)
  world_catchments <- st_transform(world_catchments,projection_crs)
  world_dams <- st_transform(world_dams,projection_crs)
  world_reservoirs <- st_transform(world_reservoirs,projection_crs)
  
  china <- world[world$name == "China",]
  
  plot(china)
  
  
  
  
  china_reservoirs <- st_intersection(world_reservoirs,china)
  china_catchment <- st_intersection(world_catchments,china)
  china_dams <- st_intersection(world_dams,china)
  
  
  saveRDS(china_dams,
          file = file.path(data_clean,"china_dams_geodar.rds"))
  
  saveRDS(china_reservoirs,
          file = file.path(data_clean,"china_reservoirs_geodar.rds"))
  
  

  saveRDS(china_catchment,
          file = file.path(data_clean,"china_catchment.rds"))
  
  china_reservoirs <- readRDS(file = file.path(data_clean,"china_reservoirs_geodar.rds"))
  china_catchment <- readRDS(file = file.path(data_clean,"china_catchment.rds"))
  
  china_dams <- readRDS(file = file.path(data_clean,"china_dams_geodar.rds"))
  
  
# plot ----
  map <- ggplot(data = china) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    # geom_sf(data = basin_shapes,
    #         alpha = .2,
    #         color = yale_lblue,
    #         fill  = yale_lblue) +
    geom_sf(data = china_catchment,
            alpha = .3,
            color = yale_lblue,
            linewidth = .5) +
    geom_sf(data = china_dams,
            alpha = .4,
            color = yale_blue,
            size = .5) +
    labs(title = "Dams and their Catchments, China",
         caption = c("Data from GeoDAR and GeoDAR-TopoCat 2023")) +
    theme_map()
  
  map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = "china_dams_catchments.png",
           width = 9,
           height = 5,
           dpi  = 300)
  
  
  map <- ggplot(data = china) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    # geom_sf(data = basin_shapes,
    #         alpha = .2,
    #         color = yale_lblue,
    #         fill  = yale_lblue) +
    geom_sf(data = china_catchment,
            alpha = .3,
            color = yale_lblue,
            linewidth = .5) +
    geom_sf(data = china_dams,
            alpha = .4,
            color = yale_blue,
            size = .5) +
    labs(title = "Dam and Lake Reservoirs, China",
         caption = c("Data from GeoDAR and GeoDAR-TopoCat 2023")) +
    theme_map()
  
  map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = "china_reservoirs.png",
           width = 9,
           height = 5,
           dpi  = 300)
  
  
  
  tmap_mode("view")
  # check that these worked
  
  tm_shape(china_catchment) + 
    tm_polygons(china_catchment)+
    tm_bubbles(china_dams) +
    tm_grid(labels.show = TRUE,
            labels.inside.frame = TRUE) # add lat long
  
  

  