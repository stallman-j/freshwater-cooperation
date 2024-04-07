# _______________________________#
# International Agreements
# Clean 02: Clean grwl global river widths from landsat
# 
# Stallman
# Started 2023-05-10
# Last edited: 
#________________________________#


# https://gee-community-catalog.org/projects/grwl/
# https://www.science.org/doi/10.1126/science.aat0636
# global extent of rivers and streams, allen and pavelsky 2018, Science

# Startup

rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

# read in data ----
  

  
  path <- file.path(data_raw,"GRWL_global-river-widths-landsat","GRWL_summaryStats_V01.01")
  
  # view available layers
  st_layers(path)
  
  #          layer_name geometry_type features fields crs_name
 #  1 GRWL_summaryStats   Line String    42077     10   WGS 84
  
  # download the countries
  rivers_grwl <- st_read(dsn = path,
                        layer = "GRWL_summaryStats")

    #   Reading layer `GRWL_summaryStats' from data source 
    #   `P:\Projects\environment\data\01_raw\GRWL_global-river-widths-landsat\GRWL_summaryStats_V01.01' 
    #   using driver `ESRI Shapefile'
    # Simple feature collection with 42077 features and 10 fields
    # Geometry type: MULTILINESTRING
    # Dimension:     XY
    # Bounding box:  xmin: -180 ymin: -54.31026 xmax: 180 ymax: 82.31062
    # Geodetic CRS:  WGS 84


  names(rivers_grwl)
  # [1] "OBJECTID"   "ID"         "width_min_" "width_med_" "width_mean" "width_max_"
  # [7] "width_sd_m" "lakeFlag"   "nSegPx"     "Shape_Leng" "geometry" 
  
  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  
  # continent_name comes from do file 00_startup_parameters
  
  my_continent <- world %>%
                  filter(continent == continent_name)
  
  plot(st_geometry(my_continent))
  
  st_crs(world)
  
  # change crs to be same as the rivers
  my_continent <- my_continent %>%
                  st_transform(crs = st_crs(rivers_grwl))
  
  
  # spatial subset so we can work with just continent's rivers
  continent_rivers_grwl <- rivers_grwl[my_continent,]
  
  saveRDS(continent_rivers_grwl,
          file = file.path(data_clean,"shape-files",paste0(continent_name,"_rivers_grwl.rds")))
  
 continent_rivers_grwl <- readRDS(file = file.path(data_clean,"shape-files",paste0(continent_name,"_rivers_grwl.rds")))
  
 
 saveRDS(rivers_grwl,
         file = file.path(data_clean,"shape-files",paste0("world","_rivers_grwl.rds")))
 
 rivers_grwl <- readRDS(file = file.path(data_clean,"shape-files",paste0("world","_rivers_grwl.rds")))
 
 
 
  
  # take out the world rivers for space
 # rm(rivers_grwl)
  
  
  path <- file.path(data_raw,"GeoDAR_georeferenced-global-dams-reservoirs","GeoDAR_v10_v11","GeoDAR_v10_v11")
  st_layers(path)
  
  world_dams <- st_read(dsn = path,
                        layer = "GeoDAR_v11_dams")
  
  world_dams <- world_dams %>%
    st_transform(crs = st_crs(continent_rivers_grwl))
  
  continent_dams <- st_intersection(world_dams,my_continent)
  
  saveRDS(continent_dams,
          file = file.path(data_clean,"shape-files",paste0(continent_name,"_dams.rds")))
  
  continent_dams <- readRDS(file = file.path(data_clean,"shape-files",paste0(continent_name,"_dams.rds")))
  
  basins_ircc_count <- readRDS(file = file.path(data_clean,"shape-files","basins_ircc.rds"))
  
  basins_ircc_count <- basins_ircc_count %>% 
                       st_transform(crs = st_crs(rivers_grwl))
  
  # https://github.com/r-spatial/sf/issues/1902
  
  sf_use_s2(FALSE)
  
  basins_ircc_count <- st_make_valid(basins_ircc_count)
  
  sf_use_s2(TRUE)
  
  sf_use_s2(FALSE)
  basins_ircc_count1 <- st_make_valid(basins_ircc_count)
  
  
  basins_my_continent <- basins_ircc_count1[my_continent,]
  
  sf_use_s2(TRUE)
  

  multipol_cast <- sf::st_cast(basins_my_continent,"MULTIPOLYGON")
  
  
  multipol_cast_world <- sf::st_cast(basins_ircc_count,"MULTIPOLYGON")
  
  
  # 7076 rivers
  
  map <- ggplot(data = my_continent) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    geom_sf(data = multipol_cast,
            alpha = .7,
            color = "white",
            mapping = aes(fill = event_count)) +
    geom_sf(data = continent_rivers_grwl,
            alpha = 1,
            color = yale_lblue,
            linewidth = .5) +
    geom_sf(data = continent_dams,
            alpha = .4,
            color = "brown",
            size = .5) +
    labs(title = paste0("Rivers of ",continent_name),
         caption = c("Data from GRLW (2018)")) +
    theme_map() +
    labs(fill = "Number of events") +
    scale_fill_gradientn(colours = yale_palette)
  
  
  #map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = paste0(continent_name,"_rivers_dams_basins.png"),
           width = 9,
           height = 5,
           dpi  = 300)
  
  
  ggplotly(map)
  
  
  map <- ggplot(data = world) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    geom_sf(data = multipol_cast_world,
            alpha = .7,
            color = "white",
            mapping = aes(fill = event_count)) +
    geom_sf(data = rivers_grwl,
            alpha = 1,
            color = yale_lblue,
            linewidth = .5) +
    geom_sf(data = world_dams,
            alpha = .4,
            color = "brown",
            size = .5) +
    labs(title = paste0("Rivers of the World"),
         caption = c("Data from GRLW (2018)")) +
    theme_map() +
    labs(fill = "Number of events") +
    scale_fill_gradientn(colours = yale_palette)
  
  
  map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = paste0("world","_rivers_dams_basins.png"),
           width = 9,
           height = 5,
           dpi  = 300)
  
  
  ggplotly(map)
  
  
  
# Get the rivers that intersect a country boundary ----
  
  library(rnaturalearth) # has ne_coastline() which is global coastlines
  if (!require(rnaturalearthdata)) install.packages("rnaturalearthdata")
  
  library(rnaturalearthdata)
  
  # scale medium means a medium-sized file, better detail
  coastlines <- ne_coastline(scale = "medium", returnclass = "sf")
  
  
    # cast the country polygon borders into lines
  
  continent_country_lines <- st_cast(my_continent, "MULTILINESTRING", 
                              group_or_split = TRUE)
  
  union_continent <- st_union(my_continent)
  
  
  continent_outline <- st_cast(st_union(my_continent), "MULTILINESTRING",
                        group_or_split = TRUE)
  
  # create a yes-no insersection if the geometries intersect
  
  # a list is returned for st_intersects, so take the ones where there's a positive length of intersection
  # ie that the river intersects a country line
  
  # 
  # generates a logical vector of length of continent_rivers_grwl
  intersects_border_yesno <- lengths(st_intersects(continent_rivers_grwl,continent_country_lines)) > 0
  coastline_yesno <- lengths(st_intersects(continent_rivers_grwl, coastlines)) >0
  outline_TF      <- lengths(st_intersects(continent_rivers_grwl, continent_outline)) >0
  
  # verify that this is getting what we want
  check_border <- intersects_border_yesno[1:30]
  check_coastline  <- coastline_yesno[1:30]
  
  check_international <- check_border & !check_coastline
  
  intersects_international_yesno <- intersects_border_yesno & !coastline_yesno & !outline_TF
  

  
  
  # so now we want IS international, but is NOT intersecting a coast
  
  rivers_international <- continent_rivers_grwl[intersects_international_yesno,]
  rivers_domestic <- continent_rivers_grwl[!intersects_international_yesno,]
  
  
  saveRDS(rivers_international,
          file = file.path(data_clean,"shape-files",paste0(continent_name,"_rivers_international.rds")))
  
  saveRDS(rivers_domestic,
          file = file.path(data_clean,"shape-files",paste0(continent_name,"_rivers_domestic.rds")))
  
  rivers_domestic <- readRDS(file = file.path(data_clean,"shape-files",paste0(continent_name,"_rivers_domestic.rds")))
  
  rivers_international <- readRDS(file = file.path(data_clean,"shape-files",paste0(continent_name,"_rivers_international.rds")))
  
  # 295 spots
  
  # try now with a function
  
  # rivers_intl_fn <- get_in_out_intersections(sf_of_interest = continent_rivers_grwl,
  #                                            yes_sf = continent_country_lines,
  #                                            no_sf  = coastlines)
  # 
  
  
  plot(st_geometry(rivers_international))
  
  # plot to check
  
  map <- ggplot(data = my_continent) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    geom_sf(data = multipol_cast,
            alpha = .7,
            color = "white",
            mapping = aes(fill = event_count)) +
    geom_sf(data = continent_rivers_grwl,
            alpha = .3,
            color = yale_lblue,
            linewidth = .3) +
    geom_sf(data = rivers_international,
            alpha = 1,
            color = yale_blue,
            linewidth = 1) +
    geom_sf(data = continent_dams,
            alpha = .4,
            color = "brown",
            size = .5) +
    labs(title = "Rivers of South America",
         caption = c("Data from GeoDAR (2022) and GRLW (2018). International segments in dark blue")) +
    theme_map()+
    labs(fill = "Number of events") +
    scale_fill_gradientn(colours = yale_palette)
  
  map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = paste0(continent_name,"_rivers.png"),
           width = 9,
           height = 5,
           dpi  = 300)
  
  
  # plot rivers if intersection is true
  
  # for interactive ggplot
  if (!require(plotly)) install.packages("plotly")
  
  library(plotly)
  
  # take a look more closely
  ggplotly(map)
  
## use fancier data ----
  # 
  # # mask
  # path <- file.path(data_raw,"GRWL_global-river-widths-landsat","GRWL_vector_V01.01","GRWL_vector_V01.01")
  # 
  # # view available layers
  # st_layers(path)
  # 
  # 
  # 
  # test_read <- st_read(path)
  # 
  #  
  # map <- ggplot(data = world) +
  #   geom_sf(color = "gray70",
  #           fill = "gray99",
  #           alpha = 0.5,
  #           linewidth = .3) +
  #   # geom_sf(data = basin_shapes,
  #   #         alpha = .2,
  #   #         color = yale_lblue,
  #   #         fill  = yale_lblue) +
  #   geom_sf(data = test_read,
  #           alpha = .3,
  #           color = yale_medblue,
  #           linewidth = 1) +
  #   # geom_sf(data = china_dams,
  #   #         alpha = .4,
  #   #         color = yale_blue,
  #   #         size = .5) +
  #   labs(title = "Finding the data",
  #        caption = c("Data from GRLW (2018)")) +
  #   theme_map()
  # 
  # map
  # 
  # ggplotly(map)
  # 
  # 
