# _______________________________#
# Environment
# Plot Rivers Dams and DHS
# 
# Stallman
# Started 2023-05-23
# Last edited: 
#________________________________#



# Startup

rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","freshwater-cooperation")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
   ggplot2,
   plotly,
   sf,
   riverdist,
   ggrepel, # for putting text on a plot
   stringr, # string ops
   parallel, # parallel ops
   ggspatial # things like scale bars
  )
  
  n_cores <- 12
  
# read in data ----
  
  path <- file.path(data_external_raw,"HydroSHEDS","HydroRIVERS_v10_af.gdb","HydroRIVERS_v10_af.gdb")
  
  system.time(
    hydro_rivers <- st_read(dsn = path) %>%
      st_transform(crs = equal_area_crs) %>%
      rename(geometry = Shape) # need this to use riverdist package
  )

  gadm_in_path <- file.path(data_external_clean,"GADM","global")
  gadm_in_filename <- paste0("GADM_global_ADM_",1,".rds")
  
  system.time(
  gadm_data <- readRDS(file.path(gadm_in_path,gadm_in_filename)) %>%
               st_transform(crs = equal_area_crs)
  )
  
  river_points_path = file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","river-points")
  


  # read in data ----
  
  # get the filenames of the countries in our list 
  main_riv_filenames <- list.files(path = river_points_path,
                                    pattern = "river_points")
  
  main_rivs <- str_remove_all(main_riv_filenames,"DHS_GLOW_MAIN_RIV_") %>%
    str_remove_all("_river_points.rds")
  
  
  main_river <- main_rivs[200]
  
  #filename <- main_riv_filenames[6]
  
  i <- 1
  
  tic("Putting all river points in one data frame")
  for (filename in main_riv_filenames){
    
    temp_data <- readRDS(file.path(river_points_path,filename)) %>%
                 mutate(n_towns = sum(type == "DHS_town"),
                        n_dams  = sum(type == "Dam"),
                        n_adhi  = sum(type == "hydrology_station"),
                        n_glow  = sum(type == "GLOW"))
    
    number_of_towns <- temp_data$n_towns %>% unique()
    
    main_river <- temp_data$MAIN_RIV %>% unique()
    
    if (i %% 10 ==0){
    print(paste0("Currently working on MAIN_RIV ",main_river,", iteration ", i, ". It has ",nrow(temp_data)," rows and ",number_of_towns," towns."))
    }
    
    #print(paste0("dataset ",filename," has ",nrow(temp_data)," rows."))
    if (filename == main_riv_filenames[1]){
      
      river_points_data <- temp_data
      
      
    } else {
      
      
      if (number_of_towns>100) { # ignoring big rivers
        
        rm(temp_data)
        gc() 
        
      }  else {
        river_points_data  <- rbind(river_points_data,temp_data)
      }
      
      
      
    } # end ifelse to get the first df to be setting things up
    
    i <- i+1
  } # end loop over filenames
  
  toc()
  
  # Putting all river points in one data frame: 316.05 sec elapsed
  
  
  

  path <- file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","all-river-points")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  
  saveRDS(river_points_data,
          file.path(path,paste0("all_river_points_under_1000_towns.rds")))
  
  river_points_data <-  readRDS(file.path(path,paste0("all_river_points_under_1000_towns.rds")))
  main_river <- "10006407"
  single_river <- hydro_rivers %>% filter(MAIN_RIV == "10006407") %>%
                  mutate(width = 1/as.numeric(ORD_CLAS))
  
  
  my_rivs <- hydro_rivers %>%
             filter(MAIN_RIV %in% main_rivs) %>%
              mutate(width = 1/as.numeric(ORD_CLAS))
  
  
  
  test_data <- river_points_data %>% filter(MAIN_RIV %in% main_rivs)
  dams      <- test_data %>% filter(type == "Dam")
  dhs     <- test_data %>% filter(type == "DHS_town")
  adhi      <- test_data %>% filter(type == "hydrology_station")
  glow      <- test_data %>% filter(type == "GLOW")
  


  system.time(
    world <- readRDS(file.path(data_external_clean,"world.rds")) 
  )
  
  africa <- world %>% filter(continent == "Africa") %>%
    st_transform(crs = equal_area_crs)
  
  #plot_river_points(main_river = main_rivs[6] )
  
  

  map <- ggplot() +
    geom_sf(data = africa,
            color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = 0.3)+
    geom_sf(data = glow,
            aes(
              color = "Width Measure"),
            show.legend = "point",
            size = 1,
            shape = 4)+
    geom_sf(data = adhi,
            aes(color = "Hydro Station"),
            show.legend = "point",
            shape = 7,
            size = 1)+
    geom_sf(data = dhs,
            aes(color = "Town",
                fill = "Town"),
            show.legend = "point",
            shape = 16,
            alpha = 0.5)+
    geom_sf(data = dams,
            aes(color = "black",
                fill  = "Dam"),
            show.legend = "point",
            shape = 22,
            size = 3,
            alpha = .4) +
    scale_color_manual(values = c("Width Measure" = "#63aaff",
                                  "Hydro Station"  = "#808080",
                                  "Town"           = "black"), 
                       name = NULL,
                       guide = guide_legend(override.aes = list(linetype = c("blank","blank","blank"), 
                                                                shape = c(7,16,4),
                                                                size  = c(1,1,1))))  +
    scale_fill_manual(values = c(
      "Dam"          = "#964B00"), name = NULL,
      guide = guide_legend(override.aes = list(linetype = c("blank"), 
                                               shape = c(22),
                                               size  = c(3))))+
    labs(title = paste0("DHS Towns on Smaller Rivers in Africa"),
         caption = c(paste0("
                              Includes ", nrow(glow), " width locations, ", nrow(dhs) ," towns, ", nrow(adhi), " hydro stations, ", nrow(dams), " dams.
                             Source: DHS, GADM (2022), AWS (2023), 
                            HydroRIVERS (2023), ADHI (2020), GDAT (2023).")))+
    #geom_sf(data = mouth_stretch %>% st_buffer(10000), col = '#C2B280', fill = NA, linewidth = .5, linetype = "dotted")+
    #geom_sf(data = dhs %>% st_buffer(10000), col = 'grey', fill = NA, linewidth = .5,linetype = "dashed", alpha = .3) +
    #ggspatial::annotation_scale() +
    # ggrepel::geom_text_repel(data = dhs,
    #                          aes(label = paste0(dhs$plot_id),#,",",points_on_river$vert),
    #                              geometry = geometry),
    #                          stat = "sf_coordinates",
    #                          max.overlaps = 20,
    #                          col = "black"#row_number(test_rivers))
    # )+
    xlab("") + ylab("")+
    theme_map(legend_position = "right"
    )
  
  map
  
  
  save_map(output_folder = file.path(output_maps),
           plotname = map,
           filename = "africa_dhs_glow_adhi_gdat.png",
           width = 8,
           height = 8,
           dpi  = 300)
  
  
  
  map <- ggplot() +
    geom_sf(data = africa,
            color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = 0.3)+
    geom_sf(data = my_rivs,
            color = "#286dc0",
            linewidth = my_rivs$width/2,
            alpha = my_rivs$width/2) +
    labs(title = paste0("Smaller Rivers in Africa"),
         caption = c(paste0("Source:  
                            HydroRIVERS (2023)")))+
    xlab("") + ylab("")+
    theme_map(
    )
  
  #map
  
  tic("Saving map with 1500 rivers")
  save_map(output_folder = file.path(output_maps),
           plotname = map,
           filename = "africa_small_river_map.png",
           width = 8,
           height = 9,
           dpi  = 300)
  toc()
  
  
  
  
  
  
  
  

  
  
  
  # parallelize ----
  tic(paste0("Get plots for rivers that have GLOW + ADHI + DHS + GDAT"))
  
  # 18 cores: CPU was at 100%, too many
  # 16 cores puts CPU close to 100% and memory at about 80, probably not the best
  # 14 cores used 100% cpu with other processes, try 12
  # 12 cores uses 85-100% CPU and 50-60GB RAM, about right
  # running this on 8 cores over all the main_rivers_all (16352 rivers in Africa) used up 50-60% CPU and hovered around 27-32 GB of RAM
  
  cl <- makeCluster(n_cores) # n_cores # runs low on RAM if hydro_rivers gets sent to too many places, try building up
  clusterEvalQ(cl,library(sf)) # send these separately, clusterEvalQ(cl, fun) is the call format
  clusterEvalQ(cl,library(riverdist))
  clusterEvalQ(cl, library(dplyr))
  clusterEvalQ(cl, library(countrycode))
  clusterEvalQ(cl, library(ggplot2))
  clusterEvalQ(cl, library(ggrepel))
  clusterEvalQ(cl, library(stringr))
  clusterEvalQ(cl, library(ggspatial))
  clusterEvalQ(cl, library(ggplot2))
  
  
  clusterExport(cl, c("hydro_rivers","gadm_data")) # this is a big export b/c the hydro_rivers is huge
  #clusterExport(cl, c("GPS_data"))
  #clusterExport(cl, c("dhs_data"))
  
  #clusterExport(cl, c("max_distance_to_snap"))
  #clusterExport(cl, c("equal_area_crs"))
  
  
  #parLapply(cl, main_rivers_under_100, get_river_points, points_data = GPS_data)
  
  tic("Getting river plots")
  parLapply(cl, main_rivs, plot_river_points)
  toc()
  # Get plots for rivers that have GLOW + ADHI + DHS + GDAT: 240.22 sec elapsed

  # Getting river points for the sub-100-town rivers: 3012.93 sec elapsed
  # 
  
  # parLapply(cl, main_rivers_singletons, get_dhs_rivers_points_glow)
  # parLapply(cl, main_rivers_dyads, get_dhs_rivers_points_glow)
  # parLapply(cl, main_rivers_triads, get_dhs_rivers_points_glow)
  
  
  
  gc()
  toc()
  
  stopCluster(cl)
  
  
  
  
  
  
  
  
  africa <- world %>% filter(continent == "Africa")
  
  dams_africa <- readRDS(file.path(data_external_clean,"GDAT_global-dam-tracker","GDAT_dams-af"))
  
  #dams_africa <- readRDS(file.path(data_external_clean,"GDAT_global-dam-tracker","GDAT_dams-af"))
  
  merged_df <- readRDS(file = file.path(data_external_clean,"merged","many","dhs_gdat_adhi.rds"))
  
  map <- ggplot(data = africa) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    geom_point(data = merged_df,
               aes(fill = factor(type),
                   shape = factor(type),
                   colour = factor(type),
                   x = lon,
                   y = lat),
               size = 1) +
    labs(title = paste0("Dams, Hydro Stations, and DHS Clusters in Africa"),
         caption = c("Data from GDAT (2023), DHS (2023), ADHI (2019)")) +
    theme_map(axis_text_x = element_blank(),
              axis_text_y = element_blank(),
              axis_title_x = element_blank(),
              axis_title_y = element_blank())
  
  
  map
  
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = paste0("gdat_adhi_dhs_africa.png"),
           width = 8,
           height = 9,
           dpi  = 300)
  
  
  
  
  # plot ----
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
  
  
  
  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  #units_name <- "Malawi"
  units_name <- "Zambezi"
  
  hydro_rivers_units <- readRDS(file = file.path(data_clean,"shape-files",paste0(units_name,"_hydro_rivers.rds")))
  
  units <- readRDS(file = file.path(data_clean,"shape-files",paste0(units_name,"_.rds")))
  
  
  # https://blog.benthies.de/blog/mapping-streams-and-rivers-with-ggplot-sf/
  
  
  map <- ggplot(data = units) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    geom_sf(data = hydro_rivers_units,
            alpha = hydro_rivers_units$width,
            color = yale_blue,
            linewidth = hydro_rivers_units$width) +
    labs(title = paste0("Rivers of ",units_name),
         caption = c("Data from HydroRIVERS (2019)")) +
    theme_map()
  
  # map
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = paste0(units_name,"_hydro_rivers_units_hydroRIVERS.png"),
           width = 9,
           height = 5,
           dpi  = 300)
  
  # take a look more closely
  #ggplotly(map)
  
  
  
  
  
  
  # 
  # 
  # 
  # map <- ggplot(data = south_america) +
  #   geom_sf(color = "gray70",
  #           fill = "gray99",
  #           alpha = 0.5,
  #           linewidth = .3) +
  #   # geom_sf(data = basin_shapes,
  #   #         alpha = .2,
  #   #         color = yale_lblue,
  #   #         fill  = yale_lblue) +
  #   geom_sf(data = rivers_sa,
  #           alpha = .3,
  #           color = yale_lblue,
  #           linewidth = .3) +
  #   geom_sf(data = rivers_international,
  #           alpha = 1,
  #           color = yale_blue,
  #           linewidth = 1) +
  #   geom_sf(data = sa_dams,
  #           alpha = .4,
  #           color = "brown",
  #           size = .5) +
  #   labs(title = "Rivers of South America",
  #        caption = c(paste0(nrow(rivers_international)," int'l river segments\n",
  #                           nrow(rivers_domestic)," domestic river segments\n",
  #                           nrow(sa_dams)," dams\n",
  #                           "Data from GeoDAR (2022) and GRLW (2018)."))) +
  #   theme_map()
  # 
  # map
  # 
  # save_map(output_folder = output_maps,
  #          plotname = map,
  #          filename = "sa_rivers.png",
  #          width = 9,
  #          height = 5,
  #          dpi  = 300)
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
  #   geom_sf(data = rivers_sa,
  #           alpha = .3,
  #           color = yale_lblue,
  #           linewidth = .3) +
  #   geom_sf(data = rivers_international,
  #           alpha = 1,
  #           color = yale_blue,
  #           linewidth = 1) +
  #   geom_sf(data = sa_dams,
  #           alpha = .4,
  #           color = "brown",
  #           size = .5) +
  #   labs(title = "Rivers of South America",
  #        caption = c(paste0(nrow(rivers_international)," int'l river segments\n",
  #                           nrow(rivers_domestic)," domestic river segments\n",
  #                           nrow(sa_dams)," dams\n",
  #                           "Data from GeoDAR (2022) and GRLW (2018)."))) +
  #   theme_map()
  # 
  # map
  # 
  # save_map(output_folder = output_maps,
  #          plotname = map,
  #          filename = "sa_rivers.png",
  #          width = 9,
  #          height = 5,
  #          dpi  = 300)
  # 
  # 


  
