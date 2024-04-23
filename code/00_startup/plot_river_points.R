# requires the following data in environment: hydro_rivers
#' @param main_river hydrorivers ID
#' @param data_clean_path the place to put cleaned data
#' @param points_data_path where the points data lives
#' @param points_leading_string header for the points data, assumes the points data name is of form paste0(points_leading_string,main_river,"_river_points.rds")
#' @param river_network_path where the river network lives
#' @param river_network_missing_path where to log that the river network is currently missing
#' @param checked_river_path where to log that the river distances have been obtained
#' @export

plot_river_points <- function(main_river,
                                max_current_points = 2000, # the max is 1886 for triads and below; biggest 30-40 are above 200
                                points_data_path = file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","river-points"), 
                                #file.path("E:","data","02_temp","merged","DHS_HydroSHEDS","river-points"), # for just DHS
                                points_leading_string  = "DHS_GLOW_MAIN_RIV_", #"DHS_MAIN_RIV_", # for just DHS      
                                river_network_path = file.path("E:","data","03_clean","HydroSHEDS","river_networks"),
                                ID_varname = "ID", #"DHSID", # else "ID" for GLOW
                                checked_river_path = file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","checked-main-rivers","plots"),
                              hydro_rivers_units_path = file.path("E:","data","03_clean","HydroSHEDS","shape-files"),
                              output_maps  = file.path("P:","Projects","freshwater-cooperation","output","03_maps"),
                              
                                #file.path("E:","data","02_temp","merged","DHS_HydroSHEDS","checked-main-rivers","dhs-river-distances"),
                                river_network_missing_path =  file.path("E:","data","02_temp","HydroSHEDS","river-network-missing")){
  
  tryCatch( {
    
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
          legend.background = element_rect(fill = "white", color = "white", linewidth = 0.3),
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
    
    
    points_filename_string <- paste0(points_leading_string,main_river,"_river_points.rds")
    
    paths_to_create <- c(checked_river_path,
                         river_network_missing_path
    )
    
    for (path in paths_to_create){
      
      if (!dir.exists(path)) dir.create(path, recursive = TRUE)
      
    }
    
    # run through conditions that should stop the function 
    # 
    # Stop if riverr network directory doesn't exist
    if (!dir.exists(river_network_path)) {
      
      stop(paste0("River network directory in ",river_network_path," doesn't exist. You either need to create the river networks or check that you wrote the path correctly."))
      
    }
    
    # if the current river network doesn't exist, create output that says the network is missing, and stop function
    if (!file.exists(file = file.path(river_network_path,paste0("MAIN_RIV_",main_river,"_cleaned_river_network.rds")))){
      
      stop(paste0("Current river network for main_river ",main_river," doesn't exist. Create it first and come back."))
      
    } 
    # Stop if the river has already been checked, don't check it again
    if (file.exists(file = file.path(checked_river_path,paste0("MAIN_RIV_",main_river,"_already_checked.rds")))){
      
      stop(paste0("The plots you wanted for main_river ",main_river," have already been plotted. No need to go through this again."))
      
    }
    
    # Stop if the river has already been checked, don't check it again
    if (!file.exists(file = file.path(points_data_path,points_filename_string))){
      
      stop(paste0("The closest points to ",main_river," have not yet been calculated. Go through a get_river_points function first to get these points."))
      
    }
    
    # bring in river network
    
    current_river_network <- readRDS(file = file.path(river_network_path,paste0("MAIN_RIV_",main_river,"_cleaned_river_network.rds")))
    
    current_points        <- readRDS(file = file.path(points_data_path,points_filename_string)) %>%
                              mutate("plot_id" = paste0(stringr::str_sub(ID,start = 1, end = 2),stringr::str_sub(ID,start = -3)),
                                     "survey_year" = paste0(stringr::str_sub(ID,start = 3, end = 6)))
    
    
    dams <- current_points %>% filter(type == "Dam")
    adhi <- current_points %>% filter(type == "hydrology_station")
    glow <- current_points %>% filter(type == "GLOW")
    dhs  <- current_points %>% filter(type == "DHS_town")
    
    if ( nrow(glow)==0 | nrow(dhs)==0){
      stop(paste0("Currently not set up for plots with no observations for DHS or GLOW."))
    }
    
    if (nrow(current_points)>max_current_points){
      
      stop(paste0("There are ",nrow(current_points)," rows in the current points data - too many to bother with for now."))
      
    }
    
    #plot(current_river_network)
    
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

    # river_polygons <- gadm_data %>% 
    #   filter(GID_0 %in% intersecting_countries & continent == "Africa")%>%
    #   group_by(GID_0) %>%
    #   summarize(geometry = st_union(geom)) %>%
    #   st_make_valid()
    # 
    # 
    # plot(st_geometry(river_polygons))
    # 

    # 
    # if (!file.exists( file.path(hydro_rivers_units_path,paste0(paste(countries,collapse = "_"),"_hydrorivers.rds")))) {
    #   
    #   # flag that the units are missing
    #   saveRDS(c(countries),
    #           file.path(hydro_rivers_units_missing_path,paste0(paste(countries,collapse = "_"),"hydro_rivers_units_missing.rds")))
    #   
    #   stop(paste0("HydroRIVERS file for countries ",paste(countries,collapse = " "), "is missing. Create elsewhere first (it takes a while) then come back."))
    #   
    # } 
    # 
    
    #hydro_rivers_units <- readRDS(file = file.path(hydro_rivers_units_path,paste0(paste(countries,collapse = "_"),"_hydrorivers.rds")))
    
    # https://stackoverflow.com/questions/51371480/format-multiple-geom-sf-legends
    # 
    #for main_river == 10508130 this is a good map to make
    #
    #
  if (nrow(adhi)==0 & nrow(dams)!= 0){ # if no hydro station but otherwise good
    
    map <- ggplot() +
      geom_sf(data = single_river,
              color = "#286dc0",
              linewidth = single_river$width*2,
              alpha = single_river$width*2) +
      geom_sf(data = glow,
              aes(
                color = "Width Measure"),
              show.legend = "point",
              size = 2,
              shape = 4)+
      geom_sf(data = dams,
              aes(color = "black",
                  fill  = "Dam"),
              show.legend = "point",
              shape = 22,
              size = 5) +
      geom_sf(data = mouth_stretch,
              aes(color = "Mouth"),
              show.legend = "line",
              linewidth = 1.5) +
      geom_sf(data = dhs,
              aes(color = "Town",
                  fill = "Town"),
              show.legend = "point",
              shape = 16)+
      scale_color_manual(values = c("Width Measure" = "#63aaff",
                                    "Mouth"          = "#C2B280",
                                    "Town"           = "black"), 
                         name = NULL,
                         guide = guide_legend(override.aes = list(linetype = c("solid","blank","blank"), 
                                                                  shape = c(NA,16,4),
                                                                  size  = c(1.5,2,2))))  +
      scale_fill_manual(values = c(
        "Dam"          = "#964B00"), name = NULL,
        guide = guide_legend(override.aes = list(linetype = c("blank"), 
                                                 shape = c(22),
                                                 size  = c(5))))+
      labs(title = paste0("DHS Towns Along a River in ", paste0(countries, collapse = " ")),
           caption = c(paste0("Labels give (Country+DHS cluster number) for DHS ", paste(unique(dhs$survey_year, collapse = " ")), ".
                              Includes ", nrow(glow), " width location(s), ", nrow(dhs) ," town(s), ", nrow(adhi), " hydro station(s), ", nrow(dams), " dam(s).
                             Source: DHS, GADM (2022), AWS (2023), HydroRIVERS (2023), ADHI (2020), GDAT (2023).")))+
      geom_sf(data = mouth_stretch %>% st_buffer(10000), col = '#C2B280', fill = NA, linewidth = .5, linetype = "dotted")+
      geom_sf(data = dhs %>% st_buffer(10000), col = 'grey', fill = NA, linewidth = .5,linetype = "dashed", alpha = .3) +
      ggspatial::annotation_scale() +
      ggrepel::geom_text_repel(data = dhs,
                               aes(label = paste0(dhs$plot_id),#,",",points_on_river$vert),
                                   geometry = geometry),
                               stat = "sf_coordinates",
                               max.overlaps = 20,
                               col = "black"#row_number(test_rivers))
      )+
      xlab("") + ylab("")+
      theme_map(legend_position = "right",
      )
  
    #map  
  } else if (nrow(adhi)==0 & nrow(dams)==0) {
    
    map <- ggplot() +
      geom_sf(data = single_river,
              color = "#286dc0",
              linewidth = single_river$width*2,
              alpha = single_river$width*2) +
      geom_sf(data = glow,
              aes(color = "Width Measure"),
              show.legend = "point",
              size = 2,
              shape = 4)+
      geom_sf(data = mouth_stretch,
              aes(color = "Mouth"),
              show.legend = "line",
              linewidth = 1.5) +
      geom_sf(data = dhs,
              aes(color = "Town"),
              show.legend = "point",
              shape = 16)+
      scale_color_manual(values = c("Width Measure" = "#63aaff",
                                    "Mouth"          = "#C2B280",
                                    "Town"           = "black"), 
                         name = NULL,
                         guide = guide_legend(override.aes = list(linetype = c("solid","blank","blank"), 
                                                                  shape = c(NA,16,4),
                                                                  size  = c(1.5,2,2))))  +
      labs(title = paste0("DHS Towns Along a River in ", paste0(countries, collapse = " ")),
           caption = c(paste0("Labels give (Country+DHS cluster number) for DHS ", paste(unique(dhs$survey_year, collapse = " ")), ".
                              Includes ", nrow(glow), " width location(s), ", nrow(dhs) ," town(s), ", nrow(adhi), " hydro station(s), ", nrow(dams), " dam(s).
                             Source: DHS, GADM (2022), AWS (2023), HydroRIVERS (2023), ADHI (2020), GDAT (2023).")))+
      geom_sf(data = mouth_stretch %>% st_buffer(10000), col = '#C2B280', fill = NA, linewidth = .5, linetype = "dotted")+
      geom_sf(data = dhs %>% st_buffer(10000), col = 'grey', fill = NA, linewidth = .5,linetype = "dashed", alpha = .3) +
      ggspatial::annotation_scale() +
      ggrepel::geom_text_repel(data = dhs,
                               aes(label = paste0(dhs$plot_id),#,",",points_on_river$vert),
                                   geometry = geometry),
                               stat = "sf_coordinates",
                               max.overlaps = 20,
                               col = "black"#row_number(test_rivers))
      )+
      xlab("") + ylab("")+
      theme_map(legend_position = "right",
      )
    
    #map 
    
    
    
    
    
    
    }else { # everything else exists
    
    
    map <- ggplot() +
      geom_sf(data = single_river,
              color = "#286dc0",
              linewidth = single_river$width*2,
              alpha = single_river$width*2) +
      geom_sf(data = glow,
              aes(
              color = "Width Measure"),
              show.legend = "point",
              size = 2,
              shape = 4)+
      geom_sf(data = dams,
              aes(color = "black",
                  fill  = "Dam"),
              show.legend = "point",
              shape = 22,
              size = 5) +
      geom_sf(data = mouth_stretch,
              aes(color = "Mouth"),
              show.legend = "line",
              linewidth = 1.5) +
      geom_sf(data = adhi,
              aes(color = "Hydro Station"),
              show.legend = "point",
              shape = 7,
              size = 5)+
      geom_sf(data = dhs,
              aes(color = "Town",
                  fill = "Town"),
              show.legend = "point",
              shape = 16)+
      scale_color_manual(values = c("Width Measure" = "#63aaff",
                                    "Mouth"          = "#C2B280",
                                    "Hydro Station"  = "#808080",
                                    "Town"           = "black"), 
                         name = NULL,
                         guide = guide_legend(override.aes = list(linetype = c("blank","solid","blank","blank"), 
                                                                  shape = c(7,NA,16,4),
                                                                  size  = c(5,1.5,2,2))))  +
      scale_fill_manual(values = c(
                                    "Dam"          = "#964B00"), name = NULL,
                         guide = guide_legend(override.aes = list(linetype = c("blank"), 
                                                                  shape = c(22),
                                                                  size  = c(5))))+
      labs(title = paste0("DHS Towns Along a River in ", paste0(countries, collapse = " ")),
           caption = c(paste0("Labels give (Country+DHS cluster number) for DHS ", paste(unique(dhs$survey_year, collapse = " ")), ".
                              Includes ", nrow(glow), " width locations, ", nrow(dhs) ," towns, ", nrow(adhi), " hydro stations, ", nrow(dams), " dams.
                             Source: DHS, GADM (2022), AWS (2023), HydroRIVERS (2023), ADHI (2020), GDAT (2023).")))+
      geom_sf(data = mouth_stretch %>% st_buffer(10000), col = '#C2B280', fill = NA, linewidth = .5, linetype = "dotted")+
      geom_sf(data = dhs %>% st_buffer(10000), col = 'grey', fill = NA, linewidth = .5,linetype = "dashed", alpha = .3) +
      ggspatial::annotation_scale() +
      ggrepel::geom_text_repel(data = dhs,
                               aes(label = paste0(dhs$plot_id),#,",",points_on_river$vert),
                                   geometry = geometry),
                               stat = "sf_coordinates",
                               max.overlaps = 20,
                               col = "black"#row_number(test_rivers))
      )+
      xlab("") + ylab("")+
      theme_map(legend_position = "right",
                )
    
    #map
    
    
  } # end ifelse if nrow(adhi)==0
    
    
  
    
    
    
    
    
    
    save_map(output_folder = file.path(output_maps,"merged","DHS_GLOW_HydroSHEDS"),
             plotname = map,
             filename = paste0(paste(countries,collapse = "_"),"_MAIN_RIV_",main_river,"_river_map.png"),
             width = 8,
             height = 7,
             dpi  = 300)

    saveRDS(main_river,
            file.path(checked_river_path,paste0("MAIN_RIV_",main_river,"_already_checked.rds")))

    # #for main_river == 10508130 this is a good map to make
    # map <- ggplot() +
    #   geom_sf(data = units,
    #           color = "gray70",
    #           fill = "gray99",
    #           alpha = 0.5,
    #           linewidth = .3)+
    #   geom_sf(data = single_river,
    #           color = "#286dc0",
    #           linewidth = single_river$width*2,
    #           alpha = single_river$width*2) +
    #   geom_sf(data = current_points)+
    #   geom_sf(data = source_stretch,
    #           color = 'green',
    #           linewidth = 1.5)+
    #   labs(title = paste0("Example: River Distance Calculations,", paste(countries,collapse = " ")," ",year," DHSs"),
    #        caption = c(paste0("Reported cluster in black; buffer of 10 km around cluster in dashed red; river source in green
    #                          Labels give (Country+DHS cluster number)
    #                          Data from a compilation of DHS surveys in ",year-5,"-",year,"GADM (2022), AWS (2023), HydroRIVERS (2023).")),
    #        colour = "Elevation (m)")+
    #   geom_sf(data = source_stretch %>% st_buffer(10000), col = 'green', fill = NA, linewidth = 1.5)+
    #   geom_sf(data = current_points %>% st_buffer(10000), col = 'red', fill = NA, linewidth = .5,linetype = "dashed")+
    #   ggrepel::geom_text_repel(data = current_points,
    #                            aes(label = paste0(current_points$DHSCC,current_points$DHSCLUST),#,",",points_on_river$vert),
    #                                geometry = geometry),
    #                            stat = "sf_coordinates",
    #                            max.overlaps = 20,
    #                            col = "black"#row_number(test_rivers))
    #   )+
    #   xlab("") + ylab("")+
    #   theme_map(legend_position = "none")
    # 
    # #map
    # 
    # 
    # save_map(output_folder = file.path(output_maps,"HydroSHEDS"),
    #          plotname = map,
    #          filename = paste0(paste(countries,collapse = "_"),"_MAIN_RIV_full_",main_river,"_",year,"_river_distances.png"),
    #          width = 8,
    #          height = 7,
    #          dpi  = 300)
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    #   saveRDS(object = dyad_upstream,
    #           file =file.path(dyad_distances_path,paste0(points_leading_string,main_river,"_dyad_distances.rds")))
    # 
    # saveRDS(object = dyad_distances,
    #         file = file.path(distance_matrices_flat_path,paste0(points_leading_string,main_river,"_distances_matrix_flat.rds")))
    # 
    # saveRDS(object = distance_mat,
    #         file = file.path(distance_matrices_path,paste0(points_leading_string,main_river,"_distances_matrix.rds")))
    # 
    # 

  }, # main part of the function for tryCatch

  error = function(e) {
    # code executed in event of an error
    return("error") },
  warning = function(w) {
    # code executed in event of a warning
    return("warning")
  }


  ) # end tryCatch
} # end function
