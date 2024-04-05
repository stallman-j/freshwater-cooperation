# _______________________________#
# Environment
# Plot 02 Merge Basins from IRCC and from TFDD basin shapefile
# Stallman
# Started 2023-03-16
# Last edited: 
#________________________________#
# Startup

rm(list = ls())


# bring in the packages, folders, paths ----

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

# read in data ----

  library(sf) # spatial package for spatial files

  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  
  ircc_path <- file.path(data_clean,"edge-level","ircc.rds")
  
  ircc <- readRDS(file = ircc_path)
  
  basin_shapes <- readRDS(file = file.path(data_clean,"shape-files","basin_shapes.rds"))
  #basin_country_shapes <- readRDS(file = file.path(data_clean,"shape-files","basin_country_shapes.rds"))
  
  
  # clean up some naming
  ircc$basinnameold[ircc$basinnameold== "Aral Sea, Syr Darya, Naryn river"] <- "Aral Sea"
  
  
  ircc$basinnameold[ircc$basinnameold== "Congo"] <- "Congo/Zaire"
  ircc$basinnameold[ircc$basinnameold== "DANUBE"] <- "Danube"
  
  ircc$basinnameold[ircc$basinnameold== "Geba"] <- "Geba-Corubal"
  ircc$basinnameold[ircc$basinnameold== " Indus"] <- "Indus"
  
  ircc$basinnameold[ircc$basinnameold== "La Plata "] <- "La Plata"
  ircc$basinnameold[ircc$basinnameold== "La Plata/ Plate"] <- "La Plata"
  
  ircc$basinnameold[ircc$basinnameold== "Ogoou\x8f"] <- "Ogooue"
  
  
  
  # join the basin shapes to the ircc data
  basins_ircc <- left_join(ircc, basin_shapes,
                              by = c("basinnameold" = "Basin_Name")) %>%
                  st_as_sf(crs = projection_crs) # make an sf object and based off projection crs
  
  
  # get ones that failed to merge (will have missing geometry)
  
  ircc_congo <- basins_ircc %>%
                filter(basinnameold == "Congo/Zaire")
  
  missing_geo <- basins_ircc[st_is_empty(basins_ircc),]
  
  missing_ircc_basins <- unique(missing_geo$basinnameold) %>% sort()
  
  
  basins_shapes_formissing <- left_join(basin_shapes,ircc,
                                        by = c("Basin_Name" = "basinnameold"))
  
  missing_shapes <- basins_shapes_formissing[is.na(basins_shapes_formissing$basinno),]
  
  
  missing_tfdd_basins <- unique(missing_shapes$Basin_Name) %>% sort()
  

  missing_tfdd_basins
  missing_ircc_basins

  
  # 4556 obs missing, deal with 
  
  
  basins_count <- basins_ircc %>% st_drop_geometry() %>% group_by(event) %>% count()
  
  count_events <- basins_ircc %>%
                  group_by(basinnameold) %>%
                  mutate(event_count = n_distinct(event)) %>%
                  filter(row_number() == 1)
  
  
  basins_ircc %>% filter(basinnameold == "Zambezi") %>% select(basinnameold, year,event) %>% view()
  
  
  
  

  basins_count <- basins_ircc %>% st_drop_geometry() %>% group_by(event) %>% count()
  
  
  basins_ircc_count <- left_join(basin_shapes,st_drop_geometry(count_events),
                           by = c("Basin_Name" = "basinnameold")) 
  
  
  saveRDS(object = basins_ircc_count,
          file = file.path(data_clean,"shape-files","basins_ircc.rds"))

  library(plotly)
  
  map <- ggplot(data = world) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    geom_sf(data = basins_ircc_count,
            alpha = .8,
            color = yale_lblue,
            mapping = aes(fill = event_count)) +#n) + #count) +
    # geom_sf_label(data = basin_shapes,
    #               aes(label = Basin_Name)) +
    labs(title = "Events in International Basins, 1997-2007",
         caption = paste0("Data from TFDD, IRCC \n","Grey is not yet merged correctly")) +
    theme_map() +
    labs(fill = "Number of events") +
    scale_fill_gradientn(colours = yale_palette)
  
  
  
  map
  
  ggplotly(map)
  
  
  save_map(output_folder = output_maps,
           plotname = map,
           filename = "basin_event_map.png",
           width = 20,
           height = 10,
           dpi  = 300)
  