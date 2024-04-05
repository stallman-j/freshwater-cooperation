# _______________________________#
# Environment
# Clean HydroRIVERS generic
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

# some parameters ----
  n_cores <- detectCores(logical = TRUE) - 2
  
  my_units <- c("Zambia","Zimbabwe","Angola","Malawi","Mozambique","Botswana","Tanzania","Namibia")
  units_name <- c("Zambezi")
  #my_units <- c("Malawi")
  
  #units_name <- c("Malawi")
  
# read in data ----
  
  library(sf)
  library(tictoc)
  library(parallel)
  #library(doParallel)
  
  
  path <- file.path(data_raw,"hydroSHEDS","HydroRIVERS_v10_af.gdb","HydroRIVERS_v10_af.gdb")
  
  system.time(
  hydro_rivers <- st_read(dsn = path)
  )
  
  # world
  # user  system elapsed 
  # 38.94   19.05  139.93 


  # africa
  # user  system elapsed 
  # 7.74    2.11   20.75 
  
  
  names(hydro_rivers)
  # [1] "HYRIV_ID"     "NEXT_DOWN"    "MAIN_RIV"     "LENGTH_KM"    "DIST_DN_KM"   "DIST_UP_KM"   "CATCH_SKM"   
  # [8] "UPLAND_SKM"   "ENDORHEIC"    "DIS_AV_CMS"   "ORD_STRA"     "ORD_CLAS"     "ORD_FLOW"     "HYBAS_L12"   
  # [15] "Shape_Length" "Shape" 
  # 
  # 
  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  # pre-fill with just the first, Afghanistan so we don't have to care about names
  units <- world[1,]
  
  # add in however many rows we need to the dataframe of the countries to include
  
  for (i in seq_along(my_units)) {
    
    new_unit <- world %>%
               filter(name == my_units[i])
    
    units[i,] <- new_unit
    
  }
  
  rm(new_unit)
  

  plot(st_geometry(units))
  
  st_crs(world)
  
  # change crs to be same as the rivers
  units <- units %>%
                  st_transform(crs = st_crs(hydro_rivers))
  
  saveRDS(units,
          file = file.path(data_clean,"shape-files",paste0(units_name,"_.rds")))
  
  units <- readRDS(file =file.path(data_clean,"shape-files",paste0(units_name,"_.rds")) )
  
  
# spatial subset so we can work with just rivers of the units we want
  
  # very slow with many countries
  # system.time(
  # hydro_rivers_units <- st_intersection(hydro_rivers,units)
  # )
  
 # https://stackoverflow.com/questions/48800375/parallelize-st-union-from-rs-sf-package
  
  
  # clearly outlines what we need
  
  # https://byuistats.github.io/M335/parallel.html
  
  
  


  

  # putting the parallel in a function got stupidly slow ,take it out of the function
  
  tic()
  split_list <- 
  get_parallel_splits(sf_df1 = hydro_rivers, 
              n_cores = n_cores)
  
  cl <- makeCluster(n_cores)
  clusterEvalQ(cl,library(sf))
  clusterExport(cl, c("units"))
  
  
  split_results <-  parLapply(cl, split_list, function(x) st_intersection(x,units))
  
  stopCluster(cl)
  
  hydro_rivers_units <- do.call("rbind", split_results)
  

  
  toc()
  # 65.16 sec elapsed for the Zambezi river basin; 19 sec for just Malawi
  
  
  plot(st_geometry(hydro_rivers_units))
  



# create a "width" variable for plotting purposes depending on hydrologic discharge ----
  
  hydro_rivers_units <- hydro_rivers_units %>%
                            mutate(width = 1/as.numeric(ORD_CLAS))
  

  saveRDS(hydro_rivers_units,
          file = file.path(data_clean,"shape-files",paste0(units_name,"_hydro_rivers.rds")))
  
  
  hydro_rivers_units <- readRDS(file = file.path(data_clean,"shape-files",paste0(units_name,"_hydro_rivers.rds")))
  
  
  
  
  
# Work to do ----
  # # randomly sample some of these
  # 
  # sample_id <- sample(nrow(hydro_rivers_units),
  #                        size = 100,
  #                        replace= FALSE)
  # 
  # river_sample <- hydro_rivers_units[sample_id,]
  # 
  # ## CHANGE THIS WHEN NOT SAMPLING
  # 
  # #hydro_rivers_units <- river_sample
  # 
  # # take out the world rivers for space
  # rm(hydro_rivers)
  # 
  # 


  

  
#   
# # Get the rivers that intersect a country boundary ----
#   
#   library(rnaturalearth) # has ne_coastline() which is global coastlines
#   if (!require(rnaturalearthdata)) install.packages("rnaturalearthdata")
#   
#   library(rnaturalearthdata)
#   
#   # scale medium means a medium-sized file, better detail
#   coastlines <- ne_coastline(scale = "medium", returnclass = "sf")
#   
#   
#     # cast the country polygon borders into lines
#   
#   sa_country_lines <- st_cast(south_america, "MULTILINESTRING", 
#                               group_or_split = TRUE)
#   
#   sa_outline <- st_cast(st_union(south_america), "MULTILINESTRING",
#                         group_or_split = TRUE)
#   
#   # create a yes-no insersection if the geometries intersect
#   
#   # a list is returned for st_intersects, so take the ones where there's a positive length of intersection
#   # ie that the river intersects a country line
#   
#   # 
#   # generates a logical vector of length of hydro_rivers_units
#   intersects_border_yesno <- lengths(st_intersects(hydro_rivers_units,sa_country_lines)) > 0
#   coastline_yesno <- lengths(st_intersects(hydro_rivers_units, coastlines)) >0
#   outline_TF      <- lengths(st_intersects(hydro_rivers_units, sa_outline)) >0
#   
#   # verify that this is getting what we want
#   check_border <- intersects_border_yesno[1:30]
#   check_coastline  <- coastline_yesno[1:30]
#   
#   check_international <- check_border & !check_coastline
#   
#   intersects_international_yesno <- intersects_border_yesno & !coastline_yesno & !outline_TF
#   
# 
#   # so now we want IS international, but is NOT intersecting a coast
#   
#   rivers_international <- hydro_rivers_units[intersects_international_yesno,]
#   rivers_domestic      <- hydro_rivers_units[!intersects_international_yesno,]
#   
#   saveRDS(rivers_international,
#           file = file.path(data_clean,"rivers_international_sa.rds"))
#   saveRDS(rivers_domestic,
#           file = file.path(data_clean,"rivers_domestic_sa.rds"))
#   
#   
#   # try now with a function
#   
#   # rivers_intl_fn <- get_in_out_intersections(sf_of_interest = hydro_rivers_units,
#   #                                            yes_sf = sa_country_lines,
#   #                                            no_sf  = coastlines)
#   # 
#   # 
#   
#   #plot(st_geometry(rivers_international))
#   
#   # plot to check
#   
#   map <- ggplot(data = south_america) +
#     geom_sf(color = "gray70",
#             fill = "gray99",
#             alpha = 0.5,
#             linewidth = .3) +
#     # geom_sf(data = basin_shapes,
#     #         alpha = .2,
#     #         color = yale_lblue,
#     #         fill  = yale_lblue) +
#     geom_sf(data = hydro_rivers_units,
#             alpha = .3,
#             color = yale_lblue,
#             linewidth = .3) +
#     geom_sf(data = rivers_international,
#             alpha = 1,
#             color = yale_blue,
#             linewidth = 1) +
#     # geom_sf(data = china_dams,
#     #         alpha = .4,
#     #         color = yale_blue,
#     #         size = .5) +
#     labs(title = "Rivers of South America",
#          caption = c("Data from HydroRIVERS (2019). International segments in medium blue")) +
#     theme_map()
#   
#   #map
#   
#   save_map(output_folder = output_maps,
#            plotname = map,
#            filename = "hydro_rivers_units_hydrosheds.png",
#            width = 9,
#            height = 5,
#            dpi  = 300)
#   
#   
#   # plot rivers if intersection is true
#   
# #   # for interactive ggplot
# #   if (!require(plotly)) install.packages("plotly")
# #   
# #   library(plotly)
# #   
# #   # take a look more closely
# #   ggplotly(map)
# #   
# # ## use fancier data ----
# #   
# #   # mask
# #   path <- file.path(data_raw,"GRWL_global-river-widths-landsat","GRWL_vector_V01.01","GRWL_vector_V01.01")
# #   
# #   # view available layers
# #   st_layers(path)
# #   
# #   
# #   
# #   test_read <- st_read(path)
# #   
# #    
# #   map <- ggplot(data = world) +
# #     geom_sf(color = "gray70",
# #             fill = "gray99",
# #             alpha = 0.5,
# #             linewidth = .3) +
# #     # geom_sf(data = basin_shapes,
# #     #         alpha = .2,
# #     #         color = yale_lblue,
# #     #         fill  = yale_lblue) +
# #     geom_sf(data = test_read,
# #             alpha = .3,
# #             color = yale_medblue,
# #             linewidth = 1) +
# #     # geom_sf(data = china_dams,
# #     #         alpha = .4,
# #     #         color = yale_blue,
# #     #         size = .5) +
# #     labs(title = "Finding the data",
# #          caption = c("Data from GRLW (2018)")) +
# #     theme_map()
# #   
# #   map
# #   
# #   ggplotly(map)
# #   
# #   
