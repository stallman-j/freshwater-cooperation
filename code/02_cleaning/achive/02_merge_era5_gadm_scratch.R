# _______________________________#
# Environment
# Merge 02: Extract polygons from precipitation rasters
# 
# Stallman
# Started 2023-10-11
# Last edited: 
#________________________________#



# Startup

rm(list = ls())


# bring in the packages, folders, paths

home_folder <- file.path("P:","Projects","environment")

source(file.path(home_folder,"code","00_startup_master.R"))


min_time <- "1940-01-01"
max_time <- "2023-09-01"
current_file <- "total_precipitation"
fun      <- "weighted_sum"
weights  <- "area"

# packages ----
if (!require(sf)) install.packages("sf") #shapes
if (!require(tmap)) install.packages("tmap") #for visual mapping
if (!require(terra)) install.packages("terra") 
if (!require(lubridate)) install.packages("lubridate")
if (!require(ecmwfr)) install.packages("ecmwfr")
if (!require(exactextractr)) install.packages("exactextractr")
if (!require(tictoc)) install.packages("tictoc")

library(ecmwfr) #European Centre for Medium-Range Weather forecasts, ERA 5 data  
library(sf) # GIS vectors
library(tmap) # plotting maps
library(terra) # GIS rasters
library(exactextractr) # extracting rasters to polygons
library(tictoc) # for timing how long things take

sf_use_s2(FALSE)

# bring in data ----
  path <- file.path(data_external_clean,"GADM","global")
  
  #levels <- 1:5
  
  level <- 1
  
  level_gadm <- readRDS(file = file.path(path,paste0("GADM_global_ADM_",level,".rds")))

  
  
  era_5 <- readRDS(file = file.path(data_external_clean,"ERA_5","raster",paste0(current_file,
                                                                       "_monthly_",
                                                                       min_time,"_to_",
                                                                       max_time,".rds")))
  
  era_5 
  
  
## parameters ----
  my_raster              <- era_5
  my_polygons            <- level_gadm
  values_varname         <- "tp_expver=1"
 
  # create an indicator for which dataset the observation is in
  my_polygons <- my_polygons %>% mutate(id = row_number())
  
  # change the names to dates
  
  # requires that the raster have unique dates in the same sequence
  time_vec <- time(my_raster) %>% ymd()
  names(my_raster) <- time_vec
  
  
  # determine type of the my_polygons
  
  class(st_geometry(my_polygons))
  
  # first cast everything to Multipolygon, then cast back to polygon:
  
  
  polygons_cast <- my_polygons %>% 
                  st_cast("MULTIPOLYGON") %>% # homogenizes the type
                  st_cast("POLYGON") %>% # puts all 
                  st_make_valid() %>% # make valid geometries %>%
                  mutate(id = row_number())

  #polygons_test <- my_polygons %>% filter(st_geometry_type(.) %in% c("POLYGON"))
  #multipolygons_test <- my_polygons %>% filter(st_geometry_type(.) %in% c("MULTIPOLYGON"))
  

  
  #polygons_counts <- my_polygons %>% count(GID_1)
  
  #num_poly <- polygons_counts %>% filter(n==1) 
  
  #test <- my_polygons %>% st_cast("POLYGON")
  
  # test
  
  #
# extract ----
  
  # use terra, raster or exact_extract?
  # exact_extract does better for big rasters or spatially fine
  # https://tmieno2.github.io/R-as-GIS-for-Economists/extract-speed.html
  
  # also it's faster to do all layers in a go
  

  # for a single layer
  # tic()
  # 
  # extract_by_level <- exact_extract(x = current_layer,
  #                            y = polygons_cast,
  #                            progress = FALSE) %>% # extracts to a list, where list element i corresponds to the ith row of the polygons sf
  #                            bind_rows(.id = "id") %>% 
  #                            mutate(id = as.numeric(id)) %>% 
  #                            group_by(id) %>% 
  #                            summarize(varname_aw = sum(value * coverage_fraction) / sum(coverage_fraction))
  # 
  # toc()
  # 7 seconds here
  
  # for multiple layers
  
  # test <- my_raster[[1:10]]
  
  
  # for a single layer
 tic()
 
 extract_by_level <- exact_extract(x = current_layer,
                            y = polygons_cast,
                            progress = FALSE) %>% # extracts to a list, where list element i corresponds to the ith row of the polygons sf
                            bind_rows(.id = "id") %>% 
                            mutate(id = as.numeric(id)) %>% 
                            group_by(id) %>% 
                            summarize(varname_aw = sum(value * coverage_fraction) / sum(coverage_fraction))
 
 toc()
 # 7 seconds here
 
 # for multiple layers
 
 test <- my_raster[[1:10]]
 
 
 tic()
 
 # https://tidyr.tidyverse.org/articles/pivot.html
 
 # https://codes.ecmwf.int/grib/param-db/?id=228
 
 extract_by_stack_level <- exact_extract(x = my_raster,
                                         y = polygons_cast,
                                         progress = F,
                                         fun = fun,
                                         weights = weights,
                                         ) 
                            
   
   extract_temp  <- extract_by_stack_level %>%
     mutate(id = row_number()) %>%
                            pivot_longer(cols = !id,
                                         names_to = "date",
                                         names_prefix = paste0(weights,"."),
                                         names_transform = ymd,
                                         values_to = paste0(values_varname,"_",fun,"_",weights))
   
   polygons_rast <- left_join(polygons_cast,extract_temp)
                                         
                                         

subset <- extract_temp[1:10,]
 
 
 %>%
                            bind_rows(.id = "id") %>% # convert from list to a dataframe 
                            mutate(id = as.numeric(id)) %>% # make id numberic rather than character
                            pivot_longer(-c(id, coverage_fraction),
                                                names_to = "raster_layer_name",
                                                values_to = "raster_value_name") %>% # pivot longer so we have the id gives which polygon, coverage fraction says how much of the grid cell is covered
                            group_by(id,raster_layer_name) %>%
                            summarize(mean_summarized = sum(raster_value_name*coverage_fraction) / sum(coverage_fraction), 
                                      total_summarized= sum(raster_value_name*coverage_fraction))  %>% # area-weighted total and mean
                            mutate(date = ymd(raster_layer_name),
                                   year = year(date),
                                   month = month(date),
                                   day   = day(date)
                                   )
 
 names(extract_by_stack_level)[3] <- values_varname
 
 toc()
 
 # merge back to the polygons_cast dataset
 
 polygons_extract <- left_join(polygons_cast,extract_by_stack_level) %>%
                      mutate(merge = case_when(!is.na(x_tracker) & !is.na(y_tracker) ~ "both",
                                               !is.na(x_tracker) & is.na(y_tracker) ~ "left",
                                               is.na(x_tracker) & !is.na(y_tracker) ~ "right"))
 
 
 # change the "summarized_value" to 
   
   
  toc()
  
  
  # 102.35 sec elapsed
  # 135.22 sec elapsed stupidly fast

# merge back to the sf with many polygons according to id number ----


# test out plotting ----

angola <- my_polygons %>% filter(GID_0== "AGO")
# 
ggplot(data = my_polygons[48:50,]) +
  geom_sf(aes(fill = GID_1))

names_raster  <- names(my_raster)
current_layer <- my_raster[[1]]

rast_df <- as.data.frame(current_layer, xy = TRUE) %>%
  na.omit() 


map <- ggplot() +
  geom_raster(data = rast_df,
              aes(x = x, y = y, fill = `tp_expver=1_1`)) +
  geom_sf(data = my_polygons,
          color = "gray70",
          fill = "gray99",
          alpha = 0,
          linewidth = 0.05)+
  scale_fill_viridis_c() +
  theme_void() +
  theme(
    legend.position = "bottom"
  )

save_map(output_folder = output_maps,
         plotname = map,
         filename = "raster_test_rast.png",
         width = 9,
         height = 5,
         dpi = 300)
