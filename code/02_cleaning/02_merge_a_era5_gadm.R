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

# packages ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tictoc, #measuring time to run operations
  zoo # time series stuff, easy calc of rolling averages
)


# parameters ----
  min_time      <- "1940-01-01"
  max_time      <- "2023-09-01"
  current_file  <- "total_precipitation"
  func           <- "weighted_sum"
  weights       <- "area"
  level         <- 2
  levels        <- 1:5
  time_interval <- "months"
  layer_substrings  <- c("tp_expver=1")
  vector_sf_path         <- file.path(data_external_clean,"GADM","global")
  
  # to check that the raster rotate works and to run this through with a raw file
  #rast_in_path <- file.path(data_external_raw,"ERA_5",paste0(current_file,".nc"))
  #terra_raster <- terra::rast(x=rast_in_path)
  
  
  rast_in_path <- file.path(data_external_clean,"ERA_5","raster",paste0(current_file,
                                                                        "_monthly_",
                                                                        min_time,"_to_",
                                                                        max_time,".rds"))
  
  terra_raster <- terra::rast(x= rast_in_path) 
  
  terra::crs(terra_raster) <- "epsg:4326"
  
  #raster_out_filename <- paste0(current_file,"_monthly_",min(time(terra_raster)),"_to_",max(time(terra_raster)),".rds")
  #raster_out_path     <- file.path(data_external_clean,"ERA_5","raster")

  source(file.path(code_startup_general,"raster_extract_to_panel.R"))

# extract rasters to a panel df and a large vector file ----


  extracted_out_path <- file.path(data_external_clean,"merged")
  extracted_out_filename <- paste0(min_time,"_to_",
                                   max_time,"_GADM_ADM_",level,"_panel_",
                                   current_file,"_monthly_df.rds")
  
  for (level in levels){
    
  vector_sf    <- readRDS(file = file.path(vector_sf_path,paste0("GADM_global_ADM_",level,".rds")))
  
  
  raster_extract_to_panel(terra_raster = terra_raster,
                          vector_sf    = vector_sf,
                          save_raster_copy=FALSE,
                          raster_out_path = NULL,
                          raster_out_filename = NULL,
                          extracted_out_path  = extracted_out_path,
                          extracted_out_filename = extracted_out_filename,
                          vector_cast_out_path = file.path(data_external_clean,"GADM","global"),
                          vector_cast_out_filename = paste0("GADM_ADM_",level,"_cast.rds"),
                          layer_substrings = layer_substrings,
                          func = "weighted_sum",
                          weights = "area",
                          time_interval = "months",
                          remove_files = TRUE
                          )
  
  
  }
  # for global and GADM adm 1 this 
  # Read in file at GADM level 2: 179.25 sec elapsed
  
# Define variables ----
  
  tic("Getting df for a single country")
  country_df <- out_df %>% filter(GID_0=="UGA")
  toc()
  # Getting df for a single country: 1.54 sec elapsed
  
  options(scipen=100, digits = 4)
  
  
  gadm_1 <- readRDS(file = file.path(data_external_clean,"GADM","global","GADM_global_ADM_1.rds"))
  
  countries <- unique(gadm_1$GID_0)
  
  rm(gadm_1)
  
  out_path <- file.path(data_external_clean,"merged","country-level")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  
  levels <- c(1,3,4,5)
  
  
   for (level in levels){
     
  tic(paste0("Read in file at GADM level ",level))
  
  out_df <- readRDS(file= file.path(extracted_out_path,extracted_out_filename))
  
  toc()
  
  

  # https://www.storybench.org/how-to-calculate-a-rolling-average-in-r/
  
  # country <- countries[1]
  
  tic(paste0("Ran through all countries and defined new vars and saved RDS at GADM level ",level))

    for (country in countries) {
    
    tic(paste0("Finished country ",country, " at GADM level ",level))
    
    out_filename <- paste0(min_time,"_to_",max_time,"_GADM_ADM_",level,"_",country,"_panel_",
                           current_file,"_monthly_df_with_vars.rds")
    temp_df <- out_df %>%
            filter(GID_0 == country) %>%
                group_by(date) %>% # arrange chronologically
                arrange(vector_cast_id) %>% # sorted now by vector_cast_id and chronoligcally within
                ungroup() %>%
                mutate(precip = `tp_expver=1_weighted_sum_area`/1000000) %>%
                group_by(vector_cast_id) %>% # group by 
                mutate(precip_003m_ra = zoo::rollmean(precip, k=3,   fill = NA, align = "right"), # rolling averages of the k prior months
                       precip_013m_ra = zoo::rollmean(precip, k=13,  fill = NA, align = "right"),
                       precip_025m_ra = zoo::rollmean(precip, k=25,  fill = NA, align = "right"),
                       precip_037m_ra = zoo::rollmean(precip, k=37,  fill = NA, align = "right"),
                       precip_121m_ra = zoo::rollmean(precip, k=121, fill = NA, align = "right")) %>%
                group_by(vector_cast_id,month(date)) %>% # generate monthly precip stats
                mutate(precip_lr_monthly_avg = mean(precip),
                       precip_lr_monthly_sd  = sd(precip),
                       precip_monthly_zscore = (precip - mean(precip))/sd(precip)) %>%
                ungroup() %>%
                group_by(vector_cast_id,year(date)) %>% # generate current
                mutate(precip_current_annual_avg  = mean(precip),
                       precip_current_annual_sd   = sd(precip),
                       precip_annual_zscore       = (precip - mean(precip))/sd(precip)) %>% 
                ungroup() %>%
                group_by(vector_cast_id) %>% # generate long-run averages
                mutate(precip_lr_sd = sd(precip),
                       precip_lr_mean = mean(precip),
                       precip_zscore  = (precip - mean(precip))/sd(precip),
                       precip_sd_deviation = precip_current_annual_sd - sd(precip)) 
  

  #(object.size(temp_df) %>% format(units = "Gb"))

  saveRDS(temp_df, file= file.path(out_path,out_filename))

  rm(temp_df)

  gc()
  
  toc()
  
  }
  
  toc() # how long did the loop go for
  
  tic("Removed out_df")
  rm(out_df)
  toc()
  
  tic("Removed garbage")
  gc()
  toc()
  
  }
  
  #Ran through all countries and defined new vars and saved RDS at GADM level 2: 1943.92 sec elapsed
  
  # test <- current_df %>%
  #         filter(vector_cast_id == unique(current_df$vector_cast_id)[1] |
  #                  vector_cast_id == unique(current_df$vector_cast_id)[2])
  # 

  # for ADM level 2: [1] "19.3 Gb"


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
