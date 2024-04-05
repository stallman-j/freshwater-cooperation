# _______________________________#
# Environment
# Clean 02: DHS Child Mortality Testing
# 
# Stallman
# Started: 2023-10-23
# Last edited: 
#________________________________#


# https://dhsprogram.com/data/Guide-to-DHS-Statistics/Adult_Mortality_Rates.htm

# Startup

  rm(list = ls())

  # Only run this the first time, otherwise start from "cleaning!"
  
# bring in the packages, folders, paths ----
  
  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))
  

  if (!require(sf)) install.packages("sf")
  if (!require(tictoc)) install.packages("tictoc")
  if (!require("countrycode")) install.packages("countrycode")
  if (!require("rdhs")) install.packages("rdhs")
  if (!require("stringr")) install.packages("stringr")
  
  library(stringr)      # string operations
  library(countrycode)  # convert country
  library(sf)
  library(tictoc)
  library(rdhs) # for getting DHS data through R

# Get administrative boundaries ----
  
  # if you can't find the admin levels files, do this
  source(file.path(code_folder,"02_clean_GADM.R"))
  
  # from 
  # dhsprogram.com/pubs/pdf/SAR7/SAR7.pdf
  
  # The geographic boundary files are usually either 
  # provided by the country or obtained from publicly available sources
  # such as the United Nation’s Second Administrative Level Boundaries (SALB) dataset and the Global
  # Administrative Areas (GADM) database (UNGIWG, 2013; GADM, 2012).
  # 
  # this is adm level 1
  level <- 2
  country <- "UG"
  dhs_gps_filename <- "UGGE7AFL"
  
  # country codes 
#  https://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10136
  
  # UGGE7AFL and #UGGC7BFL
  
  # slightly more available
  # there are geographic covariates but slightly more files available with just the limited GPS datasets
  
  gps_datasets     <- dhs_datasets() %>% filter(DatasetType == "GPS Datasets") %>% filter(FileType == "Geographic Data")
  countries <- gps_datasets$DHS_CountryCode %>% unique()
  
  filenames_list <- stringr::str_extract(gps_datasets$FileName,
                                         "[^.]+")
  
  
  gadm_in_path <- file.path(data_external_clean,"GADM","global")
  gadm_in_filename <- paste0("GADM_global_ADM_2.rds")
  polygons_shapefile <- readRDS(file.path(gadm_in_path,gadm_in_filename))
    

  
  tic("Running through all countries at ADM level 2 to merge with GPS data")
  polygons_shapefile <- readRDS(file.path(gadm_in_path,"GADM_global_ADM_2.rds"))
  
  for (country in countries){
  
  merge_dhs_gps(country = country,
                gps_datasets = gps_datasets,
                dhs_gps_filename = "all",
                gadm_in_filename = paste0("GADM_global_ADM_2.rds"),
                gadm_out_filestub = paste0("GADM_ADM_2"))

  }
  toc()

  
  tic("Running through all countries at ADM level 1 to merge with GPS data")
  polygons_shapefile <- readRDS(file.path(gadm_in_path,"GADM_global_ADM_1.rds"))
  for (country in countries){
    
    merge_dhs_gps(country = country,
                  gps_datasets = gps_datasets,
                  polygons_shapefile = polygons_shapefile,
                  dhs_gps_filename = "all",
                  gadm_out_filestub = paste0("GADM_ADM_1"))
    
  }
  toc() # 81.03 seconds; compare to 509 seconds when each time through re-read in the polygons_shapefile
  
  map <- ggplot()+
    geom_sf(data = polygons_cast,
            fill = "grey80",
            alpha = .2)+
    geom_point(data = GPS_poly,
               aes(x = LONGNUM,
                   y = LATNUM,
                   colour = factor(GPS_poly$id))
    )+
    labs(title = paste0("Survey Cluster Locations by District (ADM Level 2), Uganda 2016"),
         caption = c("Data from DHS (2016) and GADM (2022)"))+
    xlab("") + ylab("")+
    theme_map(legend_position = "none") +
    scale_color_manual(values = randomcoloR::distinctColorPalette(k=length(unique(GPS_poly$id)) ))
  
  map
  
  save_map(output_folder = file.path(output_maps,"DHS"),
           plotname = map,
           filename = paste0("survey_cluster_locations_ADM_level_2_2016_UGA.png"),
           width = 6,
           height = 7,
           dpi  = 400)
  
  