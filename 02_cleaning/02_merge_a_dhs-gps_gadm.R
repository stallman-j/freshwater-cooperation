# _______________________________#
# Environment
# Clean 02: Merge DHS and GPS Data
# 
# Stallman
# Started: 2023-10-23
# Last edited: 2023-11-13
# Edit: added get elevation to merge function merge_dhs_gps
#________________________________#


# https://dhsprogram.com/data/Guide-to-DHS-Statistics/Adult_Mortality_Rates.htm

# Startup

  rm(list = ls())


# bring in the packages, folders, paths ----
  
  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))
  source(file.path(code_startup_general,"merge_dhs_gps.R")) # function for merging dhs and gps data

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    stringr, # string operations
    countrycode, # country naming conversions
    sf, # vector spatial geometry
    rdhs, # getting DHS data
    elevatr, # for getting elevation from points
    RColorBrewer, # for getting gradients and colors
    parallel, # for parallelizing operations
    tictoc # timing # more ability to customize to output to latex. use with kableExtra to output tables
    # to console, latex, Rmarkdown, html etc.
  )

  #######################################################################
  ## NEED TO FIX THE COUNTRY CHOICES
  
  
# Get administrative boundaries ----
  
  # if you can't find the admin levels files, do this
  #source(file.path(code_folder,"02_clean_GADM.R"))
  
  # from 
  # dhsprogram.com/pubs/pdf/SAR7/SAR7.pdf
  
  # The geographic boundary files are usually either 
  # provided by the country or obtained from publicly available sources
  # such as the United Nation’s Second Administrative Level Boundaries (SALB) dataset and the Global
  # Administrative Areas (GADM) database (UNGIWG, 2013; GADM, 2012).
  # 
  # this is adm level 1
  country <- "UG"
  dhs_gps_filename <- "UGGE7AFL"
  
  # country codes 
#  https://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10136
  
  # UGGE7AFL and #UGGC7BFL
  
  # slightly more available
  # there are geographic covariates but slightly more files available with just the limited GPS datasets
  
  gps_datasets     <- dhs_datasets() %>% filter(DatasetType == "GPS Datasets") %>% filter(FileType == "Geographic Data") # this is the basic data
  # "Geographic Covariates" doesn't have SF files
  #
  countries <- gps_datasets$DHS_CountryCode %>% unique()
  
  filenames_list <- stringr::str_extract(gps_datasets$FileName,
                                         "[^.]+")

# Run for GADM ADM Level 2 (districts/state/province level) ----  
  
  gadm_in_path <- file.path(data_external_clean,"GADM","global")
  
  level <- 1
  
  for (level in c(1,2)) {
    
  gadm_in_filename <- paste0("GADM_global_ADM_",level,".rds")
  vector_sf <- readRDS(file.path(gadm_in_path,gadm_in_filename)) %>% mutate(vector_sf_id = row_number())

## Run first time ----
  # first cast everything to Multipolygon, then cast back to polygon:
  
  print("Casting polygons to POLYGON")

  tic("Casted polygons to POLYGON and saved")
  vector_cast <- vector_sf %>%
    st_cast("MULTIPOLYGON") %>% # homogenizes the type
    st_cast("POLYGON") %>% # puts all
    mutate(vector_cast_id = row_number()) %>%
    st_make_valid() # make valid geometries %>%

  saveRDS(vector_cast, file = file.path(gadm_in_path,paste0("GADM_global_ADM_",level,"_polygon_cast.rds")))


  toc() 

  # 237.33 sec elapsed # includes saving for LEVEL 2

## Run second time ----
  
  # vector_cast <- readRDS(file = file.path(gadm_in_path,paste0("GADM_global_ADM_",level,"_polygon_cast.rds")))

# Test for one country ----
  
  # country <- "UG" # in DHS lingo
  # 
  # GID_0_val    <- countrycode(country,
  #                             origin = "dhs",
  #                             destination = "iso3c")
  # 
  # print(paste0("starting on country ",GID_0_val," and time is ",Sys.time()))
  # # bring in the GADM files
  # 
  # 
  # tic("restricting polygons to current country")
  # test_country_cast <- vector_cast %>% filter(GID_0 == GID_0_val)
  # #rm(vector_cast)
  # gc()
  # toc() #  1.83 sec elapsed
  # 
  # 
  # # example with a single country
  # 
  level <- 1
  #TJGE61FL
  #TZGE6AFL
  merge_dhs_gps(country = "TJ",
                gps_datasets = gps_datasets,
                dhs_gps_filename = "TJGE71FL",
                dhs_gps_in_path = file.path(data_external_raw,"DHS"),
                country_cast = country_cast,
                out_path      = file.path(data_external_temp,"DHS","GPS"),
                filter_var = "LONGNUM",#NULL, #"LONGNUM",
                filter_val = 5, #NULL, # 5
                gadm_in_filename = paste0("GADM_global_ADM_",level,".rds"),
                gadm_out_filestub = paste0("GADM_ADM_",level))

  # 
  
# Run for all countries at administrative Level  ----
  
  tic(paste0("Running through all countries at ADM level ", level," to merge with GPS data"))
  
  #country <- "UG" # to check if errors
  for (country in countries) {
  
    GID_0_val    <- countrycode(country,
                                origin = "dhs",
                                destination = "iso3c")
    
    tic("restricting polygons to current country")
    country_cast <- vector_cast %>% filter(GID_0 == GID_0_val)
    print(paste0("starting on country ",GID_0_val," and time is ",Sys.time()))
    toc()
    
  merge_dhs_gps(country = country,
                gps_datasets = gps_datasets,
                dhs_gps_filename = "all",
                dhs_gps_in_path = file.path(data_external_raw,"DHS"),
                country_cast = country_cast,
                out_path      = file.path(data_external_temp,"DHS","GPS"),
                filter_var = NULL, #"LONGNUM",
                filter_val = NULL, # 5
                gadm_in_filename = paste0("GADM_global_ADM_",level,".rds"),
                gadm_out_filestub = paste0("GADM_ADM_",level))

  }
  toc()
  
  

  }
  
  
# merge together all the DHS GPS datasets  ----
# 

  gps_datasets     <- dhs_datasets() %>% filter(DatasetType == "GPS Datasets") %>% filter(FileType == "Geographic Data") # this is the basic data
