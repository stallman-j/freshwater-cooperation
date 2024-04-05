# _______________________________#
# Environment
# Clean 02: Merge DHS and GPS in order to get rive rnetworks
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

# if the required input datasets don't exist, run the cleaning code to get those datasets

  if (!file.exists(file.path(data_external_clean,"DHS","datasets-for-selection",
                             paste0("gps_datasets_africa.rds")))) {
    
    source(file.path(code_clean,"02_clean_choose-DHS-countries.R"))
  }

# bring in the list of datasets we want to pull from
  gps_datasets_africa <- readRDS(file= file.path(data_external_clean,"DHS","datasets-for-selection",
                                                 paste0("gps_datasets_africa.rds")))
  
# merge DHS GPS datasets into one dataset
  
  # see documentation at  
  # dhsprogram.com/pubs/pdf/SAR7/SAR7.pdf

  # country codes 
#  https://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10136
  
  # UGGE7AFL and #UGGC7BFL

  dhs_gps_filenames <- stringr::str_extract(gps_datasets_africa$FileName,
                                         "[^.]+")

  
  dhs_gps_in_path <- file.path(data_external_raw,"DHS")
  dhs_gps_filenames_rds <- paste0(dhs_gps_filenames,".rds")
  
  dhs_gps_in <- file.path(dhs_gps_in_path,dhs_gps_filenames_rds)
  

  # bring in one dataframe
  df <- readRDS(dhs_gps_in[1]) 
  

  # choose the columns to select
  desired_columns <-names(df)
  # [1] "DHSID"      "DHSCC"     
  # [3] "DHSYEAR"    "DHSCLUST"  
  # [5] "CCFIPS"     "ADM1FIPS"  
  # [7] "ADM1FIPSNA" "ADM1SALBNA"
  # [9] "ADM1SALBCO" "ADM1DHS"   
  # [11] "ADM1NAME"   "DHSREGCO"  
  # [13] "DHSREGNA"   "SOURCE"    
  # [15] "URBAN_RURA" "LATNUM"    
  # [17] "LONGNUM"    "ALT_GPS"   
  # [19] "ALT_DEM"    "DATUM"     
  # [21] "geometry" 
  
  
  i <- 2
  tic("Merged all Africa DHS GPS files into one mega GPS DHS file")
  for (i in 2:length(dhs_gps_in)){
    
    print(paste0("Working on DHS GPS file ",dhs_gps_in[i],"."))
    
    
    if (!file.exists(dhs_gps_in[i])){
      
      print(paste0("DHS GPS file ",dhs_gps_in[i]," can't be found."))
      
    } else {
      
        temp <- readRDS(dhs_gps_in[i])%>%
                st_transform(crs = st_crs(df))
        
        temp <- temp[desired_columns]
        
      
        df <- rbind(df,temp)
        
      
    } # end else statement if file exists then read in the DHS gps filename and rbind to temp 
  } # end loop over all DHS filenames

  toc()
  # "DHS GPS file E:/data/01_raw/DHS/TZGE81FL.rds can't be found."
  # [1] "DHS GPS file E:/data/01_raw/DHS/GHGE8AFL.rds can't be found."
  # [1] "DHS GPS file E:/data/01_raw/DHS/CMGE81FL.rds can't be found."
  
  # Merged all Africa DHS GPS files into one mega GPS DHS file: 11.43 sec elapsed
  
  
  out_path      = file.path(data_external_temp,"DHS","GPS","merged")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)
  
  df <- df  %>% st_transform(crs = equal_area_crs)
  
saveRDS(df,
        file = file.path(data_external_temp,"DHS","GPS","merged","africa_DHS_GPS.rds"))
