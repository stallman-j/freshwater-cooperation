# _______________________________#
# Environment
# Clean 02: Merge DHS IR Recode
# 
# Stallman
# Started: 2023-10-23
# Last edited: 2024-04-22
#________________________________#



# Startup

  rm(list = ls())

# Only run this the first time, otherwise start from "cleaning!"

# bring in the packages, folders, paths ----

  code_folder <- file.path("P:","Projects","freshwater-cooperation","code")
  source(file.path(code_folder,"00_startup_master.R"))


# if needed to get this 
# relies on there being GPS data to merge onto
# source(file.path(code_clean,"02_merge_dhs_GPS.R"))


if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  naniar, # deal with NA vars
  sjlabelled, # labeling variables
  expss, #spss?
  xlsx, # read/write excel
  sf, # spatial stuff
  #DHS.rates,
  lubridate, # dates
  stringr, # string operations
  haven, # stata stuff
  rdhs, # interface with DHS
  countrycode, # change naming conventions
  tictoc # timing
)

# if the required input datasets don't exist, run the cleaning code to get those datasets

if (!file.exists(file.path(data_external_clean,"DHS","datasets-for-selection",
                           paste0("ir_datasets_africa.rds")))) {
  
  source(file.path(code_clean,"02_clean_choose-DHS-countries.R"))
}

# bring in the list of datasets we want to pull from
ir_datasets_africa <- readRDS(file= file.path(data_external_clean,"DHS","datasets-for-selection",
                                               paste0("ir_datasets_africa.rds")))
# merge DHS IR Recode
  
  # see documentation at  
  # dhsprogram.com/pubs/pdf/SAR7/SAR7.pdf

  # country codes 
#  https://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10136
  
  # UGGE7AFL and #UGGC7BFL

  dhs_ir_filenames <- stringr::str_extract(ir_datasets_africa$FileName,
                                         "[^.]+")

  
  dhs_ir_in_path <- file.path(data_external_raw,"DHS")
  dhs_ir_filenames_rds <- paste0(dhs_ir_filenames,".rds")
  
  dhs_ir_in <- file.path(dhs_ir_in_path,dhs_ir_filenames_rds)
  

  # bring in one dataframe
  df <- readRDS(dhs_ir_in[1]) 
  

  # choose the columns to select
  desired_columns <-names(df)

  
  keep_vars <- c("hhid", # HH id
                 "v000", # country code + phase
                 "v001", # cluster number
                 "v002", #household number
                 "v003", # respondent line number
                 "v004", # ultimate area unit
                 "v005", # sample weight
                 "v006", # interview month
                 "v007", #interview year
                 "v008", # date of interview
                 "v021", # primary sampling unit
                 "v022", # sample stratum number
                 "v023", # sample domain
                 "v024", # region
                 "v025", # type of place of residence
                 "v040", # cluster altitude, meters
                 "v113", # source of drinking water
                 "v115", # time to get to water source
                 "v116", # type of toilet
                 "v119", #has electricity
                 "v120", # has radio
                 "v121", # has TV
                 "v122", #has fridge
                 "v123", # has bike
                 "v124", # has moto / scooter
                 "v125", # has car/truck
                 "v130", # religion
                 "v131", # ethnicity
                 "v167", # number trips last 12 months
                 "v168", # away for > 1 month in last 12
                 "v169a", # owns mobile phone
                 "v169b" # uses mobile phone for transactions
  )
                 
  i <- 2
  tic("Merged all Africa DHS IR files into one mega IR DHS file")
  for (i in 2:length(dhs_ir_in)){
    
    print(paste0("Working on DHS IR file ",dhs_ir_in[i],"."))
    
    
    if (!file.exists(dhs_ir_in[i])){
      
      print(paste0("DHS IR file ",dhs_ir_in[i]," can't be found."))
      
    } else {
      
        temp <- readRDS(dhs_ir_in[i])%>%
                st_transform(crs = st_crs(df))
        
        temp <- temp[desired_columns]
        
      
        df <- rbind(df,temp)
        
      
    } # end else statement if file exists then read in the DHS IR filename and rbind to temp 
  } # end loop over all DHS filenames

  toc()
  # "DHS IR file E:/data/01_raw/DHS/TZGE81FL.rds can't be found."
  # [1] "DHS IR file E:/data/01_raw/DHS/GHGE8AFL.rds can't be found."
  # [1] "DHS IR file E:/data/01_raw/DHS/CMGE81FL.rds can't be found."
  
  # Merged all Africa DHS IR files into one mega IR DHS file: 11.43 sec elapsed
  
  
  out_path      = file.path(data_external_temp,"DHS","IR","merged")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)
  
  df <- df  %>% st_transform(crs = equal_area_crs)
  
saveRDS(df,
        file = file.path(data_external_temp,"DHS","IR-recode","merged","africa_DHS_ir.rds"))
