# _______________________________#
# Environment
# Clean 02: Merge DHS HH Recode
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
  tictoc, # timing
  tidyverse
)

# if the required input datasets don't exist, run the cleaning code to get those datasets

if (!file.exists(file.path(data_external_clean,"DHS","datasets-for-selection",
                           paste0("hr_datasets_africa.rds")))) {
  
  source(file.path(code_clean,"02_clean_choose-DHS-countries.R"))
}

# bring in the list of datasets we want to pull from
hr_datasets_africa <- readRDS(file= file.path(data_external_clean,"DHS","datasets-for-selection",
                                               paste0("hr_datasets_africa.rds")))
# merge DHS HH Recode
  
  # see documentation at  
  # dhsprogram.com/pubs/pdf/SAR7/SAR7.pdf

  # country codes 
#  https://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10136
  
  # UGGE7AFL and #UGGC7BFL

  dhs_hr_filenames <- stringr::str_extract(hr_datasets_africa$FileName,
                                         "[^.]+")

  
  dhs_hr_in_path <- file.path(data_external_raw,"DHS")
  dhs_hr_filenames_rds <- paste0(dhs_hr_filenames,".rds")
  
  dhs_hr_in <- file.path(dhs_hr_in_path,dhs_hr_filenames_rds)
  
  
  # bring in one dataframe
  df <- readRDS(dhs_hr_in[1]) %>%
        mutate(SurveyYear = hr_datasets_africa$SurveyYear[1],
               DHS_CountryCode = hr_datasets_africa$DHS_CountryCode[1],
               DHSID           =paste0(DHS_CountryCode,SurveyYear,stringr::str_pad(hv001, 8, pad = "0"))
)
  
  
  df <- sjlabelled::remove_all_labels(df)
  
  # drop the vars of the form e.g. hvidx$05 which is the line number of HH number,
  # also drops hv101 (relationship to head of HH); whether HH member is de jure or de facto (hv102; hv103); 
  # sex (hv104) age (105) education (106-110); whether mother (111) father (113) is alive; marital status (115); 
  # prior marital status (116); eligibility for other surveys (118-121)
  # other education (123-129); birth certificate (hv140); illness of self, mother,father (hv130-132)
  # parents alive (hv133); sibling relationships (hv134-136); member has certain HH items (e.g. blanket, shoes) (hv137-139)
  # woman's age, height, weight hemoglobin etc)
  # 
  keep_vars <- c("hhid", # HH id
                 "SurveyYear",
                 "DHS_CountryCode",
                 "DHSID",
                 "hv002", #household number
                 "hv004", # ultimate area unit
                 "hv005", # sample weight
                 "hv006", # interview month
                 "hv007", #interview year
                 "hv008", # date of interview
                 "hv021", # primary sampling unit
                 "hv022", # sample stratum number
                 "hv023", # sample domain
                 "hv024", # region
                 "hv025", # type of place of residence
                 "hv040", # cluster altitude, meters
                 "hv045a", # language of questionnaire
                 "hv045b", # language of interview
                 "hv045c", # native language of respondent
                 "hv201", # source of drinking water
                 #https://dhsprogram.com/data/Guide-to-DHS-Statistics/Household_Drinking_Water.htm
                 "hv201a", # water not available for at least a day in last 4 weeks
                 "hv202", # source non-drinking water
                 "hv204", # time to get to water source # lots of 996 and 998. 998 is "I don't know"
                 "hv205", # type of toilet
                 "hv206", #has electricity
                 "hv207", # has radio
                 "hv208", # has TV
                 "hv209", #has fridge
                 "hv210", # has bike
                 "hv211", # has moto / scooter
                 "hv212", # has car/truck
                 "hv221", # has telephone
                 "hv235", # location of source for water 
                 "hv236", # person fetching water
                 "hv238", # # HHs sharing a toilet
                 "hv243a",# has mobile telephone
                 "hv243c",# has animal-drawn cart
                 "hv244", # own land usable for ag.
                 "hv245", # hectares of agricultural land
                 "hv246", # livestock, herds, farm animals
                 "hv246a", # cattle
                 "hv246b", #cows/bulls
                 "hv246c", # horses, donkeys, mules
                 "hv246d", # goats
                 "hv246e", # sheep
                 "hv246f", # chickens
                 "hv246g" # country specific
                 )
  
  

  #' add_na_cols(mtcars, "mpg")
  #' add_na_cols(mtcars, c("topspeed","nhj","mpg"))

  add_na_cols <- function(data, desired_cols) {
    add <-desired_cols[!desired_cols%in%names(data)]
    
    if(length(add)!=0) data[add] <- NA
    data
  }
  
  
  df <- add_na_cols(df, keep_vars) 
  
  df <- df[keep_vars] 

  


  i <- 2
  tic("Merged all Africa DHS HR files into one mega HR DHS file")
  for (i in 2:length(dhs_hr_filenames_rds)){
    
    print(paste0("Working on DHS HH file ",dhs_hr_in[i],"."))
    
    
    if (!file.exists(dhs_hr_in[i])){
      
      print(paste0("DHS HH file ",dhs_hr_in[i]," can't be found."))
      
    } else {
      
        temp <- readRDS(dhs_hr_in[i])%>%
          mutate(SurveyYear = hr_datasets_africa$SurveyYear[i],
                 DHS_CountryCode = hr_datasets_africa$DHS_CountryCode[i],
                 DHSID           =paste0(DHS_CountryCode,SurveyYear,stringr::str_pad(hv001, 8, pad = "0"))
                 
                 )
        
        temp <- sjlabelled::remove_all_labels(temp)
        
        
        # add the column varnames if they didn't exist already
        temp <- add_na_cols(temp, keep_vars)
        
        # then keep only these vars
        temp <- temp[keep_vars] 
      
      
        df <- rbind(df,temp)
        
      
    } # end else statement if file exists then read in the DHS hh filename and rbind to temp 
  } # end loop over all DHS filenames

  toc()

 # Merged all Africa DHS HR files into one mega HR DHS file: 197.96 sec elapsed   
  df <- df %>%
          mutate(SurveyYear         = as.numeric(SurveyYear),
                 altitude_m         = hv040,
                 language_survey    = hv045a,
                 language_interview = hv045b,
                 language_native    = hv045c,
                 water_scarce       = hv201a,
                 own_land           = hv244,
                 land_ha            = hv245,
                 livestock          = hv246,
                 water_source       = hv235
                 )
            

  sum(is.na(df$DHSID))
  # 0
  
  out_path      = file.path(data_external_temp,"DHS","HR","merged")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)
  
  saveRDS(df,
          file = file.path(data_external_temp,"DHS","HR","merged","africa_DHS_HR.rds"))
  
