# _______________________________#
# Environment
# Clean 02: DHS Child Mortality Births Recode
# 
# Stallman
# Started: 2023-10-23
# Last edited: 2024-04-23
#________________________________#


# https://dhsprogram.com/data/Guide-to-DHS-Statistics/Adult_Mortality_Rates.htm

# Startup

  rm(list = ls())

  # Only run this the first time, otherwise start from "cleaning!"
  
# bring in the packages, folders, paths ----
  
  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))
  
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
  br_datasets_africa <- readRDS(file= file.path(data_external_clean,"DHS","datasets-for-selection",
                                                paste0("br_datasets_africa.rds")))
  # merge DHS Births Recode
  
  # see documentation at  
  # dhsprogram.com/pubs/pdf/SAR7/SAR7.pdf
  
  # country codes 
  #  https://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10136
  
  # UGGE7AFL and #UGGC7BFL
  
  dhs_br_filenames <- stringr::str_extract(br_datasets_africa$FileName,
                                           "[^.]+")
  
  
  dhs_br_in_path <- file.path(data_external_raw,"DHS")
  dhs_br_filenames_rds <- paste0(dhs_br_filenames,".rds")
  
  dhs_br_in <- file.path(dhs_br_in_path,dhs_br_filenames_rds)
  
  
  # bring in one dataframe
  df <- readRDS(dhs_br_in[1]) %>%
    mutate(SurveyYear = br_datasets_africa$SurveyYear[1],
           DHS_CountryCode = br_datasets_africa$DHS_CountryCode[1],
           DHSID           = paste0(DHS_CountryCode,SurveyYear,stringr::str_pad(v001, 8, pad = "0"))) %>%
    mutate(year_birth = 1900+as.integer((b3 - 1)/12), # b3: date of birth of the baby in CMC
           year       = year_birth,
           age_at_death_cmc = b3 + b7, # b7: age at death in months of the baby
           year_death = 1900+as.integer((age_at_death_cmc - 1)/12)) %>%
    mutate(month_birth = b3 - 12*(as.integer(((b3 - 1)/12))),
           month_death = age_at_death_cmc - 12*(as.integer(((age_at_death_cmc - 1)/12))),
           date_birth = ymd(paste(year_birth,month_birth,01,sep = "-")),
           date_one_year = date_birth + months(12),
           date_death = case_when(b7>=0 ~ paste(year_death,month_death,01,sep = "-"),
                                  is.na(b7) ~ NA),
           date_death = ymd(date_death),
           share_year_t = (12 - month_birth + 1)/12,
           infant_death = if_else((b7>=0) & (b7<=11),
                                  true = 1,
                                  false = 0,
                                  missing = 0),
           rweight = v005/1000000, # v005: women's individual sample weight
    ) 
  
  df_2 <- df %>%
    mutate(year = year + 1,
           share_year_t = 1 - share_year_t)
  
  
  df <- rbind(df,df_2)
  
  
  keep_vars <- c("DHSID",
                 "caseid", # case id
                 "v002", #household number
                 "v005", # sample weight
                 "b3",   # date of birth of baby CMC
                 "b7",   # age at death (months, imputed)  
                 "year",
                 "year_death",
                 "month_birth",
                 "month_death",
                 "date_birth",
                 "date_death",
                 "share_year_t",
                 "infant_death",
                 "rweight"
  )
  
  
  df <- sjlabelled::remove_all_labels(df)
  
  
  
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
  tic("Merged all Africa DHS BR files into one mega HR DHS file")
  for (i in 2:length(dhs_br_filenames_rds)){
    
    print(paste0("Working on DHS BR file ",dhs_br_in[i],"."))
    
    
    if (!file.exists(dhs_br_in[i])){
      
      print(paste0("DHS BR file ",dhs_br_in[i]," can't be found."))
      
    } else {
      
      temp <- readRDS(dhs_br_in[i])%>%
        mutate(SurveyYear = br_datasets_africa$SurveyYear[i],
               DHS_CountryCode = br_datasets_africa$DHS_CountryCode[i],
               DHSID           = paste0(DHS_CountryCode,SurveyYear,stringr::str_pad(v001, 8, pad = "0"))
               ) %>%
        mutate(year_birth = 1900+as.integer((b3 - 1)/12), # b3: date of birth of the baby in CMC
               year       = year_birth,
               age_at_death_cmc = b3 + b7, # b7: age at death in months of the baby
               year_death = 1900+as.integer((age_at_death_cmc - 1)/12)) %>%
        mutate(month_birth = b3 - 12*(as.integer(((b3 - 1)/12))),
               month_death = age_at_death_cmc - 12*(as.integer(((age_at_death_cmc - 1)/12))),
               date_birth = ymd(paste(year_birth,month_birth,01,sep = "-")),
               date_one_year = date_birth + months(12),
               date_death = case_when(b7>=0 ~ paste(year_death,month_death,01,sep = "-"),
                                      is.na(b7) ~ NA),
               date_death = ymd(date_death),
               share_year_t = (12 - month_birth + 1)/12,
               infant_death = if_else((b7>=0) & (b7<=11),
                                      true = 1,
                                      false = 0,
                                      missing = 0),
               rweight = v005/1000000, # v005: women's individual sample weight
        ) 
      
    
      
      temp <- sjlabelled::remove_all_labels(temp)
      
      
      temp_2 <- temp %>%
        mutate(year = year + 1,
               share_year_t = 1 - share_year_t)
      
      
      new_df <- rbind(temp,temp_2)
      
      # add the column varnames if they didn't exist already
      new_df <- add_na_cols(new_df, keep_vars)
      
      # then keep only these vars
      new_df <- new_df[keep_vars] 
      
      
      df <- rbind(df,new_df)
      
      
    } # end else statement if file exists then read in the DHS BR filename and rbind to temp 
  } # end loop over all DHS filenames
  
  toc()
  
  # Merged all Africa DHS BR files into one mega BR DHS file: 189.36 sec elapsed

  
  out_path      = file.path(data_external_temp,"DHS","BR","merged")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)
  
  
  saveRDS(df,
          file = file.path(data_external_temp,"DHS","BR","merged","africa_DHS_BR.rds"))
  
  df <- readRDS(file = file.path(data_external_temp,"DHS","BR","merged","africa_DHS_BR.rds"))
  

  
  

  # Finished merging BR and GPS Datasets and adding mortality years: 116.08 sec elapsed 
  

    # 
  # currently it's 8 million observations
  nrow(df)
  #[1] 8086890
  
