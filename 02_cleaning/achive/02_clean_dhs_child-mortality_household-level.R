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
  
  #detach("package:DHS.rates", unload = TRUE)
  
  # library(riverdist)
  # data(abstreams)
  
  source(file.path(code_startup_general,"CHMORT10.R")) # source fixed functions
  source(file.path(code_startup_general,"chmort_fix.R")) # 

  
# parameters ----
  
  surveyyear_start <- 2018 %>% as.character()
  surveyyear_end   <- 2020 %>% as.character()
  period_length    <- 13 # use this number of months as the period for child mortality
  level <- 2 # GADM level
  
  # countries_DHS    <- countrycode(country,
  #                                 origin = "iso3c",
  #                                 destination = "dhs")
  # 
  
  countries_DHS <-  dhs_datasets() %>% 
    filter(DatasetType == "GPS Datasets") %>% 
    filter(FileType == "Geographic Data") %>%
    mutate(continent = countrycode(DHS_CountryCode, origin = "dhs",destination = "continent"))%>%
    filter(continent == "Africa") %>%
    select(DHS_CountryCode) %>% 
    filter(DHS_CountryCode!="LB") %>% # something off with Liberia
    filter(DHS_CountryCode!="TG") %>% # something off with Togo
        unique() %>%
    .[,1] # get just the character
  
  # missing LB Liberia
  #countries_DHS <- countries_DHS[c(21:38)]
  
  #country <- "AO"
  gps_datasets_all   <- dhs_datasets() %>% 
    filter(DatasetType == "GPS Datasets") %>% 
    filter(FileType == "Geographic Data") 
  
  #country <- "BJ"
  for (country in countries_DHS) {
  
    
  continent <- countrycode(country,
                           origin = "dhs",
                           destination = "continent")
  
  countryname <- countrycode(country,
                             origin = "dhs",
                             destination = "country.name")
  
  print(paste0("Country is ",country," ie ", countryname))
  
  if (continent!= "Africa") {
    print("Continent not in Africa, move on")
  } else{ # if the continent is africa, move ahead
    
  # get the GPS data first
  gps_datasets   <- dhs_datasets() %>% 
    filter(DatasetType == "GPS Datasets") %>% 
    filter(FileType == "Geographic Data") %>% 
    filter(DHS_CountryCode == country)%>% 
    #filter(SurveyYear >= surveyyear_start & SurveyYear <= surveyyear_end) %>%
    filter(SurveyType == "DHS") %>%
    arrange(DHS_CountryCode,SurveyYear)
  
  # pick out the years the GPS data exists for 
  years <- unique(gps_datasets$SurveyYear)
  
  year <- "2017"
  # now go through and do the exercise for each year
  for (year in years) {
  br_datasets     <- dhs_datasets() %>% 
                      filter(FileType == "Births Recode") %>% 
                      filter(DHS_CountryCode == country) %>%
                      filter(SurveyYear == year) %>%
                      #filter(SurveyYear >= surveyyear_start & SurveyYear <= surveyyear_end) %>%
                      filter(FileFormat == "SPSS dataset (.sav)")%>%
                      arrange(DHS_CountryCode,SurveyYear)
  

  gps_dataset <- gps_datasets %>% filter(SurveyYear == year)
  
  
  dhs_br_filenames  <- stringr::str_extract(br_datasets$FileName,"[^.]+")
  dhs_gps_filenames <- stringr::str_extract(gps_dataset$FileName,"[^.]+")
  
  
  
# DHS Data ----
   
   # Walkthrough 
   # https://docs.ropensci.org/rdhs/articles/introduction.html
   

  i <- 1
  for (i in 1:length(dhs_br_filenames)) {
  
    dhs_br_filename <- dhs_br_filenames[i]
    dhs_gps_filename <- dhs_gps_filenames[i]
    
  path_br <- file.path(data_external_raw,"DHS",paste0(dhs_br_filename,".rds"))
  path_gps <- file.path(data_external_temp,"DHS","GPS",
                        paste0(dhs_gps_filename,"_GADM_ADM_",level,".rds"))
  
  
  if (!file.exists(path_br) | !file.exists(path_gps)){
    
    print("BR or GPS file does not exist")
  
  } else {
  
  BRdata <- readRDS(path_br)
  GPS_data <- readRDS(path_gps)
 
  
  
# adapted from 
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap08_CM/CM_CHILD.R
  
  # -----------------------------------------------------------------------------#
  # # Indicators created in this file:
  # NNMR		"Neonatal Mortality Rate"
  # PNNMR		"Post-neonatal Mortality Rate"
  # IMR			"Infant Mortality Rate"
  # CMR			"Child Mortality Rate"
  # U5MR		"Under-5 Mortality Rate"
  # -----------------------------------------------------------------------------#
  
  
  # important indicators:
  # b3: date of birth
  # b7: age at death
  
  BRdata <- BRdata %>%
    mutate(child_sex = b4) %>%
    mutate(child_sex = set_label(child_sex, label = "Sex of child"))  %>%
    mutate(months_age = b3-v011) %>%
    mutate(mo_age_at_birth =
             case_when(
               months_age < 20*12   ~ 1 ,
               months_age >= 20*12 & months_age < 30*12 ~ 2,
               months_age >= 30*12 & months_age < 40*12 ~ 3,
               months_age >= 40*12 & months_age < 50*12 ~ 4)) %>%
    mutate(mo_age_at_birth = factor(mo_age_at_birth, levels = c(1,2,3,4), labels = c("Mother's age at birth < 20", "Mother's age at birth 20-29", "Mother's age at birth 30-39","Mother's age at birth 40-49"))) %>%
    mutate(mo_age_at_birth = set_label(mo_age_at_birth, label = "Mother's age at birth")) %>% # from sjlabelled
    # set_label: adds varlabels as an attribute named "label" to the variable x
    mutate(birth_order =
             case_when(
               bord == 1  ~ 1,
               bord >= 2 & bord <= 3 ~ 2,
               bord >= 4 & bord <= 6 ~ 3,
               bord >= 7  ~ 4,
               bord == NA ~ 99)) %>%
    replace_with_na(replace = list(birth_order = c(99))) %>%
    mutate(birth_order = factor(birth_order, levels = c(1,2,3,4), labels = c("Birth order:1", "Birth order:2-3", "Birth order:4-6","Birth order:7+"))) %>%
    mutate(birth_order = set_label(birth_order, label = "Birth order"))  %>%
    mutate(prev_bint =
             case_when(
               b11 <= 23 ~ 1,
               b11 >= 24 & b11 <= 35 ~ 2,
               b11 >= 36 & b11 <= 47 ~ 3,
               b11 >= 48 ~ 4)) %>%
    mutate(prev_bint = set_label(prev_bint, label = "Preceding birth interval"))   
  # %>%
  #   mutate(birth_size =
  #            case_when(
  #              m18 >= 4 & m18 <= 5 ~ 1,
  #              m18 <= 3 ~ 2,
  #              m18 > 5 ~ 99)) %>%
  #   mutate(birth_size = set_label(birth_size, label = "Birth size"))
  
  BRdata[["prev_bint"]] <- ifelse(is.na(BRdata[["prev_bint"]]), 999, BRdata[["prev_bint"]])
  #BRdata[["birth_size"]] <- ifelse(is.na(BRdata[["birth_size"]]), 999, BRdata[["birth_size"]])
  
  BRdata <- BRdata %>%
    mutate(prev_bint = factor(prev_bint, levels = c(1,2,3,4,999), labels = c("Previous birth interval <2 years", "Previous birth interval 2 years", "Previous birth interval 3 years","Previous birth interval 4+ years", "missing"))) 
  #%>%
   # mutate(birth_size = factor(birth_size, levels = c(1,2,99,999), labels = c("Birth size: Small/very small","Birth size: Average or larger", "Birth size: Don't know/missing", "missing" )))
  
  
  ##################################################################################
  # MORTALITY RATES ################################################################
  ##################################################################################
  

  BRdata_CMORT <- (BRdata[, c("v001", # cluster number, what's matched to GPS data
                              "v005", # women's individual sample weight
                              "v008", # date of interview CMC
                              "v011", # date of birth CMC
                              "v021", # primary sampling unit
                              "v022", # sample strata
                              "v024", # region
                              "v025", # type of place of residence
                              "b3",   # date of birth of baby CMC
                              "b7",   # age at death (months, imputed)
                              "v106", # highest ed level 
                              #"v190", # wealth index combined, missing often enough that let's omit for now
                              "child_sex", # sex of child
                              "mo_age_at_birth", # mother's age at birth
                              "birth_order", # birth order
                              "prev_bint"#, # previous birth interval, categorical
                              #"birth_size"
                              )]) # birth size, categorical
  
  
  # just checking birth years go through
  # BRdata_CMORT <- BRdata_CMORT %>%
  #                      mutate(child_birth_year = floor(as.integer(b3 - 1)/12+1900 ))
  # 
  
  BRdata_CMORT <- left_join(as.data.frame(BRdata_CMORT),GPS_data,
                      by = c("v001"= "DHSCLUST") ) 
  
  # NNMR, PNNMR, IMR, CMR & U5MR
  # TABLES 8.1, 8.2 and 8.3
  
  # Different datasets for period ends: 5-9 and 10-14 (0-4 years before survey data, 5-9 yrs before, 10-14 yrs before)
  
  years <- 0:4 # take years 0 to 4 before the survey
  # by cluster

  #i <- months[1]
  
  temp_data <- BRdata_CMORT #BRdata_CMORT[sample(1:nrow(BRdata_CMORT),size = 20, replace = FALSE),]
  
  survey_end_year <- as.integer((mean(temp_data$v008) - 1)/12)+1900
  
  survey_end_m    <- round(mean(temp_data$v008) - (((as.integer((mean(temp_data$v008) - 1)/12)+1900) - 1900) * 12),0)
  
  # use the prior year if the survey ends not in December; if the survey ends in december just use the current year as the 
  # most recent survey year
  if (survey_end_m < 12){
    survey_end_year <- survey_end_year - 1
  }
  
  
  
  survey_end_date <- ymd(paste(survey_end_year,12,31,sep = "-"))
  
  
  # initialize data frame
  columns <- c("Class","R","N","WN","date")
  df <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(df) = columns
  
  df
  
  
  #i <- 0
  
  tic(paste0("Getting annual measures of child mortality going back ",length(years)," years"))
  
  for (i in years){
    
  # get a new ending period
  period_end <- survey_end_date - years(i)
  period_end_year <- year(period_end)
  
  print(paste0("New period end is ",period_end))
  

  # Data.Name = temp_data
  # JK = NULL
  # CL = NULL
  # Strata = NULL
  # Cluster = NULL
  # Weight = NULL
  # Date_of_interview = NULL
  # Date_of_birth = NULL
  # Age_at_death = NULL
  # PeriodEnd = period_end
  # Period = period_length
  # Class = "v001"
  
  temp_df <- data.frame(chmort_fix(temp_data,
                                  Class = "v001", # get results by geographic cluster
                                  Period = period_length,
                                  PeriodEnd = period_end),
                           end_date       = period_end,
                           end_year       = period_end_year,
                           period_months  = period_length)
  
  
  # numbers work out much better here
  # temp_df <- data.frame(chmort_fix(temp_data,
  #                                  Class = "URBAN_RURA", # get results by geographic cluster
  #                                  Period = period_length, 
  #                                  PeriodEnd = period_end),
  #                       end_date       = period_end,
  #                       end_year       = period_end_year,
  #                       period_months  = period_length)
  
  
  df <- rbind(df,temp_df)
  
  }
  toc()
  
  # 
  df_tmp <- df %>% rename(DHSCLUST = "Class") %>% mutate(DHSCLUST = as.integer(DHSCLUST))
  
  
  #df_tmp <- df %>% rename(URBAN_RURA = "Class")
  
  df_tmp$mort_measure <- base::rep(c("NNMR","PNNMR","IMR","U5MR","U10MR"),length.out = nrow(df) )
  
  # we need to get the mortality measures to be columns rather than rows
  df_tmp2 <- df_tmp %>%
            pivot_wider(names_from = mort_measure,
                        values_from = c(R,N,WN))
  
  new_df <- df_tmp2 %>% left_join(GPS_data, by = c("DHSCLUST"="DHSCLUST"))
  
  #new_df %>% filter(DHSCLUST == 5) %>% view()
  # merge back on to the GPS data 
  
  
  path <- file.path(data_external_clean,"DHS")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  
  saveRDS(new_df,
          file= file.path(data_external_clean,"DHS",
                          paste0(dhs_br_filename,"_",period_length,"_months_window_child_mortality_with_GPS.rds")))
          
} # end IFELSE statement about making the continent be Africa
  } # end FOR loop over mortality years range 
  
  } # end IFELSE statement about file existing
  
  } # end FOR loop over DHS datasets years
  
  
  
  } # end FOR loop over DHS countries
  
 # 3:08pm at number 27; 37 at 3:18; 52 at 3:23 about a minute per, times 243 gives about 4 hours
 # finished all of them at 60 months and 13 months by 6:25pm 
  
# merge together all the mortality datasets ----
  
  countries_DHS <-  dhs_datasets() %>% 
    filter(DatasetType == "GPS Datasets") %>% 
    filter(FileType == "Geographic Data") %>%
    mutate(continent = countrycode(DHS_CountryCode, origin = "dhs",destination = "continent"))%>%
    filter(continent == "Africa") %>%
    select(DHS_CountryCode) %>% 
    filter(DHS_CountryCode!="LB") %>% # something off with Liberia
    filter(DHS_CountryCode!="TG") %>% # something off with Togo
    unique() %>%
    .[,1] # get just the character
  
  # missing LB Liberia
  #countries_DHS <- countries_DHS[c(21:38)]
  
  #country <- "AO"
  gps_datasets_all   <- dhs_datasets() %>% 
    filter(DatasetType == "GPS Datasets") %>% 
    filter(FileType == "Geographic Data") 
  
## bring in a dataframe ----
  
  period_length <- 60 # 13
  
  data <- readRDS(
          file= file.path(data_external_clean,"DHS",
                          paste0("AOBR71SV","_",period_length,"_months_window_child_mortality_with_GPS.rds")))
  
  
  #country <- "BJ"
  for (country in countries_DHS) {
    
    
    continent <- countrycode(country,
                             origin = "dhs",
                             destination = "continent")
    
    countryname <- countrycode(country,
                               origin = "dhs",
                               destination = "country.name")
    
    print(paste0("Country is ",country," ie ", countryname))
    
    if (continent!= "Africa") {
      print("Continent not in Africa, move on")
    } else{ # if the continent is africa, move ahead
      
      # get the GPS data first
      gps_datasets   <- dhs_datasets() %>% 
        filter(DatasetType == "GPS Datasets") %>% 
        filter(FileType == "Geographic Data") %>% 
        filter(DHS_CountryCode == country)%>% 
        #filter(SurveyYear >= surveyyear_start & SurveyYear <= surveyyear_end) %>%
        filter(SurveyType == "DHS") %>%
        arrange(DHS_CountryCode,SurveyYear)
      
      # pick out the years the GPS data exists for 
      years <- unique(gps_datasets$SurveyYear)
      
      # now go through and do the exercise for each year
      for (year in years) {
        br_datasets     <- dhs_datasets() %>% 
          filter(FileType == "Births Recode") %>% 
          filter(DHS_CountryCode == country) %>%
          filter(SurveyYear == year) %>%
          #filter(SurveyYear >= surveyyear_start & SurveyYear <= surveyyear_end) %>%
          filter(FileFormat == "SPSS dataset (.sav)")%>%
          arrange(DHS_CountryCode,SurveyYear)
        
        
        gps_dataset <- gps_datasets %>% filter(SurveyYear == year)
        
        
        dhs_br_filenames  <- stringr::str_extract(br_datasets$FileName,"[^.]+")
        
        
        for (i in 1:length(dhs_br_filenames)) {
          
          dhs_br_filename <- dhs_br_filenames[i]

          path_br <- file.path(data_external_clean,"DHS",
                                     paste0(dhs_br_filename,"_",period_length,"_months_window_child_mortality_with_GPS.rds"))
          
          if (!file.exists(path_br)){
            
            print(paste0("Mortality file ",dhs_br_filename," does not exist. Check your work."))
            
            data <- data
          } else {
            
            temp_df <- readRDS(
              file= file.path(data_external_clean,"DHS",
                              paste0(dhs_br_filename,"_",period_length,"_months_window_child_mortality_with_GPS.rds")))
            
            
          data <- rbind(data,temp_df)
            
          } #end ifelse read in mortality data
          
        } # end FOR loop over DHS BR filenames 
        
      } # end loop over years 
      
    } # end ifelse to get countries in just Africa, not other continents 
    
    
  } # end loop over countries
  
  data <- data %>%
    st_as_sf(crs = 4326) %>%
    st_transform(crs = equal_area_crs)
  
  # TO CHECK THIS NEXT TIME
  data <- data %>%
          rename(year = end_year) 
    
  
  path <- file.path(data_external_clean,"merged")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  saveRDS(data,
          file= file.path(data_external_clean,"merged",
                          paste0("Africa_all_years_DHS_",period_length,"_months_window_child_mortality_with_GPS.rds")))
  
