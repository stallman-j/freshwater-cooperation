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
    naniar,
    sjlabelled,
    expss,
    xlsx,
    DHS.rates,
    stringr,
    haven,
    tictoc # timing
  )
  

  source(file.path(code_startup_general,"CHMORT5.R")) # source fixed functions
  source(file.path(code_startup_general,"chmort_fix.R")) # 

  
# parameters ----
  
  dhs_gps_filename <- "UGGE7AFL"
  dhs_br_filename  <- "UGBR7BSV"
  level <- 2 # GADM level
  
# DHS Data ----
   
   # Walkthrough 
   # https://docs.ropensci.org/rdhs/articles/introduction.html
   

  
  path <- file.path(data_external_raw,"DHS",paste0(dhs_br_filename,".rds"))
  
  BRdata <- readRDS(path)
  
  path <- file.path(data_external_temp,"DHS","GPS",
                          paste0(dhs_gps_filename,"_GADM_ADM_",level,".rds"))
  
  GPS_data <- readRDS(file = path) %>%
              select(-c(VARNAME_2,NL_NAME_1,NL_NAME_2))
  
  
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
  #
  
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
    mutate(prev_bint = set_label(prev_bint, label = "Preceding birth interval"))   %>%
    mutate(birth_size =
             case_when(
               m18 >= 4 & m18 <= 5 ~ 1,
               m18 <= 3 ~ 2,
               m18 > 5 ~ 99)) %>%
    mutate(birth_size = set_label(birth_size, label = "Birth size"))
  
  BRdata[["prev_bint"]] <- ifelse(is.na(BRdata[["prev_bint"]]), 999, BRdata[["prev_bint"]])
  BRdata[["birth_size"]] <- ifelse(is.na(BRdata[["birth_size"]]), 999, BRdata[["birth_size"]])
  
  BRdata <- BRdata %>%
    mutate(prev_bint = factor(prev_bint, levels = c(1,2,3,4,999), labels = c("Previous birth interval <2 years", "Previous birth interval 2 years", "Previous birth interval 3 years","Previous birth interval 4+ years", "missing"))) %>%
    mutate(birth_size = factor(birth_size, levels = c(1,2,99,999), labels = c("Birth size: Small/very small","Birth size: Average or larger", "Birth size: Don't know/missing", "missing" )))
  
  
  ##################################################################################
  # MORTALITY RATES ################################################################
  ##################################################################################
  
  # v001: cluster number, what will be matched to the GPS data
  
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
                              "v190", # wealth index combined
                              "child_sex", # sex of child
                              "mo_age_at_birth", # mother's age at birth
                              "birth_order", # birth order
                              "prev_bint", # previous birth interval, categorical
                              "birth_size")]) # birth size, categorical
  
  
  BRdata_CMORT <- left_join(BRdata_CMORT,GPS_data,
                      by = c("v001"= "DHSCLUST") ) %>%
                      filter(!is.na(vector_cast_id)) %>%
                      mutate(test_factor = case_when(vector_cast_id < 210 ~ "A",
                                                     vector_cast_id > 210 ~ "B"))
  
  #%>% # take out the ones that failed to merge
                      #filter(vector_cast_id == 1 | vector_cast_id == 2 )
  # NNMR, PNNMR, IMR, CMR & U5MR
  # TABLES 8.1, 8.2 and 8.3
  
  # Different datasets for period ends: 5-9 and 10-14 (0-4 years before survey data, 5-9 yrs before, 10-14 yrs before)
  
  months <- 0:35 # we'll do a moving average by month, of the prior 3 years, from 0 to 35 months before the survey
  # by cluster

  library(lubridate)
  
  #i <- months[1]
  
  temp_data <- BRdata_CMORT
  
  survey_end_year <- as.integer((mean(temp_data$v008) - 1)/12)+1900
  
  survey_end_m    <- round(mean(temp_data$v008) - (((as.integer((mean(temp_data$v008) - 1)/12)+1900) - 1900) * 12),0)
  
  survey_end_date <- ymd(paste(survey_end_year,survey_end_m,01,sep = "-"))
  # initialize data frame
  columns <- c("Class","R","N","WN","date")
  df <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(df) = columns
  
  df
  
  
  #i <- 0
  

  period_length <- 37
  
  tic(paste0("Getting monthly moving averages of 5 measures of child mortality going back ",length(months)," months."))
  
 #for (period_length in c(1,37)){
  for (i in months){
    
  # get a new ending period
  period_end <- survey_end_date - months(i)
  
  
  # childhood 
  temp_df <- data.frame(chmort_fix(temp_data,
                                  Class = "vector_cast_id", # get results by geographic cluster
                                  Period = period_length, 
                                  PeriodEnd = period_end),
                           date = period_end)
  
  
  df <- rbind(df,temp_df)
  
  }
  toc()
  
  # 
  df_tmp <- df %>% rename(vector_cast_id = "Class") %>% mutate(vector_cast_id = as.integer(vector_cast_id))
  
  
  df_tmp$mort_measure <- base::rep(c("NNMR","PNNMR","IMR","CMR","U5MR"),length.out = nrow(df) )
  
  # we need to get the mortality measures to be columns rather than rows
  df_tmp2 <- df_tmp %>%
            pivot_wider(names_from = mort_measure,
                        values_from = c(R,N,WN))
  
  #new_df <- df_tmp2 %>% left_join(GPS_data, by = c("vector_cast_id"="vector_cast_id"))
  
  
  # merge back on to the GPS data 
  
  
  path <- file.path(data_external_clean,"DHS")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  
  saveRDS(df_tmp2,
          file= file.path(data_external_clean,"DHS",
                          paste0(dhs_br_filename,"_child_mortality","_GADM_ADM_",level,".rds")))
          
#  }
  
  
 