# _______________________________#
# Environment
# Clean 02: DHS Child Mortality
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
  
  level <- 2 # GADM level
  equal_area_crs   <- "ESRI:102022" # equal-area CRS for Africa, need it for calculating on-map distances 
  
# bring in common data ----
  
  countries_DHS <- readRDS(
                           file= file.path(data_external_clean,"DHS","datasets-for-selection",
                                           paste0("countries_DHS.rds")))
  
  gps_datasets_all <- readRDS(
          file= file.path(data_external_clean,"DHS","datasets-for-selection",
                          paste0("gps_datasets_all.rds")))
  
  
  #country <- "BJ"
  tic("Finished merging BR and GPS Datasets and adding mortality years")
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
  
  #year <- "2017"
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
   

  #i <- 1
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
                              "v002", # household number
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
  
  
  #i <- months[1]
  
   #set.seed(3)
   # temp_data <- BRdata_CMORT %>%
   # slice_sample(n = 10, replace = FALSE)

   temp_data <- BRdata_CMORT %>%
                mutate(year_birth = 1900+as.integer((b3 - 1)/12), # b3: date of birth of the baby in CMC
                       year       = year_birth,
                       age_at_death_cmc = b3 + b7, # b7: age at death in months of the baby
                       age_at_death_cmc = set_label(age_at_death_cmc, label = "Age at death (CMC), imputed"),
                       year_death = 1900+as.integer((age_at_death_cmc - 1)/12)) %>%
                mutate(month_birth = b3 - 12*(as.integer(((b3 - 1)/12))),
                       month_birth = set_label(month_birth, label = "Month of birth"),
                       month_death = age_at_death_cmc - 12*(as.integer(((age_at_death_cmc - 1)/12))),
                       month_death = set_label(month_death, label = "Month of death, imputed"),
                       date_birth = ymd(paste(year_birth,month_birth,01,sep = "-")),
                       date_one_year = date_birth + months(12),
                       date_death = case_when(b7>=0 ~ paste(year_death,month_death,01,sep = "-"),
                                              is.na(b7) ~ NA),
                       date_death = ymd(date_death),
                       share_year_t = (12 - month_birth + 1)/12,
                       share_year_t = set_label(share_year_t, label = "Share of first year of life falling in year t"),
                       infant_death = if_else((b7>=0) & (b7<=11),
                                              true = 1,
                                              false = 0,
                                              missing = 0),
                       infant_death = set_label(infant_death,label = "=1 if child died b/w 0-11 months, else 0"),
                       rweight = v005/1000000, # v005: women's individual sample weight
                       rweight = set_label(rweight,label = "women's individual sample weight")
                       ) %>%
                select(v001, # cluster number
                       v002,# household number, Doesn't exist for Benin
                       #v004, # ultimate area unit
                       v005, # women's individual sample weight, 6 decimals
                       v021, # primary sampling unit
                       v022, # sample strata
                       v024, # region
                       v025, # type of place of residence
                       b3, # age of birth in CMC
                       b7, # age of death in months of the child
                       rweight,year_birth,year,age_at_death_cmc,year_death,month_birth,month_death,date_death,date_birth,date_one_year,
                       share_year_t,infant_death)
   
   # labels were getting annoying
   temp_data <- remove_all_labels(temp_data)

   temp_2 <- temp_data %>%
             mutate(year = year + 1,
                    share_year_t = 1 - share_year_t)
   
   
   new_df <- rbind(temp_data,temp_2) %>%
                left_join(GPS_data, by = c("v001"="DHSCLUST"), keep = TRUE) 

  path <- file.path(data_external_clean,"DHS")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  
  saveRDS(new_df,
          file= file.path(data_external_clean,"DHS",
                          paste0(dhs_br_filename,"_HH_infant_mortality_with_GPS.rds")))
          
}  # end IFELSE statement about file BR or GPS file existing 
 
  } # end FOR loop over DHS BR filenames
  
  }  # end FOR loop over DHS datasets years
  
  } # end IFELSE statement about making the continent be Africa 
  
  
  
  } # end FOR loop over DHS countries
  
  toc()

  # Finished merging BR and GPS Datasets and adding mortality years: 116.08 sec elapsed 
  
# merge together all the mortality datasets ----
  
## bring in one of these dataframes ----
  
  data <- readRDS(
          file= file.path(data_external_clean,"DHS",
                          paste0("AOBR71SV_HH_infant_mortality_with_GPS.rds")))
  
  
  #country <- "BJ"
  #
  tic("Merged all countries with GPS and BR together")
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
                               paste0(dhs_br_filename,"_HH_infant_mortality_with_GPS.rds"))
          
          if (!file.exists(path_br)){
            
            print(paste0("Mortality file ",dhs_br_filename," does not exist. Check your work."))
            
            data <- data
          } else {
            
            temp_df <- readRDS(
              file= file.path(data_external_clean,"DHS",
                              paste0(dhs_br_filename,"_HH_infant_mortality_with_GPS.rds")))
            
            
          data <- rbind(data,temp_df)
            
          } #end ifelse read in mortality data
          
        } # end FOR loop over DHS BR filenames 
        
      } # end loop over years 
      
    } # end ifelse to get countries in just Africa, not other continents 
    
    
  } # end loop over countries
  
  toc()
  
  # TO CHECK:
  # [1] "Mortality file GHBR8ASV does not exist. Check your work."
  # [1] "Mortality file KEBR8BSV does not exist. Check your work."
  # [1] "Mortality file TZBR82SV does not exist. Check your work."
  # 
  #Merged all countries with GPS and BR together: 615.72 sec elapsed

    # 
  # currently it's 8 million observations
  nrow(data)
  #[1] 8086890
  
  tic("Transformed CRS")
  data <- data %>%
    st_as_sf(crs = 4326)  %>%
    st_transform(crs = equal_area_crs)
    
  toc()
  
  # Transformed CRS: 25.28 sec elapsed

  # this one 
  
  path <- file.path(data_external_clean,"merged")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  saveRDS(data,
          file= file.path(data_external_clean,"merged",
                          paste0("Africa_all_years_DHS_HH_infant_mortality_with_GPS.rds")))

# explore a little ----
# 
  data <- readRDS(file= file.path(data_external_clean,"merged",
                                  paste0("Africa_all_years_DHS_HH_infant_mortality_with_GPS.rds")))
  
