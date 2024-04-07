# _______________________________#
# Environment
# Clean 02: clean, transboundary waters events
# 
# Stallman
# Started 2023-04-05
# Last edited: 2023-09-21
#________________________________#


# Startup

#rm(list = ls())

home_folder <- file.path("P:","Projects","environment")

source(file.path(home_folder,"code","00_startup_master.R"))

# parameters ----

start_year <- start_year # earliest date, defined in 00_startup_master.R
current_continent <- current_continent # defined in 00_startup_master.R
  
#packages ----

if (!require("readxl")) install.packages("readxl")
if (!require("lubridate")) install.packages("lubridate")
if (!require("dplyr")) install.packages("dplyr")
  if (!require("countrycode")) install.packages("countrycode")
  
  library(dplyr)     # data wrangling
  library(readxl)  # read in excel format, use for read_xlsx
  library(lubridate) # fancy dates package
  library(countrycode) # for converting countrycode names from one system to another. transboundary waters uses the FAO-GAUL list
  # https://data.apps.fao.org/catalog/dataset/gaul-code-list-global-admin-2
  
  data(codelist)
  # codelist_panel_2019 <- codelist_panel %>%
  #   filter(year == 2019)
  

# read in files ----

  water_events <- read_xls(path = file.path(data_raw,
                                            "transboundary-waters",
                                            "EventMaster111710.xls"),
                           sheet = "EventMaster2",
                           col_names = TRUE,
                           )
  
  # create the sf_countries dataset (if not already created)
  source(file.path(code_clean,"02_clean_01_rnaturalearth_country-continent-shapefiles.R"))
  
  
  path <- file.path(data_clean,"shape-files",paste0("sf_countries_",current_continent,".rds"))
  
  sf_countries <- readRDS(file = path)
  
# cleaning ----
  
  ## set dates ----
  
  ymd <- ymd(water_events$DATE)
  
  # find number of dates missing
  missing_date <- is.na(ymd) 
  
  sum(missing_date)
  
  # 18
  # deal with later, just drop for now
  
 ## drop if date is missing ----
  

  
  water_events_tmp <- water_events %>%
                        filter(!is.na(DATE))
  
  # filter to be after 1950 when there's hope of having other related data ----
  
  # format as date just to make sure
  
  water_events_tmp$DATE <-as.Date(water_events_tmp$DATE,"%y-%m-%d")
  
  
  # UNIQUE_ID is the number of unique events, not dyads
  length(unique(water_events_tmp$UNIQUE_ID)) # 
  # 3913
  
  # ID is the number of rows ie links
  
  length(unique(water_events_tmp$ID))
  
  # 7110
  
# clean for current analysis ----
  
  water_events <- water_events_tmp %>%
    mutate(year = lubridate::year(DATE),
           month = lubridate::month(DATE),
           day   = lubridate::day(DATE)) %>%
    filter(year >= start_year)
  
  water_events_positive <- water_events %>%
                          filter(BAR_Scale>0)
  
  
  # Post 1990:
  # 3227 links
  # 1269 events
  
  length(unique(water_events_positive$UNIQUE_ID))

  
  water_events_negative <- water_events %>%
    filter(BAR_Scale<0)
  # 885 links post 1990
  
  length(unique(water_events_negative$UNIQUE_ID))
  # 653 events post 1990
  
  water_events_neutral <- water_events %>%
    filter(BAR_Scale == 0)
  
  # 221 links post 1990
  length(unique(water_events_neutral$UNIQUE_ID))
  # 130 events post 1990
  
  # # for fixing up strings if desired
  # https://sparkbyexamples.com/r-programming/r-str_replace-to-replace-matched-patterns-in-a-string/
  # if (!require("stringr")) install.packages("stringr")
  # 
  # library(stringr) # for string, replace one string with another
  # 
  # water_treaties_tmp$Signatories2 <- str_replace(water_treaties_tmp$Signatories,"")
  
  path <- file.path(data_clean,"TFDD")
  
  # if path doesn't exist, create the folders. recursive = TRUE lets you create any needed sub-directories
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) 
  

# save ----
  
  # ifdd stands for international freshwater dispute database
  
  saveRDS(water_events_positive, 
          file = file.path(path,"ifdd_events_positive.rds"))
  
  saveRDS(water_events_negative, 
          file = file.path(path,"ifdd_events_negative.rds"))

  saveRDS(water_events,
          file = file.path(path,"ifdd_events.rds"))

# keep if both the countries are located in the current continent ----
  
  # list of countries involved
  # examine whether the country codes are getting what we want
  # countries_names <- sf_countries[,c("name","sov_a3","adm0_a3","gu_a3","su_a3")]
  # water_events_countries <- water_events[,c("CCODE1","CCODE2","COUNTRY_LIST")] %>%
  #                           distinct(CCODE1,.keep_all = TRUE)
  # 
  # 
  # merged_countries <- full_join(x = countries_names,
  #                               y = water_events_countries,
  #                               by = c("sov_a3"="CCODE2"),
  #                               keep = TRUE)
  
  
  # not the Egypt-Isreal stuff
  water_events_country <- water_events %>%
                          filter(CCODE1 %in% sf_countries$sov_a3 | CCODE2 %in% sf_countries$sov_a3) %>%
                          filter(CCODE1 != "ISR" & CCODE2 !="ISR")  # take out the egypt-isreal stuff
  
  # save positive, negative and neutral
  
  
  water_events_country %>%
    filter(BAR_Scale > 0) %>%
    saveRDS(file = file.path(path,paste0("ifdd_events_positive_",current_continent,".rds")))
  
  water_events_country %>%
    filter(BAR_Scale == 0) %>%
    saveRDS(file = file.path(path,paste0("ifdd_events_neutral_",current_continent,".rds")))
  
  water_events_negative <- water_events_country %>%
                          filter(BAR_Scale <0)
  
  path <- file.path(data_external_temp,"laborious","transboundary-water")
  
  water_events_negative <- save_rds_csv(data = water_events_negative,
               output_path = path,
               output_filename = paste0("ifdd_events_negative_",current_continent,".rds"),
               remove = FALSE,
               format = "xlsx"
               )
  
# after going through the negative events to see which have reneging on a SEA, bring that back in ----
  
  water_negative <- read_xlsx(path = file.path(data_external_temp,"laborious",
                                            "transboundary-water",
                                            paste0("ifdd_events_negative_",current_continent,"_edited.xlsx")),
                           sheet = "data",
                           col_names = TRUE,
  )%>%
    filter(sea_renege == "Y" ) # #| sea_renege == "M")
  

  water_negative <- save_rds_csv(data = water_negative,
                 output_path = file.path(data_clean,"TFDD"),
                 output_filename = paste0("ifdd_events_negative_",current_continent,"_clean.rds"),
                 remove = FALSE,
                 format = "xlsx"
    )
  
  

  
  
  # 
  # water_events_country <-  left_join(countries_ia_centroids,codelist_panel_2019,
  #                                    by = c("country"="country.name.en"),
  #                                    keep = TRUE)

# take a look at a few things ----
  
  positive_after_1990 <- water_events_positive %>% filter(year >= 1990)
  
  
  hist(water_events_positive$year)
  
  hist(positive_after_1990$year)
  
  
  