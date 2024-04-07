# _______________________________#
# Environment
# Clean 02: IRCC international river ba sin conflict cooperation
# 
# Stallman
# Started 2023-05-17
# Edited: 2023-09-21
#________________________________#

# Startup

  rm(list = ls())




# paths ----

  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))

# packages ----
  
  if (!require(foreign)) install.packages("foreign")
  if (!require("lubridate")) install.packages("lubridate")
  if (!require("countrycode")) install.packages("countrycode")
  
  library(foreign) # for dealing with dta filetypes
  library(lubridate) # fancy dates package
  library(countrycode) # for converting countrycode names from one system to another. IRCC uses the correlates of war (COW) codes
    # p.11 in
  # https://ethz.ch/content/dam/ethz/special-interest/gess/cis/international-relations-dam/Publications/Data/2011_2012/IRCC_Kalbhenn_Bernauer_2012.pdf
  
  data(codelist) # from countrycode package
   
  
# parameters ----
  current_continent <- current_continent # defined in 00_startup_master.R
  
  
# bring in data ----
  
  data_path <- file.path(data_raw,"IRCC_international-river-basin-conflict-and-cooperation","IRCCreplication.csv")
  #world                         <- readRDS(file.path(data_clean,"world.rds"))
  

  codelist_subset <- codelist[,c("cowc","cown","iso3c","continent")]
  
  ircc_raw <- read.csv(file = data_path,
                              header = TRUE) %>%
              left_join(codelist_subset, by = c("ccode1"="cown"),
                        ) %>%
              rename(cowc1 = cowc,
                     iso3c1 = iso3c,
                     continent1 = continent) %>%
              left_join(codelist_subset, by = c("ccode2"="cown")) %>%
              rename(cowc2 = cowc,
                     iso3c2 = iso3c,
                     continent2 = continent)
  
  ircc_raw %>% select(country1,country2,ccode1,cowc1,ccode2,cowc2) %>% distinct() %>% view()
  
  ircc_raw_continent <- ircc_raw %>%
                        filter(continent1 == current_continent | continent2 == current_continent) # get just the current continent's events
                        #filter(!is.na(ircc)) # some event has actually occurred
  # for Africa, drops obs from 15965 to 5703 if we don't filter the is.na ircc () to 2069
  
  # check if this is an incomplete dataset by comparing with stata version
  
  #data_path <- file.path(data_raw,"IRCC_international-river-basin-conflict-and-cooperation","IRCCreplication.dta")
  #ircc_dta_raw <- read.dta(data_path)
  


# save the full version ----
  path <- file.path(data_temp,"IRCC")
  
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  saveRDS(ircc_raw_continent,
          file = file.path(path,paste0("ircc",current_continent,".rds")))
  
  ircc_raw_continent %>%
    filter(ircc > 0) %>%
    saveRDS(file = file.path(path,paste0("ircc_positive_",current_continent,".rds")))
  

  
  
  
  ircc_raw_continent %>%
    filter(ircc == 0) %>%
    saveRDS(file = file.path(path,paste0("ircc_neutral_",current_continent,".rds")))
  
  
  
  saveRDS(ircc_raw,
          file = file.path(path,"ircc.rds"))
  
  path <- file.path(data_temp,"laborious","IRCC")
  
  ircc_negative <- ircc_raw_continent %>%
    filter(ircc <0)
  

  ircc_negative <- save_rds_csv(data = ircc_negative,
                                output_path = path,
                                output_filename = paste0("ircc_negative_",current_continent,".rds"),
                                remove = FALSE,
                                format = "xlsx"
  )
  
  
  ircc_raw_continent <- save_rds_csv(data = ircc_raw_continent,
                                     output_path = path,
                                     output_filename = paste0("ircc_",current_continent,".rds"),
                                     format = "csv",
                                     remove = FALSE)
  
  

# get the geographies we want ----
  
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

  
 
  