# _______________________________#
# Environment
# Clean 02: Choose DHS Countries and GPS Datasets
# 
# Stallman
# Started: 2023-10-23
# Last edited: 2024-01-24
#________________________________#


# https://dhsprogram.com/data/Guide-to-DHS-Statistics/Adult_Mortality_Rates.htm

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

# select countries ----

countries_DHS_africa <-  dhs_datasets() %>% 
  filter(DatasetType == "GPS Datasets") %>% 
  filter(FileType == "Geographic Data") %>%
  filter(SurveyType != "SPA") %>% #SPA doesn't have DHSID as a column
  mutate(continent = countrycode(DHS_CountryCode, origin = "dhs",destination = "continent"))%>%
  filter(continent == "Africa") %>%
  select(DHS_CountryCode) %>% 
  filter(DHS_CountryCode!="LB") %>% # something off with Liberia
  filter(DHS_CountryCode!="TG") %>% # something off with Togo
  unique() %>%
  .[,1] # get just the character

# 36 unique countries in Africa
# 
countries_DHS_all <-  dhs_datasets() %>% 
  filter(DatasetType == "GPS Datasets") %>% 
  filter(FileType == "Geographic Data") %>%
  filter(SurveyType != "SPA") %>% #SPA doesn't have DHSID as a column
  select(DHS_CountryCode) %>% 
  filter(DHS_CountryCode!="LB") %>% # something off with Liberia
  filter(DHS_CountryCode!="TG") %>% # something off with Togo
  unique() %>%
  .[,1] # get just the character

# 60 DHS countries total


#country <- "AO"
gps_datasets_all   <- dhs_datasets() %>% 
  filter(DatasetType == "GPS Datasets") %>% 
  filter(FileType == "Geographic Data")  %>%
  filter(SurveyType != "SPA") 
  

# 227 GPS datasets overall

# generate Africa GPS datasets

gps_datasets_africa <- gps_datasets_all %>%
                       filter(DHS_CountryCode %in% countries_DHS_africa)

# 158 GPS datasets for Africa
# 
hr_datasets_africa   <- dhs_datasets() %>%
                      filter(DHS_CountryCode %in% countries_DHS_africa) %>% 
                      filter(DatasetType != "GPS Datasets") %>%
                      filter(FileType == "Household Recode") %>% # "Household Member Recode" makes the DF longer: each HH member is a row
                      filter(FileFormat == "SPSS dataset (.sav)")


ir_datasets_africa   <- dhs_datasets() %>%
  filter(DHS_CountryCode %in% countries_DHS_africa) %>% 
  filter(DatasetType != "GPS Datasets") %>%
  filter(FileType == "Individual Recode") %>% # "Household Member Recode" makes the DF longer: each HH member is a row
  filter(FileFormat == "SPSS dataset (.sav)")


# 227 GPS datasets overall

# generate Africa GPS datasets

gps_datasets_africa <- gps_datasets_all %>%
  filter(DHS_CountryCode %in% countries_DHS_africa)



path <- file.path(data_external_clean,"DHS","datasets-for-selection")
if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories




saveRDS(countries_DHS_all,
        file= file.path(data_external_clean,"DHS","datasets-for-selection",
                        paste0("countries_DHS_all.rds")))

saveRDS(countries_DHS_africa,
        file= file.path(data_external_clean,"DHS","datasets-for-selection",
                        paste0("countries_DHS_africa.rds")))

saveRDS(hr_datasets_africa,
        file= file.path(data_external_clean,"DHS","datasets-for-selection",
                        paste0("hr_datasets_africa.rds")))

saveRDS(ir_datasets_africa,
        file= file.path(data_external_clean,"DHS","datasets-for-selection",
                        paste0("ir_datasets_africa.rds")))


saveRDS(gps_datasets_all,
        file= file.path(data_external_clean,"DHS","datasets-for-selection",
                        paste0("gps_datasets_all.rds")))

saveRDS(gps_datasets_africa,
        file= file.path(data_external_clean,"DHS","datasets-for-selection",
                        paste0("gps_datasets_africa.rds")))

