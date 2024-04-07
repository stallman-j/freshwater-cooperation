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

# packages ----

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
# parameters ----

  start_year <- start_year # earliest date, defined in 02_clean_transboundary-waters-events.R
  end_year   <- end_year # ditto
  current_continent <- current_continent # defined in 00_startup_master.R
  

# bring in data ----

  path <- file.path(data_clean,"shape-files",paste0("sf_countries_",current_continent,".rds"))
  sf_countries <- readRDS(file = path)
  
  path <- file.path(data_clean,"TFDD",paste0("ifdd_events_negative_",current_continent,"_clean.rds"))
  water_negative <- readRDS(file = path) %>%
                  filter(CCODE1 %in% sf_countries$sov_a3 & CCODE2 %in% sf_countries$sov_a3)
  
  
  path <- file.path(data_clean,"CRU_TS",paste0("CRU_precip_",current_continent,".rds"))
  cruts <- readRDS(path) %>%
            mutate(year = as.numeric(year))

# create a df of all pairwise interactions and possible years ----
  
  pairwise <- water_negative %>% dplyr::select(CCODE1,CCODE2) %>% distinct()
  
  # create a set of years that we'll merge with the pairwise codes
  time_range <- paste0(c(start_year,end_year),"-01-01")
  
  years <- seq(from = ymd(head(time_range,n=1)),
               to   = ymd(tail(time_range,n=1)),by = "1 year") %>%
    year() # pull out just the year
  
# merge years onto pairwise ----
  
  pairwise_years <- water_negative %>% expand(DYAD_CODE,year) 
  
  # add in the events
  events_years <- dplyr::left_join(pairwise_years,water_negative,
                           by = c("DYAD_CODE" = "DYAD_CODE", 
                                  "year"="year"))
                  
  
  # add in the precip for each country in there

  
  events_years_tmp <- events_years %>%
                      dplyr::left_join(cruts,
                                       by = c("CCODE1"="sov_a3",
                                              "year"="year")) %>%
                      rename(actor1_name = name,
                             actor1_precip = precip,
                             actor1_geometry = geometry) %>%
                      dplyr::left_join(cruts,
                                       by = c("CCODE2"="sov_a3",
                                              "year" = "year")) %>%
                      rename(actor2_name = name,
                             actor2_precip = precip,
                             actor2_geometry = geometry) %>%
                      mutate(dyad = paste0(CCODE1,"_",CCODE2),
                             renege_dummy = ifelse(sea_renege=="N" | is.na(sea_renege) | sea_renege == "M",0,1), ) %>%
                      distinct(sea_renege,year,DYAD_CODE,.keep_all = TRUE)
  
  #events_years_tmp %>% dplyr::filter(renege_dummy==1) %>% dplyr::select(CCODE1,CCODE2,year,renege_dummy) %>% view()
  
  #events_years_tmp %>% filter(renege_dummy == 1) %>% view()
  path <- file.path(data_clean,"rainfall_events")
  events_years <- save_rds_csv(data = events_years_tmp,
                               output_path = path,
                               output_filename = paste0("events_years_precip_",current_continent,".rds"),
                               remove = FALSE,
                               format = "csv")
  
  