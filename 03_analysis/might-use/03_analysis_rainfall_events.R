# _______________________________#
# Environment
# Analysis 03: rainfall events
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

  if (!require("dplyr")) install.packages("dplyr")
  if (!require("countrycode")) install.packages("countrycode")
  if (!require("AER")) install.packages("AER")
  if (!require("plm")) install.packages("plm")
  if (!require("stargazer")) install.packages("stargazer")
  
  library(dplyr)     # data wrangling
  library(countrycode) # for converting countrycode names from one system to another. transboundary waters uses the FAO-GAUL list
  # https://data.apps.fao.org/catalog/dataset/gaul-code-list-global-admin-2
  library(AER) # applied econometrics with R
  library(plm) # linear models for panel data
  library(stargazer) # output tables to tex files
  
  #data(codelist)
# parameters ----

  start_year <- start_year # earliest date, defined in 02_clean_transboundary-waters-events.R
  end_year   <- end_year # ditto
  current_continent <- current_continent # defined in 00_startup_master.R


# bring in data ----

  # https://www.econometrics-with-r.org/10-rwpd.html
  
path <- file.path(data_clean,"rainfall_events",paste0("events_years_precip_",current_continent,".rds"))
  
  data <- readRDS(file = path)
  
  data %>% dplyr::filter(renege_dummy == 1) %>% view()

# declare data as panel data
  
  data <- pdata.frame(data, index = c("dyad","year"))
  
  summary(data[,c("dyad","year")])
  