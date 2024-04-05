# _______________________________#
# Environment
# Analysis 03: analyze transboundary waters events
# 
# Stallman
# Started 2023-09-21
# Last edited: 2023-09-21
#________________________________#


# Startup

#rm(list = ls())

home_folder <- file.path("P:","Projects","environment")

source(file.path(home_folder,"code","00_startup_master.R"))

# parameters ----

  start_year <- start_year # comes from 02_clean_transboundary-waters-events.R, operates as a check that we're working with the latest cleaned version
  # currently working with 1948 and on

# bring in data ----
  
  # ifdd stands for international freshwater dispute database
  
  path <- file.path(data_clean,"edge-level","ifdd_events.rds")
  
  water_events <- readRDS(file = path)

# how many unique basins are there
  
  length(unique(water_events$BCode))
  # 146   