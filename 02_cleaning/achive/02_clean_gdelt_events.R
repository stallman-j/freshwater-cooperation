# _______________________________#
# Environment
# Clean 02: Glean GDELT Events
# 
# Stallman
# Started 2023-05-29
# Last edited: 
#________________________________#


# https://github.com/abresler/gdeltr2


# Startup

rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

  # see here:
  # https://rpubs.com/BrendanKnapp/GDELT_Syrian_Conflict
  
  
  library(gdeltr2)
  
# Bring in 
 