# _______________________________#
# Environment
# Clean 02: Clean Elevation Map
# 
# Stallman
# Started 2023-04-04
# Last edited: 
#________________________________#



# Startup

rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

# read in data ----

  if (!require(elevatr)) install.packages("elevatr") #for Digital Elevation Modelling
  library(elevatr)
  
  #https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html
