# _______________________________#
# International Agreements
# Clean 02: Clean Geodar global dams and catchment areas
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

# See 
# https://www.prio.org/data/26
  # downloaded in 01_download_datasets.R to data/01_raw



path <- file.path(data_raw,"WARICC_water-related-intrastate-conflict-and-cooperation","WARICC_dataset_v_1_0")
  
  
