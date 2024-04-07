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
  
  
# might need devtools versions of the following:
  
  devtools::install_github("hadley/devtools")
  devtools::install_github("hadley/dplyr")
  devtools::install_github("hafen/trelliscopejs")
  
  
# following the tutorial ----
  
  # https://asbcllc.com/blog/2017/august/intro_to_programming_with_gdeltr2/index.html
  
  # install.packages("purrr")
  # install.packages("hrbrthemes")
  # 
  # library(purrr)
  # 
  # packages_to_install <- c("purrrlyr", "wordcloud2", "readr", "tidyr", "ggplot2", 
  #                          "tibble", "ggthemes", "jsonlite", "dplyr", "rlang", "tidyr", "stringr", 
  #                          "lubridate")
  # 
  # purrr::map(packages_to_install, install.packages)
  # 
  # 
  # devtools::install_github("hafen/trelliscopejs")
  # devtools::install_github("jbkunst/highcharter")
  # 
  # devtools::install_github("abresler/gdeltr2")
  
# Bring in 
  
  