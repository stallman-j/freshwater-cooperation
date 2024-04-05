# _______________________________#
# Environment
# Download Allafrica : Download Allafrica Water News: text
# 
# Stallman
# Started 2023-08-31
# Last edited: 
#________________________________#


# to use after "01_download_allafrica-water_headlines-summaries.R
# Startup

  rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

# packages ----

  library(stringr)
  library(dplyr)


# start RSelenium on a new computer

# follow the first answer in this:
# https://stackoverflow.com/questions/45395849/cant-execute-rsdriver-connection-refused/74735571#comment131986973_74735571

# you need to download Docker and initialize it
  
  
  path <- file.path("E:","data","02_temp","laborious","water")
  
  years <- c(2004,2005,2006,2007,2008,2022,2023)
  
  for (year in years){

  data <- readRDS(file = file.path(path,paste0(year,"_allafrica_water_text.rds")))
  

  data <- data %>%
          mutate(relevant = "N",
                 relationship = NA,
                 location     = NA,
                 actor_01     = NA,
                 actor_02     = NA,
                 impact       = NA,
                 impact_count = NA,
                 notes        = NA,
                 country      = str_extract(title,"^[^\\:]+")) %>%# ^ matches beginning of string, at least one thing before a colon 
          select(country,title,text,relevant,relationship,dates,year,full_url,location,actor_01,actor_02,impact,impact_count,notes)
  
  data <- save_rds_csv(data = data,
                       output_path = file.path("E:","data","02_temp","laborious","water"),
                       output_filename = "_allafrica_water_text.rds",
                       remove = FALSE,
                       date = year,
                       csv_vars = names(data),
                       format = "csv")
  }
  #data %>% select(title,country) %>% view()
          
  