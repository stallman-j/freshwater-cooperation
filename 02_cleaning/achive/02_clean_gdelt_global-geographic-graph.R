# _______________________________#
# Environment
# Clean 02: Clean GDELT Global geographic graph
# 
# Stallman
# Started 2023-05-10
# Last edited: 
#________________________________#


# https://blog.gdeltproject.org/announcing-the-global-geographic-graph/


# Startup

rm(list = ls())


# bring in the packages, folders, paths ----

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))
  
  library(tidyverse)
  library(jsonlite)

#generate the sequence of dates
  
  start_date <- "20170401"
  
  end_date   <- "20170601"
  
  # end date up to 202003; after that it goes into days until present
  
  dates <- seq(as.Date(start_date,"%Y%m%d"), as.Date(end_date,"%Y%m%d"), by = "month")
  dates <- format(dates, "%Y%m")
  
  filenames <- paste0(dates,".ggg.v1.english.json.gz")
  
# bring in a raw json file ----
  
  # https://stackoverflow.com/questions/56352244/how-to-read-first-few-records-of-a-json-file-using-r
  # https://stackoverflow.com/questions/61764850/error-parse-error-premature-eof-error-when-reading-n-lines-from-a-json-file
  
  path <- file.path(data_external_raw,"GDELT","ggg",paste0(dates[2],".ggg.v1.english.json.gz"))
  
  # read in the first couple records
  
  # find out how many lines we've got
  

  n_lines <- 100
  
  #n_lines <- -1L
  
  
  # create "fake text connection" and pass on to json parser
  
  system.time(
  sample_df <- readLines(path, 
                         n = n_lines) %>% # get the lines
              textConnection() %>% # get text
              jsonlite::stream_in() # read in
  )
  
  # for May 2017
 # Imported 50744384 records. Simplifying...
 # user     system  elapsed 
 # 1069.28  1952.91 35087.59 // 9.75 hours
  

  # in April 2017 captured 45,416,336 and counting records and took up 53.62 G in RAM in R
  # runtime like 3 hours
  
  # est. 2.14 billion by Sep 2021 # https://blog.gdeltproject.org/global-geographic-graph-reaches-2-14-billion-records/
  # roughly since April 2017 was about 52 months, est around 41 million records per month
  
  # https://api.gdeltproject.org/api/v2/summary/summary
  
  # if sample
  #path <- file.path(data_external_raw,"GDELT","ggg",paste0(dates[2],"_sample_", n_lines,"_ggg_v1_english.rds"))
  
  # if full
  path <- file.path(data_external_raw,"GDELT","ggg",paste0(dates[2],"_ggg_v1_english.rds"))
  
  saveRDS(sample_df,
          file = path)
  