# _______________________________#
# Environment
# Clean 02: clean, transboundary waters shapefiles
# 
# Stallman
# Started 2023-03-16
# Last edited: 
#________________________________#


# Startup

#rm(list = ls())


#packages ----

if (!require("openxlsx")) install.packages("openxlsx")
if (!require("lubridate")) install.packages("lubridate")
if (!require("dplyr")) install.packages("dplyr")

library(dplyr)     # data wrangling
library(openxlsx)  # read in excel format, use for read_xlsx
library(lubridate) # fancy dates package


# paths
  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

  
  water_treaties <- read.xlsx(xlsxFile = file.path(data_raw,
                                               "transboundary-waters",
                                               paste0("WebsiteTreatiesDB_",tbw_treaty_database_update_date,".xlsx")),
                              sheet = "Treaty Database",
                              rows = 1:2115,
                              detectDates = TRUE) # avoid the last two rows wreck the format
  
  
# do some cleaning ----
  
  ## dates ----
  
  mdy <- mdy(water_treaties$DateSigned)
  ymd <- ymd(water_treaties$DateSigned)
  
  ymd[is.na(ymd)] <- mdy[is.na(ymd)] # some dates are ambiguous, give precedence to ymd over mdy
  
  # if you want to check, make a separate date signed var and manually look. checks out
  
  #water_treaties$DateSigned2 <- ymd

  #water_treaties %>% select(DateSigned,DateSigned2) %>% view()
  
  water_treaties$DateSigned <- ymd

 ## drop if date is missing ----
  
  if (!require("dplyr")) install.packages("dplyr")
  
  library(dplyr)
  
  water_treaties_tmp <- water_treaties %>%
                        filter(!is.na(DateSigned))
  
  # rename the troublesome cols which have dates at the front
  water_treaties_tmp <- water_treaties_tmp %>%
                              rename(update_id_2016 = 7,
                                     treaty_id      = 8)
  
  # filter to be after 1960 when there's hope of having other related data ----
  
  # format as date just to make sure
  
  water_treaties_tmp$DateSigned <-as.Date(water_treaties_tmp$DateSigned,"%Y-%m-%d")
  
  
  length(unique(water_treaties_tmp$treaty_id)) # 634 in the entire dataset

  
  # make some sub-datasets ----
  
  # test out
  
  
  treaty_codes <- c("Not coded",
                    "Not a treaty",
                    "Semi-international",
                    "Not TFDD compatible",
                    "Primary agreement",
                    "Replaces primary agreement",
                    "Amendment",
                    "Protocol",
                    "Financial agreement",
                    "Missing",
                    "Available, not translated",
                    "Available, not coded")

  water_treaties_tmp$DocType <- factor(water_treaties_tmp$DocType,
                                  levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                                  labels = treaty_codes
  )
  
  

  
  # # for fixing up strings if desired
  # https://sparkbyexamples.com/r-programming/r-str_replace-to-replace-matched-patterns-in-a-string/
  # if (!require("stringr")) install.packages("stringr")
  # 
  # library(stringr) # for string, replace one string with another
  # 
  # water_treaties_tmp$Signatories2 <- str_replace(water_treaties_tmp$Signatories,"")
  
  
  path <- file.path(data_clean,"edge-level")
  
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  
  saveRDS(water_treaties_tmp, 
          file = file.path(data_clean,"edge-level","iftd_treaties.rds"))
  
  