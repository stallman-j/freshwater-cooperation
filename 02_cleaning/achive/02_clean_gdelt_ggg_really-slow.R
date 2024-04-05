# _______________________________#
# Environment
# Clean 02: Clean GDELT Global knowledge graph
# 
# Stallman
# Started 2023-05-28
# Last edited: 
#________________________________#


# https://blog.gdeltproject.org/announcing-the-global-geographic-graph/


# Startup

rm(list = ls())


# bring in the packages, folders, paths ----

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))
  library(lubridate)
  


  gkg_water_related_test <- readRDS("D:/data/01_raw/GDELT/gkg/gkg/water-related/2017-01-01_gkg_water_related.rds")
  
  gkg <- sample(gkg_water_related_test, size = 1000, seed = 4)
  
  sample_indices <- sample(nrow(gkg_water_related_test), size = 1000)
  

  gkg <- gkg_water_related_test[sample_indices,]
  # clear out the big file
  rm(gkg_water_related_test)
  
  
# exploring ----
  
  # take a look at how things are formatted
  gkg$documentSource[1:20]
  gkg$themes[1:20]
  gkg$locations[1:20]
  
  
  # bring in a raw csv file ----
  
  for (i in seq_along(dates)){
    file_path <- file.path(data_raw,"GDELT","gkg",type,filenames[i])
    
    current_file <- read_csv(file_path)
    
  }
  
  subset <- current_file[1:10,1:10]
  # https://stackoverflow.com/questions/56352244/how-to-read-first-few-records-of-a-json-file-using-r
  # https://stackoverflow.com/questions/61764850/error-parse-error-premature-eof-error-when-reading-n-lines-from-a-json-file
  
  headers <- c("GKGRECORDID","V2.1DATE","V2SOURCECOLLECTIONIDENTIFIER",
               "V2SOURCECOMMONNAME","V2DOCUMENTIDENTIFIER","V1COUNTS",
               "V2.1COUNTS","V1THEMES","V2ENHANCEDTHEMES","V1LOCATIONS",
               "V2ENHANCEDLOCATIONS","V1PERSONS","V2ENHANCEDPERSONS",
               "V1ORGANIZATIONS","V2ENHANCEDORGANIZATIONS",
               "V1.5TONE","V2.1ENHANCEDDATES","V2GCAM","V2.1SHARINGIMAGE",
               "V2.1RELATEDIMAGES","V2.1SOCIALIMAGEEMBEDS","V2.1SOCIALVIDEOEMBEDS",
               "V2.1QUOTATIONS","V2.1ALLNAMES","V2.1AMOUNTS","V2.1TRANSLATIONINFO",
               "V2EXTRASXML","")
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
  