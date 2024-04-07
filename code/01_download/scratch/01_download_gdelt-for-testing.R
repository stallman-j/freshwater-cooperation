# _______________________________#
# Environment
# download 01: download datasets and extract from their zip files
# 
# Stallman
# Started: 2023-04-13
# Last edited: 
#________________________________#


# Startup

  rm(list = ls())

  # Only run this the first time, otherwise start from "cleaning!"
  
# bring in the packages, folders, paths ----
  
  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))
  


# GDELT Events ----
  
  #https://www.gdeltproject.org/data.html#gdeltanalysisservice 
  # click "Download GDELT 1.0 Events" button
  
  # http://data.gdeltproject.org/events/index.html
  
  ## 1979 to 2005 (inclusive) are annual
  
  filenames <- c(1979:2)
  
  sub_urls <- paste0(filenames,".zip")
  
  #http://data.gdeltproject.org/events/1986.zip
  
  download_multiple_files(data_subfolder = "events",
                          data_raw = file.path(data_external_raw,"GDELT"),
                          base_url = "http://data.gdeltproject.org/events",
                          sub_urls = sub_urls,
                          filename = filenames,
                          zip_file = TRUE)
  
  
  ## 2006 01  to 2013 03 (inclusive) are monthly
  
  #example
  #http://data.gdeltproject.org/events/201103.zip
  
  start_date <- "20060301"
  #end_date   <- "20060201"
  
  end_date   <- "20130301"
  
  dates <- seq(as.Date(start_date,"%Y%m%d"), as.Date(end_date,"%Y%m%d"), by = "month")
  dates <- format(dates, "%Y%m")
  
  filenames <- dates
  
  sub_urls <- paste0(filenames,".zip")
  

  system.time(
  download_multiple_files(data_subfolder = "events",
                          data_raw = file.path(data_external_raw,"GDELT"),
                          base_url = "http://data.gdeltproject.org/events",
                          sub_urls = sub_urls,
                          filename = filenames,
                          zip_file = TRUE)
  )
  
  # test with 2006 01 and 2006 02
  # user  system elapsed 
  # 0.00    0.00    0.97
  
  # 2006-03 to 2013-03:
  # user  system elapsed 
  # 35.05   21.89  419.34 
  
  
  ## 2013 04 01 to today (2023 05 26) are daily
  
  #example
  #http://data.gdeltproject.org/events/201103.zip
  
  ## full
  # start_date <- "20130403"
  # end_date   <- "20230526"
   
  
#  start_date <- "20130403"
  
 # end_date   <- "20140122"
  ## these days took 9.97 8.4 235.5
  
  
  ## 2014 01 23 to 2014 01 25 missing
  
  # start_date <- "20140126"
  # end_date   <- "20140318"
  # 
  # 2014 01 26 to 2014 03 18 : time 2.23 1.76 46.47
  # missing 2014 03 19
  
  
  # took 24.34 19.61 568.6 seconds;
  # start_date <- "20140320"
  # end_date   <- "20150817"
  # 
  
  
  start_date <- "20150818"
  end_date   <- "20230526"

  # end date up to 202003; after that it goes into days until present
  
  dates <- seq(as.Date(start_date,"%Y%m%d"), as.Date(end_date,"%Y%m%d"), by = "day")
  dates <- format(dates, "%Y%m%d")
  
  filenames <- dates
  
  sub_urls <- paste0(filenames,".export.CSV.zip")
  
  
  system.time(
  download_multiple_files(data_subfolder = "events",
                          data_raw = file.path(data_external_raw,"GDELT"),
                          base_url = "http://data.gdeltproject.org/events",
                          sub_urls = sub_urls,
                          filename = filenames,
                          zip_file = TRUE)
  )
  
  ## test for 2013-04-01 and 2013-04-02
  # Creating data subfolder D:/data/01_raw/GDELT/events/20130402 .
  # user  system elapsed 
  # 0.02    0.00    1.11 
  
  
# Global Geographic Graph ----
  
  # http://data.gdeltproject.org/gdeltv3/ggg/MASTERFILELIST.TXT
  
  # https://www.gdeltproject.org/data.html#rawdatafiles
  # GDELT Global Geographic Graph
  # 
  # The GDELT Global Geographic Graph is the underlying dataset powering the GDELT GEO 2.0 API, 
  # covering more than 1.6 billion location mentions from worldwide English language online news 
  # coverage back to April 4, 2017, with full details of each mention, including a 600-character contextual 
  # snippet of its context and usage.
  
  
  # https://blog.gdeltproject.org/announcing-the-global-geographic-graph/
  
  #generate the sequence of dates
  
  # from 2017 04 to 2020 03 (inclusive) we only have month by month
  
  start_date <- "20170401"

  end_date   <- "20200301"
  
  # end date up to 202003; after that it goes into days until present
  
  dates <- seq(as.Date(start_date,"%Y%m%d"), as.Date(end_date,"%Y%m%d"), by = "month")
  dates <- format(dates, "%Y%m")
  
  filenames <- paste0(dates,".ggg.v1.english.json.gz")
  
  # http://data.gdeltproject.org/gdeltv3/ggg/MASTERFILELIST.TXT
  
  # URL is 
  # http://data.gdeltproject.org/gdeltv3/ggg/201704.ggg.v1.english.json.gz
  
  sub_urls <- filenames
  
  # test time to external hard drive
  system.time(
  download_multiple_files(data_subfolder = "ggg",
                          data_raw = file.path(data_external_raw,"GDELT"),
                          base_url = "http://data.gdeltproject.org/gdeltv3/ggg",
                          sub_urls = sub_urls,
                          filename = filenames,
                          zip_file = FALSE)
  )
  
  # time to download two files 201704 and 201705, 17637.4MB and 19836.1MB = 37.472 Gigs
  
  # saving to external WD elements
  # user  system elapsed 
  # 4.33   34.31  547.56  # 9.17 minutes
  # 37472 MB / 550s = 68 MB / s download 
  
  # saving to internal
  #    user  system elapsed 
  # 3.94   39.53  505.72

  # time to download 34 files (2017 09 01 to 2020 03 01) will be approx 34 files * 250 seconds per file or about 2.4 hours
  # actual time:
  # user  system elapsed 
  # 56.89  544.70 7826.85 == 2.2 hours 
  
  # from 2020 04 01 to present (2023 05 26) we have daily (inclusive)
  
  start_date <- "20210611"
  
  end_date   <- "20230526"
  
  
  ## Full
  #start_date <- "20200401"
  #end_date   <- "20230526"
  
  
  
  
  # get most recent
  
  # start_date <- "20230526"
  # end_date   <- Sys.Date()
  
  # end date up to 202003; after that it goes into days until present
  
  dates <- seq(as.Date(start_date,"%Y%m%d"), as.Date(end_date,"%Y%m%d"), by = "day")
  dates <- format(dates, "%Y%m%d")
  
  # 1151 days

  filenames <- paste0(dates,".ggg.v1.english.json.gz")
  
  # http://data.gdeltproject.org/gdeltv3/ggg/MASTERFILELIST.TXT
  
  # URL is 
  # http://data.gdeltproject.org/gdeltv3/ggg/201704.ggg.v1.english.json.gz
  
  sub_urls <- filenames
  
  system.time(
  download_multiple_files(data_subfolder = "ggg",
                          data_raw = file.path(data_external_raw,"GDELT"),
                          base_url = "http://data.gdeltproject.org/gdeltv3/ggg",
                          sub_urls = sub_urls,
                          filename = filenames,
                          zip_file = FALSE)

  )
  
  # time for 2 days, 2020-04-01 and 2020-04-02
  
  # trying URL 'http://data.gdeltproject.org/gdeltv3/ggg/20200401.ggg.v1.english.json.gz'
  # Content type 'application/octet-stream' length 613558704 bytes (585.1 MB)
  # downloaded 585.1 MB
  # 
  # trying URL 'http://data.gdeltproject.org/gdeltv3/ggg/20200402.ggg.v1.english.json.gz'
  # Content type 'application/octet-stream' length 532675804 bytes (508.0 MB)
  # downloaded 508.0 MB
  # 
  # user  system elapsed 
  # 0.07    1.14   18.54
  
  # say about 10 seconds per file, times 1150 files gives 
  # 11500 seconds approx or 192 mins or 3.19 hours
  
  ## from 2020 04 03 to 2021 04 02 or 365 days, about an hour
  # user  system elapsed 
  # 14.19  129.30 2552.86 == 42.5 minutes
  
  # from 2021 06 11 to 2023 05 26 ; 715 days
  #    user  system elapsed 
  # 17.70  174.74 3717.06  // 1.03 hours
  

# Global Knowledge Graph ----
  library(lubridate)
  
  # https://www.gdeltproject.org/data.html#rawdatafiles
  # The GDELT 1.0 Global Knowledge Graph begins April 1, 2013 and consists of two parallel data streams, 
  # one encoding the entire knowledge graph with all of its fields, and the other encoding only the subset of
  # the graph that records "counts" of a set of predefined categories like number of protesters, number killed, 
  # or number displaced or sickened. Such counts may occur independently of the CAMEO events in the primary GDELT 
  # event stream, such as mentions of those killed in industrial accidents (which are not captured in CAMEO) or those 
  # displaced by a natural disaster or sickened by a disease epidemic. In this way, the GKG Counts File can be used to 
  # produce a daily "Death Tracker" to map all mentions of death across the world each day, or an "Affected Tracker" to 
  # indicate how many persons were sickened/displaced/stranded each day (at least as recorded in the global news media). 
  # These files are named as "YYYYMMDD.gkg.csv.zip" and posted by 6AM EST each morning seven days a week.
  # 
  # The second file is the full graph file, which contains the actual graph connecting all persons, organizations, 
  # locations, emotions, themes, counts, events, and sources together each day. It also contains a list of the EventIDs 
  # of each event found in the same article as the extracted information, allowing rich contextualization of events. 
  # These files are named as "YYYYMMDD.gkgcounts.csv.zip" and posted by 6AM EST each morning seven days a week.
  # 
  # The Global Knowledge Graph is currently in "alpha" release and may change over time as we introduce new capabilities 
  # and expand its underlying algorithms.
  
  
  

  # access the files
  # https://blog.gdeltproject.org/gdelt-2-0-our-global-world-in-realtime/
  
 

  # start date: 20150218230000 and goes until current
  
  # https://stackoverflow.com/questions/43149953/create-a-time-series-with-a-row-every-15-minutes
  ## done
  # start_date <- "20150218230000"
  # end_date   <- "20150220100000"
  
  # start_date   <- "20150220101500"
  # end_date     <- "20151002000000"
   
  # month of January 2018
  
  start_date     <- "20180101000000"
  end_date      <- "20180102000000"
  #end_date        <- "20181231234500"
  
  # start_date      <- "20180102000000"
  # end_date        <- "20181231234500"
  # 
  
  dates <-   seq(ymd_hms(start_date),ymd_hms(end_date), by = '15 mins')
  
  # replace all the colons and slashes and hyphens
  dates <-  gsub(" |/|-|:","",dates)

  # file_types    <- c("_export.zip","_mentions.zip","_gkg.zip")
  # 
  # sub_url_types <- c(".export.CSV.zip",".mentions.CSV.zip",".gkg.csv.zip")
  # 
  # filenames <- paste0(rep(dates,each = 3),file_types)
  # sub_urls  <- paste0(rep(dates,each = 3),sub_url_types)

  
  
  #file_types    <- c("_export.zip")
  #sub_url_types <- c(".export.CSV.zip")
  
  file_types    <- c("_gkg.zip")
  sub_url_types <- c(".gkg.csv.zip")
  type          <- c("gkg")
  
  filenames     <- paste0(dates,file_types)
  sub_urls      <- paste0(dates, sub_url_types)

  missing_urls  <- rep(NA, length(dates))
  
  system.time(
  for (i in seq_along(dates)){
    destfile_path <- file.path(data_raw,"GDELT","gkg",type,filenames[i])
  
    missing_urls[i] <- tryCatch(
    {download.file(url = paste0("http://data.gdeltproject.org/gdeltv2/",sub_urls[i]),
                  destfile = destfile_path)},
    error = function(e) {},
    warning = function(w) {NA}
    
    )
    
      }
  )
  
  # system time for January 1 downloading the "gkg" type on laptop
  # user  system elapsed 
  # 0.12    1.06   40.14
  
  saveRDS(missing_urls,
          file = file.path(data_raw,"GDELT","gkg","missing-indices",paste0("missing_urls_",type,"_",start_date,"_to_",end_date,".rds")))
  

  
  # for the ones that are unbroken, unzip
  
  
  for (i in dates[!is.na(missing_urls)]){
    
  unzip(file.path(data_raw,"GDELT","gkg",type,paste0(i,file_types)),
        exdir = file.path(data_raw,"GDELT","gkg",type),
        junkpaths = TRUE,
        overwrite = TRUE)
  file.remove(file.path(data_raw,"GDELT","gkg",type,paste0(i,file_types)))
  }
  

 
  
  
  
  
  
  