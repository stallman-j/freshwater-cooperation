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
  
  #code_folder <- file.path("P:","Projects","environment","code")
  #source(file.path("P:","Projects","environment","code","00_startup_master.R"))
  

# GDELT GKG ----
  
#  devtools::install_github("hadley/devtools")
#  devtools::install_github("hadley/dplyr")
#  devtools::install_github("hafen/trelliscopejs")
  
  #if (!require("gdeltr2")) devtools::install_github("abresler/gdeltr2")
  #if (!require("rstudioapi")) install.packages("rstudioapi")
  
  library(gdeltr2)
  library(tictoc)
  library(rstudioapi)
  # get the URLS
  
  #my_urls <- get_urls_gkg_15_minute_log()

  
  
  # start_date <- "20180101
  # for the 
  
  year      <- "2018"
  
  #starts     <- c("0601","0701","0801","0901","1001","1101","1201")
  starts       <- c("0612","0712","0812","1022","1122","1222")
  ends         <- c("0630","0731","0831","1031","1130","1231")
  # create an iterator for going through the script
  ## Run first time through to set
  # iter <- 1
  # saveRDS(iter, file = file.path("P:","Projects","environment","code","01_download","iter.R"))
  # 
  iter     <- readRDS(file.path("P:","Projects","environment","code","01_download","iter.R"))
  print(paste0("iteration number: ",iter))
  
  max_iter <- length(starts)
  
  if (iter>max_iter)stop("Reached end, stopping")
  
       
  #ends       <- c("0630","0731","0831","0930","1031","1130","1231")
  
  start_dates <- paste0(year,starts)
  end_dates   <- paste0(year,ends)
  
  

  dates <- as.character(seq(as.Date(start_dates[iter],"%Y%m%d"), as.Date(end_dates[iter],"%Y%m%d"), by = "day"))
  
  iter <- iter+1
  saveRDS(iter, file = file.path("P:","Projects","environment","code","01_download","iter.R"))
  
  #dates <- as.character(dates[1])
  
  
  # for a single day
  # tic()
  # for (date in dates){
  #  gkg_full_one_day <-
  #   get_data_gkg_days_detailed(
  #     dates = date,
  #     table_name = 'gkg',
  #     return_message = T
  #   )
  #  
  # water_related <- gkg_full_one_day[grep("WATER",gkg_full_one_day$themes), ]
  # 
  # saveRDS(gkg_full_one_day,
  #         file = file.path(data_external_raw,"GDELT","gkg","gkg","full",paste0(date,"_gkg_full.rds")))
  # 
  # saveRDS(water_related,
  #         file = file.path(data_external_raw,"GDELT","gkg","gkg","water-related",paste0(date,"_gkg_water_related.rds")))
  # 
  # rm(gkg_full_one_day)
  # rm(water_related)
  # rm(gdelt_detailed_logs)
  # gc()
  # }
  # 
  # toc() 
  # 1 days was: 161.94 sec elapsed
  # 2 days first 130,296 events on Jan 1 2018, then 202532 gkg detailed events for 2018-01-02, 387.16 seconds / 60 = 6.45 minutes
  
  gdelt_gkg_one_day <- function(date){
    
    print(paste0("Getting gkg for",date))
    
    gkg_full_one_day <-
      get_data_gkg_days_detailed(
        dates = date,
        table_name = 'gkg',
        return_message = T
      )
    rm(gdelt_detailed_logs)
    
    water_related <- gkg_full_one_day[grep("WATER",gkg_full_one_day$themes), ]
    
    saveRDS(gkg_full_one_day,
            file = file.path("D:","data","01_raw","GDELT","gkg","gkg","full",paste0(date,"_gkg_full.rds")))
    rm(gkg_full_one_day)
    
    saveRDS(water_related,
            file = file.path("D:","data","01_raw","GDELT","gkg","gkg","water-related",paste0(date,"_gkg_water_related.rds")))
    
    rm(water_related)
    gc()
  }
  
  library(parallel)
  n_cores <- detectCores() - 4
  
  cl <- makeCluster(n_cores)
  clusterEvalQ(cl, {
    library(gdeltr2)
  })
  
  tic()
  
  parLapply(cl,dates,gdelt_gkg_one_day)
  toc()
  
  #>   toc()
  #398.3 sec elapsed for 10 days
  
  stopCluster(cl)
  
  print("Stopping cluster")
  
  print("Restarting")
  
 
  rm(list = ls(all.names = TRUE))
  
  rstudioapi::restartSession(command = "source(file.path('P:','Projects','environment','code','01_download','01_download_gdelt_with-gdeltr2.R'))")
                             
  ## some runtimes for reference
  # to run a month: 989.12 sec elapsed  = 16 minutes
  # to run 120 days: RAM got overloaded
  # to run 38 days: 1143.06 sec elapsed = 19.05 minutes
  # to run 2 days: 291.06 sec elapsed
  
  # iteration 6 278.46 sec elapsed
  # iteration 7 199.03 sec elapsed, 10 days: 20 seconds per
  
  # with a single day:    2018 06 11 287.77 sec elapsed
  # with a less busy day: 2018 07 11 179.9 sec elapsed
  
  # 20 days 10-02 to 10-21: 549.53 sec elapsed
  # 19 days 06 12 to 06 30: 629.03 sec elapsed
  # 20 days 07 12 to 07 31: 560.07 sec elapsed
  # 20 days 08 12 to 08 31: 562.89 sec elapsed
  # 9 days 11 22 to 11 30 :349.74 sec elapsed
  # 10 days 12 22 to 12 31: 279.83 sec elapsed