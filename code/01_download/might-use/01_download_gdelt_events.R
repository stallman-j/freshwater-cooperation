# _______________________________#
# Environment
# download 01: download from GDELT and extract datasets
#
# Stallman
# Started: 2023-05-29
# Last edited:
#________________________________#


# Startup ----

rm(list = ls())

# temp files will move around a lot, change working directory to somewhere that has lots of spare storage

setwd(file.path("F:","data","02_temp","GDELT","events"))

# Packages ----
if (!require("gdeltr2")) devtools::install_github("abresler/gdeltr2")
#if (!require("rstudioapi")) install.packages("rstudioapi")
#if (!require("tictoc")) install.packages("tictoc")
#if (!require(parallel)) install.packages("parallel")

library(gdeltr2)
library(tictoc)
library(rstudioapi)
library(parallel)

# bring in the packages, folders, paths ----

#code_folder <- file.path("P:","Projects","environment","code")
#source(file.path("P:","Projects","environment","code","00_startup_master.R"))

# Functions ----

split_vector_to_list <- function(vector,
                                 n_chunks) {

  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_chunks, each = length(vector) / n_chunks, length.out = length(vector))

  split_list   <- split(vector,split_vector)

  return(split_list)
}


gdelt_events_one_day <- function(date,
                              system_sleep_time = 10,
                              data_path = file.path("D:","data","01_raw","GDELT","events")){


  event_one_day <-
    gdeltr2:::get_data_gdelt_period_event(
      period = date,
      file_directory = NULL,
      remove_files = T,
      empty_trash = T,
      return_message = T
    )

  saveRDS(event_one_day,
          file = file.path(data_path,"full",paste0(date,"_events_full.rds")))


  rm(event_one_day)

  gc()
  Sys.sleep(system_sleep_time)

}



# GDELT GKG ----



n_cores <- detectCores() - 4


  # create an iterator for going through the script
  ## Run first time through to set
  # iter <- 1
  # saveRDS(iter, file = file.path("P:","Projects","environment","code","01_download","iter.R"))

  iter     <- readRDS(file.path("P:","Projects","environment","code","01_download","iter.R"))


  # dates we're interested in:
  # 2015 02 08 to present, as these are the dates available for the GKG
  # Done
  # 2015 02 08 to 2015 12 31
  
  #start_md   <- "0101"
  #end_md       <- "1231"

  start_year  <- "2016"
  start_md    <- "0101"

  end_year    <- "2023"
  end_md      <- "0530"


  start_ymd <- paste0(start_year,start_md)
  end_ymd   <- paste0(end_year,end_md)


  # create date sequence
  dates <- as.character(seq(as.Date(start_ymd,"%Y%m%d"), as.Date(end_ymd,"%Y%m%d"), by = "day"))

  n_iters <- 8  # approximately each year

  before <- Sys.time()
  print(paste0("Current time is ",before,"."))
  if (iter>n_iters)stop("That's all, folks! Iterations completed.")

  # split the dates into iterations
  iters_list <- split_vector_to_list(vector = dates,
                                       n_chunks = n_iters)

  this_iter_dates <- iters_list[[iter]]


  print(sprintf("Calculating for %d days.", length(this_iter_dates)))
  print(paste0("Initializing cluster of ",n_cores," cores..."))
  print(paste0("Calculating..."))

  cl <- makeCluster(n_cores)
  clusterEvalQ(cl, {
    library(gdeltr2)
  })

  tm <- system.time(

  parLapply(cl,this_iter_dates,gdelt_events_one_day)
  )

  stopCluster(cl)

  print(
    sprintf("Stopping cluster, the parLapply took %.1f seconds ie %.1f mins.", tm[3], tm[3]/60)
  )

  after <- Sys.time()
  print(paste0("Current time is ",after,"."))
  print(sprintf("That iteration took %.2f mins.", after - before))

  # log the time to a text file
  
  line = sprintf("%d days including %s and %s on iteration # %d of %d took %.2f seconds or %.1f mins",
                 length(this_iter_dates),head(this_iter_dates,1),tail(this_iter_dates,1),iter,n_iters,tm[3],tm[3]/60)

  # open connection to append to text file
  con <- file(file.path("D:","data","01_raw","GDELT","events","full","timing_log.txt"),
              "a")
  
  writeLines(as.character(line), con)
  close(con)
  
  iter <- iter+1
  saveRDS(iter, file = file.path("P:","Projects","environment","code","01_download","iter.R"))
  

  
  rm(list = ls(all.names = TRUE))
  
  # print(sprintf("20 second nap to let memory settle..."))
  # Sys.sleep(20)


  print("Preparing to restart. Ignore those warnings, they're from old package versions.")

  rstudioapi::restartSession(command = "source(file.path('P:','Projects','environment','code','01_download','01_download_gdelt_events.R'))")
