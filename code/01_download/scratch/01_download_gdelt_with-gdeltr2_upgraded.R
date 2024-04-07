# _______________________________#
# Environment
# download 01: download from GDELT and extract datasets
# 
# Stallman
# Started: 2023-05-29
# Last edited: 
#________________________________#


# Startup

rm(list = ls()) 

# noticed that the 15-min files were downloading into P:/Projects/environment/code/01_download which will require upload effort
# so try changing working directory to somewhere that has lots of spare storage

setwd(file.path("F:","data","02_temp","GDELT","gkg"))
# setwd(file.path("D:","data","02_temp","GDELT","gkg"))

# Only run this the first time to get data - it's a bugger, 
# a single year is about 300-500 GB and it uses terabytes of drive space

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


gdelt_gkg_one_day <- function(date, 
                              system_sleep_time = 10,
                              data_path = file.path("D:","data","01_raw","GDELT","gkg","gkg")){
  

  gkg_full_one_day <-
    get_data_gkg_days_detailed(
      dates = date,
      table_name = 'gkg',
      return_message = T
    )
  
  rm(gdelt_detailed_logs)
  
  Sys.sleep(system_sleep_time)
  
  water_related <- gkg_full_one_day[grep("WATER",gkg_full_one_day$themes), ]
  
  Sys.sleep(system_sleep_time)
  
  saveRDS(gkg_full_one_day,
          file = file.path(data_path,"full",paste0(date,"_gkg_full.rds")))

  Sys.sleep(system_sleep_time)
  
  rm(gkg_full_one_day)
  
  Sys.sleep(system_sleep_time)
  saveRDS(water_related,
          file = file.path(data_path,"water-related",paste0(date,"_gkg_water_related.rds")))
  
  Sys.sleep(system_sleep_time)
  rm(water_related)
  Sys.sleep(system_sleep_time)
  
  gc()
  Sys.sleep(system_sleep_time)
  
}



# GDELT GKG ----

#if (!require("gdeltr2")) devtools::install_github("abresler/gdeltr2")
#if (!require("rstudioapi")) install.packages("rstudioapi")
#if (!require("tictoc")) install.packages("tictoc")
#if (!require(parallel)) install.packages("parallel")

library(gdeltr2)
library(tictoc)
library(rstudioapi)
library(parallel)

n_cores <- detectCores() - 4


  # create an iterator for going through the script
  ## Run first time through to set
  # iter <- 1
  # saveRDS(iter, file = file.path("P:","Projects","environment","code","01_download","iter.R"))

  iter     <- readRDS(file.path("P:","Projects","environment","code","01_download","iter.R"))
  
  
  # dates we're interested in.
  ## Finished: 2016 2017 2018 2019 2020 2021
  ## Need 2022 05 10 to 2022 05 26
  ## Need 2015 start date is 02 18 23:00:00
  ## 2015: doen through 2015 07 08 (inc)
  ## Need 2023 to present
  
  #start_md   <- "0101"
  #end_md       <- "1231"
  
  start_year  <- "2015"
  start_md    <- "0709"
  
  end_year    <- "2016"
  end_md      <- "0104"


  start_ymd <- paste0(start_year,start_md)
  end_ymd   <- paste0(end_year,end_md)


  # create date sequence
  dates <- as.character(seq(as.Date(start_ymd,"%Y%m%d"), as.Date(end_ymd,"%Y%m%d"), by = "day"))
  # if final round through
  #this_iter_dates <- dates
  
  # split the date sequence into this many iterations
  # round when splitting will end up with the remainder into the first iter, and then the rest pretty even
  # so as long as it's not that the length(dates) ends up with n_iters*n_cores with a remainder of n_cores-1 
  # then we're pretty even
  n_iters <- round(length(dates)/n_cores)
  
  before <- Sys.time()
  print(paste0("Current time is ",before,"."))
  if (iter>n_iters)stop("That's all, folks! Iterations completed.")
  
  print(paste0("This is iteration number ",iter," of ",n_iters,"."))
  
  print(sprintf("One full round through takes approx 10-15 mins, so remaining time is approx %d-%d mins or %.1f-%.1f hours",
          (n_iters-iter+1)*10,(n_iters-iter+1)*15,((n_iters-iter+1)*10)/60,((n_iters-iter+1)*15)/60)
  )
  # split the dates into iterations
  iters_list <- split_vector_to_list(vector = dates,
                                       n_chunks = n_iters)
  
  # split the dates within an iteration to each core.
  
  this_iter_dates <- split_vector_to_list(vector = iters_list[[iter]],
                                         n_chunks = n_cores)
  
  
  print(sprintf("Calculating for %s to %s by day, ie %d days.",
                head(this_iter_dates,1),tail(this_iter_dates,1), length(this_iter_dates)))
  #print(unlist(this_iter_dates))
  


 
  
  
  iter <- iter+1
  saveRDS(iter, file = file.path("P:","Projects","environment","code","01_download","iter.R"))



  print(paste0("Initializing cluster of ",n_cores," cores..."))
  print(paste0("Calculating..."))
  
  cl <- makeCluster(n_cores)
  clusterEvalQ(cl, {
    library(gdeltr2)
  })
  
  tm <- system.time(
  
  parLapply(cl,this_iter_dates,gdelt_gkg_one_day)
  )
  

  
  stopCluster(cl)
  
  print(
    sprintf("Stopping cluster, the parLapply took %.1f seconds ie %.1f mins.", tm, tm/60)
  )
  
  sprintf("10 second nap to let memory settle...")
  Sys.sleep(10)
  
  
  
  
  after <- Sys.time()
  print(paste0("Current time is ",after,"."))
  print(sprintf("That iteration took %.2f mins.", after - before))
  
  rm(list = ls(all.names = TRUE))
  # without sys.sleep R was aborting; haven't found out exactly where it needs the extra time 
  print(sprintf("20 second nap to let memory settle..."))
  Sys.sleep(20)
  

  print("Preparing to restart. Ignore those warnings, they're from old package versions.")
  
  rstudioapi::restartSession(command = "source(file.path('P:','Projects','environment','code','01_download','01_download_gdelt_with-gdeltr2_upgraded.R'))")
  
  
  ## Timing information ----
  
  ## iterations smooth, trying 2016 almost in one go (360 days first, then do remaining 5)
  
  ## 2015 was the final month
  # 2015 02 19 to 2015 03 10: 513.1
  # 2015 03 11 to 2015 03 30: 528.4
  # 2015 03 31 to 2015 04 19: 512.9
  # 2015 04 20 to 2015 05 09: 526.0
  # 2015 05 10 to 2015 05 29: 508.9
  # 2015 05 30 to 2015 06 18: 507.3
  # 2015 06 19 to 2015 07 08: 570.7
  
  ## 18 iterations of 20 + 1 iteration of 5
  # 2016 01 01 to 2016 01 20 20 days: 603.78 sec elapsed
  # 2016 01 21 to 2016 02 09 20 days: 712.89 sec elapsed
  # 2016 02 10 to 2016 02 29 20 days: 721.63 sec elapsed
  # 2016 03 01 to 2016 03 20 20 days: 715.49 sec elapsed
  # 2016 03 21 to 2016 04 09 20 days: 691.33 sec elapsed
  # 2016 04 10 to 2016 04 29 20 days: 769.63 sec elapsed
  # 2016 04 30 to 2016 05 19 20 days: 819.44 sec elapsed
  # 2016 05 20 to 2016 06 08 20 days: 816.74 sec elapsed
  # stopped to let a longer series run overnight
  # 2016 06 09 to 2016 06 28 20 da7s: 798.69 sec elapsed
  # 2016 06 29 to 2016 07 18 20 days: 729.39 sec elapsed
  # 2016 07 19 to 2016 08 07 20 days: 731.37
  # 2016 08 08 to 0217 08 27 20 days: 805.83
  # 2016 08 28 to 2017 09 16 20 days: 746.64
  # 2016 09 17 to 2017 10 06 20 days: 791.45
  # 2016 10 07 to 2017 10 26 20 days: 749.39
  # 2016 10 27 to 2017 11 15 20 days: 795.37 
  # 2016 11 16 to 2017 12 05 20 days: 727.68
  # 2016 12 06 to 2017 12 25 20 days: 715.59
  # 2016 12 26 to 2016 12 31 5 days : 455.47 sec elapsed
  

  # iterations all running smoothly
  # Jan 2017 are bigger files than 2019 or 2018, frequently over a gig vs. consistently under for the later years
  # 2017 01 01 to 2017 01 20 20 days: 768.61 sec elapsed 
  # 2017 01 21 to 2017 02 09 20 days: 781.02 sec elapsed
  # 2017 02 10 to 2017 03 01 20 days: 781.55 sec elapsed
  # 2017 03 02 to 2017 03 21 20 days: 746.55 sec elapsed
  # 2017 03 22 to 2017 04 10 20 days: 729.14 sec elapsed
  # 2017 06 10 to 2017 06 29 20 days: 648.43 sec elapsed
  # 2017 06 30 to 2017 07 19 20 days: 542.66 sec elapsed
  # 2017 07 20 to 2017 08 08 20 days: 575.14 sec elapsed
  # 2017 08 09 to 2017 08 28 20 days: 580.61 sec elapsed
  # 2017 08 29 to 2017 09 17 20 days: 600.14 sec elapsed
  # 2017 09 18 to 2017 10 07 20 days: 580.51 sec elapsed
  # 2017 10 08 to 2017 10 27 20 days: 621.53 sec elapsed
  # 2017 10 28 to 2017 11 16 20 days: 623.75 sec elapsed
  # 2017 11 17 to 2017 12 06 20 days: 597.13 sec elapsed
  # 2017 12 07 to 2017 12 26 20 days: 578.79 sec elapsed
  
  ## 2019 was the first set tried
  # 2019 01 01 to 2019 01 20 20 days: 487.42 sec elapsed
  # 2019 01 21 to 2019 03 01 in one go: 40 days, broke, likely maxed out the memory somewhere
  # 2019 01 21 to 2019 03 01 ie 40 days in two iterations: 612.56 sec elapsed for the first and 552.91 sec elapsed the second
  # 2019 05 01 to 2019 05 20 20 days: 608.29 sec elapsed 
  # started including total 60 seconds rest time in each call to gkg to allow memory to clear
  # 2019 05 20 to 2019 06 09 20 days: 559.57 sec elapsed includes 60 seconds rest
  # 2019 06 10 to 2019 06 29 20 days: 554.35 sec elapsed
  # 2019 06 30 to 2019 07 19 20 days: 541.1  sec elapsed
  # 2019 07 20 to 2019 08 08 20 days: 567.67 sec elapsed and an error in 5 nodes; temp files in P:
  # 2019 08 09 to 2019 08 28 20 days: 698.21 sec elapsed with setwd to D: for putting temp files, guess it adds 2 mins
  # iterations started running smoothly here
  # 2019 08 29 to 2019 09 17 20 days: 534.42 sec elapsed with setwd() to C: for putting temp files. keep temp files in C:
  # 2019 09 18 to 2019 10 07 20 days: 528.92 sec elapsed
  # 2019 10 08 to 2019 10 27 20 days: 559.53 sec elapsed
  # 2019 10 28 to 2019 11 16 20 days: 548.23 sec elapsed
  # 2019 11 17 to 2019 12 06 20 days: 515.69 sec elapsed
  # 2019 12 07 to 2019 12 26 20 days: 530.16 sec elapsed
  # 2019 12 27 to 2019 12 31 5 days : 250.19 sec elapsed
  
  
  # iterations smooth
  # 2020 01 01 to 2022 12 20 gives 720 days, would be 36 iterations if all goes through
  # iterations all of 20 days
  # 2020 01 01 to 2020 01 20: 513.7 sec elapsed
  # 2020 01 21 to 2020 02 09: 503.3 sec elapsed
  # 2020 02 10 to 2020 02 29: 500.58 sec elapsed
  # 2020 03 01 to 2020 03 20: 526.39 sec elapsed
  # 2020 03 21 to 2020 04 09: 520.84 sec elapsed
  # 2020 04 11 to 2020 04 29: 520.84 sec elapsed
  # 2020 04 30 to 2020 05 19: 514.53 sec elapsed
  # 2020 05 20 to 2020 06 08: 503.47 sec elapsed
  # 2020 06 09 to 2020 06 28: 505.61 sec elapsed
  # 2020 06 29 to 2020 07 18: 494.63 sec elapsed
  # 2020 07 19 to 2020 08 07: 507.5 sec elapsed
  # 2020 08 08 to 2020 08 27: 501 sec elapsed
  # 2020 08 28 to 2020 09 16: 483.76 sec elapsed
  # 2020 09 17 to 2020 10 06: 467.13 sec elapsed
  # 2020 10 07 to 2020 10 26: 378.25 sec elapsed
  # 2020 10 27 to 2020 11 15: 312.34 sec elapsed
  # 2020 11 16 to 2020 12 05: 459.95 sec elapsed
  # 2020 12 06 to 2020 12 25: 467.23 sec elapsed
  # 2020 12 26 to 2021 01 14: 448.64 sec elapsed
  # 2020 01 15 to 2021 02 03: 486.3 sec elapsed
  # 2021 02 04 to 2021 02 23: 474.81 sec elapsed
  # 2021 02 24 to 2021 03 15: 464.48 sec elapsed
  # 2021 03 16 to 2021 04 04: 480.8 sec elapsed
  # 2021 04 05 to 2021 04 24: 455.97 sec elapsed
  # 2021 04 25 to 2021 05 14: 452.27 sec elapsed
  # 2021 05 15 to 2021 06 03: 448.33 sec elapsed
  # 2021 06 04 to 2021 06 23: 436.25 sec elapsed
  # 2021 06 24 to 2021 07 13: 420.85 sec elapsed
  # 2021 07 14 to 2021 08 02: 413.65 sec elapsed
  # 2021 08 03 to 2021 08 22: 379.75 sec elapsed
  # 2021 08 23 to 2021 09 11: 417.05 sec elapsed
  # 2021 09 12 to 2021 10 01: 428.84 sec elapsed
  # 2021 10 02 to 2021 10 21: 412.84 sec elapsed
  # 2021 10 22 to 2021 11 10: 422.11 sec elapsed
  # 2021 11 11 to 2021 11 30: 418.19 sec elapsed
  # 2021 12 01 to 2021 12 20: 406.83 sec elapsed
  ## another set of iterations: 
  # 2021 12 21 to 2022 01 09: 405.03 sec elapsed
  # 2022 01 10 to 2022 01 29: 431.3  sec elapsed
  # 2022 01 30 to 2022 02 18: 428.79 sec elapsed
  # 2022 02 19 to 2022 03 10: 424.05 sec elapsed
  # 2022 03 11 to 2022 03 30: 419.38 sec elapsed
  # 2022 03 31 to 2022 04 19: 413.45 sec elapsed
  # 2022 04 20 to 2022 05 09: 411.85 sec elapsed
  
  ## another set
  # 2022 05 10 to 2022 05 26: missing
  # 2022 05 27 to 2022 06 15: 398.74 sec elapsed
  # 2022 06 16 to 2022 07 05: 408.6 sec elapsed
  # 2022 07 06 to 2022 07 25: 380.16 sec elapsed
  # 2022 07 26 to 2022 08 14: didn't print time to console but like 6:50
  # 2022 08 15 to 2022 09 03: time didn't print to console
  # 2022 09 04 to 2022 09 23: 381.4 s
  # 2022 09 24 to 2022 10 13: 377.4
  # 2022 10 14 to 2022 11 02: 373.5
  # 2022 11 03 to 2022 11 22: 371.4
  # 2022 11 23 to 2022 12 12: 361.2 
  # 2022 12 13 to 2023 01 01: 342.0
  
  ## 2023
  # 2023 01 02 to 2023 01 21: 384.0
  # 2023 01 22 to 2023 02 10: 381.9
  # 2023 02 11 to 2023 03 02: 374.1
  # 2023 03 03 to 2023 03 22: 368.2
  # 2023 03 23 to 2022 04 11: 498.4
  # 2023 04 12 to 2023 05 01: 476.2
 
 