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
  #library(lubridate)
  library(gdeltr2)
  glibrary(tictoc)
  library(dplyr)
#generate the sequence of dates
  
  start_ymd     <- "20160507"
  end_ymd       <- "20160508"
  
  dates <- as.character(seq(as.Date(start_ymd,"%Y%m%d"), as.Date(end_ymd,"%Y%m%d"), by = "day"))
  
  #dates <-   seq(ymd_hms(start_date),ymd_hms(end_date), by = 'month')
  
  type          <- c("water-related")
  
  filenames     <- paste0(dates,"_gkg_","water_related",".rds")

# append the files for a month ----
  

  for (i in seq_along(dates)){
    file_path <- file.path(data_external_raw,"GDELT","gkg","gkg",type,filenames[i])
    
    if (i==1){
      df      <- readRDS(file_path)
    }
    else{
    tmp_df    <- readRDS(file_path)
    
    df        <- bind_rows(df,tmp_df)
    rm(tmp_df)
    }
    
  }
  
# parse locations ----
  
  location_query <- "France" #"Zambia | Malawi | Zimbabwe | Botswana | Angola | Mozambique | Namibia"
    
  subset_df    <- df[grep(location_query,df$locations), ]
  # sources
  subset_df$documentSource
  
  tic()
  locations_df <- parse_gkg_mentioned_locations(subset_df,
                                                  location_column = "locations")
  toc()
  
  saveRDS(subset_df,
          file = file.path(data_temp,"GDELT","gkg","gkg",paste0("gkg_water_related_sample_subsetted_",start_ymd,"_to_",end_ymd,".rds")))
  
  saveRDS(locations_df,
          file = file.path(data_temp,"GDELT","gkg","gkg","locations",paste0("gkg_locations_water_related_sample_",start_ymd,"_to_",end_ymd,".rds")))
  
  saveRDS(df,
          file = file.path(data_temp,"GDELT","gkg","gkg",paste0("gkg_water_related_sample_df_",start_ymd,"_to_",end_ymd,".rds")))
  
  
  