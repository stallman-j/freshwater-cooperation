# Downloading Functions

#' @param date # specific date to be downloaded
#' @note needs package gdelt2

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