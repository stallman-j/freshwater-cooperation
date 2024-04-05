# ___________________________#
# International Agreements
# 02 Clean trade data
# Stallman
# Started 2022-12-20
# Last edited: 
#
# ___________________________#


# clear everything out

rm(list = ls())

# Setup ----

## Parameters ----

## bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","international-agreements")
  
  source(file.path(home_folder,"code","00_master_ia.R"))

# bring in the data ----
  
  # run first time
  # path <- file.path(data_manual,"atlas-economic-complexity","country_partner_hsproduct2digit_year.dta")
  # 
  # trade_data <- read.dta(file = path)
  # 
  # saveRDS(trade_data, file = file.path(data_temp,"trade_data.rds"))
  # 
  
  
  trade_data <- readRDS(file = file.path(data_temp,"trade_data.rds"))

  # set.seed(5)
  # size_pilot <- 1
  # 
  # # create test dataframe 
  # 
  # trade_pilot  <- trade_data %>%
  #   filter(location_id %in% sample(unique(location_id), size_pilot)) 
  # 
  # saveRDS(trade_pilot, file = file.path(data_temp,"trade_pilot.rds"))
  # 
  trade_pilot <- readRDS(file = file.path(data_temp,"trade_pilot.rds"))

  # try to add everything in there 
  
  total_trade_export_pilot <- aggregate(trade_pilot$export_value,
                               by = list(trade_pilot$location_code,
                                         trade_pilot$partner_code,
                                         trade_pilot$year),
                               FUN = sum)

  colnames(total_trade_export_pilot) <- c("location_code","partner_code","year","export_value")
  
  total_trade_import_pilot <- aggregate(trade_pilot$import_value,
                                  by = list(trade_pilot$location_code,
                                            trade_pilot$partner_code,
                                            trade_pilot$year),
                                  FUN = sum)
  
  colnames(total_trade_import_pilot) <- c("location_code","partner_code","year","import_value")
  
  saveRDS(total_trade_import_pilot,file.path(data_temp,"total_trade_import_pilot.rds"))
  saveRDS(total_trade_export_pilot,file.path(data_temp,"total_trade_export_pilot.rds"))
  
  
  
# now with the real data ----
  
  
  
  total_trade_export <- aggregate(trade_data$export_value,
                                  by = list(trade_data$location_code,
                                            trade_data$partner_code,
                                            trade_data$year),
                                  FUN = sum)
  
  colnames(total_trade_export) <- c("location_code","partner_code","year","export_value")
  
  total_trade_import <- aggregate(trade_data$import_value,
                                  by = list(trade_data$location_code,
                                            trade_data$partner_code,
                                            trade_data$year),
                                  FUN = sum)
  
  colnames(total_trade_import) <- c("location_code","partner_code","year","import_value")
  
  saveRDS(total_trade_import,file.path(data_temp,"total_trade_import.rds"))
  saveRDS(total_trade_export,file.path(data_temp,"total_trade_export.rds"))
  
  