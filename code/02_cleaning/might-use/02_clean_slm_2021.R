# _______________________________#
# Environment
# Clean 02: Climate Bones of Contention 2021
# 
# Stallman
# Started 2023-05-17
# Edited: 2023-09-21
#________________________________#

# Startup

rm(list = ls())




# paths ----

  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))

# packages ----

  if (!require(haven)) install.packages("haven")
  if (!require("lubridate")) install.packages("lubridate")
  if (!require("countrycode")) install.packages("countrycode")
  
  library(haven) # for dealing with dta filetypes
  library(lubridate) # fancy dates package
  library(countrycode) # for converting countrycode names from one system to another.
    #    # paper doi:
    # https://journals.sagepub.com/doi/full/10.1177/0022343320973738
    
    
  data(codelist) # from countrycode package

# get data ----
  
  path <- file.path(data_raw,"SLM_ClimateBonesofContention_Replication","SLM_Onset_19012001.dta")
  ircc_dta_raw <- haven::read_dta(path)
  
  
