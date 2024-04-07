# _______________________________#
# Environment
# Clean 02: ADHI African Database of Hydrometric Indices
# 
# Stallman
# Last edited: 2024-03-27
#________________________________#



# Startup

rm(list = ls())


# bring in the packages, folders, paths

home_folder <- file.path("P:","Projects","environment")

source(file.path(home_folder,"code","00_startup_master.R"))

# packages ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  sf,
  tmap,
  countrycode
)


# bring in data ----

if (!file.exists(file.path(data_external_clean,"world.rds"))) {
  
  source(file.path(code_clean,"02_clean_01_world-tmap.R"))
}

world <- readRDS(file.path(data_external_clean,"world.rds"))

# note that this is NOT downloaded by 01_download_datasets_in_use.R because there's a
# checkbox you have to okay first and that'll take RSelenium


path <- file.path(data_external_raw,
                  "ADHI_african-database-hydrometric-indices",
                  "dataverse_files",
                  "ADHI",
                  "data")
