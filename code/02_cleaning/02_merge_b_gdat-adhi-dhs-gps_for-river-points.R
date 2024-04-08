# _______________________________#
# Freshwater Cooperation
# Merge 02 b: merge dams, hydrology stations, and DHS GPS data to get river points
# 
# Stallman
# Started: 2023-10-23
# Last edited: 2024-04-08
#________________________________#



# Startup

rm(list = ls())


# bring in the packages, folders, paths ----

home_folder <- file.path("P:","Projects","freshwater-cooperation")
source(file.path(home_folder,"code","00_startup_master.R"))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  stringr, # string operations
  countrycode, # country naming conversions
  sf, # vector spatial geometry
  rdhs, # getting DHS data
  elevatr, # for getting elevation from points
  RColorBrewer, # for getting gradients and colors
  parallel, # for parallelizing operations
  tictoc # timing # more ability to customize to output to latex. use with kableExtra to output tables
  # to console, latex, Rmarkdown, html etc.
)


dhs_gps <- readRDS(
        file = file.path(data_external_temp,"DHS","GPS","merged","africa_DHS_GPS.rds")) %>%
        mutate(country_iso3c = countrycode::countrycode(DHSCC,
                                                  origin = "dhs",
                                                  destination = "iso3c")) %>%
        rename(lat = LATNUM,
               lon = LONGNUM,
               ID = DHSID) %>%
        select(lat,lon,ID,country_iso3c) %>%
        mutate(type = "DHS_town") %>% 
        st_transform(crs = 4326)
               

adhi     <- readRDS(
  file = file.path(data_external_clean,"ADHI",paste0("adhi_stations_for_river_points.rds"))) %>%
  mutate(type = "hydrology_station")%>% 
  st_transform(crs = 4326)

dams_africa     <- readRDS(
  file = file.path(data_external_clean,"GDAT_global-dam-tracker",paste0("GDAT_dams-africa_for_river_points.rds"))
) %>%
  mutate(type = "Dam") %>% 
  st_transform(crs = 4326)


merged_df <- rbind(dhs_gps,adhi,dams_africa) 

  out_path <- file.path(data_external_clean,"merged","many")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  
  
saveRDS(merged_df, file = file.path(data_external_clean,"merged","many","dhs_gdat_adhi.rds"))

  

