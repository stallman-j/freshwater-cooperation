# _______________________________#
# Environment
# clean 02: CRU-TS climate 
# 
# Stallman
# Started: 2023-09-12
# Last edited: 
#________________________________#


# Startup

rm(list = ls())

# Only run this the first time, otherwise start from "cleaning!"

# bring in the packages, folders, paths ----

  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))

  current_continent <- current_continent # set in 00_startup_master.R
  time_range <- time_range 
  
# first run the cleaning for 
# packages ----

  if (!require(cruts)) install.packages("cruts")
  if (!require(R.utils)) install.packages("R.utils")
  if (!require(lubridate)) install.packages("lubridate")
  if (!require(stringr)) install.packages("stringr")
  if (!require(gifski)) install.packages("gifski")
  
  library(stringr) # for string operations
  library(lubridate) # for fancy dates operations
  library(cruts)
  library(gifski) # for making gifs
  library(R.utils)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(sf)
  library(dplyr)
  library(terra)

# bring in data ----

# create the sf_countries dataset (if not already created), ditto for the em-dat files
#source(file.path(code_clean,"02_clean_01_rnaturalearth_country-continent-shapefiles.R"))
#source(file.path(code_clean,"02_clean_em_dat.R"))
  


# bring in shapefiles for any sub-area that had a disaster
#gdis_geo <- readRDS(file = file.path(data_external_clean,"EM-DAT_geocoded","GDIS_1960_2018_disasterlocations.rds")) 

path <- file.path(data_clean,"shape-files",paste0("sf_countries_",current_continent,".rds"))

sf_countries_temp  <- readRDS(file = path) 

sf_countries <- sf_countries_temp %>% rename(iso3 = "iso_a3") %>% select(iso3,geometry,sovereignt) %>%
                rename(country = "sovereignt") %>%
                mutate(id      = NA,
                       gwno    = NA,
                       geo_id  = NA,
                       geolocation = "",
                       level   = 0,
                       adm1    = NA,
                       adm2    = NA,
                       adm3    = NA,
                       location = "",
                       historical = "",
                       hist_country = "",
                       disastertype = NA,
                       disasterno   = NA,
                       continent = current_continent
                       )

sf_countries[sf_countries$country == "Democratic Republic of the Congo",]$country <- "Democratic Republic Of The Congo"
sf_countries[sf_countries$country == "United Republic of Tanzania",]$country <- "Tanzania"
sf_countries[sf_countries$country == "eSwatini",]$country <- "Swaziland"
sf_countries[sf_countries$country == "Ivory Coast",]$country <- "Cote D'Ivoire"


path <- file.path(data_clean,"shape-files",paste0("sf_countries_for_gdis",current_continent,".rds"))

saveRDS(sf_countries, file = path)

sf_countries_gdis <- readRDS(file = path)


gdis_geo <- readRDS( file = file.path(data_external_clean,
                                            "EM-DAT_geocoded",
                                            current_continent,
                                            paste0("GDIS_1960_2018_",current_continent,".rds")
)) %>% select(-centroid)

gdis_countries <-rbind(gdis_geo,sf_countries_gdis) %>% # create identifier
                  mutate(gdis_country_id = row_number())

saveRDS(gdis_countries,
                          file = file.path(data_external_clean,"EM-DAT_geocoded",current_continent,
                                           paste0("GDIS_1960_2018_",current_continent,"_with_countries.rds")))

gdis_countries <- readRDS(file = file.path(data_external_clean,"EM-DAT_geocoded",current_continent,
                                           paste0("GDIS_1960_2018_",current_continent,"_with_countries.rds")))

shapefile_to_crop <- gdis_countries


crop_df <- shapefile_to_crop %>% st_drop_geometry()

# need polygons in sp format, not sf
# sp_continent <- as(sf_continent, Class = "Spatial")
# sp_countries <- as(sf_countries, Class = "Spatial")


#shapefile_to_crop <- gdis_continent
sp_shapefile <- as(shapefile_to_crop, Class = "Spatial")

# unzip the gz file ----


filename <- "cru_ts4.07.1901.2022.pre.dat.nc.gz"
path <- file.path(data_raw,"CRU_TS",filename)

out_path <- gsub(".gz$","",path)

# just run the first time
# R.utils::gunzip(filename = path,
#                 destname = out_path,
#                 remove = TRUE)

# get raster bricks and polygons ----
# see code at https://github.com/cran/cruts



# as rasters
# brick <- cruts2raster(ncfile = out_path,
#                       timeRange = time_range,
#                       type = "brick")

#stack <- terra::rast(out_path)

# stack <- cruts2raster(ncfile = out_path,
#                       timeRange = time_range,
#                       type = "stack")

#nc <- ncdf4::nc_open(out_path)

# turns CRU-TS into polygons according to the countries given
# adjusts crs in the function

# takes a few mins
temp  <- cruts2poly(ncfile = out_path,
                    poly   = sp_shapefile,
                    timeRange = time_range,
                    na.rm = TRUE)

saveRDS(temp, file = file.path(data_external_temp,"temp_CRUTS_polygons.rds"))
saveRDS(temp, file = file.path(data_temp,"temp_CRUTS_polygons.rds"))

temp <- readRDS(file = file.path(data_external_temp,"temp_CRUTS_polygons.rds"))

# generates the monthly averages within each polygon
temp_sf <- st_as_sf(temp)

# for testing
#test_sf <- temp_sf[,1:24]

# get annual averages ----

# generate a sequence of years in the time range and extract the year
years <- seq(from = ymd(head(time_range,n=1)),
             to   = ymd(tail(time_range,n=1)),by = "1 year") %>%
          year() # pull out just the year

#years<- c(1948,1949)

# this already took just the centroil
annuals_sf <- as.data.frame(st_geometry(temp_sf)) %>% mutate(gdis_country_id = row_number())
# annuals_sf <- true_annuals 
# temp_sf <- true_temp_Sf

# # try with just a single one
# test_vec <- c(1:2)
# annuals_sf <- annuals_sf[test_vec,]
# temp_sf <- temp_sf[test_vec,]
# year <- 2016

# get long-run mean and variance ----

long_run_sf <- temp_sf %>%
  mutate(row = row_number()) %>%
  st_drop_geometry() %>% # need to drop geometry to get this to work
  tidyr::pivot_longer(-row) %>% # pivot long
  group_by(row) %>% 
  summarize(long_run_mean = mean(value), # for each unit give mean and SD (over all months and years)
            long_run_sd  = sd(value)) %>%
  bind_cols(annuals_sf, .)

year <- 2017

for (year in years){
  
  # pick out the columns are in the current year
  relevant_cols <- stringr::str_detect(names(temp_sf),as.character(year))
  # check that we're getting the right rowmeans
  # my_cols_1 <- test_sf[1,relevant_cols] %>%
  #           st_drop_geometry()  %>% as.numeric()
  # 
  # for just those columns in our year of interest, generate a mean, sum, and variance
  
  # https://stackoverflow.com/questions/70841310/what-is-the-fastest-way-to-use-dplyr-to-find-the-row-wise-mean-and-variance-of
  my_cols <- temp_sf[,relevant_cols] 
  
  # check calcs
  
  #vec <- my_cols[1,] %>% st_drop_geometry() %>% as.numeric()
  
  # test 
  #print(mean(vec)) # 47.74
  #print(var(vec)) #2862.371
  
  annuals_sf <- my_cols %>%
    mutate(row = row_number()) %>%
    st_drop_geometry() %>% # need to drop geometry to get this to work
    tidyr::pivot_longer(-row) %>%
    group_by(row) %>%
    summarize(annual_mean = mean(value),
              annual_sd  = sd(value),
              annual_sum  = sum(value)) %>%
    select(annual_mean,annual_sd,annual_sum) %>%
    bind_cols(.,annuals_sf)
  
  
  # assign the name to what was just "annual_"
  name_mean <- paste0("mean_",year)
  name_sd   <- paste0("sd_",year)
  name_sum  <- paste0("sum_",year)
  
  
  names(annuals_sf)[1:3] <- c(name_mean,name_sd,name_sum)
  
}


# this is to help identify the locations
  identifiers <-shapefile_to_crop %>%
                  st_drop_geometry() %>%
                  dplyr::select(gdis_country_id,country,iso3,gwno,level,adm1,adm2,adm3,disasterno)
  
  #countries_sf_select <- sp_shapefile %>%  dplyr::select(name,sov_a3)

  annual_precip <- left_join(identifiers,annuals_sf) %>% st_as_sf() %>% arrange(level)
  
  #annual_precip <- annual_precip[test_vec,]
  
  # we have locations, and precip vals
  # annual_precip <- cbind(identifiers,
  #                     annuals_sf) %>% st_as_sf() %>% arrange(level) # arrange by level so that the largest level
  # like adm0 the country gets drawn first

  precip_path <- file.path(data_external_clean,"CRU_TS",
                           paste0("CRU_precip_",
                                  disaster_type,"_",
                                  current_continent,"_",
                                  year(first(time_range)),
                                  "_",
                                  year(last(time_range)),
                                  ".rds"))
  
  saveRDS(annual_precip, file = precip_path)
  
  annual_precip <- readRDS(precip_path)
  
  annual_precip <- annual_precip[test_vec,]
# make a longer df with country, year, precip as cols
  vignette("pivot") # helps set this up
  
  long_sf_duplicates <- annual_precip %>%
    pivot_longer(cols =-c(gdis_country_id,country,iso3,gwno,level,adm1,adm2,adm3,disasterno,geometry),
                 names_to = c(".value","year"),
                 names_sep = "_",
                 values_drop_na = TRUE)
    
    long_sf_duplicates_path <- file.path(data_clean,"CRU_TS",
                              paste0("geo_CRU_precip_long_duplicates_",current_continent,"_",
                                     year(first(time_range)),"_",year(last(time_range)),".rds"))
  saveRDS(long_sf_duplicates, long_sf_duplicates_path )
  
  
  long_sf_duplicates <- readRDS(file = long_sf_duplicates_path)
  
  
  long_sf <- long_sf_duplicates %>%
             #st_drop_geometry() %>%
            group_by(country, iso3, gwno, level, adm1, adm2, adm3, year, precip) %>%
            mutate(distinct_group = row_number()) %>% # within the unique combo of all the above, make an id
            filter(distinct_group == 1) %>%  # only keep the first one of thes
            ungroup() %>% mutate(long_sf_id = row_number()) 

  # need to pull out the distinct values, but asking for dplyr::distinct() on a big sf polygon takes 
  # a really long time
  
  # long_df <- long_sf %>% st_drop_geometry() %>%
  #             distinct(country,iso3,gwno,level,adm1,adm2,adm3,year,precip) %>%
  #             group_by(country, iso3, gwno, level, adm1, adm2, adm3, year, precip) %>%
  #             mutate(id = row_number())
  # 
  # long_sf_group <- long_sf %>% group_by(country, iso3, gwno, level, adm1, adm2, adm3, year, precip) %>%
  #                   mutate(id = row_number()) %>%
  #                   
  # 
  # long_sf <- left_join(long_df,long_sf_group,
  #                           by = c("country","iso3","gwno","level","adm1","adm2","adm3","year","precip","id"))
  # 

  long_sf_path <- file.path(data_clean,"CRU_TS",
                            paste0("geo_CRU_precip_long","_",
                                   disaster_type,"_",current_continent,"_",
                                   year(first(time_range)),"_",year(last(time_range)),".rds"))
  
  saveRDS(long_sf, long_sf_path )
  long_sf <- readRDS(file = long_sf_path)
  
  long_df <- long_sf %>% st_drop_geometry()
  
  long_df <- save_rds_csv(data = long_df,
               output_path   = file.path(data_external_clean,"CRU_TS"),
               output_filename = paste0("no_geo_CRU_precip_long","_",
                                        disaster_type,"_",current_continent,"_",
                                        year(first(time_range)),"_",year(last(time_range)),".rds"),
               remove = FALSE,
               csv_vars = names(long_df),
               format   = "csv")
  
  long_df <- readRDS(file = file.path(data_external_clean,"CRU_TS",paste0("CRU_precip_",
                                                                          disaster_type,"_",
                                                                          current_continent,"_",
                                                                          year(first(time_range)),"_",year(last(time_range)),
                                                                          ".rds")))
 
  
  
# merge the shapefile back ----
  # don't need to do this anymore because I kept the shapefile around
  # mini_crop <- shapefile_to_crop %>% dplyr::select(country,iso3,gwno,level,adm1,adm2,adm3)
  # 
  # long_sf <- left_join(long_df,mini_crop)
  # 
  # annual_precip <- annual_precip %>% st_drop_geometry() %>% left_join(mini_crop)
  # 
  # 
  # saveRDS(annual_precip, file.path(data_external_clean,"CRU_TS",paste0("geo_annual_precip_",
  #                                                                      current_continent,"_",
  #                                                                      disaster_type,".rds")))
  
  


