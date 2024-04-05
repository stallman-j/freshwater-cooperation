# _______________________________#
# Environment
# clean 02: EM-DAT 
# 
# Stallman
# Started: 2023-09-24
# Last edited: 
#________________________________#


# Startup

rm(list = ls())


# bring in the packages, folders, paths ----
if (!require("readxl")) install.packages("readxl")
if (!require("sf")) install.packages("sf")
if (!require("countrycode")) install.packages("countrycode")
if (!require("splitstackshape")) install.packages("splitstackshape")
if (!require("lubridate")) install.packages("lubridate")

library(lubridate) # for doing things with dates
library(splitstackshape) # for rearranging the data frame to add obs within each year, expandRows function
library(readxl)  # read in excel format, use for read_xlsx
library(sf)
library(countrycode) # for switching between different choices of countrynames, adding continents


  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))
  
  current_continent <- current_continent # set in 00_startup_master.R

# bring in Em-DAT ----

  path <- file.path(data_external_raw,"EM-DAT",
                    current_continent,paste0(current_continent,"_1900-2023_drought-flood-storm.xlsx")
                    )
  
  emdat <- read_xlsx(path = path,
                     sheet = "emdat_data",
                     col_names = TRUE)
  
# for versions of EM-DAT data that added the ISO3 country code to the disasterno, use this code to remove the ISO3 code to enable merge with GDIS
# rename the variable "Dis No" from EMDAT and remove the three-letter ISO from the disasterno identifier
  emdat <- emdat %>%
    mutate(disasterno=substr(`Dis No`,1,nchar(`Dis No`)-4)) %>%
    rename(Year       = "Year",
           start_year = "Start Year",
           start_month= "Start Month",
           start_day  = "Start Day",
           end_year   = "End Year",
           end_month  = "End Month",
           end_day    = "End Day",
           n_injured  = "No Injured",
           n_affected = "No Affected",
           n_homeless = "No Homeless",
           deaths     = "Total Deaths",
           aid_thousand_usd        = "AID Contribution ('000 US$)",
           river_basin = "River Basin",
           disaster_type = "Disaster Type",
           disaster_subtype = "Disaster Subtype",
           country      = "Country",
           disaster_id  = "Dis No",
           iso3         = "ISO"
           ) %>%
    mutate(
           start_year = ifelse(!is.na(start_year), yes = start_year, no = Year),
           end_year   = ifelse(!is.na(end_year), yes = end_year, no = Year),
           end_month   = case_when(is.na(end_month)&is.na(start_month)&(end_year==start_year) ~ 12,
                                   is.na(end_month)&is.na(start_month)&(end_year!=start_year) ~ 1,
                                   is.na(end_month)&!is.na(start_month)&(end_year==start_year) ~ 12,
                                   is.na(end_month)&!is.na(start_month)&(end_year!=start_year) ~ 1,
                                   !is.na(end_month) ~ end_month),
           start_month = ifelse(!is.na(start_month), yes = start_month, no = 1),
           start_day   = ifelse(!is.na(start_day), yes = start_day, no = 1),
           end_day     = ifelse(!is.na(end_day), yes = end_day, no = 1),
           
           length_disaster_years = ifelse(!is.na(end_year - start_year + 1), yes = end_year - start_year + 1, no= 1),
           is_disaster = 1,
           deaths       = ifelse(!is.na(deaths), yes = deaths, no = 0),
           deaths_dummy = ifelse(deaths==0, yes = 0, no = 1),
           disaster_factor = factor(disaster_type),
           start_ymd = ymd(paste(start_year,start_month,01,sep = "-")),
           end_ymd   = ymd(paste(end_year,end_month,01,sep = "-")),
           length_disaster_months = interval(start_ymd, end_ymd) %/% months(1)+1
    ) #%>% select(river_basin,disaster_id,start_year,start_month,start_day,end_year,end_month,end_day,length_disaster_years,start_ymd,end_ymd) 
    
  # https://stackoverflow.com/questions/1995933/number-of-months-between-two-dates

                  
  
  
  # rename countries so we can add shape-files more easily
  emdat[emdat$country == "Congo (the Democratic Republic of the)",]$country <- "Democratic Republic Of The Congo"
  emdat[emdat$iso3 == "CIV",]$country <- "Cote D'Ivoire"
  emdat[emdat$country == "Gambia (the)",]$country <- "Gambia"
  emdat[emdat$country == "Niger (the)",]$country <- "Niger"
  emdat[emdat$country == "Congo (the)",]$country <- "Republic of the Congo"
  emdat[emdat$country == "Sudan (the)",]$country <- "Sudan"
  emdat[emdat$country == "Tanzania, United Republic of",]$country <- "Tanzania"
  emdat[emdat$country == "Eswatini",]$country <- "Swaziland"
  emdat[emdat$country == "Cabo Verde",]$country <- "Cape Verde"
  
  
  
  #emdat %>% select(Year,start_year,end_year,length_disaster,disaster_id) %>% view()
  
  
  # by count, most are floods. we'll end up keeping just drought
  # barplot(table(emdat$disaster_factor))
  
  
  # generate duplicate rows for the start_ and end_ years
  # https://stackoverflow.com/questions/33181287/r-duplicating-rows-based-on-a-sequence-of-start-and-end-dates
  
  emdat <-   expandRows(emdat, "length_disaster_months", drop = FALSE) %>% # duplicate the rows according to the number given in length_disaster
                  dplyr::group_by(start_ymd,end_ymd,disaster_id) %>% # group together
                  mutate(date = seq(first(start_ymd),
                                    first(end_ymd),
                                    by = 'month'))  %>%# create the "date" variable which captures what date is getting duplicated
              filter(start_year >= year(first(time_range)) & end_year <= year(last(time_range)))
  
  #emdat %>% select(year,disaster_id,start_year,end_year,length_disaster) %>% view()
  
  
  #emdat_type %>% select(year,disaster_id,start_year,end_year,length_disaster) %>% view()
  
  emdat_path <- file.path(data_external_clean,"EM-DAT",current_continent)
  emdat_filename <- paste0("emdat_",current_continent,"_",year(first(time_range)),"_",year(last(time_range)),".rds")
  
  
  emdat <- save_rds_csv(data = emdat,
                             output_path   = emdat_path,
                             output_filename = emdat_filename,
                             remove = FALSE,
                             csv_vars = names(emdat),
                             format   = "csv")
  

  emdat <- readRDS(file = file.path(emdat_path,emdat_filename))
  
  # emdat_type <- emdat %>% filter(disastertype == disaster_Type) %>% mutate(disastertype = tolower(disastertype))
  # 
  # emdat_type_path <- file.path(data_external_clean,"EM-DAT",current_continent)
  # emdat_type_filename <- paste0("emdat_",current_continent,"_",year(first(time_range)),"_",year(last(time_range)),"_",disaster_type,".rds")
  # 
  # emdat_type <- save_rds_csv(data = emdat_type,
  #                            output_path   = emdat_type_path,
  #                            output_filename = emdat_type_filename,
  #                            remove = FALSE,
  #                            csv_vars = names(emdat_type),
  #                            format   = "csv")
  # 
  # emdat_type <- readRDS(file = file.path(emdat_type_path,emdat_type_filename))
  # 
  # get info from EM-DAT (in this case year of disaster)
  disasteryear <- emdat%>%
    select(ear, disasterno)%>%
    group_by(disasterno)%>%
    summarise(year=first(start_year))
  
# bring in GDIS ----
  
  path <- file.path(data_external_raw,"EM-DAT_geocoded",
                    "pend-gdis-1960-2018-disasterlocations-rdata",
                    "pend-gdis-1960-2018-disasterlocations.rdata")
  
  load(path)
  
  gdis_geo <- GDIS_disasterlocations 
  
  rm(GDIS_disasterlocations)
  
  #gdis_geo <- gdis_geo %>% rename(gdis_id = id)
  
  names(gdis_geo)
  
  # Remove geography to ease data operations and save a csv version
  gdis_nogeo <- gdis_geo %>% 
    st_drop_geometry() %>%
    as.data.frame()
  
  # add continent
  gdis_geo$continent <- countrycode(sourcevar = gdis_nogeo[, "country"],
                                    origin = "country.name",
                                    destination = "continent")
  
  # slight adjustments to remove ambiguity
  gdis_geo <- gdis_geo%>%
    mutate(continent=ifelse(country=="Micronesia","Oceania",continent))%>%
    mutate(continent=ifelse(country=="Kosovo","Europe",continent)) 
  
  gdis_test <- gdis_geo
  # 
  # getting warnings about old-style CRS:
  # old-style crs object detected; please recreate object with a recent sf::st_crs()
  # https://stackoverflow.com/questions/72609297/r-map-with-eurostat-package 
  st_crs(gdis_geo) <- "EPSG:4326"
  
  # try transforming the CRS to get rid of warning
  #sf::st_transform(gdis_geo, crs = "EPSG:4326") 
  
  # get just the desired continent
  gdis_continent <- gdis_geo %>% filter(continent == current_continent) # & disastertype == disaster_type)
  
  #st_transform(gdis_continent, crs = "EPSG:4326")
  
  
  

  
  
  path <- file.path(data_external_clean,"EM-DAT_geocoded",current_continent)
   
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
   
  
 ## Remove geography to ease data operations
 # disasterlocations <- GDIS_disasterlocations %>% 
 #   as_tibble() %>% 
 #   select(-geometry) %>%
 #   as.data.frame()
 
  
  saveRDS(gdis_geo,file = file.path(data_external_clean,"EM-DAT_geocoded","GDIS_1960_2018_disasterlocations.rds"))
  
  saveRDS(gdis_continent, file = file.path(data_external_clean,
                                           "EM-DAT_geocoded",
                                           current_continent,
                                           paste0("GDIS_1960_2018_",current_continent,".rds")
                                           ))
  
  gdis_continent <- readRDS(file = file.path(data_external_clean,
                                             "EM-DAT_geocoded",
                                             current_continent,
                                             paste0("GDIS_1960_2018_",current_continent,".rds"))
                              )
  
  gdis_geo <- readRDS(file = file.path(data_external_clean,"EM-DAT_geocoded","GDIS_1960_2018_disasterlocations.rds")) 
  
  gdis_nogeo <- save_rds_csv(data = gdis_nogeo,
               output_path   = file.path(data_external_clean,"EM-DAT_geocoded"),
               output_filename = "GDIS_1960_2018_disasterlocations_nogeo.rds",
               remove = FALSE,
               csv_vars = names(gdis_nogeo),
               format   = "csv")
  

  
# merge EM-DAT with GDIS geocoding ----
  # from https://sedac.ciesin.columbia.edu/data/set/pend-gdis-1960-2018
  
  gdis_emdat <- left_join(emdat,gdis_nogeo, by = join_by("iso3","country","disasterno"),
                          relationship = "many-to-many") 
  
  
  gdis_emdat <- save_rds_csv(data = gdis_emdat,
                             output_path   = file.path(data_external_clean,"EM-DAT_geocoded"),
                             output_filename = "GDIS_EMDAT_1960_2018.rds",
                             remove = FALSE,
                             csv_vars = names(gdis_emdat),
                             format   = "csv")
  

  