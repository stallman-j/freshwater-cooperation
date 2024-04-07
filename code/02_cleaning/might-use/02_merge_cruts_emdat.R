# _______________________________#
# Environment
# merge 02: EM-DAT and CRUTS
# 
# Stallman
# Started: 2023-09-24
# Last edited: 
#________________________________#


# Startup

  #rm(list = ls())

# packages ----

  if (!require("sf")) install.packages("sf")
  if (!require("lubridate")) install.packages("lubridate")
  if (!require("splitstackshape")) install.packages("splitstackshape")

  library(lubridate) # for doing things with dates
  library(sf)
  library(splitstackshape) # for function getanID

# paths ----

  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))

  
# parameters ----
  current_continent <- current_continent # set in 00_startup_master.R
  time_range <- time_range
  disaster_type <- disaster_type
  
# prior required code ----
  
  #source(file.path(code_clean,"02_clean_em_dat.R"))
  #source(file.path(code_clean,"02_clean_cruts.R"))
  
  
# bring in data ----
  emdat_type_path <- file.path(data_external_clean,"EM-DAT",current_continent)
  emdat_type_filename <- paste0("emdat_",current_continent,"_",year(first(time_range)),"_",year(last(time_range)),"_",disaster_type,".rds")
  emdat_type <- readRDS(file = file.path(emdat_type_path,emdat_type_filename)) #%>% rename(iso3 = "ISO")
  
  
  emdat_path <- file.path(data_external_clean,"EM-DAT",current_continent)
  emdat_filename <- paste0("emdat_",current_continent,"_",year(first(time_range)),"_",year(last(time_range)),".rds")
  emdat <- readRDS(file = file.path(emdat_path,emdat_filename)) %>% filter(disastertype == disaster_Type)
  
  
  gdis_countries <- readRDS(file =  file.path(data_external_clean,"EM-DAT_geocoded",current_continent,
                           paste0("GDIS_1960_2018_",current_continent,"_",disaster_type,"_with_countries.rds")))
  
  
  path <- file.path(data_clean,"shape-files",paste0("sf_countries_for_gdis",current_continent,".rds"))
  sf_countries_gdis <- readRDS(file = path) %>% select(country,geometry) %>% mutate(sf_id = row_number())
  
  long_sf_path <- file.path(data_external_clean,"CRU_TS",
                            paste0("geo_CRU_precip_long","_",
                                   disaster_type,"_",current_continent,"_",
                                   year(first(time_range)),"_",year(last(time_range)),".rds"))
  
  long_sf <- readRDS(file = long_sf_path) %>% ungroup() %>% mutate(long_sf_id = row_number()) 
  

  
  long_sf_duplicates_path <- file.path(data_external_clean,"CRU_TS",
                                       paste0("geo_CRU_precip_long_duplicates","_",
                                              disaster_type,"_",current_continent,"_",
                                              year(first(time_range)),"_",year(last(time_range)),".rds"))
  
  #saveRDS(long_sf_duplicates, long_sf_duplicates_path )
  long_sf_duplicates <- readRDS(file = long_sf_duplicates_path) %>% mutate(long_sf_id = row_number()) %>%
    dplyr::select(disasterno,-adm1,-adm2,-adm3,-gwno,)
  
  
  
# merge gdis and emdat ----
  
  # emdat_type has fewer obs because it's already filtered on being between 1960 and 2018
  # 
  # gdis_continent <- readRDS(file = file.path(data_external_clean,
  #                                            "EM-DAT_geocoded",
  #                                            current_continent,
  #                                            paste0("GDIS_1960_2018_",current_continent,"_",disaster_type,".rds"))
  # )
  # 
  # gdis_nogeo <- gdis_countries %>% st_drop_geometry() 
  # 
  
  # first, put all the events you can onto the existing geography blocks
  gdis_emdat <- full_join(gdis_countries,emdat,
                          by = c("disasterno","country")) %>%
    mutate(merge = case_when(!is.na(geo_id) & !is.na(disaster_id) ~ "both",
                             !is.na(geo_id) & is.na(disaster_id) ~ "left",
                             is.na(geo_id) & !is.na(disaster_id) ~ "right",
                             TRUE ~ "missing")
    )
    
  
  emdat_country_level <- gdis_emdat %>%
                  filter(merge == "right") %>%
                select(-merge) %>% st_drop_geometry()
  
  # checked manually, good
  country_emdat <- full_join(sf_countries_gdis,emdat_country_level,
                             by = c("country")) %>% # joins by country
    mutate(merge = case_when(!is.na(sf_id) & !is.na(disaster_id) ~ "both",
                             !is.na(sf_id) & is.na(disaster_id) ~ "left",
                             is.na(sf_id) & !is.na(disaster_id) ~ "right",
                             TRUE ~ "missing"),
           level = 0)

  
# rowbind the two groups ----
  
  gdis_emdat_to_merge <- gdis_emdat %>% filter(merge == "both")
  country_emdat_to_merge <- country_emdat %>% filter(merge=="both") %>% select(-sf_id)
  
  emdat_gdis_countries <- rbind(country_emdat_to_merge,gdis_emdat_to_merge)
  
  # save
  
  emdat_gdis_countries_path <- file.path(data_external_clean,"EM-DAT",current_continent)
  emdat_gdis_countries_filename <- paste0("emdat_gdis_countries_",current_continent,"_",year(first(time_range)),"_",year(last(time_range)),"_",disaster_type,".rds")
  
  
  saveRDS(emdat_gdis_countries, file = file.path(emdat_gdis_countries_path,emdat_gdis_countries_filename))
  
  emdat_final <-   readRDS(file = file.path(emdat_gdis_countries_path,emdat_gdis_countries_filename)) %>%
                    ungroup() %>% mutate(emdat_final_id = row_number())
  # find obs where the countries from emdat and gdis are not the same:
  
  #emdat_gdis_differ <- subset(emdat_gdis_inner, emdat_gdis_inner$country.x!= emdat_gdis_inner$country.y)
  
  # lost the ivory cost observations
  #emdat_gdis_los   <- anti_join(gdis_nogeo,emdat, by = c("disasterno"))
  

  
# 
  # we'll use the duplicates to attach the correct geometry to emdat, then merge on those things for the panel
  
  emdat_final_nogeo <- emdat_final %>% st_drop_geometry() 
  
  long_sf_tz <- long_sf %>% filter(country == "Tanzania")
  emdat_final_tz <- emdat_final %>% filter(country == "Tanzania") %>% st_drop_geometry()
  joined_sf_tz <- full_join(long_sf_tz,emdat_final_tz,
                         by = c("country",
                                "level",
                                "adm1",
                                "adm2",
                                "adm3",
                                "year")) %>%
    mutate(merge = case_when(!is.na(long_sf_id) & !is.na(disaster_id) ~ "both",
                             !is.na(long_sf_id) & is.na(disaster_id) ~ "left",
                             is.na(long_sf_id) & !is.na(disaster_id) ~ "right",
                             TRUE ~ "missing")) %>% 
    group_by(country,iso3,level,adm1,adm2,adm3,year,precip,is_disaster) %>% # just keep a single disaster per unit per year for now
    mutate(distinct_group = row_number()) %>%
    filter(distinct_group == 1) %>%
    mutate(disaster_dummy = ifelse(!is.na(is_disaster==1),yes = 1, no = 0))
  
  # national_disaster <-   joined_sf_tz %>% filter(level == 0) %>% select(disaster_dummy,year,country) %>% 
  #                       mutate(national_disaster = ifelse(disaster_dummy==1, yes = 1, no = 0)) %>%
  #                       st_drop_geometry()
  # 
  # long_disaster <- left_join(long_sf_tz, national_disaster)
  
  joined_sf <- full_join(long_sf,emdat_final_nogeo,
                       by = c("country",
                              "level",
                              "adm1",
                              "adm2",
                              "adm3",
                              "year")) %>%
    mutate(merge = case_when(!is.na(long_sf_id) & !is.na(disaster_id) ~ "both",
                             !is.na(long_sf_id) & is.na(disaster_id) ~ "left",
                             is.na(long_sf_id) & !is.na(disaster_id) ~ "right",
                             TRUE ~ "missing")) %>% 
    group_by(country,iso3,level,adm1,adm2,adm3,year,precip,is_disaster) %>% # just keep a single disaster per unit per year for now
    mutate(distinct_group = row_number()) %>%
    filter(distinct_group == 1)
           
  # replace the lower levels of disaster: if the entire country was hit, require 
  # that lower-level administrative units were also hit
  joined_sf <- joined_sf %>% ungroup()
  
  # https://stackoverflow.com/questions/42921674/assign-unique-id-based-on-two-columns
  joined_sf <- joined_sf %>% 
    mutate(unit_id = group_indices_(joined_sf,
                                    .dots = c("country","level","adm1","adm2","adm3"))) %>%
    mutate(disaster_dummy = ifelse(!is.na(is_disaster==1),yes = 1, no = 0)) %>%
    filter(!is.na(level))
  
  

  joined_path <- file.path(data_external_clean,"EM-DAT",current_continent)
  joined_filename <- paste0("emdat_panel_",current_continent,"_",year(first(time_range)),"_",year(last(time_range)),"_",disaster_type,".rds")
  saveRDS(joined_sf, file = file.path(joined_path,joined_filename))
  
  # create the observation id so that we can set a panel
  
 

# scratch 
  
  
  # 
  # test <- test_sf %>% filter(merge == "both")
  # 
  # test_sf <- full_join(long_sf,emdat_final_nogeo,
  #                      by = c("country","level","year"))%>%
  #   mutate(merge = case_when(!is.na(long_sf_id) & !is.na(disaster_id) ~ "both",
  #                            !is.na(long_sf_id) & is.na(disaster_id) ~ "left",
  #                            is.na(long_sf_id) & !is.na(disaster_id) ~ "right",
  #                            TRUE ~ "missing")
  #   ) %>% group_by(country,iso3,gwno,level,adm1,adm2,adm3,year,precip,is_disaster) %>% # just keep a single disaster per unit per year
  #   mutate(distinct_group = row_number()) %>%
  #   filter(distinct_group == 1)
  # 
  # sf_use_s2(FALSE)
  # 
  # dest_sf <- st_join(long_sf, emdat_final,
  #                    join = st_equals)%>%
  #   mutate(merge = case_when(!is.na(long_sf_id) & !is.na(emdat_final_id) ~ "both",
  #                            !is.na(long_sf_id) & is.na(emdat_final_id) ~ "left",
  #                            is.na(long_sf_id) & !is.na(emdat_final_id) ~ "right",
  #                            TRUE ~ "missing")
  #   ) 
  # 
  # 
  # 
  # test_merge <- test_sf %>% filter(merge == "both")
  # 
  # test_smaller_sf <- test_sf %>% 
  #   filter(country == "Tanzania") %>% 
  #   select(country,iso3,adm1,level,year,disasterno,disaster_id,merge,emdat_final_id,long_sf_id,precip)
  # 
  # test_smaller_sf %>% filter(merge == "missing") %>% view()
  # 
  # long_sf_duplicates_path <- file.path(data_external_clean,"CRU_TS",
  #                                      paste0("geo_CRU_precip_long_duplicates","_",
  #                                             disaster_type,"_",current_continent,"_",
  #                                             year(first(time_range)),"_",year(last(time_range)),".rds"))
  # 
  # 
  # long_sf_duplicates <- readRDS(file = long_sf_duplicates_path)
  # 
  # # merge emdat and long_sf_duplicates to get correct geometries ----
  # 
  # test_emdat <- emdat_type %>% filter(country == "Namibia")
  # test_long_sf <- long_sf_duplicates %>% filter(country == "Namibia")
  # 
  # 
  # test_sf <- left_join(long_sf_duplicates, emdat_type) %>%
  #   group_by(country, iso3, gwno, level, adm1, adm2, adm3, year, precip,disasterno) %>%
  #   mutate(distinct_group = row_number()) %>% # within the unique combo of all the above, make an id
  #   filter(distinct_group == 1) # only keep the first one of these
  # 
  # test_sf %>% select(country,level,adm1,adm2,adm3,iso3,disasterno,year,is_disaster) %>% filter(is_disaster==1) %>% view()
  # 
  # 
  # # merge ----
  # 
  # # start with long_sf which has 496 units