# _______________________________#
# Environment
# Clean 02: Glean GDELT GKG: Filter Themes
# 
# Stallman
# Started 2023-05-29
# Last edited: 
#________________________________#


# See 02_log_gdelt-relevant-themes-and-countries.R for theme and country names

# Startup

rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

  if (!require("purrr")) install.packages("purrr")# for executing a safe value of a function
  library(purrr)
  
    date <- "2017-04-09"
  
 get_filters <- function(date,
                         input_path_gkg = file.path("E:","data","01_raw","GDELT","gkg","gkg","full"),
                         input_filename_gkg = "_gkg_full.rds",
                         input_path_events  = file.path("E:","data","01_raw","GDELT","events","full"),
                         input_filename_events = "_events_full.rds",
                         output_path = file.path("C:","data_C_drive","02_temp","GDELT","merged","filtered","africa-country-theme"),
                         output_filename = "_af_gkg_events.rds",
                         themes = c(
                           "WATER_SECURITY", 
                           "ENV_WATERWAYS",
                           "ENV_HYDRO",
                           "WB_138_WATER_SUPPLY",
                           "WB_140_AGRICULTURAL_WATER_MANAGEMENT",
                           "WB_141_WATER_RESOURCES_MANAGEMENT",
                           "WB_142_ENERGY_AND_WATER",
                           "WB_143_RURAL_WATER",
                           "WB_144_URBAN_WATER",
                           "WB_155_WATERSHED_MANAGEMENT",
                           "WB_156_GROUNDWATER_MANAGEMENT",
                           "WB_157_ENVIRONMENTAL_WATER_USE_AND_CATCHMENT_PROTECTION",
                           "WB_161_DAMS_AND_RESERVOIRS",
                           "WB_159_TRANSBOUNDARY_WATER",
                           "WB_423_INTEGRATED_URBAN_WATER_MANAGEMENT",
                           "WB_427_WATER_ALLOCATION_AND_WATER_ECONOMICS",
                           #"WB_525_RENEWABLE_ENERGY", too big and only interested in hydropower anyways
                           "WB_527_HYDROPOWER",
                           "WB_988_LEVEES",
                           "WB_1000_WATER_MANAGEMENT_STRUCTURES",
                           "WB_1002_IRRIGATION_WATER_QUALITY",
                           "WB_1064_WATER_DEMAND_MANAGEMENT",
                           "WB_1021_WATER_LAW",
                           "WB_1063_WATER_ALLOCATION_AND_WATER_SUPPLY",
                           "WB_1199_WATER_SUPPLY_AND_SANITATION",
                           "WB_1220_SURFACE_WATER_MANAGEMENT",
                           "WB_1729_URBAN_WATER_FINANCIAL_SUSTAINABILITY",
                           "WB_1731_NON_REVENUE_WATER",
                           "WB_1778_FRESHWATER_ECOSYSTEMS",
                           "WB_1790_INTERNATIONAL_WATERWAYS",
                           "WB_1805_WATERWAYS",
                           "WB_1941_WATERBORNE_TRANSPORT",
                           "WB_1998_WATER_ECONOMICS",
                           "WB_2005_COMMUNITY_WATER_SUPPLY_MANAGEMENT",
                           "WB_2007_WATER_SAFETY_PLANS", # 
                           "WB_2971_WATER_PRICING",
                           "WB_2972_GROUNDWATER_CONJUNCTIVE_USE"
                           #"WB_2992_FRESHWATER_FISHERIES" # really does just catch fishing related
                           #"UNGP_FORESTS_RIVERS_OCEANS",
                         ),
                         countries = c("AG", "AO", "BN", "BC", "UV", "BY", "CM", 
                                       "CV", "CT", "CD", "CN", "CF", "CG", "IV", "DJ", "EG", "EK", "ER", 
                                       "WZ", "ET", "GB", "GA", "GH", "GV", "PU", "KE", "LT", "LI", "LY", 
                                       "MA", "MI", "ML", "MR", "MP", "MF", "MO", "MZ", "WA", "NG", "NI", 
                                       "RW", "RE", "SG", "SE", "SL", "SO", "SF", "SH", "SU", 
                                       "TP", "TZ", "TO", "TS", "UG", "WI", "ZA", "ZI"), # all African Countries
                         system_sleep_time = 10,
                         year = "2017"
                         ){
   
   # if the output path doesn't exist, create it
   # the input path should exist otherwise you have no data
   
   output_path <- file.path(output_path,"daily",year)
   
   if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
   
   if (!require("stringi")) install.packages("stringi")
   if (!require("tidyverse")) install.packages("tidyverse")
   
   library(stringi)
   library(tidyverse)

# bring in gkg and events data ----
   
   pattern <- paste0("#",paste(countries, collapse = "#|#"),"#")
   
   gkg_data <- readRDS(file = file.path(input_path_gkg, paste0(date,input_filename_gkg))) %>%
               filter(!is.na(documentSource))
   
   
   locations_filter <- grepl(pattern, gkg_data$locations)
   
   gkg_data <- gkg_data[locations_filter, ]
   

   events_data <- readRDS(file = file.path(input_path_events,paste0(date,input_filename_events))) %>%
                  filter(!is.na(urlSource))
   
   # take only events that take place in Africa
   pattern <- paste(countries,collapse = "|")
   locations_filter <- grepl(pattern,events_data$idCountryAction)
   
   events_data <- events_data[locations_filter, ]

   
   
# merge events and gkg by url ----
   
  merged_data <- gkg_data %>%
     dplyr::full_join(events_data, # left_join to limit events
                      by = c("documentSource" ="urlSource"),
                      keep = TRUE,
                      relationship = "many-to-many") %>% 
     mutate(merge = case_when(!is.na(idGKG) & !is.na(idGlobalEvent) ~ "both",
                              !is.na(idGKG) & is.na(idGlobalEvent)  ~ "gkg",
                              is.na(idGKG) & !is.na(idGlobalEvent) ~ "events",
                              TRUE ~ "missing"
     ))
   
   # save the full merged events and gkg (for a reference of how many total events are happening)
   saveRDS(merged_data,
           file = file.path(output_path,paste0(date,output_filename)))
           
   rm(gkg_data,events_data,locations_filter,pattern)
   
# get themes filters ----
   Sys.sleep(system_sleep_time)
   
     for (theme in themes){

   # nas were getting carried throughout, replace NAs with empty strings (some themes are just missing, we'll ignore those obs)
   themes_filter <- stringi::stri_detect_fixed(str = replace_na(merged_data$themes,''),
                                              pattern = theme)
   
   merged_data <- merged_data[themes_filter, ]

   saveRDS(merged_data,
           file = file.path(output_path,paste0(date,"_",theme,output_filename)))
   }
   
     rm(merged_data,themes_filter)
     Sys.sleep(system_sleep_time)
     # 
     gc()
  
 }
 

 get_filters_safe <- purrr::possibly(get_filters, 
                                     otherwise = NULL, 
                                     quiet = FALSE)
 
 # test with a single date
 #  system.time(
 # test <- get_filters_safe(date = date)
 #  )

 
 start_year  <- "2017"
 start_md    <- "0101"
 
 end_year    <- "2017"
 end_md      <- "1231"
 
 year        <- end_year
 
 start_ymd <- paste0(start_year,start_md)
 end_ymd   <- paste0(end_year,end_md)
 
 # create date sequence
 dates <- as.character(seq(as.Date(start_ymd,"%Y%m%d"), as.Date(end_ymd,"%Y%m%d"), by = "day"))
 
 library(parallel)
 n_cores <- detectCores() - 2
 
 cl <- makeCluster(n_cores)
 #clusterExport(cl, varlist = c('year'))
 
 clusterEvalQ(cl, {
   library(purrr) # to export possibly() to give an error if the function stops working 
   library(tidyverse)
 })
 
 print(Sys.time())
 
 system.time(
   
   parLapply(cl,dates,get_filters_safe, year = "2017")
   
 )
 
 stopCluster(cl)
 
 print(Sys.time())
 

 
 # 2018-01-01 to 2018-02-09
 # user  system elapsed 
 # 0.30    0.19  362.62 
 
 
 # for Jan 2017
 # user  system elapsed 
 # 0.02    0.02  283.78 

 # for Jan 21 2016 through Dec 31 2016 with input and output paths as E: external hard drive
 # user  system elapsed 
 # 2.28    1.11 4605.25: 76.76 mins
 
 # for all of 2017 with input path in E: external; and output path to C: internal hard drive
 # user  system elapsed 
 # 2.47    0.86 3731.63 : 62.2 mins
 
 # for all of 2018 with input path as E: external, output path C: internal

 # for Jan 01 2020 to Apr 30 2023
 # user  system elapsed 
 # 3.04    1.33 7064.27: 117 minutes
 
 
 filtered_2016 <- append_dates(input_path_gkg = file.path("E:","data","02_temp","GDELT","gkg","gkg","full","filtered","dams-canals-north-africa"),
                               output_path = file.path("E:","data","02_temp","GDELT","gkg","gkg","full","filtered","yearly"),
                              input_filename_gkg = "_gkg_filtered.rds",
                               dates = dates)
 
 
 
 stub <- get_url_stub(data = filtered_2016)
 
 filtered_2016_mini <- cbind(filtered_2016,stub) %>%
   select(idGKG,
          #documentSource,
          stub,themes)
 
    
  
# testing with a few days ----
  
  events_full_2017_01_01 <- readRDS("E:/data/01_raw/GDELT/events/full/2017-01-01_events_full.rds")
  events_full_2017_01_02 <- readRDS("E:/data/01_raw/GDELT/events/full/2017-01-02_events_full.rds")
  gkg_water_related_2017_01_01 <- readRDS("E:/data/01_raw/GDELT/gkg/gkg/water-related/2017-01-01_gkg_water_related.rds")
  gkg_water_related_2017_01_02 <- readRDS("E:/data/01_raw/GDELT/gkg/gkg/water-related/2017-01-02_gkg_water_related.rds")
  
  
  events_full_2017_01_01_and_01_02 <- append_dates_general(dates = c("2017-01-01","2017-01-02"))  
  
  water_events_2017_01_01 <- full_join(gkg_water_related_2017_01_01,events_full_2017_01_01,
             by = c("documentSource" ="urlSource"),
             keep = TRUE)%>% 
    mutate(merge = case_when(!is.na(idGKG) & !is.na(idGlobalEvent) ~ "both",
                             !is.na(idGKG) & is.na(idGlobalEvent) ~ "left",
                              is.na(idGKG) & !is.na(idGlobalEvent) ~ "right",
                             TRUE ~ "missing"
    ))
  
  test_parsed <- parse_gkg_mentioned_themes(gdelt_data = water_events_2017_01_01)
  
  # check approx what the shakedown is:
  plot <- ggplot(water_data,
         aes(x = merge)) +
    geom_bar() +
    geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -.5) +
    labs(title = "Merging Events and 'Water' in GKG Themes, Jan 2017",
         x = "Merge results")+
    theme_plot(title_size = 16,
               axis_title_x = element_text(color = "black"),
               axis_title_y = element_text(color = "black"),
               axis_text_x  = element_text(color = "darkgrey"),
               axis_text_y  = element_text(color = "darkgrey"))
  
  plot
  
  save_map(output_folder = output_figures,
           plotname = plot,
           filename = "events_gkg_2017_01.png")
  
  in_gkg_water_events <-  water_events_2017_01_01 %>% 
                          filter(merge == "left") %>%
                          select(
                            idGKG,nameSource.x,
                            domainSource,themes,locations,persons,organizations,
                            mentionedNamesCounts,mentionedNumericsCounts,documentSource)
                         
  test_themes_parsed <- parse_gkg_mentioned_themes(gdelt_data = in_gkg_water_events[1:100,])
  
 
 