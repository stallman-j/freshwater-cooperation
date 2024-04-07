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
                         output_path = file.path("C:","data_C_drive","02_temp","GDELT","merged","filtered","allafrica"),
                         output_filename = "_af_gkg_events.rds",
                         themes = c(
                           "WATER_SECURITY", 
                           "ENV_WATERWAYS",
                           "ENV_HYDRO",
                           "WB_137_WATER",
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
                           "WB_525_RENEWABLE_ENERGY", #, too big and only interested in hydropower anyways
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
                           "WB_2972_GROUNDWATER_CONJUNCTIVE_USE",
                           "WB_2992_FRESHWATER_FISHERIES", # really does just catch fishing related
                           "UNGP_FORESTS_RIVERS_OCEANS"
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
   print(paste0("Saving merged (unfiltered) data for ",date))
   saveRDS(merged_data,
           file = file.path(output_path,paste0(date,output_filename)))
           
   rm(gkg_data,events_data,locations_filter,pattern)
   
# get themes filters ----
   Sys.sleep(system_sleep_time)
   
   print(paste0("Starting loop through themes"))
     for (theme in themes){

   # nas were getting carried throughout, replace NAs with empty strings (some themes are just missing, we'll ignore those obs)
   themes_filter <- stringi::stri_detect_fixed(str = replace_na(merged_data$themes,''),
                                              pattern = theme)
   
   # can't put merged_data here or else we lose obs with each time through the loop
   filtered_data <- merged_data[themes_filter, ]

   saveRDS(filtered_data,
           file = file.path(output_path,paste0(date,"_",theme,output_filename)))
   
     }
   
   print(paste0("Finished looping through themes, deleting data..."))
   
     rm(merged_data,themes_filter,filtered_data)
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

 get_date_sequence <- function(start_year,
                               start_md = "0101",
                               end_year,
                               end_md = "1231") {
   
   
   start_ymd <- paste0(start_year,start_md)
   end_ymd   <- paste0(end_year,end_md)
   
   # create date sequence
   dates <- as.character(seq(as.Date(start_ymd,"%Y%m%d"), as.Date(end_ymd,"%Y%m%d"), by = "day"))
   
 }
 
 dates_2016 <- get_date_sequence(start_year = "2016",
                                 end_year   = "2016")
 
 dates_2020 <- get_date_sequence(start_year = "2020",
                   end_year = "2020")
 
  dates_2021 <- get_date_sequence(start_year = "2021",
                                  end_year = "2021")
  
  dates_2022 <- get_date_sequence(start_year = "2022",
                                  end_year = "2022")
  
  
 
 library(parallel)
 n_cores <- detectCores() - 1
 
 cl <- makeCluster(n_cores)
 #clusterExport(cl, varlist = c('year'))
 
 clusterEvalQ(cl, {
   library(purrr) # to export possibly() to give an error if the function stops working 
   library(tidyverse)
 })
 
 print(Sys.time())
 
 system.time(
   
   parLapply(cl,dates_2016,get_filters_safe, year = "2016")
   
 )
 
 system.time(
   
   parLapply(cl,dates_2017,get_filters_safe, year = "2017")
   
 )
 
 system.time(
   
   parLapply(cl,dates_2018,get_filters_safe, year = "2018")
   
 )
 
 system.time(
   
   parLapply(cl,dates_2019,get_filters_safe, year = "2019")
   
 )
 
 print(Sys.time())
 
 system.time(
   
   parLapply(cl,dates_2020,get_filters_safe, year = "2020")
   
 )
 
 print(Sys.time())
 
 system.time(
   
   parLapply(cl,dates_2021,get_filters_safe, year = "2021")
   
 )
 
 print(Sys.time())
 
 system.time(
   
   parLapply(cl,dates_2022,get_filters_safe, year = "2022")
   
 )
 
 
 print(Sys.time())
 
 stopCluster(cl)
 
 
 # 2016:
 # user  system elapsed 
 # 0.62    0.41 4573.33
 # 
 # 2020
 # user  system elapsed 
 # 0.16    0.08 2416.44 
 # 
 # 2021
 # user  system elapsed 
 # 0.13    0.14 2025.64
 # 
 # 2022
 # user  system elapsed 
 # 0.14    0.02 1672.83

# 23 days
 # user  system elapsed 
 # 0.00    0.00  318.57
 
 # 2017, Jan 24 to end of year
 # user  system elapsed 
 # 0.78    0.29 3791.18 
 
 # 2018
 # user  system elapsed 
 # 0.73    0.29 3746.58 63 mins
 
 # 2019
 # user  system elapsed 
 # 0.94    0.39 2920.63 
 
 
# testing with a few days ----
  
  # events_full_2017_01_01 <- readRDS("E:/data/01_raw/GDELT/events/full/2017-01-01_events_full.rds")
  # events_full_2017_01_02 <- readRDS("E:/data/01_raw/GDELT/events/full/2017-01-02_events_full.rds")
  # gkg_water_related_2017_01_01 <- readRDS("E:/data/01_raw/GDELT/gkg/gkg/water-related/2017-01-01_gkg_water_related.rds")
  # gkg_water_related_2017_01_02 <- readRDS("E:/data/01_raw/GDELT/gkg/gkg/water-related/2017-01-02_gkg_water_related.rds")
  # 
  # 
  # events_full_2017_01_01_and_01_02 <- append_dates_general(dates = c("2017-01-01","2017-01-02"))  
  # 
  # water_events_2017_01_01 <- full_join(gkg_water_related_2017_01_01,events_full_2017_01_01,
  #            by = c("documentSource" ="urlSource"),
  #            keep = TRUE)%>% 
  #   mutate(merge = case_when(!is.na(idGKG) & !is.na(idGlobalEvent) ~ "both",
  #                            !is.na(idGKG) & is.na(idGlobalEvent) ~ "left",
  #                             is.na(idGKG) & !is.na(idGlobalEvent) ~ "right",
  #                            TRUE ~ "missing"
  #   ))
  # 
  # test_parsed <- parse_gkg_mentioned_themes(gdelt_data = water_events_2017_01_01)
  # 
  # # check approx what the shakedown is:
  # plot <- ggplot(water_data,
  #        aes(x = merge)) +
  #   geom_bar() +
  #   geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -.5) +
  #   labs(title = "Merging Events and 'Water' in GKG Themes, Jan 2017",
  #        x = "Merge results")+
  #   theme_plot(title_size = 16,
  #              axis_title_x = element_text(color = "black"),
  #              axis_title_y = element_text(color = "black"),
  #              axis_text_x  = element_text(color = "darkgrey"),
  #              axis_text_y  = element_text(color = "darkgrey"))
  # 
  # plot
  # 
  # save_map(output_folder = output_figures,
  #          plotname = plot,
  #          filename = "events_gkg_2017_01.png")
  # 
  # in_gkg_water_events <-  water_events_2017_01_01 %>% 
  #                         filter(merge == "left") %>%
  #                         select(
  #                           idGKG,nameSource.x,
  #                           domainSource,themes,locations,persons,organizations,
  #                           mentionedNamesCounts,mentionedNumericsCounts,documentSource)
  #                        
  # test_themes_parsed <- parse_gkg_mentioned_themes(gdelt_data = in_gkg_water_events[1:100,])
  # 
  # 
  # 