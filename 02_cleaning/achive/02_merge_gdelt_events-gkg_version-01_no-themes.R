# _______________________________#
# Environment
# Clean 02: Glean GDELT Events
# 
# Stallman
# Started 2023-05-29
# Last edited: 
#________________________________#


# https://github.com/abresler/gdeltr2


# Startup

rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))
  
  library(tidyverse)
  library(gdeltr2)
  
  # see here:
  # https://rpubs.com/BrendanKnapp/GDELT_Syrian_Conflict
  
# merge the events and gkg water for particular dates ----
  
  merge_events_gkg_water <- function(date,
                                     gkg_filename = "_gkg_water_related.rds",
                                     events_filename = "_events_full.rds",
                                     events_path      = file.path("E:","data","01_raw","GDELT","events","full"),
                                     gkg_path         = file.path("E:","data","01_raw","GDELT","gkg","gkg","water-related"),
                                     output_path      = file.path("E:","data","02_temp","GDELT","merge","daily","full"),
                                     output_filename  = "_water_gkg_events.rds"
                           
                           ) {
  events_tmp <- readRDS(file = paste0(events_path, "/",paste0(date,"_events_full.rds"))) %>%
    filter(!is.na(urlSource))

  # a day is like 5-80 MB
  
  gkg_water   <- readRDS(file = paste0(gkg_path,"/",paste0(date,"_gkg_water_related.rds"))) %>%
                  filter(!is.na(documentSource))
  
  # by gkg is "documentSource" and for events is "urlSource"
  # merge variable documents what kind of match
  sprintf(paste0("Doing join for ",date))
  
  water_events <- gkg_water %>%
                  dplyr::full_join(events_tmp,
                                   by = c("documentSource" ="urlSource"),
                                   keep = TRUE,
                                   relationship = "many-to-many") %>% 
    mutate(merge = case_when(!is.na(idGKG) & !is.na(idGlobalEvent) ~ "both",
                             !is.na(idGKG) & is.na(idGlobalEvent)  ~ "gkg",
                              is.na(idGKG) & !is.na(idGlobalEvent) ~ "events",
                             TRUE ~ "missing"
    ))
  
  saveRDS(water_events,
          file = paste0(output_path,"/",paste0(date,base_file_name)))
  
 # clean out memory
  rm(water_events)
  rm(events_tmp)
  rm(gkg_water)
  gc()
  
  }
  
# merge all the dates ----
  
  append_dates <- function(input_path              = file.path("E:","data","02_temp","GDELT","merge","daily","full"),
                           output_path             = file.path("E:","data","02_temp","GDELT","merge","yearly","full"),
                           base_file_name          = "_water_gkg_events.rds",
                           dates        
  ) {
    
    for (date in dates){
      # if it's the first element, initialize
      if (date == dates[1]){
        data <- readRDS(file =file.path(input_path,paste0(date,base_file_name)))
        print(paste0("Reading in for ",date))
        #
        
      } else{
        print(paste0("Reading in for ",date))
        
        tmp <- readRDS(file =file.path(input_path,paste0(date,base_file_name)))
        
        #
        
        data <- rbind(data,tmp)
        print(paste0("Appended ",date))
        
      } # end else
      
      
      
    } # end for loop
    
    print(paste0("Finished, saving. "))
    saveRDS(data,file = file.path(output_path,
                                  paste0(head(dates,1),"_to_",tail(dates,1),
                                      base_file_name)))
    
    return(data)
    
  } # end function
  

  
# get myself a year of data
  
  start_year  <- "2017"
  start_md    <- "0101"
  
  end_year    <- "2017"
  end_md      <- "0131"
  
  
  start_ymd <- paste0(start_year,start_md)
  end_ymd   <- paste0(end_year,end_md)
  

  
  
  # create date sequence
  dates <- as.character(seq(as.Date(start_ymd,"%Y%m%d"), as.Date(end_ymd,"%Y%m%d"), by = "day"))
  
  # remove 2017-07-25 since that's producing errors
  
  dates <- dates[!dates %in% c("2017-07-25")]
  
  # system.time(
  # merge_events_gkg_water(date = "2017-01-01"
  #                          )
  # 
  # )
  
  # single day, no filtering
  # user  system elapsed 
  # 6.64    0.11   13.08 
  

  
  library(parallel)
  n_cores <- detectCores() - 4
  
  cl <- makeCluster(n_cores)
  #clusterExport(cl, varlist = c('data_external_temp'))
  
  clusterEvalQ(cl, {
    #library(gdeltr2) 
    library(tidyverse)
  })
  
  before <- Sys.time()
  
  
  #   date <- "2017-07-25"

   system.time(
    
    parLapply(cl,dates,merge_events_gkg_water)
  )
   

   # 2017 with no location restriction
   # user  system elapsed 
   # 3.00    1.21 1377.30
   
  stopCluster(cl)
  

  
  after <- Sys.time()
  print(paste0("Current time is ",after,"."))
  print(sprintf("That iteration took %.2f mins.", after - before))
  
  
  

  # get this for 
  system.time(
  gkg_water <- append_dates(dates = dates,
                             input_path              = file.path("E:","data","01_raw","GDELT","gkg","gkg","water-related"),
                             output_path             = file.path("E:","data","02_temp","GDELT","gkg","gkg","water-related","filtered","yearly"),
                             base_file_name          = "_gkg_water_related.rds"
                             )
  
  )
  
  # user  system elapsed 
  # 374.27    7.50  940.81
 
  # 2016 in Zambezi countries: c("MI","ZA","ZI","AO","MZ","BC","TZ","WA")
  # user  system elapsed 
  # 30.15    3.42   76.70 
  
  # 2017 in in Zambezi countries: c("MI","ZA","ZI","AO","MZ","BC","TZ","WA")
  # note 2017-07-25 is missing
  # user  system elapsed 
  # 19.36    1.96   58.75 
  
  # 2017 January, all locations, just gkg
  # size 
  


  # 2017 January, all locations
  object.size(gkg_water)
  
   water_data <- readRDS(file = file.path(data_external_temp,"GDELT","merge",
                                         paste0(head(dates,1),"_to_",tail(dates,1),
                                                "_water_gkg_events.rds")
                                         )
                         )
  
  nrow(water_data)
  #[1] 128553 # is for the 8 countries in Zambezi for year 2016
  # [1] 94028
  
  # select the interesting cols
  peruse <-   water_data %>% select(#merge,
                                    dateEvent,
                                    urlSource,nameQuad,
                                    #dateTimeDocument.x,
                                    #documentSource,dateTimeDocument.y,
                          codeActor1,codeActor2,
                          idCAMEOEvent,
                          locationAction, nameActor1, nameActor2,
                          idGKG,idGlobalEvent,codeCAMEOTypeActor1,codeCAMEOTypeActor2,
                          idDateTimeArticle,nameSource.x,nameSource.y,
                          domainSource,themes,locations,persons,organizations,
                          mentionedNamesCounts,mentionedNumericsCounts,idGlobalEvent,monthYearEvent,
                          yearEvent,idCAMEOEventBase,idCAMEOEventRoot,
                          locationActor1,idCountryActor1,idADM1CodeActor1,locationActor2,idCountryActor2,
                          idADM1CodeActor2)
  
  peruse_gov_gov <- peruse %>% 
              filter(codeCAMEOTypeActor1 == "GOV" & codeCAMEOTypeActor2 == "GOV")  # %>%
              #filter(grepl('ZA',locations)) # with MI for Malawi in the location of the action
              
  peruse_bus <- peruse %>% 
    filter(codeCAMEOTypeActor1 == "BUS" | codeCAMEOTypeActor2 == "BUS")
  
  length(unique(peruse_2$urlSource))
  
  
  codes_gkg_themes <-get_codes_gkg_themes() # 59,315 possible themes
  countries_cameo_gdelt <- get_codes_cameo_country()
  type_cameo <- get_codes_cameo_type()
  codes_cameo_events <- get_codes_cameo_events()
  

# merge all year events ----
  
  


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
  
  # 200 themes contain "water" in them
  
  exclude_themes_list <- c("TAX_WEAPONS_WATER_CANNONS",
                           "TAX_WEAPONS_WATER_CANNON",
                           "TAX_WORLDBIRDS_WATERFOWL",
                           "HUMAN_RIGHTS_ABUSES_WATERBOARDING",
                           "TAX_FNCACT_WATERMAN",
                           "HUMAN_RIGHTS_ABUSES_WATERBOARDED",
                           "TAX_WORLDMAMMALS_WATER_BUFFALO",
                           "HUMAN_RIGHTS_ABUSES_WATERBOARD",
                           "TAX_FNCACT_WATER_DOG",
                           "TAX_FNCACT_WATERMEN",
                           "TAX_FNCACT_WATERBOY",
                           "TAX_FNCACT_WATER_DOGS",
                           "TAX_WORLDMAMMALS_WATER_BUFFALOS",
                           "TAX_FNCACT_WATER_BOY",
                           "TAX_FNCACT_WATERBOYS",
                           "TAX_FNCACT_WATER_BOYS",
                           "TAX_WORLDFISH_FRESHWATER_EEL",
                           "TAX_WORLDBIRDS_WATER_RAIL",
                           "TAX_WORLDMAMMALS_WATER_BUFFALOS",
                           "TAX_WORLDMAMMALS_WATERBUCK",
                           "TAX_FNCACT_WATERMANS",
                           "TAX_DISEASE_REDWATER",
                           "TAX_WORLDBIRDS_MANX_SHEARWATERS",
                           "TAX_WORLDBIRDS_MANX_SHEARWATER",
                           "HUMAN_RIGHTS_ABUSES_WATERBOARDS",
                           "TAX_WORLDBIRDS_SHORTTAILED_SHEARWATERS",
                           "TAX_WORLDBIRDS_WATERFOWLS",
                           "TAX_WORLDBIRDS_WEDGETAILED_SHEARWATERS",
                           "TAX_WORLDBIRDS_SHORTTAILED_SHEARWATER",
                           "TAX_WORLDMAMMALS_CHINESE_WATER_DEER",
                           "TAX_WORLDMAMMALS_WATERBUCKS",
                           "TAX_WORLDBIRDS_LOUISIANA_WATERTHRUSH",
                           "TAX_WORLDREPTILES_NORTHERN_WATER_SNAKE",
                           "TAX_WORLDBIRDS_SOOTY_SHEARWATER",
                           "TAX_WORLDBIRDS_NORTHERN_WATERTHRUSH",
                           "TAX_DISEASE_WATERMELON_STOMACH",
                           "TAX_WORLDFISH_FRESHWATER_EELS",
                           "TAX_WORLDBIRDS_WATER_RAILS",
                           "TAX_WORLDREPTILES_NORTHERN_WATER_SNAKES",
                           "TAX_WORLDBIRDS_HUTTON_SHEARWATER",
                           "TAX_WORLDBIRDS_GREAT_SHEARWATERS",
                           "TAX_WORLDBIRDS_WATER_PIPITS",
                           "TAX_WORLDMAMMALS_WATER_CHEVROTAIN",
                           "TAX_WORLDBIRDS_WEDGETAILED_SHEARWATER",
                           "TAX_WORLDREPTILES_DIAMONDBACK_WATER_SNAKE",
                           "TAX_WORLDBIRDS_FLESHFOOTED_SHEARWATER",
                           "TAX_WORLDBIRDS_WATER_PIPIT",
                           "TAX_WORLDBIRDS_CORY_SHEARWATERS",
                           "TAX_WORLDFISH_FRESHWATER_SHARKS",
                           "TAX_WORLDBIRDS_GREAT_SHEARWATER",
                           "TAX_WORLDFISH_TIDEWATER_GOBY",
                           "TAX_WORLDFISH_FRESHWATER_SHARK",
                           "TAX_WORLDBIRDS_FLESHFOOTED_SHEARWATERS",
                           "TAX_WORLDREPTILES_BANDED_WATER_SNAKE",
                           "TAX_DISEASE_BLACKWATER_FEVER",
                           "TAX_WORLDBIRDS_CORY_SHEARWATER",
                           "TAX_DISEASE_WATERING_EYE",
                           "TAX_WORLDBIRDS_STREAKED_SHEARWATER",
                           "TAX_WORLDBIRDS_WHITEBREASTED_WATERHEN",
                           "TAX_DISEASE_HEARTWATER",
                           "TAX_WORLDMAMMALS_WATER_OPOSSUM",
                           "TAX_WORLDREPTILES_BANDED_WATER_SNAKES",
                           "TAX_WORLDBIRDS_PINKFOOTED_SHEARWATER",
                           "TAX_WORLDBIRDS_HUTTON_SHEARWATERS",
                           "TAX_WORLDFISH_FRESHWATER_HERRING",
                           "TAX_WORLDBIRDS_FLUTTERING_SHEARWATERS",
                           "TAX_WORLDBIRDS_STREAKED_SHEARWATERS",
                           "TAX_WORLDBIRDS_PLUMBEOUS_WATER_REDSTART",
                           "TAX_WORLDBIRDS_FLUTTERING_SHEARWATER",
                           "TAX_WORLDBIRDS_WHITEBREASTED_WATERHENS",
                           "TAX_WORLDREPTILES_DIAMONDBACK_WATER_SNAKES",
                           "TAX_WORLDMAMMALS_EURASIAN_WATER_SHREW",
                           "TAX_WORLDBIRDS_BULLER_SHEARWATER",
                           "TAX_WORLDBIRDS_AUDUBON_SHEARWATERS",
                           "TAX_WORLDMAMMALS_EURASIAN_WATER_SHREWS",
                           "TAX_WORLDFISH_DEEPWATER_FLATHEAD",
                           "TAX_WORLDBIRDS_SHORT_TAILED_SHEARWATERS",
                           "TAX_WORLDBIRDS_WATERCOCK",
                           "TAX_WORLDBIRDS_AUDUBON_SHEARWATER",
                           "TAX_WORLDBIRDS_BULLER_SHEARWATERS",
                           "TAX_WORLDBIRDS_PINKFOOTED_SHEARWATERS",
                           "TAX_DISEASE_BLACK_WATER_FEVER",
                           "TAX_WORLDBIRDS_LITTLE_SHEARWATER",
                           "TAX_WORLDBIRDS_WHITE_BREASTED_WATERHEN",
                           "TAX_WORLDBIRDS_BLACKVENTED_SHEARWATER",
                           "TAX_WORLDBIRDS_TOWNSEND_SHEARWATER",
                           "TAX_DISEASE_HEARTWATER_DISEASE",
                           "TAX_WORLDBIRDS_MEDITERRANEAN_SHEARWATER",
                           "TAX_WORLDBIRDS_PLUMBEOUS_WATERREDSTART",
                           "TAX_WORLDBIRDS_WEDGE_TAILED_SHEARWATER",
                           "TAX_WORLDBIRDS_FLESH_FOOTED_SHEARWATER",
                           "TAX_WORLDBIRDS_WATERCOCKS",
                           "TAX_WORLDBIRDS_BLACKVENTED_SHEARWATERS",
                           "TAX_DISEASE_WATERELECTROLYTE_IMBALANCE",
                           "TAX_WORLDMAMMALS_JAPANESE_WATER_SHREW",
                           "TAX_WORLDBIRDS_WATERFALL_SWIFT",
                           "TAX_WORLDMAMMALS_AMERICAN_WATER_SHREW",
                           "TAX_WORLDBIRDS_FLESH_FOOTED_SHEARWATERS",
                           "TAX_WORLDBIRDS_LITTLE_SHEARWATERS",
                           "TAX_WORLDMAMMALS_ELEGANT_WATER_SHREW",
                           "TAX_WORLDFISH_NORTH_AMERICAN_FRESHWATER_CATFISH",
                           "TAX_WORLDMAMMALS_FALSE_WATER_RAT",
                           "TAX_WORLDMAMMALS_ELEGANT_WATER_SHREWS",
                           "TAX_WORLDBIRDS_MEDITERRANEAN_SHEARWATERS",
                           "TAX_WORLDFISH_DEEPWATER_STINGRAY",
                           "TAX_WORLDBIRDS_WHITE_CAPPED_WATER_REDSTART",
                           "TAX_WORLDBIRDS_WATER_THICKKNEE",
                           "TAX_WORLDBIRDS_LUZON_WATER_REDSTART",
                           "TAX_WORLDMAMMALS_WATER_OPOSSUMS",
                           "TAX_WORLDBIRDS_WATERFALL_SWIFTS",
                           "TAX_WORLDBIRDS_PIED_WATER_TYRANT",
                           "TAX_WORLDREPTILES_FLORIDA_BANDED_WATER_SNAKE",
                           "TAX_WORLDFISH_FRESHWATER_HATCHETFISH",
                           "TAX_WORLDBIRDS_PIED_WATER_TYRANTS",
                           "TAX_WORLDREPTILES_REDBELLY_WATER_SNAKE",
                           "TAX_WORLDREPTILES_REDBELLY_WATER_SNAKES",
                           "TAX_WORLDMAMMALS_WATERHOUSE_LEAFNOSED_BAT",
                           "TAX_WORLDBIRDS_WEDGE_TAILED_SHEARWATERS",
                           "TAX_WORLDMAMMALS_WATER_CHEVROTAINS",
                           "TAX_WORLDMAMMALS_COMMON_WATER_RAT",
                           "TAX_WORLDBIRDS_PLUMBEOUS_WATER_REDSTARTS",
                           "TAX_WORLDBIRDS_PERSIAN_SHEARWATER",
                           "TAX_WORLDMAMMALS_FALSE_WATER_RATS",
                           "TAX_WORLDBIRDS_SHORT_TAILED_SHEARWATER",
                           "TAX_DISEASE_WATERCLEAR_CELL_ADENOMA",
                           "TAX_WORLDBIRDS_WATER_THICKKNEES",
                           "TAX_WORLDBIRDS_WHITE_BREASTED_WATERHENS",
                           "TAX_WORLDBIRDS_NORTHERN_WATERTHRUSHS",
                           "TAX_WORLDBIRDS_WATER_THICK_KNEES",
                           "TAX_WORLDFISH_DEEPWATER_STINGRAYS",
                           "TAX_WORLDBIRDS_DRAB_WATER_TYRANT",
                           "TAX_WORLDBIRDS_PIED_WATERTYRANT",
                           "TAX_WORLDBIRDS_PERSIAN_SHEARWATERS"
                           )
  
  include_themes_list <- c("CRISISLEX_C06_WATER_SANITATION",
                           "WATER_SECURITY",
                           "UNGP_CLEAN_WATER_SANITATION",
                           "ENV_WATERWAYS",
                           "NATURAL_DISASTER_WATER_LEVEL",
                           "NATURAL_DISASTER_FLOODWATERS",
                           "NATURAL_DISASTER_HIGH_WATER",
                           "NATURAL_DISASTER_FLOOD_WATERS",
                           "NATURAL_DISASTER_FLOODWATER",
                           "NATURAL_DISASTER_FLOOD_WATER",
                           "TAX_DISEASE_WATERBORNE_DISEASES",
                           "NATURAL_DISASTER_HIGH_WATERS",
                           "TAX_DISEASE_WATERBORNE_DISEASE",
                           "TAX_AIDGROUPS_WATERAID",
                           "TAX_DISEASE_WATERBORNE_ILLNESS",
                           "TAX_DISEASE_WATER_INTOXICATION",
                           "TAX_AIGROUPS_WATERAID_AMERICA",
                           "TAX_AIDGROUPS_MILLENNIUM_WATER_ALLIANCE",
                           "TAX_AIDGROUPS_SINGLE_DROP_FOR_SAFE_WATER",
                           "WB_137_WATER",
                           "WB_1462_WATER_SANITATION_AND_HYGIENE",
                           "WB_1199_WATER_SUPPLY_AND_SANITATION",
                           "WB_139_SANITATION_AND_WASTEWATER",
                           "WB_140_AGRICULTURAL_WATER_MANAGEMENT",
                           "WB_141_WATER_RESOURCES_MANAGEMENT",
                           "WB_1000_WATER_MANAGEMENT_STRUCTURES",
                           "WB_138_WATER_SUPPLY",
                           "WB_1805_WATERWAYS",
                           "WB_2008_WATER_TREATMENT",
                           "WB_427_WATER_ALLOCATION_AND_WATER_ECONOMICS",
                           "WB_1064_WATER_DEMAND_MANAGEMENT",
                           "WB_159_TRANSBOUNDARY_WATER",
                           "WB_1798_WATER_POLLUTION",
                           "WB_1021_WATER_LAW",
                           "WB_144_URBAN_WATER",
                           "WB_1063_WATER_ALLOCATION_AND_WATER_SUPPLY",
                           "WB_143_RURAL_WATER",
                           "WB_1215_WATER_QUALITY_STANDARDS",
                           "WB_1998_WATER_ECONOMICS",
                           "WB_149_WASTEWATER_TREATMENT_AND_DISPOSAL",
                           "WB_2971_WATER_PRICING",
                           "WB_2981_DRINKING_WATER_QUALITY_STANDARDS",
                           "WB_2009_WATER_QUALITY_MONITORING",
                           "WB_1778_FRESHWATER_ECOSYSTEMS",
                           "WB_155_WATERSHED_MANAGEMENT",
                           "WB_156_GROUNDWATER_MANAGEMENT",
                           "WB_1790_INTERNATIONAL_WATERWAYS",
                           "WB_2005_COMMUNITY_WATER_SUPPLY_MANAGEMENT",
                           "WB_150_WASTEWATER_REUSE",
                           "WB_2992_FRESHWATER_FISHERIES",
                           "WB_1729_URBAN_WATER_FINANCIAL_SUSTAINABILITY",
                           "WB_1731_NON_REVENUE_WATER",
                           "WB_3014_WASTEWATER_DISPOSAL_FACILITIES",
                           "WB_2007_WATER_SAFETY_PLANS",
                           "WB_1220_SURFACE_WATER_MANAGEMENT",
                           "WB_1941_WATERBORNE_TRANSPORT",
                           "WB_1002_IRRIGATION_WATER_QUALITY",
                           "WB_142_ENERGY_AND_WATER",
                           "WB_423_INTEGRATED_URBAN_WATER_MANAGEMENT",
                           "WB_991_DRAINAGE_WATER_QUALITY",
                           "WB_2978_WATER_POLLUTION_LOAD",
                           "WB_157_ENVIRONMENTAL_WATER_USE_AND_CATCHMENT_PROTECTION",
                           "WB_1995_ENERGY_FOR_WATER_AND_WASTEWATER_TREATMENT",
                           "WB_1996_ENERGY_RECOVERY_FROM_WASTEWATER",
                           "WB_1197_ENERGY_EFFICIENCY_IN_WATER_AND_WASTEWATER_UTILITIES",
                           "WB_2972_GROUNDWATER_CONJUNCTIVE_USE"
                           )
                           
                           
                           
                           
  