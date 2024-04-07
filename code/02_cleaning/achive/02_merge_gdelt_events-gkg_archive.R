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
  
  
# exploration, read in an example date ----
  


  
# read in the GKG full and GKG water related and Events full
  
  # get in full data for the day, events and just water
  
  # https://stackoverflow.com/questions/48219732/pass-a-string-as-variable-name-in-dplyrfilter
  # pass string as varname into dplyr filter
 
  # set up filters so that we can filter the full events down to whatever filter variable
  
  # filter variable
  location_vars <- "idCountryAction"
  # Malawi, Zambia, Zimbabwe, Angola, Mozambique, Botswana,  Tanzania, Namibia
  
  location_vals <- c("MI","ZA","ZI","AO","MZ","BC","TZ","WA")
  

  
  
  actor_vars    <- c("codeCAMEOTypeActor1","codeCAMEOTypeActor2")
  actor_vals    <- c("GOV","GOV")
  
  
  # merge the events and gkg water for particular dates and with filters allowed
  
  merge_events_gkg_water <- function(date,
                                     location_vars = c("idCountryAction"),
                           location_vals = c("MI","ZA","ZI","AO","MZ","BC","TZ","WA"),
                           file_name_string = "_water_events.rds"
                         #  actor_vars    = c("codeCAMEOTypeActor1","codeCAMEOTypeActor2"),
                          # actor_vals    = c("GOV","GOV")
                           ) {
  
  
  # allow either of the locations given
  filter_location <- paste0(location_vars, "==", "'",location_vals ,"'")
  filter_location <- paste(filter_location, collapse = "|")
  
  # allow either of the actors conditions given
 # filter_actor <- paste0(actor_vars,"==","'",actor_vals,"'")
  #filter_actor <- paste(filter_actor, collapse = "|")
  
  # find out hwo many CAMEO events are missing
  
  #length(is.na(water_data$ca))

  #filter_statement

  # a day is like 8 MB
  # if you write it out
  # events_full <- readRDS(file.path(data_external_raw,"GDELT","events","full",paste0(date,"_events_full.rds"))) %>%
  #               filter(idCountryAction == 'MI' | idCountryAction == "ZA")
   
  
  events_tmp <- readRDS(file.path("E:","data","01_raw","GDELT","events","full",paste0(date,"_events_full.rds"))) %>%
    #filter(eval(rlang::parse_expr(filter_location)))  %>%
    filter(!is.na(urlSource))
  
  # can add later if actors gets more clear
  #%>%
   # filter(eval(rlang::parse_expr(filter_actor)))
                

  # a day is 500-800MB, probably don't need the full thing
  #gkg_full    <- readRDS(file.path(data_external_raw,"GDELT","gkg","gkg","full",paste0(date,"_gkg_full.rds")))
  
  # a day is like 5-80 MB
  
  gkg_water   <- readRDS(file.path("E:","data","01_raw","GDELT","gkg","gkg","water-related",paste0(date,"_gkg_water_related.rds"))) %>%
                  filter(!is.na(documentSource))
  
## merge the events onto the gkg water ----
  
  # by gkg is "documentSource" and for events is "urlSource"
  # merge logs where the match is
  
  sprintf(paste0("Doing join for ",date))
  
  water_events <- gkg_water %>%
                  dplyr::inner_join(events_tmp,
                                   by = c("documentSource" ="urlSource"),
                                   keep = TRUE) %>% 
    mutate(merge = case_when(!is.na(idGKG) & !is.na(idGlobalEvent) ~ "both",
                             !is.na(idGKG) & is.na(idGlobalEvent) ~ "left",
                            # is.na(idGKG) & !is.na(idGlobalEvent) ~ "right",
                             TRUE ~ "missing"
    ))
  
  saveRDS(water_events,
          file = file.path("E:","data","02_temp","GDELT","merge",paste0(date,
                                                                        file_name_string,
                                                                        #paste0(location_vals,collapse="_"),
                                                                     # "_actors_",paste0(actor_vals,collapse="_"),
                                                                     ".rds")))
  
 

  
  
  }
  
# merge all the dates ----
  
  append_dates_general <- function(folder_path = file.path(data_external_raw,"GDELT","events","full"),
                                   base_file_name           = "_events_full.rds",
                                   # actor_vars    = c("codeCAMEOTypeActor1","codeCAMEOTypeActor2"),
                                   #actor_vals    = c("GOV","GOV"),
                                   dates        
  ) {
    
    
    for (date in dates){
      # if it's the first element, initialize
      if (date == dates[1]){
        data <- readRDS(file =paste0(folder_path,"/",paste0(date,base_file_name)))
        
        #
        
      } else{
        tmp <- readRDS(file =paste0(folder_path,"/",paste0(date,base_file_name)))
        
        #
        
        data <- rbind(data,tmp)
      } # end else
      
      
      
    } # end for loop
    
    saveRDS(data,file = paste0(folder_path,"/",
                               paste0(head(dates,1),"_to_",tail(dates,1),
                                      base_file_name)))
    
    return(data)
    
  } # end function
  
  
  append_dates <- function(location_vars = c("idCountryAction"),
                           location_vals = location_vals = c("MI","ZA","ZI","AO","MZ","BC","TZ","WA"),
                           file_name_string    = "_water_events_locations_",
                          # actor_vars    = c("codeCAMEOTypeActor1","codeCAMEOTypeActor2"),
                           #actor_vals    = c("GOV","GOV"),
                           dates        
  ) {
    
    
    for (date in dates){
      # if it's the first element, initialize
    if (date == dates[1]){
      data <- readRDS(file =file.path(data_external_temp,"GDELT","merge",paste0(date,
                                                                                file_name_string, paste0(location_vals,collapse="_"),
                                                                                #"_actors_",paste0(actor_vals,collapse="_"),
                                                                                ".rds")))
      
      #
      
    } else{
      tmp <- readRDS(file =file.path(data_external_temp,"GDELT","merge",paste0(date,
                                                                               file_name_string, paste0(location_vals,collapse="_"),
                                                                             # "_actors_",paste0(actor_vals,collapse="_"),
                                                                             ".rds")))
      
      #
      
      data <- rbind(data,tmp)
    } # end else
    
    
    
    } # end for loop
    
    saveRDS(data,file = file.path(data_external_temp,"GDELT","merge",
                                  paste0(head(dates,1),"_to_",tail(dates,1),
                                         file_name_string, paste0(location_vals,collapse="_"),
                                         #"_actors_",paste0(actor_vals,collapse="_"),
                                         ".rds")))
    
    return(data)
    
  } # end function
  

# get myself a year of data
  
  start_year  <- "2017"
  start_md    <- "0101"
  
  end_year    <- "2017"
  end_md      <- "1231"
  
  
  start_ymd <- paste0(start_year,start_md)
  end_ymd   <- paste0(end_year,end_md)
  

  
  
  # create date sequence
  dates <- as.character(seq(as.Date(start_ymd,"%Y%m%d"), as.Date(end_ymd,"%Y%m%d"), by = "day"))
  
  # remove 2017-07-25 since that's producing errors
  
  dates <- dates[!dates %in% c("2017-07-25")]
  
  location_vals <- c("MI","ZA","ZI","AO","MZ","BC","TZ","WA")
  
  system.time(
  merge_events_gkg_water(date = "2017-01-01"
                           )
  
  )
  # 2016 in Zambezi countries: c("MI","ZA","ZI","AO","MZ","BC","TZ","WA")
  # user  system elapsed 
  # 226.35    4.10 1371.27 
  # 
  
  library(parallel)
  n_cores <- detectCores() - 4
  
  cl <- makeCluster(n_cores)
  clusterExport(cl, varlist = c('data_external_temp','location_vals'))
  
  clusterEvalQ(cl, {
    #library(gdeltr2) 
    library(tidyverse)
  })
  
  before <- Sys.time()
  
  
  #   date <- "2017-07-25"

   system.time(
    
    parLapply(cl,dates,merge_events_gkg_water)
  )
   
   # Error in checkForRemoteErrors(val) : 
   # one node produced an error: Join columns in `x` must be present in the data.
   # ✖ Problem with `documentSource`.
   # 0.23 0.03 212.8
   
   # user  system elapsed 
   # 0.04    0.05   78.74 
  
  stopCluster(cl)
  

  
  after <- Sys.time()
  print(paste0("Current time is ",after,"."))
  print(sprintf("That iteration took %.2f mins.", after - before))
  
  
  

  
  system.time(
  water_data <- append_dates(location_vals = location_vals,
                             dates = dates)
  )
  
 
  # 2016 in Zambezi countries: c("MI","ZA","ZI","AO","MZ","BC","TZ","WA")
  # user  system elapsed 
  # 30.15    3.42   76.70 
  
  # 2017 in in Zambezi countries: c("MI","ZA","ZI","AO","MZ","BC","TZ","WA")
  # note 2017-07-25 is missing
  # user  system elapsed 
  # 19.36    1.96   58.75 
  
   water_data <- readRDS(file = file.path(data_external_temp,"GDELT","merge",
                                         paste0(head(dates,1),"_to_",tail(dates,1),
                                                "_water_events_locations_", paste0(location_vals,collapse="_"),
                                                #"_actors_",paste0(actor_vals,collapse="_"),
                                                ".rds")))
  
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
  ggplot(water_events_2017_01_01,
         aes(x = merge)) +
    geom_bar() +
    geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -.5)
  
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
                           "TAX_WORLDBIRDS_STREAKED_SHEARWATER",
                           "TAX_WORLDBIRDS_WHITEBREASTED_WATERHEN",
                           "TAX_DISEASE_BLACKWATER_FEVER",
                           "TAX_DISEASE_WATERING_EYE",
                           "TAX_DISEASE_REDWATER",
                           "TAX_DISEASE_WATERMELON_STOMACH",
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
                           "NATURAL_DISASTER_HIGH_WATERS",
                           "TAX_DISEASE_WATERBORNE_DISEASES",
                           "NATURAL_DISASTER_HIGH_WATERS",
                           "TAX_DISEASE_WATERBORNE_DISEASE",
                           "TAX_DISEASE_WATERBORNE_DISEASE",
                           "TAX_AIDGROUPS_WATERAID",
                           "TAX_DISEASE_WATERBORNE_ILLNESS",
                           "TAX_DISEASE_WATER_INTOXICATION",
                           "TAX_AIGROUPS_WATERAID_AMERICA",
                           "TAX_AIDGROUPS_MILLENNIUM_WATER_ALLIANCE",
                           
                           
                           )
  