# _______________________________#
# Environment
# Clean 02: Glean GDELT GKG: Filter Themes
# 
# Stallman
# Started 2023-05-29
# Last edited: 
#________________________________#


# See 02_log_gdelt-relevant-themes-and-countries.R for theme and country names
# SEE 00_startup_parameters for the vector include_themes which has all the themes

# filter by a smaller set of countries (those sharing the Zambezi river basin)
# and also merge together the formal agreements by URL

# Startup

rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))
  


  


 
 
 
 
 # function ----
 
 
 scrape_and_save <- function(year = "2016") {
 
   # packages
   library(RSelenium)
   
   library(rvest)
   
   library(tidyverse)

   library(netstat)
   
   # functions ----
   
   print(paste0("Working on ",year))
   
   merge_data_by_theme <- function(themes= c("WATER_SECURITY", 
                                             "ENV_HYDRO"),
                                   input_path  = file.path("C:","data_C_drive","02_temp","GDELT","merged","filtered","africa-country-theme","yearly","zambezi_countries"),
                                   input_filename  = "_formal_agreements_af_gkg_events.rds",
                                   name_of_filter  = "zambezi_countries",
                                   output_path      = file.path("C:","data_C_drive","02_temp","GDELT","merged","filtered","africa-country-theme","yearly","zambezi_countries"),
                                   output_filename  = "_formal_agreements_water_gkg_events.rds",
                                   output_subfolder = "formal_agreements",
                                   year = "2017",
                                   remove = TRUE,
                                   format = "csv"
                                   
   ) {
     
     # Create output folder ----
     input_path <- file.path(input_path, year)
     
     output_path <- file.path(output_path,year,output_subfolder)
     
     if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE) # recursive lets you create any needed subdirectories
     
     # Packages ----

     library(stringi)
     library(tidyverse)
     
     
     
     for (theme in themes){
       # if it's the first element, initialize the df
       if (theme == themes[1]){
         
         data <- readRDS(file =file.path(input_path,paste0(year,"_",theme,"_",name_of_filter,input_filename)))
         
         
       } else{ # if not first element then append
         
         tmp_path <- file.path(input_path,paste0(year,"_",theme,"_",name_of_filter,input_filename))
         
         if (!file.exists(tmp_path)){ # if file is missing just say it's missing
           print(paste0("File missing for ",theme))
           
         } else{
           
           
           tmp <- readRDS(file = tmp_path)
           
           data <- rbind(data,tmp)
           
         } # end else file.exists
       }# end else if not the first theme
       
     } # end for loop
     
     # keep only the distinct observations
     data <- data %>% distinct()
     
     # add binary variables for whether it's in each theme
     
     for (theme in themes) {
       
       themes_filter <- grepl(theme, data$themes)
       
       data[theme] <- stringi::stri_detect_fixed(str = data$themes,
                                                 pattern = theme
       )
     }
     
     out_path <- file.path(output_path,paste0(year,output_filename))
     
     
     saveRDS(data,
             file = out_path)
     
     
     csv_path <- gsub(pattern = ".rds", replacement = ".csv", x = out_path)
     
     
     
     if (format == "csv") {
       library(readr)
       readr::write_csv(data,
                        file =csv_path)
     } else if (format == "xlsx"){
      
       library(writexl)
       writexl::write_xlsx(data,
                           file = csv_path)
     }
     
     rm(out_path,csv_path)
     
     if (remove == TRUE){
       rm(data) 
     } else{
       return(data)
     }
     gc()
     
   }
   
   save_rds_csv <- function(data,
                            output_path,
                            date,
                            output_filename,
                            remove = TRUE,
                            csv_vars = c("all"),
                            format   = "csv"){
     
     
     if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE) # recursive lets you create any needed subdirectories
     
     out_path <- file.path(output_path,
                           paste0(date,
                                  output_filename))
     
     saveRDS(data,file = out_path)
     
     csv_path <- gsub(pattern = ".rds", replacement = ".csv", x = out_path)
     
     if (csv_vars[1] == "all") {
       csv_data <- data
     } else {
       csv_data <- data[,csv_vars]
     }
     
     if (format == "csv") {
       if (!require("readr")) install.packages("readr")
       library(readr)
       readr::write_csv(csv_data,
                        file =csv_path)
     } else if (format == "xlsx"){
       if (!require("writexl")) install.packages("writexl")
       library(writexl)
       xlsx_path <- gsub(pattern = ".rds", replacement = ".xlsx", x = out_path)
       
       writexl::write_xlsx(csv_data,
                           path = xlsx_path)
     }
     if (remove == TRUE){
       rm(data,csv_data) 
     } else{
       rm(csv_data)
       return(data)
     }
   }
     
     
   # parameters
   
   include_themes <- c(
     "WATER_SECURITY", 
     #"ENV_WATERWAYS", # mostly related to transportation or things happening on rivers, not water quantity
     "ENV_HYDRO",
     #"WB_137_WATER", # way too broad
     "WB_138_WATER_SUPPLY",
     "WB_140_AGRICULTURAL_WATER_MANAGEMENT",
     "WB_141_WATER_RESOURCES_MANAGEMENT",
     "WB_142_ENERGY_AND_WATER", # really small
     "WB_143_RURAL_WATER",
     "WB_144_URBAN_WATER",
     "WB_155_WATERSHED_MANAGEMENT",
     "WB_156_GROUNDWATER_MANAGEMENT",
     #"WB_157_ENVIRONMENTAL_WATER_USE_AND_CATCHMENT_PROTECTION", 0 hits from 2016 to 2022 in Africa
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
     #"UNGP_FORESTS_RIVERS_OCEANS", too broad
   )
   
 admissible_actions <- c("Consider policy option",
                         "Acknowledge or claim responsibility",
                         "Deny responsibility",
                         "Express accord",
                         "APPEAL",
                         "Appeal for material cooperation, not specified below",
                         "Appeal for economic cooperation",
                         "Appeal for diplomatic cooperation, such as policy support",
                         "Appeal for aid, not specified below",
                         "Appeal for economic aid",
                         "Appeal for humanitarian aid",
                         "Appeal for policy change",
                         "Appeal to yield",
                         "Appeal for target to allow international involvement (non-mediation)",
                         "Appeal to others to meet or negotiate",
                         "Appeal to others to settle dispute",
                         "Appeal to others to engage in or accept mediation",
                         "EXPRESS INTENT TO COOPERATE",
                         "Express intent to cooperate, not specified below",
                         "Express intent to engage in material cooperation,  not specified below",
                         "Express intent to cooperate economically",
                         "Express intent to provide diplomatic cooperation such as policy support",
                         "Express intent to provide matyerial aid, not specified below",
                         "Express intent to provide economic aid",
                         "Express intent to provide humanitarian aid",
                         "Express intent to change policy",
                         "Express intent allow international involvement (not mediation)",
                         "Express intent to meet or negotiate",
                         "Express intent to settle dispute",
                         "Express intent to accept mediation",
                         "Express intent to mediate",
                         "Make a visit",
                         "Host a visit",
                         "Meet at a ÒhirdÓlocation",
                         "Mediate",
                         "Engage in negotiation",
                         "ENGAGE IN DIPLOMATIC COOPERATION",
                         "Engage in diplomatic cooperation, not specified below",
                         "Sign formal agreement",
                         "ENGAGE IN MATERIAL COOPERATION",
                         "Engage in material cooperation, not specified below",
                         "Cooperate economically",
                         "PROVIDE AID",
                         "Provide aid, not specified below",
                         "Provide economic aid",
                         "Provide humanitarian aid",
                         "YIELD",
                         "Yield, not specified below",
                         "Accede to demands for change in policy",
                         "Accede to demands for change in institutions, regime",
                         "Return, release property",
                         "Allow international involvement not specified below",
                         "DEMAND",
                         "Demand, not specified below",
                         "Demand economic cooperation",
                         "Demand economic aid",
                         "Demand political reform, not specified below",
                         "Demand policy change",
                         "Demand that target allows international involvement (non-mediation)",
                         "Demand meeting, negotiation",
                         "DISAPPROVE",
                         "Disapprove, not specified below",
                         "Criticize or denounce",
                         "Accuse, not specified below",
                         "Accuse of crime, corruption",
                         "Complain officially",
                         "REJECT",
                         "Reject, not specified below",
                         "Reject material cooperation",
                         "Reject economic cooperation",
                         "Reject request or demand for material aid, not specified below",
                         "Reject request for economic aid",
                         "Reject request for humanitarian aid",
                         "Reject request for policy change",
                         "Refuse to yield, not specified below",
                         "Refuse to allow international involvement (non mediation)",
                         "Reject proposal to meet, discuss, or negotiate",
                         "Reject plan, agreement to settle dispute",
                         "Defy norms, law",
                         "THREATEN",
                         "Threaten, not specified below",
                         "Threaten non-force, not specified below",
                         "Threaten to reduce or stop aid",
                         "Threaten to reduce or break relations",
                         "Threaten to halt negotiations",
                         "Threaten to halt mediation",
                         "Threaten to halt international involvement (non-mediation)",
                         "REDUCE RELATIONS",
                         "Reduce relations, not specified below",
                         "Reduce or break diplomatic relations",
                         "Reduce or stop aid, not specified below",
                         "Reduce or stop economic assistance",
                         "Reduce or stop humanitarian assistance",
                         "Halt negotiations",
                         "Halt mediation"
 )

 
  
  
 
  csv_vars <- c("idGKG","idDateTimeArticle","idCAMEOEvent","locations","organizations","themes","urlSource","domainSource","stub",
                "working_url","relevant","title","text","duplicate","certainty","paywall","notes")
  
  
  
# start working

    # set up AllAfrica's scraping
    # login via scraping
 
    
    merged_data <- merge_data_by_theme(year = year,
                        themes = include_themes,
                        input_path  = file.path("C:","data_C_drive","02_temp","GDELT","merged","filtered","africa-country-theme","yearly","zambezi_countries"),
                        input_filename  = "_af_gkg_events.rds",
                        name_of_filter  = "zambezi_countries",
                        output_path      = file.path("C:","data_C_drive","02_temp","GDELT","merged","filtered","africa-country-theme","yearly","zambezi_countries"),
                        output_filename  = "_zambezi_countries_water_gkg_events.rds",
                        output_subfolder = "full",
                        remove = FALSE) %>%
                  mutate(working_url = urlSource,
                         relevant    = 0,
                         text = "",
                         title = stub,
                         duplicate = 0,
                         certainty = "",
                         paywall   = 0,
                         notes     = ""
                         )
    
    merged_data <- merged_data %>% 
                filter(domainSource!="www.jowhar.com")
    
    merged_data_allafrica <- merged_data %>%
                             filter(domainSource == "allafrica.com") %>%
                             distinct(urlSource, .keep_all = TRUE) %>%
                             filter(!is.na(urlSource))
    
    # comment out if you don't want to run a test
    #merged_data_allafrica <- merged_data_allafrica[c(371,sample(x=1:nrow(merged_data_allafrica),size = 10,replace = FALSE)),]
    

    # for running R in parallel ignore this part
    rD <- RSelenium::rsDriver(browser = "firefox",
                              chromever = NULL,
                              port = sample(x = netstat::free_ports(), size = 1))

    remDr <- rD[["client"]]

    url <- "https://allafrica.com/commerce/user/manage/"
    
    # open
    # navigate to login page
    remDr$navigate(url)
    Sys.sleep(sample(x = 4:10, size = 1)) # Give page time to load
    
    webElem1 <- remDr$findElement(using = "xpath", "//input[@name = 'login_username']")
    webElem1$sendKeysToElement(list("jillian.stallman@yale.edu"))
    Sys.sleep(sample(x = 3:9, size = 1)) # Give page time to load
    
    # Find 'password' element and send 'saved_pass' and 'enter' keystroke as input
    webElem2 <- remDr$findElement(using = "xpath", "//input[@name = 'login_password']")
    webElem2$sendKeysToElement(list("brt_DZA0xrd*xyu.yup"))
    Sys.sleep(sample(x = 4:12, size = 1)) # Give page time to load
    
    
    webElem3 <- remDr$findElement(using = "xpath", "//input[@name = 'login']")
    webElem3$clickElement()
    Sys.sleep(sample(x = 6:17, size = 1)) # Give page time to load
    
    #print(paste0("Working on ",year))
    print(paste0("Current time is ",Sys.time()))
    # for 2016 gives 97k obs
    # 2017: 62k obs
    # 2022: 25.8k obs
    
    i <- 1
    
    for (url in merged_data_allafrica$urlSource){
      
      print(paste0("This is iteration ",i," of ",nrow(merged_data_allafrica),"."))
      
      remDr$navigate(url)
      
      html <- remDr$getPageSource()[[1]]
      
      title <- read_html(html) %>% # parse html
        html_nodes("h2.headline") %>% # get the headline
        html_text() # remove the extra info
      Sys.sleep(sample(x = c(5:21,3,33,47,12,12,53), size = 1)) # Give page time to load
      
      text <- read_html(html) %>%
        html_nodes("p.story-body-text") %>%
        html_text() %>%
        unlist() %>%
        paste(collapse = " \n")
      
      merged_data_allafrica$title[merged_data_allafrica$urlSource==url] <- ifelse(length(nchar(title)) !=0 , title, 0)
      merged_data_allafrica$text[merged_data_allafrica$urlSource==url]  <- ifelse(length(nchar(text)) !=0 , text, 0)
      
      i <- i+1
                               
    }
  
    print(paste0("Current time is ",Sys.time()))
    
    
    merged_data_allafrica <- save_rds_csv(data = merged_data_allafrica,
                                 output_path = file.path("C:","data_C_drive","02_temp","GDELT","merged","filtered","africa-country-theme","yearly","zambezi_countries","allafrica"),
                                 output_filename = "_zambzei_countries_allafrica.rds",
                                 remove = FALSE,
                                 date = year,
                                 csv_vars = csv_vars,
                                 format = "csv")
    
    
    # clean up RSelenium
    remDr$closeWindow()
    
    rD <- rD$server$stop()
    
    #remDr$closeall()
    
    #system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
    #system("taskkill /F /IM ChromeDriver.exe", intern=FALSE, ignore.stdout=FALSE) 
    
  } # end scrape_and_save_function

  years <- c("2016","2017","2018","2019","2020","2021","2022")
  # uncomment and run the first time
  # iter <- 1
  # saveRDS(iter, file = file.path("P:","Projects","environment","code","01_download","iter.R"))
  
  
  iter     <- readRDS(file.path("P:","Projects","environment","code","01_download","iter.R"))
  
 system.time( 
    out_data <- scrape_and_save(year = years[iter])
 )
 

 
 iter <- iter+1
 saveRDS(iter, file = file.path("P:","Projects","environment","code","01_download","iter.R"))
 
 
 # restart session so that RSelenium can restart
 rstudioapi::restartSession(command = "source(file.path('P:','Projects','environment','code','02_cleaning','02_clean_gdelt_filter-04.R'))")
 
 
 # 2017 with the quick version
 # user  system elapsed 
 # 43.75    1.52 5822.48
 
 # 2016 with built in pauses and such
 # user   system  elapsed 
 # 53.22     1.61 21784.30 # about 6 hours
 
 # 2017 with built-in pauses and all that
 # user   system  elapsed 
 # 31.61     1.22 13184.11 or about 3.7 hours
    
 
