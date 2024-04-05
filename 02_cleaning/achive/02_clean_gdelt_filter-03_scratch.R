# _______________________________#
# Environment
# Clean 02: Glean GDELT Events: Append for a year
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
  

  
  date <- "2016-02-05"
  
append_dates_events <- function(year,
                               input_path = file.path("E:","data","01_raw","GDELT","events","full"),
                               input_filename = "_events_full.rds",
                               output_path = file.path("C:","data_C_drive","02_temp","GDELT","events","full","yearly"),
                               output_filename = "_events_unfiltered.rds"
                                      
    ) {
      
# Create output folder ----
  input_path <- file.path(input_path)
  output_path <- file.path(output_path,year)
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
# create date sequence ----

  start_md    <- "0101"
  end_md      <- "1231"
  start_ymd   <- paste0(year,start_md)
  end_ymd     <- paste0(year,end_md)
  
  # create sequence 
  dates <- as.character(seq(as.Date(start_ymd,"%Y%m%d"), as.Date(end_ymd,"%Y%m%d"), by = "day"))
  
  
# packages ----
  
  if (!require("stringi")) install.packages("stringi")
  if (!require("tidyverse")) install.packages("tidyverse")
  
  library(stringi)
  library(tidyverse)
  
# function get url stub----
  get_url_stub <- function(data,
                           varname_url = documentSource,
                           varname_domain = domainSource
  ){
    # convert the argument to a string
    varname_url <- deparse(substitute(varname_url))
    varname_domain <- deparse(substitute(varname_domain))
    
    # pull the columns as vectors
    domain <- data[[varname_domain]]
    url    <- data[[varname_url]]
    
    ## extract the characters after the domain
    # sub cannot vectorize the pattern so we need stringr or stringi
    # https://stackoverflow.com/questions/36971116/removing-string-out-of-string-rowwise-in-data-frame-using-another-column-in-r
    
    stub <- stringi::stri_replace_all_regex(str = url,
                                                    pattern = paste0("((http://|https://)",domain,"/)|(",domain,"/)"),
                                                    replacement = "")
    return(stub)
  }

# append ----
    # loop over the dates and append them into a single DF
  
   print("Reading in data and appending.")
  print(Sys.time())
  
      for (date in dates){
        # if it's the first element, initialize the df
        if (date == dates[1]){
          
          data <- readRDS(file =file.path(input_path,paste0(date,input_filename)))
          
          
          
        } else{ # if not first element then append
          
          tmp_path <- file.path(input_path,paste0(date,input_filename))
          
          if (!file.exists(tmp_path)){ # if file is missing just say it's missing
            print(paste0("File missing for ",date))
            
          } else{

            
          tmp <- readRDS(file =tmp_path) 
          
          #
          
          data <- rbind(data,tmp)
          

        } # end else file.exists
          }# end else if not the first date
        
      } # end for loop
      
  print(Sys.time())
  
  rm(tmp,tmp_path)
      print(paste0("Finished appending, adding URL stubs."))
      
      data <- data %>%
              mutate(stub = get_url_stub(data))
      

      out_path <- file.path(output_path,
                            paste0(year,
                                   "_",theme,
                                   output_filename))
      
      saveRDS(data,file = out_path)
      
      csv_path <- gsub(pattern = ".rds", replacement = ".csv", x = out_path)

      csv_data <- data %>%
                  select(idGKG,documentSource,stub,themes,locations,domainSource)
                  
      rm(data)
      
      write.csv(csv_data,
                file =csv_path)
      
      rm(csv_data,out_path) 
      #return(data)
      
    } # end function

 append_dates_by_theme_safe <- purrr::possibly(append_dates_by_theme, 
                                     otherwise = NULL, 
                                     quiet = FALSE)
 


# test ----
 
 # test with a single date
 system.time(
   test <- append_dates_by_theme(year = "2016",
                                 theme = include_themes[4])
 )
 
 # big theme, water security
 # user  system elapsed 
 # 58.95    1.85  171.75 
 
 # small theme, agricultural water management, 37718 rows total
 # user  system elapsed 
 # 20.40    0.58   39.53 
 
 
# run in parallel ----
 
 
 library(parallel)
 n_cores <- detectCores() - 4
 
 cl <- makeCluster(n_cores)
 #clusterExport(cl, varlist = c('year'))
 
 clusterEvalQ(cl, {
   #library(purrr) # to export possibly() to give an error if the function stops working 
   library(tidyverse)
 })
 
 print(Sys.time())
 
 system.time(
   parLapply(cl,include_themes,append_dates_by_theme, year = "2016")
   )

 system.time(
   parLapply(cl,include_themes,append_dates_by_theme, year = "2017")
 )

 system.time(
   parLapply(cl,include_themes,append_dates_by_theme, year = "2018")
   )

 system.time(
   parLapply(cl,include_themes,append_dates_by_theme, year = "2019")
   )
 
 system.time(
   parLapply(cl,include_themes,append_dates_by_theme, year = "2020")
 )

 system.time(
   parLapply(cl,include_themes,append_dates_by_theme, year = "2021")
 )

 system.time(
   parLapply(cl,include_themes,append_dates_by_theme, year = "2022")
 )
 
 stopCluster(cl)
 
 print(Sys.time())
 
 # user  system elapsed 
 # 0.02    0.02  304.33 
 # 
 # user  system elapsed 
 # 0.00    0.00  223.42
 # 
 # user  system elapsed 
 # 0.02    0.00  175.28
 # 
 # user  system elapsed 
 # 0.00    0.00  180.58 
 # 
 # user  system elapsed 
 # 0.00    0.00  169.31
 # 
 # user  system elapsed 
 # 0.0     0.0   164.5 
 # 
 # user  system elapsed 
 # 0.00    0.00  154.64 
 
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
  
 
 