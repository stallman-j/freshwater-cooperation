# _______________________________#
# Environment
# download 01: download datasets and extract from their zip files
# 
# Stallman
# Started: 2023-04-13
# Last edited: 
#________________________________#


# Startup

  rm(list = ls())

  # Only run this the first time, otherwise start from "cleaning!"
  
# bring in the packages, folders, paths ----
  
  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))
  

   
# DHS Data ----
   
   # Walkthrough 
   # https://docs.ropensci.org/rdhs/articles/introduction.html
   
   if (!require(httr)) install.packages("httr")
   library(httr)

   
   if (!require(rdhs)) install.packages("rdhs")
   library(rdhs)
   
   paths <- c(file.path(data_temp,"DHS","cache"),
              file.path(data_raw,"DHS"),
              file.path(data_clean,"DHS"),
              file.path(data_external_raw,"DHS"),
              file.path(data_external_temp,"DHS"),
              file.path(data_external_clean,"DHS"))
   
   for (path in paths){
   if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
   }

   set_rdhs_config(email = "jillian.stallman@yale.edu",
                    project = "Weather and Mortality",
                    cache_path = file.path(data_temp,"DHS","Weather and Mortality","cache"),
                    config_path = "rdhs.json",
                    verbose_download = TRUE,
                    verbose_setup    = TRUE,
                    password_prompt  = FALSE,
                    global = FALSE # so that we have specific directories
                    )
   
   sc <- dhs_survey_characteristics()
   
   
   system.time(
   datasets_available <- get_available_datasets(clear_cache = FALSE)
   )
   
   # show country IDs
   # see countries available
    countries_crosswalk <- dhs_countries(returnFields = c("CountryName","DHS_CountryCode"))
   # let's take AO angola
    
    
   all_datasets <- dhs_datasets()
   
   gps_datasets     <- all_datasets %>% filter(DatasetType == "GPS Datasets")
   flat_datasets    <- dhs_datasets(fileFormat = "SV") %>% 
                        filter(DatasetType!="HIV Datasets") %>%
                        filter(FileType!= "Fieldworker Questionnaire") %>%
                        filter(FileType!= "Women's Status") %>%
                        filter(FileType!="Household Member Raw") %>%
                        filter(FileType!="Children's Raw") %>%
                        filter(FileType!="Expenditure") %>%
                        filter(FileType!="Experimental") %>%
                      
   
   desired_datasets <- rbind(gps_datasets,flat_datasets)
   # Storage.
   flag <- rep(0,times = nrow(desired_datasets))
               

   # for GPS datasets
   for (i in 1:nrow(desired_datasets)){
   tryCatch(
     {
     download_paths <- get_datasets(dataset_filenames =  desired_datasets$FileName[i],
                                  download_option = "rds",
                                  reformat = FALSE,
                                  all_lower = TRUE,
                                  output_dir_root = file.path(data_external_raw,"DHS"),
                                  clear_cache = FALSE)
    
     },
     # Handle the errors.
     error = function(err) {
       message('On iteration',i,'there was an error: ',err)
       flag[i] <<- c(i)
     }
   )

   }
    print(flag) 
    
    desired_data_success <- data.frame(desired_datasets,
                                       flag = flag)
    
    save_rds_csv(data = desired_data_success,
                 output_path = file.path(data_external_temp,"DHS"),
                 date = "",
                 output_filename = "datasets_flags.rds",
                 format = "csv")
   
   
   # pass protected will ask for the SEDAC NASA login
   
   
   filenames <- c("contour","contourp","pplace","pplacep","river","riverp","rlroad","rlroadp","road",
                  "roadp","uarea","uareap","uareap1","uareap2")
   
   sub_urls <- paste0(filenames,"?flag=1")
   
   
   download_multiple_files(data_subfolder = "DHS",
                           data_raw = data_external_raw,
                           base_url = "https://www.dhsprogram.com/data/dataset/Angola_MIS_2006.cfm?flag=1",
                           sub_urls = sub_urls,
                           filename = filenames,
                           zip_file = TRUE,
                           pass_protected = TRUE,
                           username = "jillian.stallman",
                           password = "oT3DeWEpYrAoK-i7LHQz")


#   CRUTS ----
   
   # cld is cloudcover
   url <- "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.07/cruts.2304141047.v4.07/cld/"
   
   #Grab filenames from separate URL
   helplinks <- read_html(url) %>% html_nodes("a") %>% html_text(trim = T)
   
   #Keep only filenames relevant for download
   helplinks <- helplinks[grepl("\\.gz", helplinks)]
   
   helplinks[1:5]
   #> [1] "p000001.psv" "p000002.psv" "p000003.psv" "p000004.psv" "p000005.psv"
   
   #https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.07/cruts.2304141047.v4.07/cld/cru_ts4.07.1901.1910.cld.dat.gz
   
   
   download_multiple_files(data_subfolder = "CRU_TS",
                           data_raw = data_raw,
                           base_url = "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.07/cruts.2304141047.v4.07/cld",
                           sub_urls = helplinks,
                           filename = helplinks,
                           zip_file = FALSE)
   
   # GRanD Global Reservoir and Dam ----
   
   # https://sedac.ciesin.columbia.edu/data/set/grand-v1-dams-rev01
   # 2011
   # https://sedac.ciesin.columbia.edu/downloads/data/grand-v1/grand-v1-dams-rev01/dams-rev01-global-shp.zip
   
   if (!require(httr)) install.packages("httr")
   library(httr)
   
   download_data(data_subfolder = "GRanD_global-reservoir-and-dam",
                 data_raw = data_raw,
                 url = "https://sedac.ciesin.columbia.edu/downloads/data/grand-v1/grand-v1-dams-rev01/dams-rev01-global-shp.zip",
                 zip_file = TRUE,
                 pass_protected = TRUE)
   
   
   
   download.file(url = "https://zenodo.org/record/7420810/files/Lake-TopoCat-v1.0_product_description.pdf?download=1",
                 destfile = file.path(data_raw,"Lake-TopoCat","Lake-TopoCat-v1.0_product_description.pdf"),
                 quiet = TRUE,
                 mode = "wb")
   
   # Yang and Huang 2022 - 30 m annual land cover datasets, 1990-2021 ----
   
   # https://zenodo.org/record/5816591
   
   #https://zenodo.org/record/5816591/files/CLCD_v01_1995_albert_province.zip?download=1
   
   
   filenames <- paste0("CLCD_v01_",c(1985,1990:2021),"_albert_province")
   sub_urls <- paste0(filenames,".zip?download=1")
   
   
   download_multiple_files(data_subfolder = "CLCD_china-land-cover-dataset",
                           data_raw = data_raw,
                           base_url = "https://zenodo.org/record/5816591/files",
                           sub_urls = sub_urls,
                           filename = filenames,
                           zip_file = TRUE)
   
   
   sub_urls <- paste0("CLCD_v01_",c(1985,1990:2021),"_albert.tif?download=1")
   filenames <- paste0("CLCD_v01_",c(1985,1990:2021),"_albert.tif")
   
   download_multiple_files(data_subfolder = "CLCD_china-land-cover-dataset",
                           data_raw = data_raw,
                           base_url = "https://zenodo.org/record/5816591/files",
                           sub_urls = sub_urls,
                           filename = filenames,
                           zip_file = FALSE)
   
   
   # documentation
   
   download.file(url = "https://zenodo.org/record/5816591/files/CLCD_classificationsystem.xlsx?download=1",
                 destfile = file.path(data_raw,"CLCD_china-land-cover-dataset","CLCD_classificationsystem.xlsx")
   )
   
   
 