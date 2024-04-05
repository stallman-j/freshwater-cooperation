# ______________________________#
# International Agreements: Master
# 
# Stallman
# Started 2022-08-20
# Last edited: 
#
# ______________________________#


# Packages ----


#rm(list = ls())

# increase max timeout time so we can install large databases
  options(timeout = max(1000, getOption("timeout")))


# list most of the packages here, but I prefer to call packages within sub-scripts so that there's no crazy conflicts
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, # data wrangling
  ggplot2, # pretty plots
  #xtable, # tables to tex
  #readxl, # for reading in excel formats
  #texreg, # output estimation results
  #readr, # for better reading of CSV files
  tidyr, # data manipulation
  rmarkdown, # writing R and commenting 
  markdown, # for RMD writing
  knitr, # extends markdown syntax to allow chunks of R code
  tidyverse # for all your data wrangling needs
  # purrr # for using nicer functions like map() i.e. avoiding FOR loops
  #data.table,
  #stringr, # for ops with strings
  #dichromat, # to get colors between a group, need to use yale_scheme in 00-startup_plot-functions
  #Matrix, # for working quickly with matrices
  #igraph, # networks
  #rnaturalearth, # for getting country maps
  #rgeos, # for doing geocoding
  #rworldmap, # another map
  #rworldxtra, # for high-res map of world
  #sf, # spatial operations
  #terra, # raster spatial data
  #spData, # spatial data fancier
  #rgdal, # allows for importing raster/vector map data
  #tmap, # plotting maps
  #statnet, # networks
  #cshapes, # gis dataset of country borders, 1886 to today, includes 
  #states, # list of independent states from 1816 on 
  #countrycode, # for converting among differing codings for countries
  #parallel, # for running on multiple cores
  #MASS, # for high-powered statistics operations
  #sna, # network objecst
  #maps, # mapping
  #network, # careful will clash with igraph
  #ggrepel, # automatically position non-overlapping text labels with ggplot2
  #foreign, # for de-garbling stata.dta files
  #btergm, # bootstrapping temporal exponential random graphs
  #ergm, # exponential random graph models
  #modelsummary, # regression table generation
  #lfe, # fast regression with many fixed effects
  #haven, # for converting datasets
  #sandwich, # for Stata's robust SEs
  #lmtest, # for coeff tests for robust SEs
  #boot,  # for cross-validation and bootstrapping
  # stargazer # for pretty reg tables into latex 
  #cobalt, # for pretty balance tables
  
  # scraping packages
  # rvest # for web scraping
  
  )

#install.packages("devtools")

#devtools::install_dev("remotes")

  #if (!require("ggnet")) devtools::install_github("briatte/ggnet")
  
  #library(ggnet)

  #if (!require("ggalt")) devtools::install_github("eliocamp/ggalt@new-coord-proj")

  #library(ggalt) # for chagning coordinate projections 


# parameters ----
  
    # projection info, use robinson
    #projection_crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
     
    equal_area_crs   <- "ESRI:102022"
    current_continent   <- "Africa"
    #start_year          <- 1948
    #end_year            <- 2007
    #disaster_type       <- "drought"
    #disaster_Type       <- "Drought"
    time_range          <- c("1960-01-01","2018-12-31")
    
    # transboundary waters update date for using in downloading and then cleaning
      tbw_treaty_database_update_date <- "20181124"

# directories
  # decide where the working directory is
  home_folder <- file.path("P:","Projects","environment")
  setwd(home_folder)
  

    # Code Paths
    
    # this path can differ, these functions are general across projects
    code_startup_general          <- file.path("P:","Projects","coding","r-code","00_startup")
   
    code_folder                   <- file.path(home_folder,"code")
    code_startup_project_specific <- file.path(code_folder,"00_startup")
    code_download                 <- file.path(code_folder,"01_download")
    code_clean                    <- file.path(code_folder,"02_cleaning")
    code_analysis                 <- file.path(code_folder,"03_analysis")
    code_plots                    <- file.path(code_folder,"04_plots")
    code_simulations              <- file.path(code_folder,"05_simulations")
    code_scratch                  <- file.path(code_folder,"scratch")

    # Output Paths
    output_folder                 <- file.path(home_folder,"output")
    output_tables                 <- file.path(output_folder, "01_tables")
    output_figures                <- file.path(output_folder, "02_figures")
    output_maps                   <- file.path(output_folder, "03_maps")
    output_manual                 <- file.path(output_folder, "x_manual-output")
    output_scratch                <- file.path(output_folder, "scratch")
    
    
    
    # Data Paths if 
    data_folder                   <- file.path("P:","data") #file.path("C:","environment_data") 
    data_manual                   <- file.path(data_folder,"00_manual-download")
    data_raw                      <- file.path(data_folder, "01_raw")
    data_temp                     <- file.path(data_folder, "02_temp")
    data_clean                    <- file.path(data_folder, "03_clean")
    
    # for really big data
    data_external                 <- file.path("E:","data")
    data_external_raw             <- file.path(data_external,"01_raw")
    data_external_temp            <- file.path(data_external,"02_temp")
    data_external_clean           <- file.path(data_external,"03_clean")
    
    # change data paths if we're not on the big machine to make it just the regular
    if (Sys.info()[["nodename"]]=="LAPTOP-T9CCTHDK" ){
      
      # Data Paths
      data_folder                   <- file.path(home_folder,"data") #
      data_manual                   <- file.path(data_folder,"00_manual-download")
      data_raw                      <- file.path(data_folder, "01_raw")
      data_temp                     <- file.path(data_folder, "02_temp")
      data_clean                    <- file.path(data_folder, "03_clean")
      
      # for really big data
      data_external                 <- file.path(home_folder,"data")
      data_external_raw             <- file.path(data_external,"01_raw")
      data_external_temp            <- file.path(data_external,"02_temp")
      data_external_clean           <- file.path(data_external,"03_clean")
      

    }
    
# _______________________________#
# Turning on scripts ----
# 1 means "on," anything else is "don't run"
# _______________________________#
  # 00 startup
  startup_create_folders_general                           <-    0
  startup_download_functions_general                       <-    1
  startup_clean_functions_general                          <-    1
  startup_map_functions_general                            <-    1
  startup_spatial_functions_general                        <-    0
  startup_plot_functions_general                           <-    1
  startup_analysis_functions_general                       <-    0
  startup_palette_general                                  <-    1
  startup_parallel_functions_general                       <-    1
  
  # 00 startup environment
  startup_env_download_functions                            <-    0
  startup_env_cleaning_functions                            <-    0
  startup_env_analysis_functions                            <-    0
  startup_parameters                                        <-    0
  
  
# _______________________________#
# Running Files  ----
# _______________________________#
    
    # 00 startup ----
  
  if(startup_create_folders_general==1){
    source(file.path(code_startup_general,"00_startup_create-folders.R"))
  }
  
  
  if(startup_download_functions_general==1){
    source(file.path(code_startup_general,"00_startup_download-functions.R"))
  }
  
  
  if(startup_clean_functions_general==1){
    source(file.path(code_startup_general,"00_startup_cleaning-functions.R"))
  }
  

  if(startup_map_functions_general==1){
    source(file.path(code_startup_general,"00_startup_map-functions.R"))
  }
  
  if(startup_spatial_functions_general==1){
    source(file.path(code_startup_general,"00_startup_spatial-functions.R"))
  }
  
  
  if(startup_plot_functions_general==1){
    source(file.path(code_startup_general,"00_startup_plot-functions.R"))
  }
  
  if(startup_analysis_functions_general==1){
    source(file.path(code_startup_general,"00_startup_analysis-functions.R"))
  }
  
    if(startup_parallel_functions_general==1){
    source(file.path(code_startup_general,"00_startup_parallel-functions.R"))
  }
  
  if(startup_palette_general==1){
    source(file.path(code_startup_general,"00_startup_palette.R"))
  }
  
  
  
## 00 startup, get functions for my specific project ----
  
  if(startup_env_download_functions==1){
    source(file.path(code_startup_project_specific,"00_startup_env-download-functions.R"))
  }
  
    if(startup_env_cleaning_functions==1){
    source(file.path(code_startup_project_specific,"00_startup_env-cleaning-functions.R"))
  }
  
  if(startup_env_analysis_functions==1){
    source(file.path(code_startup_project_specific,"00_startup_env-analysis-functions.R"))
  }
  
  if(startup_parameters==1){
    source(file.path(code_startup_project_specific,"00_startup_parameters.R"))
  }
  

# clean up some locals
  rm(startup_create_folders_general,
     startup_download_functions_general,
     startup_clean_functions_general,
     startup_map_functions_general,
     startup_spatial_functions_general,
     startup_plot_functions_general,
     startup_analysis_functions_general,
     startup_palette_general,
     startup_parallel_functions_general,
     startup_env_download_functions,
     startup_env_cleaning_functions,
     startup_env_analysis_functions,
     startup_parameters
     )
  
