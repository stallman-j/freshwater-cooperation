# _______________________________#
# Environment
# Clean 02: DHS Child Mortality Testing
# 
# Stallman
# Started: 2023-10-23
# Last edited: 
#________________________________#


# https://dhsprogram.com/data/Guide-to-DHS-Statistics/Adult_Mortality_Rates.htm

# Startup

  rm(list = ls())

  # Only run this the first time, otherwise start from "cleaning!"
  
# bring in the packages, folders, paths ----
  
  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))
  
  if (!require(naniar)) install.packages("naniar")
  if (!require(sjlabelled)) install.packages("sjlabelled")
  if (!require(expss)) install.packages("expss")
  if (!require(xlsx)) install.packages("xlsx")
  if (!require(DHS.rates)) install.packages("DHS.rates")
  if (!require(stringr)) install.packages("stringr")
  if (!require(randomcoloR)) install.packages("randomcoloR")
  
  if (!require(haven)) install.packages("haven")
  if (!require(sf)) install.packages("sf")
  
  if (!require(tictoc)) install.packages("tictoc")
  
  library(naniar)
  library(sjlabelled)
  library(expss)
  library(xlsx)
  library(DHS.rates)
  library(stringr)
  library(haven)
  library(sf)
  library(tictoc)
  library(randomcoloR) # to get tons of colors
  
# Get administrative boundaries ----
  
  # if you can't find the admin levels files, do this
  source(file.path(code_folder,"02_clean_GADM.R"))
  
  # from 
  # dhsprogram.com/pubs/pdf/SAR7/SAR7.pdf
  
  # The geographic boundary files are usually either 
  # provided by the country or obtained from publicly available sources
  # such as the United Nation’s Second Administrative Level Boundaries (SALB) dataset and the Global
  # Administrative Areas (GADM) database (UNGIWG, 2013; GADM, 2012).
  # 
  
  
  
  # this is adm level 1
  level <- 2
  country <- "UGA"
  dhs_gps_filename <- "UGGE7AFL"
  # UGGE7AFL and #UGGC7BFL
  
  
  gadm_path <- file.path(data_external_clean,"GADM","global",paste0("GADM_global_ADM_",level,".rds"))

  polygons <- readRDS(file = gadm_path)
  
  country_polygons <- polygons %>% filter(GID_0 == "UGA")
  
  plot(st_geometry(country_polygons))
  
  my_polygons <- country_polygons %>% mutate(id = row_number())
  
  #class(st_geometry(my_polygons))
  
  # first cast everything to Multipolygon, then cast back to polygon:
  print("Casting polygons to POLYGON")
  
  tic("Casted polygons to POLYGON")
  polygons_cast <- my_polygons %>% 
    st_cast("MULTIPOLYGON") %>% # homogenizes the type
    st_cast("POLYGON") %>% # puts all 
    st_make_valid() %>% # make valid geometries %>%
    mutate(id = row_number())
  
  toc()
  
# DHS Data ----
   
   # Walkthrough 
   # https://docs.ropensci.org/rdhs/articles/introduction.html
   
  
  
  
  path <- file.path(data_external_raw,"DHS",paste0(dhs_gps_filename,".rds"))
  
  GPSdata <- readRDS(path) %>%
              filter(LONGNUM>5) # filters out the points that are stuck at (0,0)
  
  #plot(st_geometry(GPSdata))
  
  #rainbow_palette <- randomcoloR::distinctColorPalette()
  
  # give the points the polygon information
  GPS_poly <- st_join(x = GPSdata,
                      y = polygons_cast)

  map <- ggplot()+
    geom_sf(data = polygons_cast,
            fill = "grey80",
            alpha = .2)+
    geom_point(data = GPS_poly,
               aes(x = LONGNUM,
                   y = LATNUM,
                   colour = factor(GPS_poly$id))
            )+
    labs(title = paste0("Survey Cluster Locations by District (ADM Level 2), Uganda 2016"),
         caption = c("Data from DHS (2016) and GADM (2022)"))+
    xlab("") + ylab("")+
    theme_map(legend_position = "none") +
    scale_color_manual(values = randomcoloR::distinctColorPalette(k=length(unique(GPS_poly$id)) ))
  
    map
  
  save_map(output_folder = file.path(output_maps,"DHS"),
           plotname = map,
           filename = paste0("survey_cluster_locations_ADM_level_2_2016_UGA.png"),
           width = 6,
           height = 7,
           dpi  = 400)
  
  saveRDS(GPS_poly,
          file= file.path(data_external_temp,"DHS",
                          paste0(dhs_gps_filename,"_GADM_ADM_",level,".rds"))
  )
  