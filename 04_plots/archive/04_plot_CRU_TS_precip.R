# _______________________________#
# Environment
# plot 04: CRU_TS precipitation (over time)
# 
# Stallman
# Started: 2023-09-12
# Last edited: 
#________________________________#


# Startup

rm(list = ls())

# Only run this the first time, otherwise start from "cleaning!"

# bring in the packages, folders, paths ----

  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))
  
  current_continent <- current_continent # set in 00_startup_master.R
  time_range <- time_range  # set in 00_startup_master.R
  disaster_type <- disaster_type
  
  
# first run the cleaning for 
# packages ----

if (!require(lubridate)) install.packages("lubridate")
if (!require(gifski)) install.packages("gifski")

library(stringr) # for string operations
library(lubridate) # for fancy dates operations
library(gifski) # for making gifs
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(dplyr)



# plot ----


precip_path <- file.path(data_external_clean,"CRU_TS",
                         paste0("CRU_precip_",
                                disaster_type,"_",
                                current_continent,"_",
                                year(first(time_range)),
                                "_",
                                year(last(time_range)),
                                ".rds"))


annual_precip <- readRDS(precip_path) %>% arrange(level)


path <- file.path(data_clean,"shape-files",paste0("sf_countries_",current_continent,".rds"))

sf_countries  <- readRDS(file = path) 



# get a year cross-section
fill_2005 <- annual_precip[,"2005"]


names(fill_2005)[1] <- "precip"

# get the absolute max value
my_max <- annual_precip %>%
  dplyr::select(-country,-iso3,-gwno,-level,-adm1,-adm2,-adm3,-disasterno) %>%
  st_drop_geometry() %>%
  max(.,na.rm = TRUE)

# get a sense of the distribution
summary(annual_precip$`2016`)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 3.547  35.383  56.597  62.146  87.213 212.533       1
# 
# get legend extent
#https://stackoverflow.com/questions/24265652/label-minimum-and-maximum-of-scale-fill-gradient-legend-with-text-ggplot2

# get year sequence ----

years <- seq(from = ymd(head(time_range,n=1)),
             to   = ymd(tail(time_range,n=1)),by = "1 year") %>%
  year() # pull out just the year



# output a plot for each of the years we have 
for (year in years)  {
  
  
  fill_x <- annual_precip[,as.character(year)]
  
  print(paste0("year = ",as.character(year)," and df name is ",as.character(colnames(fill_x)[1])))
  
  names(fill_x)[1] <- "precip"
  
  map <- ggplot(data = sf_countries) +
    geom_sf(color = "gray70",
            fill = "gray99",
            alpha = 0.5,
            linewidth = .3) +
    geom_sf(data = fill_x,
            aes(
              fill  = precip)) +
    # geom_sf(data = test_read,
    #         alpha = .3,
    #         color = yale_medblue,
    #         linewidth = 1) +
    # geom_sf(data = china_dams,
    #         alpha = .4,
    #         color = yale_blue,
    #         size = .5) +
    labs(title = paste0("Annual Precipitation, ",year),
         caption = c("Data from CRU-TS Version 4.07 (2023) and GADM (2022)")) +
    theme_map() +
    scale_fill_gradientn(colours = yale_palette,
                         breaks = c(0,50,100,200,300),
                         limits = c(0,300),
                         labels = c(0,50,100,200,300),
                         name = "Rain \n(annual avg \n mm/month)")
  
  #map
  
  save_map(output_folder = file.path(output_maps,"CRU_TS"),
           plotname = map,
           filename = paste0("annual_precipitation_",year,".png"),
           width = 6,
           height = 7,
           dpi  = 400)
  
}

path_to_pngs <- file.path(output_maps,"CRU_TS",paste0("annual_precipitation_",1960:2018,".png"))

# make into a gif
gifski(png_files = path_to_pngs,
       gif_file = file.path(output_maps,
                            "CRU_TS",
                            paste0("precipitation_",year(first(time_range)),"_",year(last(time_range)),".gif")),
       width = 1800,
       height = 2000,
       delay = .3)


