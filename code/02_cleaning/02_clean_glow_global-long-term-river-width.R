# _______________________________#
# Environment
# Clean 02: Clean Global Long Term River Widths
# 
# Stallman
# Started 2023-10-11
# Last edited: 
#________________________________#



# Startup

rm(list = ls())


# bring in the packages, folders, paths

home_folder <- file.path("P:","Projects","environment")

source(file.path(home_folder,"code","00_startup_master.R"))

# required scripts ----
# 
# need to download GLOW global long term river width
#source(file.path(home_folder,"code","01_download","01_download_datasets_in_use.R"))

# first 
# packages ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  parallel, # for parallelization
  stringr, # for string operatioons
  terra, # handle raster data
  exactextractr, # fast extractions
  tictoc, # for measuring time
  tmap, # quick plotting
  sf, # vector data operations
  dplyr, # data wrangling
  data.table, # hardcore data wrangling
  lubridate # date operations
)


# parameters ----

equal_area_crs   <- "ESRI:102022"

# paths ----

  temp_path <- file.path(data_external_temp,"GLOW_global-long-term-river-width")
  if (!dir.exists(temp_path)) dir.create(temp_path, recursive = TRUE) # recursive lets you create any needed subdirectories


# bring in data: shapefiles, then going to merge on river widths ----

# find out which region is the relevant one(s) ----

# 

# 1:6 would go through all to find out which lat long we want
for (region in 1){
  path <- file.path(data_external_raw,"GLOW_global-long-term-river-width","cross-section",
                    paste0("GLOW_crosssection_region_",region,".dbf"))
  
  
  system.time(
  locations_sf <- st_read(path)
  )
}

data(World)

africa <- World %>% filter(continent == "Africa")
# randomly select 100 rows 
# 
locations_sf_sample <- locations_sf %>% sample_n(size = 100000)

map <- ggplot() +
  geom_sf(data = africa,
          color = "gray70",
          fill = "gray99",
          alpha = 0.5,
          linewidth = .3) +
  geom_sf(data = locations_sf_sample) +
  labs(title = paste0("Africa and GLOW River Measurement Locations"),
       caption = c("Source: R package tmap, Feng et al. (2022)")) +
  theme_map() 

map


save_map(output_folder = output_maps,
         plotname = map,
         filename = paste0(units_name,"_hydro_rivers_units_hydroRIVERS.png"),
         width = 9,
         height = 5,
         dpi  = 300)


locations_equal_area <- locations_sf %>% st_transform(crs = equal_area_crs)

  saveRDS(locations_equal_area, file = file.path(temp_path,"africa_river_width_locations_equal_area.rds"))
  

# region 1 is 1.2 million rows, short to come in like 20 seconds
  
#   Reading layer `GLOW_crosssection_region_1' from data source 
#   `E:\data\01_raw\GLOW_global-long-term-river-width\cross-section\GLOW_crosssection_region_1.dbf' 
#   using driver `ESRI Shapefile'
# Simple feature collection with 1200826 features and 3 fields
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: -17.24328 ymin: -34.72917 xmax: 54.21974 ymax: 37.25892
# Geodetic CRS:  WGS 84

path <- file.path(data_external_raw,"GLOW_global-long-term-river-width","Width","width","GLOW_width_region_1.csv")

# system.time(
# width_df <- read.csv(path)
# )

# user  system elapsed 
# 19.28    0.29   99.33 
# 
# 
system.time(
width_dt <- fread(path, header = T)
)

# user  system elapsed 
# 1.49    0.35    2.31

# environmental parameters

# path <- file.path(data_external_raw,"GLOW_global-long-term-river-width","enve-parameters","environmental_parameters_global.csv")
# 
# system.time(
#   enve_parms <- fread(path, header = T)
# )
# 
# head_enve <- head(enve_parms) %>% view()
# 
# # just by riverr ID, not worth really getting into

# 76 million rows

# clean the width dt by aggregating up to annual

width_dt2 <- width_dt %>%head(n=100) 

# create the variable year
tic("Getting years")
temp <- width_dt2[, `:=` (year = data.table::year(as.Date(date)))]

toc()
# get the mean width by ID and year
temp2 <- temp[,
                  .(width = mean(width)), 
                  by = .(ID,year)]

# do this for the big ol' thing
# 
tic("Getting years for the dt")

temp <- width_dt[, `:=` (year = data.table::year(as.Date(date)))]
toc()
# Getting years for the dt: 7.96 sec elapsed

tic("Averaging width over year and ID")
width_annual <- temp[,
              .(width = mean(width)), 
              by = .(ID,year)]
toc()
#Averaging width over year and ID: 2.5 sec elapsed

# merge onto locations ----
# 

# syntax https://medium.com/analytics-vidhya/r-data-table-joins-48f00b46ce29
# takes a right outer join between left_table[right_table]

# we have to drop the SF part of the cross_section to do this join with data.table


cross_section_dt <- data.table(cross_section_sf)

tic("Merge cross sections to widths")
width_geo <- width_annual[cross_section_dt, 
                   on = .(ID),
                   nomatch = NULL] # if no match, don't include row

toc()

#Merge cross sections to widths: 1.52 sec elapsed


width_geo <- st_as_sf(width_geo)

# save cleaned file


  saveRDS(width_geo,
          file = file.path(data_external_clean,"GLOW_global-long-term-river-width","africa_river_width_panel_sf.rds")
  )
  

  
  

  
  
