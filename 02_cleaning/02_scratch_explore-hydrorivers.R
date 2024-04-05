# _______________________________#
# Environment
# Clean 02: Merge DHS and GPS Data
# 
# Stallman
# Started: 2023-10-23
# Last edited: 2023-11-13
# Edit: added get elevation to merge function merge_dhs_gps
# Last edited: 2023-12-09
# Edit: made it just getting the river network given having dhs_child-mortality data
#________________________________#




# Startup

rm(list = ls())

# https://cran.r-project.org/web/packages/riverdist/vignettes/riverdist_vignette.html

# bring in the packages, folders, paths ----

code_folder <- file.path("P:","Projects","environment","code")
source(file.path(code_folder,"00_startup_master.R"))
#source(file.path(code_startup_general,"merge_dhs_gps.R")) # function for merging dhs and gps data

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  stringr, # string operations
  countrycode, # country naming conversions
  sf, # vector spatial geometry
  ggrepel, # for jittering text in a plot
  riverdist, # calculating river distances
  rdhs, # getting DHS data
  elevatr, # for getting elevation from points
  RColorBrewer, # for getting gradients and colors
  parallel, # for parallelizing operations
  tictoc # timing # more ability to customize to output to latex. use with kableExtra to output tables
  # to console, latex, Rmarkdown, html etc.
)

level <- 1
dhs_gps_filename <- "UGGE7AFL" # # UGGE7AFL and #UGGC7BFL

country          <- "UG"
n_cores          <- 8 #detectCores(logical = TRUE) - 2
#units_name       <- c("Uganda")
equal_area_crs   <- "ESRI:102022"
max_distance_to_snap <- 10000 # max distance at which to snap to a river; 10km b/c that's max perturbation
surveyyear_start <- 2018 %>% as.character()
surveyyear_end   <- 2020 %>% as.character()
long_river_threshold <- 12000 # original was 4000 
period_length    <- 60 # what months window is used for the mortality data
# bring in hydrorivers ----

path <- file.path(data_raw,"hydroSHEDS","HydroRIVERS_v10_af.gdb","HydroRIVERS_v10_af.gdb")

system.time(
  hydro_rivers <- st_read(dsn = path) %>% 
    st_transform(crs = equal_area_crs) %>% 
    rename(geometry = Shape) # need this to use riverdist package
)
# without st_transform
# user  system elapsed 
# 1.83    1.47   31.17
# 
# with st_transform
# user  system elapsed 
# 7.39    2.53   40.36

names(hydro_rivers)
# [1] "HYRIV_ID"     "NEXT_DOWN"    "MAIN_RIV"     "LENGTH_KM"    "DIST_DN_KM"  
# [6] "DIST_UP_KM"   "CATCH_SKM"    "UPLAND_SKM"   "ENDORHEIC"    "DIS_AV_CMS"  
# [11] "ORD_STRA"     "ORD_CLAS"     "ORD_FLOW"     "HYBAS_L12"    "Shape_Length"
# [16] "geometry" 

temp <- hydro_rivers %>%
        group_by(MAIN_RIV) %>%
        summarise(segments = n())


out_path <- file.path(data_external_clean,"HydroSHEDS","summary-information")
if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)

saveRDS(temp,file = file.path(out_path,paste0("rivers_and_segment_size.rds")))

temp_nogeo <- temp %>% st_drop_geometry() 

temp_nogeo <- temp_nogeo %>% arrange(desc(segments))


hist(temp_nogeo$segments)

summary(temp_nogeo)

temp_nogeo %>% filter(segments>mean(segments)) %>% summary()
