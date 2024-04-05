# _______________________________#
# Environment
# Merge 02: Extract points with precipitation from points
# 
# Stallman
# Started 2023-10-11
# Last edited: 
#________________________________#


# need 02_merge_a_era5_dhs


# Startup

rm(list = ls())


# bring in the packages, folders, paths

home_folder <- file.path("P:","Projects","environment")

source(file.path(home_folder,"code","00_startup_master.R"))

# requires having run
# 02_merge_a_era5_dhs
# 02_clean_dhs_child-mortality_annual

# packages ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tictoc, #measuring time to run operations
  countrycode, # for translating between country names
  rdhs, # for dealing with DHS in R
  sf, # vector operations
  terra, # raster operations
  zoo # time series stuff, easy calc of rolling averages
)


# parameters ----
   period_length <- 60
  
  # this will give all the countries that the river networks try to loop over
  countries_DHS <-  dhs_datasets() %>% 
    dplyr::filter(DatasetType == "GPS Datasets") %>% 
    dplyr::filter(FileType == "Geographic Data") %>%
    mutate(continent = countrycode(DHS_CountryCode, origin = "dhs",destination = "continent"))%>%
    dplyr::filter(continent == "Africa") %>%
    dplyr::select(DHS_CountryCode) %>% 
    dplyr::filter(DHS_CountryCode!="LB") %>% # something off with Liberia
    dplyr::filter(DHS_CountryCode!="TG") %>% # something off with Togo
    unique() %>%
    .[,1] # get just the character
  
  
# bring in data ----

  era5_gps_childmort <- readRDS(file.path(data_external_clean,"merged",paste0("Africa_all_years_DHS_",period_length,"_month_window_child_mortality_GPS_ERA5.rds")))
  
  # show some quantiles of where child mortality gets funky
  quantile(era5_gps_childmort$R_IMR, c(.25,.31,.45,.5,.6,.7,.8,.9,.99),na.rm = TRUE)
  
  # 25%    31%    45%    50%    60%    70%    80%    90%    99% 
  # 0.00   0.00  16.53  37.74  58.82  83.33 111.11 157.19 303.23 
   
  quantile(era5_gps_childmort$WN_IMR, c(.25,.31,.45,.5,.6,.7,.8,.9,.99),na.rm = TRUE)
  # 25% 31% 45% 50% 60% 70% 80% 90% 99% 
  # 6   8  12  13  17  22  28  40  85
  
  quantile(era5_gps_childmort$R_NNMR, c(.25,.31,.45,.5,.6,.7,.8,.9,.99),na.rm = TRUE)
  # 25%    31%    45%    50%    60%    70%    80%    90%    99% 
  # 0.00   0.00   0.00   0.00  21.28  37.04  55.56  85.71 200.00 
  
  quantile(era5_gps_childmort$WN_NNMR, c(.25,.31,.45,.5,.6,.7,.8,.9,.99),na.rm = TRUE)
  # 25% 31% 45% 50% 60% 70% 80% 90% 99% 
  # 8  10  15  17  21  27  34  46  97
  
  era5_gps_childmort_sen <- era5_gps_childmort %>% filter(DHSCC=="SN")
  
  names(era5_gps_childmort)
  
  era5_gps_childmort <- era5_gps_childmort %>%
                        select(DHSID,DHSCC,year,DHSCLUST,ADM1DHS,ADM1NAME,DHSREGCO,DHSREGNA,URBAN_RURA,
                               LATNUM,LONGNUM,ALT_DEM,GID_1,NAME_1,GID_2,NAME_2,ENGTYPE_2,elevation,
                               precip_current_annual_avg_mm_month,precip_current_annual_sd_mm_month,
                               precip_annual_zscore_mm_month,
                               precip_lr_sd,precip_lr_mean,precip_lr_zscore,precip_lr_sd_deviation,
                               precip_003y_ra,precip_005y_ra,precip_011y_ra,period_months,
                               R_NNMR, R_IMR, R_U5MR, R_U10MR,
                               N_NNMR, N_IMR, N_U5MR, N_U10MR,
                               WN_NNMR,WN_IMR,WN_U5MR,WN_U10MR,
                               dhs_gps_filename) 
  
  
                        # mutate(R_U10MR_trunc = pmin(pmax(R_U10MR, quantile(R_U10MR, .33,na.rm = TRUE)),
                        #                             quantile(R_U10MR, .67, na.rm = TRUE)))
                               
  #hist(era5_gps_childmort$WN_U10MR)
  #hist(era5_gps_childmort$R_U10MR)
  
  # filter the data to just have the observations that are within the middle third
  # 
  # childmort_trunc <- era5_gps_childmort %>%
  #                    filter(WN_IMR >= 10) %>%
  #                    filter(R_IMR > quantile(R_IMR, .33,na.rm = TRUE) & R_IMR < quantile(R_IMR, .67,na.rm = TRUE) )
  #                     
  #hist(childmort_trunc$WN_U10MR)
  #hist(childmort_trunc$R_U10MR)
  
  

  
# choose countries or rivers to include ----
  
  # use iso3c for this
  countries_iso3c <- c("SEN")
  
  countrynames <- countrycode(countries_iso3c,
                             origin = "iso3c",
                             destination = "country.name")
  
  dhs_countrycodes <- countrycode(countries_iso3c,
                             origin = "iso3c",
                             destination = "dhs")
  
  
  countries_DHS <-  dhs_datasets() %>% 
    dplyr::filter(DatasetType == "GPS Datasets") %>% 
    dplyr::filter(FileType == "Geographic Data") %>%
    mutate(continent = countrycode(DHS_CountryCode, origin = "dhs",destination = "continent"))%>%
    dplyr::filter(continent == "Africa") %>%
    dplyr::select(DHS_CountryCode) %>% 
    dplyr::filter(DHS_CountryCode!="LB") %>% # something off with Liberia
    dplyr::filter(DHS_CountryCode!="TG") %>% # something off with Togo
    unique() %>%
    .[,1] # get just the character
  
  countries_DHS_iso3c <- countrycode(countries_DHS,
                                     origin = "dhs",
                                     destination = "iso3c")
  
  # get the folder where the dyad distances are located
  # this is the
  dyad_distances_path <- file.path(data_external_clean,"merged","DHS_HydroSHEDS")
  
  # get the filenames of the countries in our list 
  distances_filenames <- list.files(path = dyad_distances_path,
                                    pattern = "dyad_distances")
  
  i <- 1
  for (i in 1:2){
    
    if (i==1){countries <- countries_iso3c}
    if (i==2){countries <- countries_DHS_iso3c}
    
  countries_indices <- stringr::str_detect(distances_filenames,
                                              pattern = paste(countries,collapse = "|"))
  
  countries_filenames <- distances_filenames[countries_indices]
  

  # rbind all the rows we want to bring in   
  tic("Looped over and rbinded all data in")
  for (filename in countries_filenames){
  
    temp_data <- readRDS(file.path(dyad_distances_path,filename))
    
    #print(paste0("dataset ",filename," has ",nrow(temp_data)," rows."))
    if (filename == countries_filenames[1]){

      dyad_data <- temp_data


    } else{

      dyad_data  <- rbind(dyad_data,temp_data)


    } # end ifelse to get the first df to be setting things up
    
  } # end loop over filenames
  
  toc()
  
  # Looped over and rbinded all data in: 1.29 sec elapsed
  # Looped over and rbinded all data in: 50.42 sec elapsed
  
  dyad_data <- dyad_data %>%
    group_by(upstream,downstream) %>%
    mutate(dyad_id = cur_group_id()) %>%
    ungroup()
                   
  dyad_data_upstream <- left_join(dyad_data, era5_gps_childmort,
                                  by = c("upstream" = "DHSID"),
                                  keep = TRUE) 
  
  # filter years that don't actually exist, and create variables 
  dyad_data_updown <- left_join(dyad_data_upstream, era5_gps_childmort,
                                by = c("downstream" = "DHSID", "year" = "year"),
                                suffix = c("_u","_d")) %>%
                       filter(!is.na(precip_current_annual_avg_mm_month_u) & !is.na(precip_current_annual_avg_mm_month_d)) %>% # take out the NAs from common distance but not common years
                       mutate(R_U10MR_ud = R_U10MR_u/R_U10MR_d,
                              R_U5MR_ud = R_U5MR_u/R_U5MR_d,
                              R_IMR_ud   = R_IMR_u/R_IMR_d,
                              R_NNMR_ud  = R_NNMR_u/R_NNMR_d,
                              urban_u_rural_d = ifelse((URBAN_RURA_u == "U" & URBAN_RURA_d == "R"), yes = 1, no = 0 ),
                              urban_u_urban_d = ifelse((URBAN_RURA_u == "U" & URBAN_RURA_d == "U"), yes = 1, no = 0 ),
                              rural_u_rural_d = ifelse((URBAN_RURA_u == "R" & URBAN_RURA_d == "R"), yes = 1, no = 0 ),
                              rural_u_urban_d = ifelse((URBAN_RURA_u == "R" & URBAN_RURA_d == "U"), yes = 1, no = 0 ),
                              cross_country   = ifelse((DHSCC_u != DHSCC_d), yes = 1, no = 0),
                              cross_adm_1     = ifelse((GID_1_u != GID_1_d), yes = 1, no = 0),
                              cross_adm_2     = ifelse((GID_2_u != GID_2_d), yes = 1, no = 0)
                              ) 
  

  
  
  dyad_data_updown_cut <- dyad_data_updown %>%
    filter(WN_NNMR_u >= 15 & WN_NNMR_d >=15 & 
             WN_IMR_u >=15 & WN_IMR_d >=15) %>%
    filter(R_NNMR_u > quantile(R_NNMR_u, .5,na.rm = TRUE) & R_NNMR_u < quantile(R_NNMR_u, .90,na.rm = TRUE) &
           R_NNMR_d > quantile(R_NNMR_d, .5,na.rm = TRUE) & R_NNMR_d < quantile(R_NNMR_d, .90,na.rm = TRUE) &
           R_IMR_u > quantile(R_IMR_u, .5,na.rm = TRUE) & R_IMR_u < quantile(R_IMR_u, .90,na.rm = TRUE) &
             R_IMR_d > quantile(R_IMR_d, .5,na.rm = TRUE) & R_IMR_d < quantile(R_IMR_d, .90,na.rm = TRUE))

  
  out_path <- file.path(data_external_clean,"merged","DHS_GPS_childmort_ERA5","country-level","annual")
  
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  if (i==1){
  
  saveRDS(dyad_data_updown,
          file = file.path(out_path,paste0(paste(countries,collapse = "|"),"_",period_length,"_month_window_child_mortality_GPS_ERA5_annual.rds")))
  
  
  
  saveRDS(dyad_data_updown_cut,
          file = file.path(out_path,paste0(paste(countries,collapse = "|"),"_",period_length,"_month_window_child_mortality_GPS_ERA5_annual_cut.rds")))
  
  } else {
    
    saveRDS(dyad_data_updown,
            file = file.path(out_path,paste0("Africa_all_countries_",period_length,"_month_window_child_mortality_GPS_ERA5_annual.rds")))
    
    
    
    saveRDS(dyad_data_updown_cut,
            file = file.path(out_path,paste0("Africa_all_countries_",period_length,"_month_window_child_mortality_GPS_ERA5_annual_cut.rds")))
    
    
  } # end ifelse over how to name
  
  } # end FOR loop over i=1 or 2 for which countries to include
