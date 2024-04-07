# Cleaning Functions, IEA

#' Read GKG Data ----
#' @description
#' Reads in GDELT data (events or GKG) from a particular input file and given a set of dates
#' Set up this way so it can be parallelized easily with the "date" parameter
#' 
#' @param input_path the filepath where your data are stored
#' @param base_file_name The namestring minus the date
#' @param date date to be downloaded, in "yyyy-mm-dd" format
#' @returns the dataframe to be read in

read_gdelt_data <- function(input_path = file.path("E:","data","01_raw","GDELT","gkg","gkg","water-related"),
                          #output_path = file.path("E:","data","02_temp","GDELT","gkg","gkg","water-related","filtered","daily"),
                          base_file_name = "_gkg_water_related.rds",
                          date ){
  
  data <- readRDS(file =file.path(input_path,paste0(date,base_file_name)))
  
  return(data)
}

#' Get Continent Outlines ----
#' #' Get Basins ----
#' @description
#' Given a vector (possibly just one) of basin IDs from HydroBASINS, select the continents which outline
#' @param basin_ids a vector of numerics giving the HYBAS_ID in HydroATLAS of the desired basins
#' @param basin_atlas_path path leading to the BasinATLAS.gdb data
#' @param output_path path for outputting the RDS file that holds the vector of countries which intersect the basin
#' @param output_filename the name of the RDS file to store the vector of country names
#' @returns a df of three-letter ISO_a3 country codes of countries intersected by the basin(S) and saves this as a vector in the output_path path
#' view old version in 02_clean_hydrobasins_scratch.R
#' 
get_continent_outlines <- function(basin_ids = c(1030022420,1030040210),
                                      basin_atlas_path = file.path("E:","data","01_raw","HydroSHEDS","HydroATLAS","BasinATLAS_Data_v10","BasinATLAS_v10.gdb"),
                                      output_path      = file.path("P:","data","02_temp","saved-information"),
                                      output_filename  = "countries_to_include.rds"
){
  
  # create the output directory if it's not already there
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  if (!require("tmap")) install.packages("tmap")
  if (!require("sf")) install.packages("sf")
  if (!require("dplyr")) install.packages("dplyr")
  
  library(tmap)
  library(sf)
  library(dplyr)
  
  data(World)
  
  sf_use_s2(FALSE)
  
  selection <- sf::st_read(dsn = basin_atlas_path,
                           layer = "BasinATLAS_v10_lev03" ) %>% # read in the data
    dplyr::filter(HYBAS_ID %in% basin_ids) %>%
    st_transform(crs = 4326)
  
  World <- sf::st_transform(World, crs = 4326)
  
  included_countries <- sf::st_intersection(World, selection) %>% # intersect with the selection we got
    st_drop_geometry() %>%
    dplyr::select(iso_a3) %>% # select the countries column
    #as.character() %>% # change to a character
    unique() # just give back the unique ones
  
  #included_countries <- unique(as.character(intersected_basins$iso_a3))
  
  saveRDS(as.character(included_countries),
          file = file.path(output_path,output_filename))
  
  rm(World,selection)
  
  return(included_countries)
}


#' Get Basins ----
#' @description
#' Given a vector (possibly just one) of basin IDs from HydroBASINS, select the countries which 
#' spatially intersect the basin
#' @param basin_ids a vector of numerics giving the HYBAS_ID in HydroATLAS of the desired basins
#' @param basin_atlas_path path leading to the BasinATLAS.gdb data
#' @param output_path path for outputting the RDS file that holds the vector of countries which intersect the basin
#' @param output_filename the name of the RDS file to store the vector of country names
#' @returns a df of three-letter ISO_a3 country codes of countries intersected by the basin(S) and saves this as a vector in the output_path path
#' view old version in 02_clean_hydrobasins_scratch.R
#' 
get_countries_from_basins <- function(basin_ids = c(1030022420,1030040210),
                                      basin_atlas_path = file.path("E:","data","01_raw","HydroSHEDS","HydroATLAS","BasinATLAS_Data_v10","BasinATLAS_v10.gdb"),
                                      output_path      = file.path("P:","environment","data","02_temp","saved-information"),
                                      output_filename  = "countries_to_include.rds"
                                      ){
  
  # create the output directory if it's not already there
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  if (!require("tmap")) install.packages("tmap")
  if (!require("sf")) install.packages("sf")
  if (!require("dplyr")) install.packages("dplyr")
  
  library(tmap)
  library(sf)
  library(dplyr)

  data(World)
  
  sf_use_s2(FALSE)
  
  selection <- sf::st_read(dsn = basin_atlas_path,
                             layer = "BasinATLAS_v10_lev03" ) %>% # read in the data
               dplyr::filter(HYBAS_ID %in% basin_ids) %>%
               st_transform(crs = 4326)

  saveRDS(selection,
          file = file.path(output_path,paste0("basins_for_",output_filename)))
  
  
  World <- sf::st_transform(World, crs = 4326)
  
  included_countries <- sf::st_intersection(World, selection) %>% # intersect with the selection we got
                        st_drop_geometry() %>%
                        dplyr::select(iso_a3) %>% # select the countries column
                        unique() # just give back the unique ones


  included_countries <- as.character(included_countries$iso_a3)
  
  saveRDS(included_countries,
          file = file.path(output_path,output_filename))
  
  
  rm(World,selection)
  
  return(included_countries)
  }


#' Get Rivers in Countries 
#' #' @description
#' Given a set of countries (which can be outputs of the get_countries_from_basins() function which come from basins) which are a vector,
#' make a shapefile of all the river segments in those countries
#' 
#' @param countries a vector of iso_a3 character names for countries
#' @param hydro_rivers_path path leading to the BasinATLAS.gdb data
#' @param output_path path for outputting the RDS file that holds the vector of countries which intersect the basin
#' @param output_filename character, name of the output filename
#' @param n_cores number of cores to use in parallel processing the shapefiles (because this intersection is a beast to run)
#' @returns a list where the first element is a sf of the rivers from HydroRIVERS in the countries given; and the second is an sf of the countries included in this
#' also saves to the output paths the associated .rds sf files
#' 
get_countries_for_splitting <- function(countries = c("BEN","BFA","CIV"),
                                      hydro_rivers_path = file.path("E:","data","01_raw","HydroSHEDS","HydroRIVERS"),
                                      countries_output_path   = file.path("P:","data","03_clean","shape-files"),
                                      countries_output_filename = "test_countries.rds",
                                      #rivers_output_path      = file.path("P:","data","03_clean","shape-files"),
                                      #rivers_output_filename  = "test_hydroRIVERS.rds",
                                      
                                      n_cores                 = detectCores(logical = TRUE) - 2
                                      ){
  
  # create the output directory if it's not already there
  if (!dir.exists(countries_output_path)) dir.create(countries_output_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
 # if (!dir.exists(rivers_output_path)) dir.create(countries_output_path, recursive = TRUE) # recursive lets you create any needed subdirectories

    if (!require("tmap")) install.packages("tmap")
  if (!require("sf")) install.packages("sf")
  if (!require("dplyr")) install.packages("dplyr")
  
  library(tmap)
  library(sf)
  library(dplyr)
  library(parallel)
  
  data(World)
  
  # get continent of the countries in the countries list; assuming for now only one continent is included
  
  continent <- World %>%
              filter(iso_a3 %in% countries) %>%
              select(continent) %>%
              st_drop_geometry() %>%
              unique()
  
  continent_string <- case_when(continent=="Africa" ~ "_af",
                                continent=="Asia" ~ "_as",
                                continent=="Europe" ~ "_eu",
                                continent=="North America" ~"_na",
                                continent=="South America" ~ "_sa",
                                TRUE ~ "")
  
  file_name_to_download <- paste0("HydroRIVERS_v10",continent_string,".gdb")
  
  hydrorivers_download_path <- file.path(hydro_rivers_path,file_name_to_download,file_name_to_download)
  
  print(paste0("Reading in ",file_name_to_download))
  hydro_rivers <- st_read(dsn = hydrorivers_download_path) %>% st_transform(crs = 4326)
  
  print("Read in HydroRIVERS")
  
  my_countries <- World %>%
                  filter(iso_a3 %in% countries) %>%
                  st_transform(crs = 4326)
  
  saveRDS(my_countries,
          file = file.path(countries_output_path,countries_output_filename))
  
  # create a vector to split up the hydro_rivers sf for parallelization of an intersection
  split_vector <- rep(1:n_cores, each = nrow(hydro_rivers) / n_cores, length.out = nrow(hydro_rivers))
  
  # split the df by the vector
  split_list <- split(hydro_rivers, split_vector)
  
  return(list(split_list,my_countries,n_cores))
         }


  
save_countries_and_hydrorivers <- function( hydro_rivers_sf,
                                   my_countries,
                                   rivers_output_path      = file.path("P:","data","03_clean","shape-files"),
                                   rivers_output_filename  = "test_hydroRIVERS.rds") {
  

  saveRDS(hydro_rivers_sf,
          file = file.path(rivers_output_path,rivers_output_filename))
  
  print("Returning the list of HydroRIVERS sf and the countries requested")
  return(list(hydro_rivers_sf,my_countries))
  
}

#' 
#' Filter themes ----
#' @description
#' Given downloaded GDELT GKG data, filters themes in the $themes column according to a vector given
#' @param data GDELT data which has at least been merged with a GKG dataframe and includes the $themes column
#' @param include_themes a vector of themes from the themes column to include
#' @returns a logical vector of length the number of obs of the df of whether the requested themes are included in each row
#' 
filter_themes <- function(data,
                          include_themes = water_management_themes)
  
{
  
  # test patterns with grepl
  pattern = paste(include_themes, collapse = "|")
  in_theme <- grepl(pattern,data$themes)
  
  return(in_theme)
  
}

#' Filter themes ----
#' @description
#' Given downloaded GDELT GKG data, adds an indicator boolean variable for whether a given theme is included and names that indicator according to the theme
#' @param data GDELT data which has at least been merged with a GKG dataframe and includes the $themes column
#' @param themes a vector of themes from the themes column to include, defauls to the include_themes included in 00_startup_parameters
#' @returns a logical vector of length the number of obs of the df of whether the requested themes are included in each row
#' 
filter_themes_create_indicator <- function(data,
                                     themes = include_themes)
  
{
 
  if (!require("stringi")) install.packages("stringi")
  library(stringi)
  
  for (theme in themes) {
    
    themes_filter <- grepl(theme, data$themes)
    
    data[theme] <- stringi::stri_detect_fixed(str = data$themes,
                                              pattern = theme
    )
  }
  

  
}

#' Filter and reduce ----
#' @description
#' Given downloaded GDELT GKG data, filters in a particular column (the searching_varname), 
#' and reduces the df to unique instances on another varname (the distinct_varname, could be same as searching_varname), and returns the data thus 
#' reduced 
#' @param data GDELT data which has at least been merged with a GKG dataframe and includes the $themes column
#' @param pattern a vector of themes from the themes column to include
#' @param pattern_type if "countries", makes a pattern that's exact in searching for the specific country. If anything else, does a regex search
#' with the pattern given; if "regex" takes the pattern as given
#' @param varname name of the variable that you want to search for the 
#' @returns the data with distinct instances on the desired column
#' 
filter_and_reduce <- function(data,
                          countries = NULL,
                          pattern_type = "regex",
                          pattern = NULL,
                          searching_varname = locations,
                          distinct_varname = urlSource,
                          reduce = TRUE)
  
{
  
  if (!require("stringi")) install.packages("stringi")
  if (!require("tidyverse")) install.packages("tidyverse")
  
  library(stringi)
  library(tidyverse)

  # get the vecnames 
  
  # convert the searching varname to a string
  searching_varname <- deparse(substitute(searching_varname))

  # pull the searching column as a vec
  searching_strings <- data[[searching_varname]]

  
  
  if (pattern_type == "countries"){
  pattern = paste(countries, collapse = "#|#")
  } else{
    pattern = pattern
  }
  
  indices <- stringi::stri_detect_regex(str = searching_strings,
                                        pattern = pattern)
  
  data <- data[indices,]
  
  if (reduce == TRUE){
  # https://www.schmidtynotes.com/blog/r/2021-12-20-pass-column-name-to-dplyr-function-in-function/
  data <- data %>%
          distinct({{distinct_varname}}, .keep_all = TRUE)
  }
  
  return(data)
  
}




#' Get URL stub----
#' @description 
#' Given a domain name and URL for a data frame, removes the domain name and leaves just the more detailed URL stub
#' @param data a dataframe which includes a URL column and a domain column (both strings)
#' @param varname_url the variable name of the URL column, in symbol (don't put quotations around it)
#' @param varname_domain the variable name of the domain column, in symbol (no quotations around it)
#' @returns outputs the column vector of URL stubs
 
get_url_stub <- function(data,
                         varname_url = documentSource,
                         varname_domain = domainSource
){
  
  set.seed(4)
  # convert the argument to a string
  varname_url <- deparse(substitute(varname_url))
  varname_domain <- deparse(substitute(varname_domain))
  
  # read in data
  
  # Extract characters after the pattern
  
  
  # pull the columns as vectors
  domain <- data[[varname_domain]]
  url    <- data[[varname_url]]
  
  ## extract the characters after the domain
  # note that sub cannot vectorize the pattern so we need stringr or stringi
  # https://stackoverflow.com/questions/36971116/removing-string-out-of-string-rowwise-in-data-frame-using-another-column-in-r
  
  after_domain <- stringi::stri_replace_all_regex(str = url,
                                                  pattern = paste0("((http://|https://)",domain,"/)|(",domain,"/)"),
                                                  replacement = "")
  
  
  return(after_domain)
  
  # stub <- sub(gkg_data)
  # pattern = paste(include_themes, collapse = "|")
  # in_theme <- grepl(pattern,gkg_data$themes)
  # 
  # gkg_data <- cbind(gkg_data,in_theme)
  # return(gkg_data)
  
} 


#' Append Dates ----
#' @description 
#' Appends into a single dataframe the dataframes given by specific dates. Warning, this can't be parallelized so if each 
#' day file is big, this will be a very computationally-intensive operation. Try to do all operations in parallel with single days first and then
#' only append dates with the smallest day datasets. Also adds "stub" the URL stub as the first col
#' @param input_path the folder where the input data live
#' @param output_path the desired location for the fully appended dataset
#' @param base_file_name the filename given which has form paste0(date,base_file_name)
#' @param dates a vector of dates where each element is a character string. Usually assumes it's in "yyyy-mm-dd" but code will run regardless
#' @returns The dataframe with all dates row-binded together, and saves the df as a csv and rds
#' 

append_dates <- function(input_path              = file.path("E:","data","02_temp","GDELT","merge","daily","full"),
                         output_path             = file.path("E:","data","02_temp","GDELT","merge","yearly","full"),
                         base_file_name          = "_water_gkg_events.rds",
                         dates        
) {

  
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  for (date in dates){
    # if it's the first element, initialize
    if (date == dates[1]){
      data <- readRDS(file =file.path(input_path,paste0(date,base_file_name)))
      print(paste0("Reading in for ",date))
      #
      
    } else{
      print(paste0("Reading in for ",date))
      
      tmp <- readRDS(file =file.path(input_path,paste0(date,base_file_name)))
      
      #
      
      data <- rbind(data,tmp)
      print(paste0("Appended ",date))
      
    } # end else
    
    
    
  } # end for loop
  
  print(paste0("Finished, saving. "))
  
  data <- cbind(data, get_url_stub(data))
  
  #data <- cbind(data, stub)
  
  out_path <- file.path(output_path,
                        paste0(head(dates,1),"_to_",tail(dates,1),
                               base_file_name))
  
  saveRDS(data,file = out_path)
  
  csv_path <- gsub(pattern = ".rds", replacement = ".csv", x = out_path)
  
  write.csv(data,
            file =csv_path)
  
  return(data)
  
} # end function

# Filter by location ----

#' @description Takes data from an input path, filters by a set of countries given, and then outputs both 
#' a RDS file and a CSV file with more limited variables, for a particular theme as given in GDELT
#' @param theme a string, the theme for which there already exist yearly data in the form "year_theme_input_filename"
#' @param input_path folder location of the input file, minus "year" subfolders which get added on in the function
#' @param input_filename character, the tail end of the file name, e.g. "_af_gkg_events.rds". Assumes it ends in .rds
#' @param output_path file-path, partial, the name_of_filter/year subfolder will get appended on
#' @param output_filename tail 
#' @param countries in FIPS 2-letter (which is what GDELT uses) the countries you want to filter on
#' @param name_of_filter the short name you want that goes in put the output filename
filter_by_location <- function(theme ,
                               input_path = file.path("C:","data_C_drive","02_temp","GDELT","merged","filtered","africa-country-theme","yearly"),
                               input_filename = "_af_gkg_events.rds",
                               output_path  = file.path("C:","data_C_drive","02_temp","GDELT","merged","filtered","africa-country-theme","yearly"),
                               output_filename  = "_af_gkg_events.rds",
                               countries = c("AO","BC","MW","MZ","NA","TZ","ZM","ZW"),
                               name_of_filter = "zambezi_countries", # goes in folder and filename
                               year = "2017",
                               format = "csv"
) {
  
  # Create output folder ----
  input_path <- file.path(input_path,year)
  

  
  output_path <- file.path(output_path,"yearly",name_of_filter,year)
  
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  
  # packages ----
  
  if (!require("stringi")) install.packages("stringi")
  library(stringi)
  
  if (!require("tidyverse")) install.packages("tidyverse")
  library(tidyverse)
  
  # append ----
  # loop over the dates and append them into a single DF
  
  
  pattern <- paste0("#",paste(countries, collapse = "#|#"),"#")
  
  print("Reading in data.")
  
  data <- readRDS(file =file.path(input_path,
                                  paste0(year,
                                         "_",theme,
                                         input_filename)))
  
  
  locations_filter <- grepl(pattern, data$locations)
  
  data <- data[locations_filter, ]
  
  
  out_path <- file.path(output_path,paste0(year,"_",theme,"_",name_of_filter,output_filename))
  
  saveRDS(data,file = out_path)
  
  csv_path <- gsub(pattern = ".rds", replacement = ".csv", x = out_path)
  
  csv_data <- data %>%
    select(idGKG,idGlobalEvent,nameQuad,merge,dateEvent,documentSource,stub,idCAMEOEvent,themes,locations,
           codeActor1,nameActor1,codeActor2,nameActor2,idCAMEOEvent,locationAction,domainSource,idCAMEOEventRoot,idCAMEOEventBase)
  
  just_agreements <- data %>%
    filter(idCAMEOEvent=="Sign formal agreement")
  
  saveRDS(just_agreements,
          file=file.path(output_path,paste0(year,"_",theme,"_",name_of_filter,"_formal_agreements",output_filename)))    
  
  rm(data)
  
  if (format == "csv") {
    if (!require("readr")) install.packages("readr")
    library(readr)
    readr::write_csv(csv_data,
                     file =csv_path)
  } else if (format == "xlsx"){
    if (!require("writexl")) install.packages("writexl")
    library(writexl)
    writexl::write_xlsx(csv_data,
                        file = csv_path)
  }

  
  rm(csv_data,out_path) 
  #return(data)
  
} # end function

filter_by_location_safe <- purrr::possibly(filter_by_location, 
                                           otherwise = NULL, 
                                           quiet = FALSE)




# Merge data by theme----
#' @description
#' Takes in data which has already been filtered and then merged to be annual, binds data annually across themes given, creates
#' indicator variables for each theme, and exports the merged data to both csv and rds
#' 

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
  input_path <- file.path(input_path,year)
  
  output_path <- file.path(output_path,year,output_subfolder)
  
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  # Packages ----
  if (!require("stringi")) install.packages("stringi")
  if (!require("tidyverse")) install.packages("tidyverse")
  
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
    if (!require("readr")) install.packages("readr")
    library(readr)
    readr::write_csv(data,
                     file =csv_path)
  } else if (format == "xlsx"){
    if (!require("writexl")) install.packages("writexl")
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


#' Get projection matrices
#'
#' @param network_df # a df that contains at least the variable you want
#' cumulatively, the desired column (group that you want the network for), 
#' and the undesired column (group that you want to drop the network for)
#' @param running_var       # character, the variable that you'll iterate through to get cumulative
#' network dataframes for, e.g. make a network for each year of members of a club
#' @param undesired_column  # character, name of column for the group that you're ultimately not interested in the projection for
#' @param desired_column    # character name of column for the group that you want the projection for
#' @param path              # character, filepath where you want these dfs to go
#' @param df_name_root      # root of the iteration for data frame names
#'
#' @return                  # puts in the path the number of dfs as indices of running_var with the min of 
#'                          # actual vals the running var takes up to each value of that vector
#' @export
#'
#' @examples get_projection_matrices(network_df       = members_pilot_all_data,
#'                                   running_var      = "year",
#'                                   undesired_column = "treaty_id", # treaty ID
#'                                   desired_column   = "country", # country ID
#'                                   path             = file.path(data_clean,"iea_matrices_pilot"),
#'                                   mat_name_root    = "iea_full_matrix")
#'
  get_projection_matrices <- function(network_df,
                                    running_var,
                                    undesired_column,
                                    desired_column,
                                    path,
                                    mat_name_root) {
    
    # check that output path exists
    if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
    
      cumulative_path    <- file.path(path,"cumulative")
      single_valued_path <- file.path(path,"single_valued")
    
    if (!dir.exists(cumulative_path)) dir.create(cumulative_path, recursive = TRUE) 
    if (!dir.exists(single_valued_path)) dir.create(single_valued_path, recursive = TRUE)
            
    # create the vector that's going to be iterating, here it's years
    running_vec <- sort(unique(network_df[[running_var]]))
    
    # the vars that you want turned into nas cumulatively, i.e all but the desired column
    na_vars <- names(network_df)[!names(network_df) %in% c(desired_column)]
    
    # this will stay the same so we can add matrices
    edgelist <- network_df[, c(desired_column, undesired_column)]
    

    # for testing
    #t <- 3
    
    for (t in 1:length(running_vec)) {
      
      # create a network which is all the correct nodes but edges / weights not added up
      
      temp_network <- network_df
      
      for (var in na_vars) {
        
        temp_network[[var]][network_df[running_var]>running_vec[t]] <- NA
      }
      
      # get the adj matrix for the full network
      M_full <- temp_network[,c(desired_column,undesired_column)] %>%
        table() %>%
        as.matrix()
      
      
      # matrix multiplication to get edge weights for just the projection network of interest
      Mrow <- tcrossprod(M_full)
      
      # divide by 2 and round up to account for double-counting
      
      Mrow_final <- ceiling(Mrow/2)
      
      # save the new cumulative matrix
      
      # if t==1 then make two versions, one annual with just that year and one that works with the subtracting format to get other annual matrices
      
      if(t==1){
        saveRDS(Mrow_final,
                file = file.path(single_valued_path,paste0(mat_name_root,"_",running_vec[t],".rds")))
        saveRDS(Mrow_final,
                file = file.path(cumulative_path,paste0(mat_name_root,"_",min(running_vec),"_to_",running_vec[t],".rds")))
      }
      else{
        saveRDS(Mrow_final,
                file = file.path(cumulative_path,paste0(mat_name_root,"_",min(running_vec),"_to_",running_vec[t],".rds")))
      }
      
      # bring in the old matrix
      
      if (t>1) {
        prior_M <- readRDS(file = file.path(cumulative_path,paste0(mat_name_root,"_",min(running_vec),"_to_",running_vec[t-1],".rds")))
        
        # get the yearly addition by subtracting current matrix from prior matrix
        
        M_current <- Mrow_final - prior_M
        
        # save the one-year matrix
        saveRDS(M_current,
                file = file.path(single_valued_path,paste0(mat_name_root,"_",running_vec[t],".rds"))
        )
      }
      
    }
    
    saveRDS(edgelist,
            file = file.path(path,paste0(mat_name_root,"_full_edgelist.rds"))
    )
    
    
    
  }

#' Get cumulative projection dfs ----
#'
#' @param full_network # a df that contains at least the variable you want
#' cumulatively, the desired column (group that you want the network for), 
#' and the undesired column (group that you want to drop the network for)
#' @param running_var       # character, the variable that you'll iterate through to get cumulative
#' network dataframes for, e.g. make a network for each year of members of a club
#' @param undesired_column  # character, name of column for the group that you're ultimately not interested in the projection for
#' @param desired_column    # character name of column for the group that you want the projection for
#' @param path              # character, filepath where you want these dfs to go
#' @param df_name_root      # root of the iteration for data frame names
#'
#' @return                  # puts in the path the number of dfs as indices of running_var with the min of 
#'                          # actual vals the running var takes up to each value of that vector
#' @export
#'
#' @examples get_cumulative_projection_dfs(full_network = members_pilot_all_data,
#'                                         running_var = "Year",
#'                                         undesired_column = "Mitch.ID", # treaty ID
#'                                         desired_column   = "country_preferred", # country ID
#'                                         path             = file.path(data_clean,"iea_matrices"),
#'                                         df_name_root     = "iea_country")
#' 
  get_cumulative_projection_dfs <- function(full_network,
                                 running_var,
                                 undesired_column,
                                 desired_column,
                                 path,
                                 df_name_root
                                 )
  {
  
  # check that output path exists, if not create it
    if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  # create the vector that's going to be iterating, here it's years
    running_vec <- sort(unique(full_network[[running_var]]))
  
  for (t in running_vec) {
    
      # back out just the edges we need
      
      # filter on the running var
      temp_network        <- full_network[full_network[running_var]<=t, ]
      
      # select only desired column
      temp_edgelist       <- temp_network[,c(desired_column,undesired_column)]
      
      temp_graph          <- graph_from_edgelist(as.matrix(temp_edgelist))
      
      # assign type so we can use bipartite projection
      V(temp_graph)$type <- bipartite_mapping(temp_graph)$type
      
      temp_projection     <- bipartite_projection(temp_graph,
                                                  multiplicity = TRUE)$proj1 %>%
        as_data_frame()
      
      saveRDS(temp_projection,
              file = file.path(path,paste0(df_name_root,"_",min(running_vec),"_to_",t,".rds")))
    
  }

}