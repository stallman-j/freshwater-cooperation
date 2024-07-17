#' compile_error_messages reads error messages which have been outputted to text files, and compiles the information from them into a single data frame for easy viewing. Useful when you've run something in parallel and have a lot of units that have been processed. Assumes you've stored the information about the unit in the name of the file somewhere, and then that you've got the message itself within the text file
#' @param error_message_path file path of where a txt file will be stored saying what the stopping error was for a particular main_river in the event that distances aren't calculated (makes parallel computing way easier)
#' @param error_message_type default is txt, plan to add rds later (for error messages stored like a data frame)
#' @param filename_to_cut a character vector containing the things to remove from the filename character string
#' @param units_varname the variable name you want in the final data frame for the units
#' @param output_path place to save the RDS which stores the df of unique units and their error messages
#' @param output_filename .rds character filename 
#' @export

compile_error_messages <- function(error_message_path = file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","river-distances-errors"),
                                   error_message_type = "txt",
                                   filename_to_cut = c("_river_distances_","error",".txt"),
                                   units_varname = "MAIN_RIV",
                                   output_path = file.path("E:","data","02_temp","merged","DHS_GLOW_HydroSHEDS","checked-main-rivers","error-messages"),
                                   output_filename = "river-distances-errors.rds"
){
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    stringr,
    tidyverse,
    tictoc
  )
  
  paths_to_create <- c(output_path
  )
  
  for (path in paths_to_create){
    
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    
  }
  
  file_list <- list.files(error_message_path)
  units_list <- file_list
  
  
  # get the unit identifier from dropping the fluff from the filename
  for (i in seq_along(filename_to_cut)) {
  units_list_tmp <- stringr::str_remove(units_list,filename_to_cut[3])

  units_list <- units_list_tmp
  
  }
  
  # build out an empty dataframe that has the length of the units being used
  # 
  
  out_df <- data.frame(character(length = length(units_list)),
                       character(length = length(units_list)),
                       character(length = length(units_list))
                       )
  names(out_df) <- c(units_varname,"error_message","error_message_no_units")
  
  tictoc::tic("Get error text")
  for (i in 1:length(units_list)){
    error_text <- readLines(con = file.path(error_message_path,file_list[i]))
    
    error_text_no_units <- stringr::str_remove(error_text,units_list[i])
    
    out_df[i,] <- c(units_list[i],error_text,error_text_no_units)
  }
  tictoc::toc()
  

  out_df <- out_df %>%
                dplyr::group_by(error_message_no_units) %>%
                dplyr::slice(1) %>%
                dplyr::ungroup()
  

  saveRDS(out_df,
          file.path(output_path,output_filename))
} # end function
                                     
                                     
                                     
