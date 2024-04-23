#' Create NA columns
#' @description creates columns of NAs for DFs if the columns don't already exist. Good for merging a lot of similar but not exactly the same dfs
#' https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist
#' @param data data frame / df
#' @param desired_cols vector of colnames
#' @returns data frame with columns added in NA if they didn't already exist
#' @@examples
#' # example code
#' add_na_cols(mtcars, "mpg")
#' add_na_cols(mtcars, c("topspeed","nhj","mpg"))
#' @export

add_na_cols <- function(data, desired_cols) {
  add <-desired_cols[!desired_cols%in%names(data)]
  
  if(length(add)!=0) data[add] <- NA
  data
}





#' Description: Create Long-run variables
#' @description With a df with a time dimension, create a bunch of long-run variables
#' @param panel_df panel data frame
#' @param id_varname the variable name of the
#' @param time_varname variable name of the largest
#' @param variable_to_manipulate character vector of the variable that you want long-run values for
#' @param variable_namestub character vector, the namestub that you want appended to the long-run varnames
#' @param out_path output file.path() to where to save the final data frame. Default is NULL, in which case the df isn't saved
#' @param output_filename filename to save the final data frame, if you provide an out_path. Default NULL doesn't do anything
#' @returns a data frame that for the variable_to_manipulate
#' @examples
#' # example code
#'
#' @export
create_long_run_vars <- function(panel_df,
                                 id_varname   = "DHSID",
                                 time_varname = "year",
                                 variable_to_manipulate = "precip_annual_mean",
                                 variable_namestub      = "precip",
                                 out_path = NULL,
                                 out_filename = NULL
                                 
){
  # create some variables to manipulate; this is easier in base R than dplyr?
  panel_df[["time_var"]]          <- panel_df[[time_varname]]
  panel_df[["var_to_manipulate"]] <- panel_df[[variable_to_manipulate]]
  panel_df[["id_var"]]            <- panel_df[[id_varname]]
  
  panel_df_tmp <- panel_df %>%
    dplyr::filter(!is.na(time_var)) %>%
    dplyr::group_by(time_var)%>%
    dplyr::arrange(id_varname) %>% # sorted by ID, chronologically within
    dplyr::ungroup() %>%
    dplyr::group_by(id_var) %>%
    dplyr::mutate(lr_avg = mean(var_to_manipulate),
                  lr_sd  = sd(var_to_manipulate),
                  lr_zscore = (var_to_manipulate - mean(var_to_manipulate))/(sd(var_to_manipulate))) %>%
    dplyr::ungroup()
  
  
  # change the variable names
  for (varname in c("lr_avg","lr_sd","lr_zscore")) {
    names(panel_df_tmp)[names(panel_df_tmp)== varname] <- paste0(variable_namestub, "_",varname)
    
  }
  
  # take out the temp vars we made to make the code easy
  panel_df_out <- panel_df_tmp %>%
    dplyr::select(-c(time_var,var_to_manipulate,id_var))
  
  if (!is.null(out_path) & !is.null(out_filename)){
    if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE) # recursive lets you create any needed subdirectories
    
    saveRDS(panel_df_out,
            file = file.path(out_path,
                             out_filename))
    
  }
  rm(panel_df_tmp)
  
  return(panel_df_out)
}
