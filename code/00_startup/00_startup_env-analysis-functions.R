# Analysis Functions, IEA
#' load_matrices()
#' Getting the output from "get_projection_matrices()" back into your workspace
#' @param network_df # a df that contains at least the variable you want
#' cumulatively, the desired column (group that you want the network for), 
#' and the undesired column (group that you want to drop the network for)
#' @param running_var       # character, the variable that you'll iterate through to get cumulative
#' network dataframes for, e.g. make a network for each year of members of a club
#' @param path              # character, filepath to the folder where the dfs live
#' @param mat_name_root      # character, root of the iteration for data frame names
#' @param cumulative           # TRUE or FALSE, if you want to get cumulative values (e.g. t from 61 to 95)
#' @param single_valued        # boolean, if you want just to iterate over single-valued values of the running var
#' 
#' @return                  # brings the desired matrices into the workspace, returns a df of the filenames of the desired vecs
#' @export                  
#'
#' @examples              load_matrices(network_df               = iea_members 
#'                                      running_var             = "year"
#'                                      path                    = file.path(data_clean,"iea_matrices")
#'                                      mat_name_root           = "iea_full_matrix"
#'                                      cumulative              = TRUE # if you want cumulative
#'                                      single_valued           = TRUE # if you want just iterating through each value


# Get Matrices

  load_matrices <- function(network_df,
                            running_var,
                            path,
                            mat_name_root,
                            cumulative = FALSE,
                            single_valued = TRUE) {


  running_vec <- sort(unique(network_df[[running_var]]))
  
  
  # t<- 3
  
  # prefill a vector with the names

  cumulative_names <- rep(NA, times = length(running_vec))
  single_valued_names <- rep(NA, times = length(running_vec))
  
  
  for (t in 1:length(running_vec)) {
    
    if (cumulative) {
      
      temp_mat <- readRDS(file = file.path(path,paste0(mat_name_root,"_",min(running_vec),"_to_",running_vec[t],".rds")))
      
      mat_name <- paste0(mat_name_root,"_",min(running_vec),"_to_",running_vec[t])
      assign(mat_name, temp_mat, envir = .GlobalEnv)
      
      cumulative_names[t] <- mat_name 
    } # end if cumulative
    
    if (single_valued) {
      
      temp_mat <- readRDS(file = file.path(path,paste0(mat_name_root,"_",running_vec[t],".rds")))
      
      mat_name <- paste0(mat_name_root,"_",running_vec[t])
      assign(mat_name, temp_mat, envir = .GlobalEnv ) # set environment to global to make it stick around after the file ends
      
      single_valued_names[t] <- mat_name
      
    } # end if single-valued
    
  } # end t to length(running_vec)
  
  # if (cumulative & single_valued){
  #   df <- data.frame(cumulative_names = cumulative_names,
  #                    single_valued_names = single_valued_names)
  #   
  #   return(df)
  # } else if (!cumulative & single_valued) {
  #   return(as.data.frame(single_valued_names))
  # } else if (cumulative & !single_valued) {
  #   return(as.data.frame(cumulative_names))
  # }
  
  
  }
  
#' Function set_diag_zero()
#' Sets the diagonal of a matrix to zero
#' @param matrix input matrix
#' returns the matrix with diagonals as zero

  set_diag_zero <- function(matrix){
    {
      diag(matrix) <- 0
      
      return(matrix)
    }
  }
  
#'   # sum over a list of matrices
#'   @param list_of_matrices a list where each element is a matrix

  sum_mats_over_list <- function(list_of_matrices) {
    Reduce("+",list_of_matrices)
  }
  
# Network variables ----
  
# Get Eigenvector centralities  ----
  # given a net, get the undirected but weighted vector of eigen centralities
  # "df" return a dataframe, "vec" return a vector
  
  get_eigen_centralities <- function(net,
                                     vec_or_df = "df") {
    

    eigen_centrality_vec <- eigen_centrality(net,
                                             directed = FALSE,
                                             weights = E(net)$weight)$vector
    
    eigen_centrality_df <- as.data.frame(x = eigen_centrality_vec)
    
    if (vec_or_df == "vec") {
      return(eigen_centrality_vec)
    } else{
      return(eigen_centrality_df)
    }
  }

  
  get_eigen_centralities_vec <- function(net,
                                     vec_or_df = "df") {
    
    
    eigen_centrality_vec <- eigen_centrality(net,
                                             directed = FALSE,
                                             weights = E(net)$weight)$vector
    

  }
  
  
  
# Get weights
  
  get_weights <- function(net,
                          vec_or_df = "df") {
    
    weights_vec <- E(net)$weight
    
    weights_df <- data.frame(weight = weights_vec)
    
    if (vec_or_df == "vec") {
      return(weights_vec)
    } else{
      return(weights_df)
    }
    
  }
  
  get_weights_vec <- function(net) {
    
    weights_vec <- E(net)$weight
    
    
  }

# Get rows of examples of vars ----
  
#' Get number of unique and exmaples for placement in a table 
#' @param var vector of style df$varname, vector that you'd like to get the unique number
#' of values for and want some examples for
#' @param df dataframe from which you're taking it
#' @param sample_size numeric, number of examples
#' @param notes character, if you'd like to add any notes to the end of examples
#' 
#' @example  get_examples_table_row(var =iea_members$Treaty.Name.from.Treaty.Table,
#'                                  df  = iea_members,
#'                        sample_size   = sample_size,
#'                        notes         = "Test Notes")
#'                        
#' @return character vector of size 3,
#' the above yields 
#'  [2] "5"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#'  [3] "Agreement on cooperation in the field of veterinary medicine between the Government 
#'  of the State of Israel and the Government of the Republic of Albania (N = 4);  Arrangement between
#'   the Government of the Federal Republic of Germany and the Government of the Republic of Ghana 
#'   concerning technical and economic cooperation in the project 'Forest Protection and Resource 
#'   Use Management Project (FORUM)' (N = 4);  Agreement between the government of the Russian 
#'   Federation and the government of the Republic of Senegal on cooperation in the area of fisheries
#'    (N = 4);  Agreement between the Government of the Russian Federation and the Government of 
#'    the United States of America about cooperation according to the GLOUB program (N = 2);  
#'    Agreement on cross-border cooperation in the field of research, development and protection
#'     of natural resources (N = 16).Test Notes"
  
  # tells it to add linebreaks for output to tex upon collapsing
  
  line_break_function <- function(x){
    gsub("$<$br$>$","<br>",x)
  }
  
  options(xtable.sanitize.text.function = identity)
  
  
  get_examples_table_row <- function(var,
                                     df,
                                     sample_size,
                                     notes = "",
                                     newline = TRUE,
                                     seed = 4) {
    set.seed(seed)
    num_unique <- length(unique(var))
    examples   <- sample(unique(var),
                         size = sample_size)
    
    # get obs count
    obs_counts <- rep(NA, times = sample_size )
    examples_plus_counts <- rep(NA, times = sample_size)
    
    for (i in 1:sample_size) {
      obs_counts[i]        <- df[var==examples[i],] %>%
        nrow()
      
      if (i<sample_size) {
        examples_plus_counts[i] <- paste0(examples[i]," (N = ",obs_counts[i],"); ")
      } else
      {examples_plus_counts[i] <- paste0(examples[i]," (N = ",obs_counts[i],").")
      }
    }
    
    if(notes == "" & newline ==TRUE){
      # collapse examples into a single string 
      collapsed <- paste0("Examples: ", paste(examples_plus_counts, collapse = " \\newline "))
      
    } else if (notes!= "" & newline == TRUE) {
      
      collapsed <- paste0(notes, "\\newline Examples: ", paste(examples_plus_counts, collapse = " \\newline "))
    
      } else if (notes =="" & newline !=TRUE){
      
      collapsed <- paste0("Examples: ", paste(examples_plus_counts, collapse = " "))
      
    }else if (notes != "" & newline !=TRUE){
      collapsed <- paste0(notes, "\\newline Examples: ", paste(examples_plus_counts, collapse = " "))
      
    }
    
    
    # generate row vector
    row_vec <- c(num_unique, collapsed)
    
  }
  
  
  
  get_hypothesis_test_row <- function(observed_net,
                                      list_of_nets,
                                      stat_func,
                                      aggregating_func) {
    
    observed_vec <- stat_func(observed_net)
    
    observed_val <- aggregating_func(observed_vec)
    
    list_vec     <- lapply(list_of_nets, stat_func)
    
    list_vals    <- sapply(list_vec, aggregating_func)
    
    get_row      <- get_stats(observed_val,
                              list_vals)
    
    return(get_row)
    
  }
  
  get_2_point_5_percentile <- function(vector) {
    out <- quantile(vector, 0.025)
  }
  
  get_97_point_5_percentile <- function(vector){
    out <- quantile(vector, 0.975)
    
  }
  
  get_median <- function(vector){
    out <- quantile(vector, 0.5)
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  