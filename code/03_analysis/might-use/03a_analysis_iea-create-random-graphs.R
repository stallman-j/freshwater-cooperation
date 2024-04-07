# _______________________________#
# International Agreements
# Analysis 03: IEA Database, Create Random Graphs
# 
# Stallman
# Started 2022-12-16
# Last edited: 
#________________________________#
# Startup

  rm(list = ls())

  # BIG ASK, DON'T RUN UNLESS YOU HAVE THE SPACE / TIME 

# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","international-agreements")
  
  source(file.path(home_folder,"code","00_master_ia.R"))
  
# bring in the original data----
  
  

  iea_members <- readRDS(file = file.path(data_clean,"iea_dfs","iea_members.rds"))

 
 cumulative_data_list <-readRDS(file = file.path(data_clean,"iea_matrices_year","iea_list_all_cumulative_years.rds"))
 
 sv_data_list <- readRDS( file = file.path(data_clean,"iea_matrices_year","iea_list_all_single_valued_years.rds"))
 
# testing matrix permutation ----
 
 # assume we have upper (or lower) triangular matrix
 
 make_symmetric <- function(mat) {
   M <- mat + t(mat) - diag(diag(mat))
   return(M)
 }
 
 #mat_list <- sv_data_list
 
 bootstrap_symmetric_mat <- function(mat_list){
   
   # assume dimensions of each element of mat_list are the same
   
   nrows <- nrow(mat_list[[1]])
   ncols <- ncol(mat_list[[1]])
   
   
   # initialize a new list
   new_list <- replicate(n =length(mat_list), 
                         expr = matrix(0, 
                                      nrow = nrows,
                                      ncol = ncols),
                         simplify = FALSE)
   

   
   # bootstrap by permuting each element, but recognizing that links are symmetric
   #i= 62
   
  for (i in 1:length(mat_list)) {
    
    current_mat <- mat_list[[i]]
    temp_mat <- matrix(0,
                             nrow = nrows,
                             ncol = ncols)
    
    sampling_frame <- current_mat[upper.tri(current_mat,diag= FALSE)]
    
    upper_sample <- sample(sampling_frame,
                           replace = TRUE,
                           size = length(sampling_frame))
    
    # fill the upper triangle with the randomly drawn samples
    temp_mat[upper.tri(temp_mat, diag = FALSE)] <- upper_sample 
   
    #temp_mat[1:10,1:10]
    
    new_mat <- make_symmetric(temp_mat)
    
    #new_mat[1:10,1:10]
    # fill the lower triangle with the symmetric side
    # this ws not working
    #temp_mat[lower.tri(temp_mat, diag = FALSE)] <- temp_mat[upper.tri(temp_mat, diag = FALSE)]
    
    new_list[[i]] <-  new_mat
    
  }
   
   return(new_list)
  
  
 }
 

 
 library(parallel)
 library(MASS)
 
 
 n_cores <- 15 # detectCores(logical = FALSE) - 1 # set a good number of cores
 n_boots <- 2000
 
 
 cl <- makeCluster(n_cores)
 
 # export data and functions to the cluster
 clusterExport(cl, c("n_boots","sv_data_list","bootstrap_symmetric_mat","make_symmetric"))
 
 
 # invoke clusterCall
 
 clusterEvalQ(cl, library(MASS))
 

 set.seed(12)
 
 system.time(
 boots_parLapply <- parLapply(cl = cl, 
                              1:n_boots,
                          fun = function(x,mat) bootstrap_symmetric_mat(mat_list = mat), mat = sv_data_list)
 ) 
 
 # for n_boots = 2000
 # user  system elapsed 
 # 13.64   11.97   80.03 
 
  # look at a few elements
 
 
 stopCluster(cl)
 
 system.time(
 saveRDS(boots_parLapply, file = file.path(data_clean,"iea_matrices_year",paste0("random_net_",n_boots,"_boots.rds")))
 )
 
 
 # for n_boots = 2000 and boots_parLapply about 40 G
 # user  system elapsed 
 # 283.74    7.90  739.99
 
 
 if (n_boots > 1000) {
 # set up some smaller dfs
   
   boots_num_vec <- c(3, 10, 50, 100,1000)
   
   for (i in boots_num_vec){
    
     set.seed(12)
     temp_boots <- sample(boots_parLapply, size = i)
     saveRDS(temp_boots, file = file.path(data_clean, "iea_matrices_year", paste0("random_net_",i,"boots.rds")))
   }
 }
 
 