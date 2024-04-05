# _______________________________#
# International Agreements
# Analysis 03a: IEA Database, Analyze Random Graphs
# 
# Stallman
# Started 2022-12-16
# Last edited: 
#________________________________#
# Startup

rm(list = ls())


# bring in the packages, folders, paths

home_folder <- file.path("P:","Projects","international-agreements")

source(file.path(home_folder,"code","00_master_ia.R"))

p_unload(btergm, sna, network)

# bring in the random graphs ----

  n_boots <- 3

  boots_list <-  readRDS(file = file.path(data_clean,"iea_matrices_year",paste0("random_net_",n_boots,"boots.rds"))
                         )
  

  
  iea_members <- readRDS(file = file.path(data_clean,"iea_dfs","iea_members.rds"))

  cumulative_data_list <- readRDS(file = file.path(data_clean,"iea_matrices_year","iea_list_all_cumulative_years.rds"))
  
  # turn all the diagonals to zero for the cumulative data list
  
  cumulative_data_list <- lapply(cumulative_data_list, set_diag_zero)
  
  # will use this to check that matrix sum is working
  single_valued_data_list <- readRDS(file = file.path(data_clean,"iea_matrices_year","iea_list_all_single_valued_years.rds"))
  
  # bring in the current graph ----
  
  iea_full_matrix_1961_to_2022 <- readRDS(file = file.path(data_clean,"iea_matrices_year","cumulative","iea_full_matrix_1961_to_2022.rds"))
  
  # If you don't set diagonals to zero, we just get that the country itself is its most common partner
  iea_full_matrix_1961_to_2022 <- set_diag_zero(iea_full_matrix_1961_to_2022)
  
  
  
  # look at a few elements to make sure things came in okay
  # 43 gives the 43rd year or here 1961+43 = 2004, but this is just giving the annual addition
  
  boots_list[[3]][[43]][1:5,1:5]
  
  # this is the total number of agreements from 1961 up to 2004
  
  cumulative_data_list[[43]][1:5,1:5]

  

  
  # check that it worked 
  cumulative_data_list[[43]][1:5,1:5]
  
  

# Take sums of the boots list to get the random graphs ----
  

  # look at a single year
  A_mat <- boots_list[[1]][[1]][1:5,1:5]
  B_mat <- boots_list[[1]][[62]][1:5,1:5]
  C_mat <- boots_list[[1]][[43]][1:5,1:5]
  
  # > A_mat
  # [,1] [,2] [,3] [,4] [,5]
  # [1,]    0    0    0    0    0
  # [2,]    0    0    0    1    0
  # [3,]    0    0    0    0    0
  # [4,]    0    1    0    0    0
  # [5,]    0    0    0    0    0
  # > B_mat
  # [,1] [,2] [,3] [,4] [,5]
  # [1,]    0    0    0    0    2
  # [2,]    0    0    0    0    0
  # [3,]    0    0    0    2    0
  # [4,]    0    0    2    0    2
  # [5,]    2    0    0    2    0
  # > C_mat
  # [,1] [,2] [,3] [,4] [,5]
  # [1,]    0    8    5    4    1
  # [2,]    8    0    1   13   12
  # [3,]    5    1    0    6    7
  # [4,]    4   13    6    0    3
  # [5,]    1   12    7    3    0
  # 
  # 
  test_list <- list(A_mat, B_mat, C_mat)
  
  saveRDS(test_list, file.path(data_clean,"test_list_of_matrices.rds"))
  
  test_nets <- lapply(test_list, graph_from_adjacency_matrix, 
         mode = c("undirected"),
         weighted = TRUE)
  
  saveRDS(test_nets, file.path(data_clean,"test_list_of_nets.rds"))
  
  out <- sum_mats_over_list(list(A_mat,B_mat,C_mat))
  
  
  # show how the weights should look
  plot(test_net[[3]], 
       edge.width = E(test_net[[3]])$weight)
  
  # get eigen centralities for a single node
  eigen_centrality(test_net[[1]],
                   directed = FALSE,
                   weights = E(test_net[[1]])$weight)$vector
  

# check that summing works by comparing original data ----
  
  # check the sum
  cumulative_from_single_valued <- sum_mats_over_list(single_valued_data_list)
  
  # then set diagonals to zero
  
  cumulative_from_single_valued <- set_diag_zero(cumulative_from_single_valued)
  
  # look manually
  # get a few from the summing up
  cumulative_from_single_valued[20:25,20:25]
  
  # get a few from cumulative original
  
  iea_full_matrix_1961_to_2022[20:25,20:25]
  
  #all_equal(iea_full_matrix_1961_to_2022,cumulative_from_single_valued)
  # true up to naming
  
# Do with the full list ----
  
  cumulative_boots <- lapply(boots_list,sum_mats_over_list)
  
  
  cumulative_boots <- saveRDS(cumulative_boots, file = file.path(data_clean,"iea_matrices_year",paste0("cumulative_boots_",n_boots,"_boots.rds")))
  
  cumulative_boots <- readRDS(file = file.path(data_clean,"iea_matrices_year",paste0("cumulative_boots_",n_boots,"_boots.rds")))
  
  
  
  # compare a few manually
  cumulative_boots[[1]][20:25,20:25]
 
  iea_full_matrix_1961_to_2022[20:25,20:25]
  
  # hist(cumulative_boots[[1]])
  # hist(cumulative_boots[[2]])
  # hist(cumulative_boots[[3]])
  
  
  hist(iea_full_matrix_1961_to_2022)
  
# Make Networks ----
  
  iea_net <- graph_from_adjacency_matrix(iea_full_matrix_1961_to_2022,
                                         mode = c("undirected"),
                                         weighted = TRUE)
  
  
  saveRDS(iea_net, file.path(data_clean,"iea_matrices_year","iea_net.rds"))
  
  iea_net <- readRDS(file.path(data_clean,"iea_matrices_year","iea_net.rds"))
 
  boots_net <- lapply(cumulative_boots, graph_from_adjacency_matrix, 
                      mode = c("undirected"),
                      weighted = TRUE)
  
  saveRDS(boots_net, file.path(data_clean,"iea_matrices_year",paste0("boots_net_",n_boots,"_boots.rds")))
  
  boots_net <- readRDS(file = file.path(data_clean,"iea_matrices_year",paste0("boots_net_",n_boots,"_boots.rds")))