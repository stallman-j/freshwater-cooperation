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

  n_boots <- 1000

  boots_list <-  readRDS(file = file.path(data_clean,"iea_matrices_year",paste0("random_net_",n_boots,"boots.rds"))
                         )
  
  iea_members <- readRDS(file = file.path(data_clean,"iea_dfs","iea_members.rds"))

  cumulative_data_list <- readRDS(file = file.path(data_clean,"iea_matrices_year","iea_list_all_cumulative_years.rds"))
  
  
  # will use this to check that matrix sum is working
  single_valued_data_list <- readRDS(file = file.path(data_clean,"iea_matrices_year","iea_list_all_single_valued_years.rds"))
  
  # look at a few elements to make sure things came in okay
  # 43 gives the 43rd year or here 1961+43 = 2004, but this is just giving the annual addition
  
  boots_list[[3]][[43]][1:5,1:5]
  
  # this is the total number of agreements from 1961 up to 2004
  
  cumulative_data_list[[43]][1:5,1:5]

  
  # turn all the diagonals to zero for the cumulative data list
  
  cumulative_data_list <- lapply(cumulative_data_list, set_diag_zero)
  
  # check that it worked 
  cumulative_data_list[[43]][1:5,1:5]
  
  
# bring in the current graph ----
  
  iea_full_matrix_1961_to_2022 <- readRDS(file = file.path(data_clean,"iea_matrices_year","cumulative","iea_full_matrix_1961_to_2022.rds"))
  
  # If you don't set diagonals to zero, we just get that the country itself is its most common partner
  iea_full_matrix_1961_to_2022 <- set_diag_zero(iea_full_matrix_1961_to_2022)
  
  
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
  
  out <- sum_mats_over_list(list(A_mat,B_mat,C_mat))

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
  
  cumulative_boots <- readRDS(file=file.path(data_clean,"iea_matrices_year",paste0("cumulative_boots_",n_boots,"_boots.rds")))
  
  
  # compare a few manually
  cumulative_boots[[1]][20:25,20:25]
 
  iea_full_matrix_1961_to_2022[20:25,20:25]
  
  # hist(cumulative_boots[[1]])
  # hist(cumulative_boots[[2]])
  # hist(cumulative_boots[[3]])
  
  
  hist(iea_full_matrix_1961_to_2022)
  
  
# Want: 1) average weight, 2) max weight 3) min weight 3) sd of weights 4) 
  
# Get distributions for a single function ----
  
  iea_net <- graph_from_adjacency_matrix(iea_full_matrix_1961_to_2022,
                                         mode = c("undirected"),
                                         weighted = TRUE)
  

  boots_net <- lapply(cumulative_boots, graph_from_adjacency_matrix, 
                      mode = c("undirected"),
                      weighted = TRUE)
  
  test_net <- lapply(test_list, graph_from_adjacency_matrix,
                     mode = c("undirected"),
                     weighted = TRUE)
  
  # show how the weights should look
  plot(test_net[[3]], 
       edge.width = E(test_net[[3]])$weight)
  
  # get eigen centralities for a single node
  eigen_centrality(test_net[[1]],
                   directed = FALSE,
                   weights = E(test_net[[1]])$weight)$vector
  
  

  par(mfrow = c(1,1))
  

  
 iea_net_eigen_df <- get_eigen_centralities(iea_net)
 
 boots_eigens <- sapply(boots_net, get_eigen_centralities)
 
 eigens_mat <- sapply(boots_eigens, cbind)
 
 
 # https://stackoverflow.com/questions/49024023/displaying-a-density-plot-from-a-bootstrap
 
 # plot all layers
 
 density_plot_all_layers <- function(base_df,
                             plot_var_index,
                             layers_list,
                             where_text_y,
                             where_text_x,
                             ...) {
   
   dx <- density(x = base_df[,plot_var_index])
   
   plot(dx,
        #prob = TRUE,
        col  = yale_blue,
        bty  = "l", # remove the box around 
        lwd = 4,
          ...)


  for (i in 1:length(layers_list)) {
      lines(density(layers_list[[i]]),
                   col = rgb(.39,.67,1, alpha = 0.1))
  }
   
   abline(v = mean(base_df[,plot_var_index]),
          col = yale_blue,
          lty = "dashed")
   
   text(x= quantile(base_df[,plot_var_index], where_text_x[1]),
        y = where_text_y[1],
        col = yale_blue,
        labels = "Observed Density")
   
   text(x= quantile(base_df[,plot_var_index], where_text_x[2]),
        y = where_text_y[2],
        col = yale_blue,
        labels = "Observed Mean")
   

   text(x= quantile(base_df[,plot_var_index], where_text_x[3]),
        y = where_text_y[3],
        col = yale_lblue,
        labels = "Bootstrapped Densities")
  
 }
 
 par(xpd = FALSE) # to keep line from running outside the plot
 
 png(file = file.path(output_figures,paste0("eigenvector_centrality_actual_v_",n_boots,"_boots.png")),
     width = 800,
     height = 800,
     units = "px",
     bg = "white",
     res = 120)
  density_plot_all_layers(base_df = iea_net_eigen_df,
                          plot_var_index = 1,
                          layers_list = boots_eigens,
                          xlim = c(0,1.2),
                          ylim = c(0,30),
                          where_text_y = c(2,4,20),
                          where_text_x = c(.05,.67,.99),
                          xlab = "Eigenvector Centrality (Higher == More Central)",
                          ylab = "Density",
                          main = paste0("Actual v. Bootstrapped Eigenvector Centrality, ",n_boots," Boots"))

  dev.off()

# Do this as well with weights ----
  
  
 # get all weights from the original
  
  # 19791 weights
  iea_weights_df <- data.frame(weight = E(iea_net)$weight)

  get_weights <- function(net) {
    weights_vec <- data.frame(weight = E(net)$weight)
  }

  boots_weights <- sapply(boots_net, get_weights)    
  
  
  png(file = file.path(output_figures,paste0("shared_ieas_actual_v_",n_boots,"_boots.png")),
      width = 1000,
      height = 800,
      units = "px",
      bg = "white",
      res = 120)
  
  
  density_plot_all_layers(base_df = iea_weights_df,
                          plot_var_index = 1,
                          layers_list = boots_weights,
                          xlim = c(0,1200),
                          ylim = c(0,.0125),
                          where_text_y = c(.0008,.0056,.01),
                          where_text_x = c(.99,.72,.23),
                          xlab = "Number of Mutually Joined IEAs Across Countries",
                          ylab = "Density",
                          main = paste0("Actual v. Bootstrapped Mutual IEA Counts, ",n_boots," Boots")
                          )
  dev.off()
  
  