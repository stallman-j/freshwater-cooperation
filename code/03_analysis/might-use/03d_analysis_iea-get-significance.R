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

p_unload(btergm, sna, network,ergm)

# bring in the random graphs ----

  n_boots <- 1000

  cumulative_boots <- readRDS(file = file.path(data_clean,"iea_matrices_year",paste0("cumulative_boots_",n_boots,"_boots.rds")))

  iea_full_matrix_1961_to_2022 <- readRDS(file = file.path(data_clean,"iea_matrices_year","cumulative","iea_full_matrix_1961_to_2022.rds"))
  
  # If you don't set diagonals to zero, we just get that the country itself is its most common partner
  iea_full_matrix_1961_to_2022 <- set_diag_zero(iea_full_matrix_1961_to_2022)
  
  test_list_of_nets <- readRDS(file.path(data_clean,"test_list_of_nets.rds"))

  test_list_of_matrices <- readRDS(file.path(data_clean,"test_list_of_matrices.rds"))
  
# Bring in the networks ----
  
  iea_net <- readRDS(file.path(data_clean,"iea_matrices_year","iea_net.rds"))
  
  boots_net <- readRDS(file = file.path(data_clean,"iea_matrices_year",paste0("boots_net_",n_boots,"_boots.rds")))
  
 
# Get desired values ----

  par(mfrow = c(1,1))
  
 iea_net_eigen_vec <- get_eigen_centralities_vec(iea_net)
 
 boots_eigens <- sapply(boots_net, get_eigen_centralities_vec)
 
 test_eigens  <- sapply(test_list_of_nets, get_eigen_centralities_vec)
 
 observed_mean <- mean(iea_net_eigen_vec)

 
 get_stats <- function(observed_val,
                       stats_vec) {
   
   quantile_2_point_5  <- quantile(stats_vec, 0.025)
   quantile_97_point_5 <- quantile(stats_vec, 0.975)
   median              <- quantile(stats_vec, 0.5)
   mean                <- mean(stats_vec)
   
   out <- c(Observed = observed_val, "Bootstrap Mean" = mean, median, quantile_2_point_5,quantile_97_point_5)
   return(out)
 }
 
 
 # 19791 weights
 iea_weights_df <- data.frame(weight = E(iea_net)$weight)
 
 observed_max_weight <- max(iea_weights_df)
 
 test_weights <- sapply(test_list_of_nets, get_weights)
 
 max_test_weights <- sapply(test_weights, max)
 
 get_max_weights_row <- get_stats(observed_max_weight,
                                  max_test_weights)
 
 get_max_weights_row
 

 

 
 
 
 mean_weights <- get_hypothesis_test_row(observed_net = iea_net,
                                        list_of_nets = boots_net,
                                        stat_func = get_weights_vec,
                                        aggregating_func = mean)
 
 
 median_weights <- get_hypothesis_test_row(observed_net = iea_net,
                                           list_of_nets = boots_net,
                                           stat_func = get_weights_vec,
                                           aggregating_func = get_median)

 
 min_weights <- get_hypothesis_test_row(observed_net = iea_net,
                                        list_of_nets = boots_net,
                                        stat_func = get_weights_vec,
                                        aggregating_func = min)
 
 pctl_2_point_5_weights <- get_hypothesis_test_row(observed_net = iea_net,
                                        list_of_nets = boots_net,
                                        stat_func = get_weights_vec,
                                        aggregating_func = get_2_point_5_percentile)
 
 
 pctl_97_point_5_weights <- get_hypothesis_test_row(observed_net = iea_net,
                                                  list_of_nets = boots_net,
                                                  stat_func = get_weights_vec,
                                                  aggregating_func = get_97_point_5_percentile)
 
 
 
 max_weights <- get_hypothesis_test_row(observed_net = iea_net,
                                        list_of_nets = boots_net,
                                        stat_func = get_weights_vec,
                                        aggregating_func = max)
 
 data <- as.data.frame(rbind(mean_weights,
               median_weights,
               min_weights,
               pctl_2_point_5_weights,
               pctl_97_point_5_weights,
               max_weights,
               c(nrow(iea_weights_df),n_boots,n_boots,n_boots,n_boots)))
 
 colnames(data) <- c("Observed","Bootstrap: Mean","Median","2.5\\%","97.5\\%")
 
 rownames(data) <- c("Mean Weight",
                     "Median Weight",
                     "Min. Weight",
                     "2.5\\% Weight",
                     "97.5\\% Weight",
                     "Max Weight",
                     "N")
 
 data
 
 tabpath <- file.path(output_tables,paste0("iea_weights_percentiles.tex"))
 
 data_tex <- xtable(data, 
                      caption = "IEA Distribution, Actual v. Bootstrapped",
                      label = "tab:iea_weights_percentiles",
                      digits = c(0,2,2,2,2,2))
 print(data_tex,
       file=tabpath,
       append=FALSE,
       table.placement = "H",
       caption.placement="top")
 
 data
 
 # show how the weights should look
 plot(test_list_of_nets[[3]], 
      edge.width = E(test_list_of_nets[[3]])$weight)

 
# Do this as well with weights ----
  
 # get all weights from the original
  
  

  #boots_weights <- sapply(boots_net, get_weights)    
  

# Export to Latex ----
  
  # treatment effect table
  te_tab <- data.frame("S_RF" = c(3.0293840243985, 4.340987, 3.6),
                       "T_RF" = c(1.00000, 5.8, 4.4))
  rownames(te_tab) <- c("ATE", "ATT", "ATC")
  
  te_tab
  
  tabpath <- file.path(output_tables,paste0("scratch-tab.tex"))
  
  te_tab_tex <- xtable(te_tab, 
                       caption = "Scratch Table",
                       label = "scratch-tab",
                       digits = c(0,4,4))
  print(te_tab_tex,
        file=tabpath,
        append=FALSE,
        table.placement = "H",
        caption.placement="bottom")
  
  
  