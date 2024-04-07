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

  cumulative_boots <- readRDS(file = file.path(data_clean,"iea_matrices_year",paste0("cumulative_boots_",n_boots,"_boots.rds")))

  iea_full_matrix_1961_to_2022 <- readRDS(file = file.path(data_clean,"iea_matrices_year","cumulative","iea_full_matrix_1961_to_2022.rds"))
  
  # If you don't set diagonals to zero, we just get that the country itself is its most common partner
  iea_full_matrix_1961_to_2022 <- set_diag_zero(iea_full_matrix_1961_to_2022)
  
  
# Bring in the networks ----
  
  iea_net <- readRDS(file.path(data_clean,"iea_matrices_year","iea_net.rds"))
  
  boots_net <- readRDS(file = file.path(data_clean,"iea_matrices_year",paste0("boots_net_",n_boots,"_boots.rds")))
  
 
# Get desired values ----

  par(mfrow = c(1,1))
  
  
  
 iea_net_eigen_df <- get_eigen_centralities(iea_net)
 
 boots_eigens <- sapply(boots_net, get_eigen_centralities)
 
 eigens_mat <- sapply(boots_eigens, cbind)
 
 
 # https://stackoverflow.com/questions/49024023/displaying-a-density-plot-from-a-bootstrap
 
 # plot all layers
 
 
 
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
  
  
  
