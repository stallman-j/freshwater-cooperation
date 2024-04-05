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

# bring in the random graphs ----

  n_boots <- 3

  boots_list <-  readRDS(file = file.path(data_clean,"iea_matrices_year",paste0("random_net_",n_boots,"boots.rds"))
                         )
  
  iea_members <- readRDS(file = file.path(data_clean,"iea_dfs","iea_members.rds"))

  cumulative_data_list <- readRDS(file = file.path(data_clean,"iea_matrices_year","iea_list_all_cumulative_years.rds"))
  
# tell ten_boots to make each subelement a network ----
  
  set_diag_zero <- function(matrix){
    diag(matrix) <- 0
  }
  
  lapply(cumulative_data_list, set_diag_zero)
  
  cumulative_data_list_net <-  lapply(cumulative_data_list, graph_from_adjacency_matrix, mode = c("undirected"),weighted = TRUE, diag = FALSE)
  
  
  edge_weight_norm <- function(network){
    
    w2 <- E(network)$weight / max(E(network)$weight)
  }
  
  edge_weights_norm <- lapply(cumulative_data_list_net,edge_weight_norm)
  
  get_layouts <- function(network){
    m2 <- layout.fruchterman.reingold(network)
  }
  
  layouts <- lapply(cumulative_data_list_net, get_layouts)
  
  g <- cumulative_data_list_net[[60]]
  
  cut_off <- mean(E(g)$weight)
  
  trunc_g <- delete_edges(g,E(g)[weight<cut_off])
  
  plot(trunc_g,
       edge.width = E(g)/1122,
       layout = layout_with_lgl(trunc_g) )
  
  
  plot_net <- function(net,
                       max_edge_weight,
                       )
  
    
    
    
    hist(between_vec)
  
  gsize(boots_net[[1]])
  gsize(iea_net)
  
  gorder(iea_net) # number of vertices, 200
  gorder(boots_net[[1]]) # 200
  hist(betweenness(boots_net[[1]], normalized = TRUE))
  
  # max number of edges of n nodes is n(n-1) /2 for undirected : 
  # desired functions: gsize (number edges)
  # weight: not a func
  # farthest_vertices(g)$distance is greatest distance
  # degree(g, mode = c("all"))
  # betweenness(g, directed = FALSE, normalized = TRUE)
  # eigenvector_centrality()
  # 
  func = gsize
  
  
  q
  theme_bw()
  
  boots_eigen_vec   <- get_eigen_centralities(boots_net[[1]])
  
  par(mfrow = c(1,2))
  
  
  hist(iea_net_eigen_vec)
  hist(boots_eigen_vec)
  
  
  eigen_centrality_vecs_list <- lapply(test_net, get_eigen_centralities)
  
  between_vec <-  betweenness(iea_net,
                              normalized = TRUE,
                              weights = iea_net$weight)
  
  
  
  
  
  # make all the 
  cumulative_data_list_net <-  lapply(cumulative_data_list, graph_from_adjacency_matrix, mode = c("undirected"),weighted = TRUE, diag = FALSE)
  
  
  edge_weight_norm <- function(network){
    
    w2 <- E(network)$weight / max(E(network)$weight)
  }
  
  edge_weights_norm <- lapply(cumulative_data_list_net,edge_weight_norm)
  
  get_layouts <- function(network){
    m2 <- layout.fruchterman.reingold(network)
  }
  
  layouts <- lapply(cumulative_data_list_net, get_layouts)
  
  g <- cumulative_data_list_net[[60]]
  
  cut_off <- mean(E(g)$weight)
  
  trunc_g <- delete_edges(g,E(g)[weight<cut_off])
  
  plot(trunc_g,
       edge.width = E(g)/1122,
       layout = layout_with_lgl(trunc_g) )
  
  
  plot_net <- function(net,
                       max_edge_weight,
  )
    
    
    