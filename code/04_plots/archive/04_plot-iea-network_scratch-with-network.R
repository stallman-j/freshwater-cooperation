# _______________________________#
# International Agreements
# Plot 04 Plot IEA Network
# # # https://stackoverflow.com/questions/42960248/how-to-plot-networks-over-a-map-with-the-least-overlap
# almost verbatim from the above
# Stallman
# Started 2022-12-18
# Last edited: 
#________________________________#
# Startup

rm(list = ls())


# bring in the packages, folders, paths ----

  home_folder <- file.path("P:","Projects","international-agreements")

  source(file.path(home_folder,"code","00_master_ia.R"))

  # temporarily unload packages that ggnet doesn't like
  # pacman::p_unload(
  #   purrr,
  #   igraph,
  #   terra,
  #   readr)
    

  library(tidyverse)
  library(sna)
  library(maps)
 # library(network)
  if (!require("ggnet")) devtools::install_github("briatte/ggnet")
  library(ggplot2)
  library(ggnet)
  
# bring in data ----

  countries_with_codes <- readRDS(file = file.path(data_clean,"node-level","countries_with_codes.rds"))
  
  # get ids on which to merge other stuff
  ids <- seq(1, nrow(countries_with_codes))
  
  countries <- cbind(id = ids,
                     countries_with_codes)
  
  distlist_2019        <- readRDS(file = file.path(data_clean,"edge-level","distlist_2019.rds"))
  iea_full_matrix_1961_to_2022 <- readRDS(file = file.path(data_clean,"iea_matrices_year","cumulative","iea_full_matrix_1961_to_2022.rds"))
  
  # get the names and make sure they match up with the countries list
  
  matrix_rownames <- rownames(iea_full_matrix_1961_to_2022)
  
  countrynames    <- countries_with_codes$country
  
  # iea_members<- readRDS(file = file.path(data_clean,"iea_dfs","iea_members.rds"))
  # country_economic_indicators <- readRDS(file = file.path(data_temp,"node-level","country_economic_indicators.rds"))
  # country_centroids_sf <- readRDS(file = file.path(data_temp,"node-level","country_centroids_sf.rds"))
  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  
  netty <- network(iea_full_matrix_1961_to_2022,
                   matrix.type = "adjacency",
                   ignore.eval = FALSE,
                   names.eval  = "weights")
  
  
  
  # plot it quick
  
  gdata <- ggnet2(netty,
                  color = "Country",
                  legend.position = "right",
                  label = TRUE,
                  alpha = 0.2,
                  label.size = 0.1,
                  edge.size = "weights",
                  size = "degree",
                  size.legend = "Degree Centrality") +
    theme(legend.box = "horizontal")
  
# get edges from network ----
  
  netty
  
  # Network attributes:
  #   vertices = 200 
  # directed = TRUE # this is false but we'll use it
  # hyper = FALSE 
  # loops = FALSE 
  # multiple = FALSE 
  # bipartite = FALSE # this is also wrong but we're not using the bipartiteness for now
  # total edges= 39582 
  # missing edges= 0 
  # non-missing edges= 39582 
  # 
  # Vertex attribute names: 
  #   vertex.names 
  # 
  # Edge attribute names not shown 
  
  
  # show in, out, weights for the first edge in the edgelist
  netty$mel[[1]]
  # $inl
  # [1] 1
  # 
  # $outl
  # [1] 2
  # 
  # $atl
  # $atl$na
  # [1] FALSE
  # 
  # $atl$weights
  # [1] 129
  
  networkdata <- sapply(netty$mel, function(x)
    c('id_inl' = x$inl,
      'id_outl'= x$outl,
      'weight' = x$atl$weights))

  # transpose to we have edgelist, make a df 
  networkdata <- t(networkdata) %>% as.data.frame()
  
  head(networkdata)
  
  # id_inl id_outl weight
  # 1      1       2    129
  # 2      1       3    121
  # 3      1       4     55
  # 4      1       5    102
  # 5      1       6    128
  # 6      1       7    144
  
# join by countries on the left by the in id to get the node name
  
  dt <- left_join(networkdata, countries, by = c('id_inl'='id'))
  
  
  
  
  
  map_centroids <- ggplot() +
    geom_sf(data = world, alpha = 0,
            size = .3) +
    geom_sf(data = countries_with_codes,
            aes(color = ar5),
            shape = 21)
  
  map_centroids
  