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


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","international-agreements")
  
  source(file.path(home_folder,"code","00_master_ia.R"))
  
# bring in the random graphs ----
  
  

  iea_members <- readRDS(file = file.path(data_clean,"iea_dfs","iea_members.rds"))
  
# Function Inputs ----
  # if matrices have been cleaned to have annual adjacency matrices, pull them
  # back out to do analysis 'n stuff
  
  
  # Get Matrices
  
  
  network_df               <- iea_members 
  running_var             <- "year"
  path                    <- file.path(data_clean,"iea_matrices")
  mat_name_root           <- "iea_full_matrix"
  cumulative              <- TRUE # if you want cumulative
  single_valued           <- TRUE # if you want just iterating through each value
  # Get Matrices
  
  running_vec <- sort(unique(network_df[[running_var]]))
  
  
  # t<- 3
  
  # prefill a vector with the names
  
  cumulative_names <- rep(NA, times = length(running_vec))
  single_valued_names <- rep(NA, times = length(running_vec))
  
  
  for (t in 1:length(running_vec)) {
  
    if (cumulative) {
    
      temp_df <- readRDS(file = file.path(path,paste0(mat_name_root,"_",min(running_vec),"_to_",running_vec[t],".rds")))
      assign(paste0(mat_name_root,"_",min(running_vec),"_to_",running_vec[t]), temp_df)
      
    } # end if cumulative
    
    if (single_valued) {
      
      temp_df <- readRDS(file = file.path(path,paste0(mat_name_root,"_",running_vec[t],".rds")))
      assign(paste0(mat_name_root,"_",running_vec[t]), temp_df)
      
    }
    
    # if (cumulative & single_valued){
    #   df <- data.frame(cumulative_names = cumulative_names,
    #                    single_valued_names = single_valued_names)
    #   
    #   return(df)
    # } else if (!cumulative & single_valued) {
    #   return(as.data.frame(single_valued))
    # } else if (cumulative & !single_valued) {
    #   return(as.data.frame(cumulative))
    # }
    
  

  
  }
  
  rm(list = ls())

  home_folder <- file.path("P:","Projects","international-agreements")
  
  source(file.path(home_folder,"code","00_master_ia.R"))
  
  # bring in the random graphs ----
  
  
  
  iea_members <- readRDS(file = file.path(data_clean,"iea_dfs","iea_members.rds"))
  
  
 names <- load_matrices(network_df        = iea_members,
               running_var             = "year",
               path                    = file.path(data_clean,"iea_matrices_year"),
               mat_name_root           = "iea_full_matrix",
               cumulative              = TRUE, 
               single_valued           = TRUE)
 
 
 
 cumulative_path <- file.path(data_clean,"iea_matrices_year","cumulative")
 cumulative_list_RDS <- list.files(path = cumulative_path)
 cumulative_data_list           <- lapply(file.path(cumulative_path,cumulative_list_RDS), readRDS)
 names_cumulative_data          <- gsub(".rds","",cumulative_list_RDS)
 
 names(cumulative_data_list) <- names_cumulative_data
 
 
 
# testing matrix permutations ----
 
 
 
 matty <- matrix(data = c(0,1,2,3,5,6,7,8,9,10,11,12), nrow = 3, ncol = 4)
 
 sample(matty, replace = TRUE)
 
 set.seed(4)
 
 matty_list <- list(a_mat = matrix(rpois(9,20), nrow = 3),
                    b_mat = matrix(rpois(9,20), nrow = 3),
                    c_mat = matrix(rpois(9,20), nrow = 3))
 
 
 mat_list = matty_list
 
 mat_list
 # $a_mat
 # [,1] [,2] [,3]
 # [1,]   20   27   19
 # [2,]   17   23   20
 # [3,]   17   14   23
 # 
 # $b_mat
 # [,1] [,2] [,3]
 # [1,]   32   24   19
 # [2,]   19   20   19
 # [3,]   24   16   18
 # 
 # $c_mat
 # [,1] [,2] [,3]
 # [1,]   26   25   25
 # [2,]   20   22   24
 # [3,]   25   18   15
 
 
 
 # sample with replacement from the upper triangle of the matrices,
 # and then create new matrix assuming that the lower triangle is the same 
 # because these adj matrices are all symmetric
 
 bootstrap_symmetric_mat(mat_list = matty_list) 
 
 n_boots <- 10
 
 boots_data <- replicate(n = n_boots,
                         bootstrap_symmetric_mat(mat_list = matty_list) )
 
 
 
 # display the first run through t=1 to t=3
 
 boots_data[, 1]
 # [[1]]
 # [,1] [,2] [,3]
 # [1,]    0   19   20
 # [2,]   19    0   19
 # [3,]   20   19    0
 # 
 # [[2]]
 # [,1] [,2] [,3]
 # [1,]    0   19   19
 # [2,]   19    0   19
 # [3,]   19   19    0
 # 
 # [[3]]
 # [,1] [,2] [,3]
 # [1,]    0   25   25
 # [2,]   25    0   25
 # [3,]   25   25    0
 
 # compare with sampling frame
 

 
 
 # display all values of the permutations of the first draw
 # all the first row which is the samples of a_mat
 
 boots_data[1,] # 10 of them, has only 19 20 and 27
 
 # graphing ----
 
 
  g <- graph_from_adjacency_matrix(cumulative_data_list[[62]],
                                   mode = c("undirected"),
                                   weighted = TRUE)

  norm_weight <- E(g)$weight / max(E(g)$weight)
  m2          <- layout_nicely(g)

  plot(g,
       edge.width = norm_weight,
       layout = m2)

  as_data_frame(g) %>% view()

  g1 <- graph_from_adjacency_matrix(mat_list$a_mat,
                                    mode = c("undirected"),
                                    weighted = TRUE)

  plot(g1,
       edge.width = E(g)$weight)

  # test if this gives the same thing



 # check on original data ----

  net <- graph_from_edgelist(as.matrix(iea_members[,c("country","treaty_id")]))


  V(net)$type <- bipartite_mapping(net)$type

  net_df <- as_data_frame(net)

  par(mfrow=c(1,1),mar=c(0,1,1,1))

  m <- layout_nicely(net)

  plot(net, main="Full Network",edge.arrow.size=0.5,
       vertex.label.color = "black",
       edge.color = "black",
       vertex.color = yale_lblue,
       layout = m)

  m1 <- layout_nicely(bipartite_projection(net)$proj1)

  plot(bipartite_projection(net)$proj1,
       main="Treaty Network",
       vertex.label.color = "black",
       edge.color = "black",
       vertex.color = yale_lblue,
       layout = m1
  )

  m2 <- layout_nicely(bipartite_projection(net)$proj2)

  plot(bipartite_projection(net)$proj2,
       main="Country Ties",
       vertex.label.color = "black",
       edge.color = "black",
       vertex.color = yale_lblue,
       layout = m2)


  # try out


  # countries ----

  # multiplicity = TRUE gives the total number of times
  # these guys show up together

  country_net <- bipartite_projection(net,
                                      multiplicity = TRUE)$proj2
  
  
# bootstrap with different functions ----
  
  
  # calculate for real ----
  
  
  set.seed(4)
  n_boots <- 100
  
  
  system.time(
    boots_rep <- replicate(n = n_boots,
                           bootstrap_symmetric_mat(mat_list = sv_data_list) )
  )
  
  # user  system elapsed 
  # 0.29    0.02    0.53
  
  set.seed(4) # faster
  system.time(
    boots_sapply <- sapply(X = 1:n_boots, # x says to repeat 1:n_boots times
                           FUN = function(x,mat) bootstrap_symmetric_mat(mat_list = mat), mat = sv_data_list)
  )
  
  # user  system elapsed 
  # 0.33    0.03    0.58
  # 
  
  set.seed(4)
  system.time(
    boots_lapply <- lapply(X = 1:n_boots,
                           FUN = function(x,mat) bootstrap_symmetric_mat(mat_list = mat), mat = sv_data_list)
  ) 
  
  # user  system elapsed 
  # 0.50    0.06    0.84
  
  # 
  boots_lapply[[1]][[43]][1:10,1:10]
   boots_rep[[43]][1:10,1:10]
   boots_sapply[[43]][1:10,1:10]
  #  
  #  
  


 