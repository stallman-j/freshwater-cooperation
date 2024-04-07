# _______________________________#
# International Agreements
# Analysis 03: IEA Database, Run some temporal ERGMs
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

# bring in the original data----

  
  
 # iea_members <- readRDS(file = file.path(data_clean,"iea_dfs","iea_members.rds"))
  
  
  cumulative_data_list <-readRDS(file = file.path(data_clean,"iea_matrices_year","iea_list_all_cumulative_years.rds"))
  
  sv_data_list <- readRDS(file = file.path(data_clean,"iea_matrices_year","iea_list_all_single_valued_years.rds"))
  
  distance_mat <- readRDS(file = file.path(data_clean,"edge-level","distance_mat_clean.rds"))
  
  distance_mat <- distance_mat/1000
  
  # create a matrix that's 1 if a positive number, else 0
  #iea_members_pilot <- readRDS(file.path(data_clean,"iea_members_pilot.rds"))
  
  #test_list_of_matrices <- readRDS(file.path(data_clean,"test_list_of_matrices.rds"))
  
  node_level <- readRDS(file = file.path(data_clean,"node-level","countries_with_codes.rds"))
  rownames(node_level) <- node_level$country
  
  
  
  deweight_matrix <- function(matrix) {
    matrix[matrix>0] <- 1
    
    return(matrix)
  }
  
  empty_mat <- matrix(0,nrow = length(rownames(sv_data_list[[1]])),
                      ncol = length(colnames(sv_data_list[[1]])))
  rownames(empty_mat) <- rownames(sv_data_list[[1]])
  colnames(empty_mat) <- colnames(sv_data_list[[1]])
  
  
  # last year's agreements
  sv_data_one_lag_cov_mat_list <- append(list(empty_mat), sv_data_list[1:61])
  
  # shared history
  cumulative_data_one_lag_cov_mat_list <- append(list(empty_mat), cumulative_data_list[1:61])
  
  
  sv_data_edge_mat_list <- lapply(sv_data_list, deweight_matrix)
  
  # manually inspect 
  # sv_data_one_lag_cov[[1]][1:10,1:10]
  # sv_data_one_lag_cov[[2]][1:10,1:10]
  # sv_data_one_lag_cov[[3]][1:10,1:10]
  # sv_data_one_lag_cov[[4]][1:10,1:10]
  # sv_data_one_lag_cov[[5]][1:10,1:10]
  # 
  # sv_data_edges[[1]][1:10,1:10]
  # sv_data_edges[[2]][1:10,1:10]
  # sv_data_edges[[3]][1:10,1:10]
  # sv_data_edges[[4]][1:10,1:10]
  # sv_data_edges[[5]][1:10,1:10]

# add nodal covs to the networks ----
  
  # desired node covs
  region <- node_level$region23
  names(region) <- 1:length(region)
  
  # edgecovs
  distances <- handleMissings(distance_mat, na = NA, method = "remove")

  # set node covs
    for (i in 1:length(sv_data_edge_mat_list)) {
    #d <- adjust(distances, sv_data_edge_mat_list[[i]])
    
    sv_data_edge_mat_list[[i]] <- network(sv_data_edge_mat_list[[i]],
                                          directed = FALSE)

    #sv_data_edge_mat_list[[i]] <- set.vertex.attribute(sv_data_edge_mat_list[[i]], "region", region)
    
    
  }

  
  # run btergm ----
  require("parallel")
  
  cores <- detectCores() - 1
  
  cl<- makeCluster(cores,
                   type = "PSOCK")
  
  system.time(
    model_1 <- btergm(sv_data_edge_mat_list ~ edges + gwesp(0, fixed = TRUE) + timecov(transform = function(t) t),
                      R = 50,
                      parallel = "snow",
                      ncpus = cores,
                      cl = cl)
  )
  

  
  system.time(
    model_2 <- btergm(sv_data_edge_mat_list ~ edges + gwesp(0, fixed = TRUE) + memory(type = "stability"),
                      R = 50,
                      parallel = "snow",
                      ncpus = cores,
                      cl = cl)
  )
  
  system.time(
    model_3 <- btergm(sv_data_edge_mat_list ~ edges + gwesp(0, fixed = TRUE) + edgecov(distances) +  timecov(transform = function(t) t),
    R = 50,
    parallel = "snow",
    ncpus = cores,
    cl = cl)
  )

  system.time(
    model_4 <- btergm(sv_data_edge_mat_list ~ edges + gwesp(0, fixed = TRUE) + edgecov(distances) + timecov(transform = function(t) t) + memory(type = "stability"),
                      R = 50,
                      parallel = "snow",
                      ncpus = cores,
                      cl = cl)
  )
  
  system.time(
    model_5 <- btergm(sv_data_edge_mat_list ~ edges + gwesp(0, fixed = TRUE) + timecov(transform = function(t) t) + timecov(transform = function(t) t^2) + memory(type = "stability"),
                      R = 50,
                      parallel = "snow",
                      ncpus = cores,
                      cl = cl)
  )
  
  system.time(
    model_6 <- btergm(sv_data_edge_mat_list ~ edges + gwesp(0, fixed = TRUE) + edgecov(distances) + timecov(transform = function(t) t) + timecov(transform = function(t) t^2) + memory(type = "stability"),
                      R = 50,
                      parallel = "snow",
                      ncpus = cores,
                      cl = cl)
  )
  

  # model_2 <- btergm(sv_data_edge_mat_list ~ edges + nodecov("region") + ,
  #                   R = 10,
  #                   parallel = "snow",
  #                   ncpus = cores,
  #                   cl = cl)
  # 
  # model_3 <- btergm(sv_data_edge_mat_list ~ edges + nodecov("region") + edgecov(distances) + edgecov(cumulative_data_one_lag_cov_mat_list),
  #                   R = 10,
  #                   parallel = "snow",
  #                   ncpus = cores,
  #                   cl = cl)
  
  stopCluster(cl)
  theta <- coef(model_1)
  inv.logit(theta)
  
  
  summary(model_2)
  summary(model_3)
  summary(model_4)
  summary(model_5)
  summary(model_6)

  # compare this: edge + Tri + hist
  # edge + tri + dist + timetrend
  # edge + tri + dist + time + hist
  
  texreg(l = list(model_2,model_3,model_4),
         file = file.path(output_tables,"btergm_table_3.tex"),
         label = "table:btergm_table_3",
         center = FALSE,
         caption = "Temporal Dependence and Geographic Distance",
         digits  = 3,
         dcolumn = TRUE,
         use.packages = FALSE,
         custom.coef.names= c("Edges",
                              "Shared Partners",
                              "Network in $t-1$",
                              "Distance (1000km)",
                              "Time Trend"),
         custom.model.names = c("(1)","(2)","(3)"))
  
  
  
  
  texreg(l = list(model_4,model_5,model_6),
         file = file.path(output_tables,"btergm_table_4.tex"),
         label = "table:btergm_table_4",
         center = FALSE,
         caption = "Temporal Dependence and Geographic Distance",
         digits  = 3,
         dcolumn = TRUE,
         use.packages = FALSE,
         # custom.coef.names= c("Edges",
         #                      "Distance (1000km)",
         #                      "$\\#$ Ties to Present",
         #                      "$\\#$ Ties in $t-1$"),
         custom.model.names = c("(1)","(2)","(3)"))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  require("parallel")
  
  cores <- detectCores() - 1
  
  cl<- makeCluster(cores,
                  type = "PSOCK")
  
  system.time(
  model_7 <- btergm(sv_data_edge_mat_list ~ edges + edgecov(distances),
                    R = 50,
                    parallel = "snow",
                    ncpus = cores,
                    cl = cl)
)
  system.time(
  model_8 <- btergm(sv_data_edge_mat_list ~ edges + edgecov(cumulative_data_one_lag_cov_mat_list),
                    R = 50,
                    parallel = "snow",
                    ncpus = cores,
                    cl = cl)
  )

  system.time(
  model_9 <- btergm(sv_data_edge_mat_list ~ edges +  edgecov(sv_data_one_lag_cov_mat_list),
                R = 50,
                parallel = "snow",
                ncpus = cores,
                cl = cl)
  )
  

  
  system.time(
  model_10 <- btergm(sv_data_edge_mat_list ~ edges + edgecov(distances) + edgecov(cumulative_data_one_lag_cov_mat_list) + edgecov(sv_data_one_lag_cov_mat_list),
                    R = 50,
                    parallel = "snow",
                    ncpus = cores,
                    cl = cl)
  )
  # model_2 <- btergm(sv_data_edge_mat_list ~ edges + nodecov("region") + ,
  #                   R = 10,
  #                   parallel = "snow",
  #                   ncpus = cores,
  #                   cl = cl)
  # 
  # model_3 <- btergm(sv_data_edge_mat_list ~ edges + nodecov("region") + edgecov(distances) + edgecov(cumulative_data_one_lag_cov_mat_list),
  #                   R = 10,
  #                   parallel = "snow",
  #                   ncpus = cores,
  #                   cl = cl)
  
  stopCluster(cl)
  theta <- coef(model_1)
  inv.logit(theta)
  
  
  summary(model_7)
  summary(model_8)
  summary(model_9)
  summary(model_10)

  
  texreg(l = list(model_7,model_8,model_9,model_10),
         file = file.path(output_tables,"btergm_table_1.tex"),
         label = "table:btergm_table_1",
         center = FALSE,
         caption = "Agreement History and Geographic Distance",
         digits  = 3,
         dcolumn = TRUE,
         use.packages = FALSE,
         custom.coef.names= c("Edges",
                           "Distance (1000km)",
                           "Network Ties to Present",
                           "Network in $t-1$"),
         custom.model.names = c("(1)","(2)","(3)","(4)"))
  
  require("parallel")
  
  
  cores <- detectCores() - 1
  
  cl<- makeCluster(cores,
                   type = "PSOCK")
  
  system.time(
    model_11 <- btergm(sv_data_edge_mat_list ~ edges + gwesp(0,fixed=TRUE)+ edgecov(distances),
                      R = 50,
                      parallel = "snow",
                      ncpus = cores,
                      cl = cl)
  )
  system.time(
    model_12 <- btergm(sv_data_edge_mat_list ~ edges + gwesp(0,fixed=TRUE) + edgecov(cumulative_data_one_lag_cov_mat_list),
                      R = 50,
                      parallel = "snow",
                      ncpus = cores,
                      cl = cl)
  )
  
  system.time(
    model_13 <- btergm(sv_data_edge_mat_list ~ edges + gwesp(0,fixed=TRUE) + edgecov(sv_data_one_lag_cov_mat_list),
                      R = 50,
                      parallel = "snow",
                      ncpus = cores,
                      cl = cl)
  )
  
  
  
  system.time(
    model_15 <- btergm(sv_data_edge_mat_list ~ edges + gwesp(0,fixed=TRUE) + edgecov(distances) + edgecov(cumulative_data_one_lag_cov_mat_list) + edgecov(sv_data_one_lag_cov_mat_list),
                      R = 50,
                      parallel = "snow",
                      ncpus = cores,
                      cl = cl)
  )
  # model_2 <- btergm(sv_data_edge_mat_list ~ edges + nodecov("region") + ,
  #                   R = 10,
  #                   parallel = "snow",
  #                   ncpus = cores,
  #                   cl = cl)
  # 
  # model_3 <- btergm(sv_data_edge_mat_list ~ edges + nodecov("region") + edgecov(distances) + edgecov(cumulative_data_one_lag_cov_mat_list),
  #                   R = 10,
  #                   parallel = "snow",
  #                   ncpus = cores,
  #                   cl = cl)
  
  stopCluster(cl)
  theta <- coef(model_1)
  inv.logit(theta)
  
  
  summary(model_11)
  summary(model_12)
  summary(model_13)
  summary(model_15)
  
  theta <- coef(model_15)
  inv.logit(theta)
  
  texreg(l = list(model_11,model_12,model_13,model_15),
         file = file.path(output_tables,"btergm_table_2.tex"),
         label = "table:btergm_table_2",
         center = FALSE,
         caption = "History and Geography, with Network Dependencies",
         digits  = 3,
         dcolumn = TRUE,
         use.packages = FALSE,
         custom.coef.names= c("Edges",
                              "Edgewise Shared Partners",
                              "Distance (1000km)",
                              "Network Ties to Present",
                              "Network Ties in $t-1$"),
         custom.model.names = c("(1)","(2)","(3)","(4)"),
         caption.above = TRUE)
  