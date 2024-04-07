# _______________________________#
# Geoengineering
# Clean 02: IEA Database
# 
# Stallman
# Started 2022-12-05
# Last edited: 
#________________________________#
# Startup

rm(list = ls())


# bring in the packages, folders, paths

  home_folder <- file.path("P:","Projects","international-agreements")

  source(file.path(home_folder,"code","00_master_ia.R"))

  
  data_path <- file.path(data_raw,"iea-database-project","db_members.csv")
  
  # for ease only take recent
  members_raw_all <- read.csv(file = data_path,
                              header = TRUE) %>%
                      rename(treaty_id = Mitch.ID,
                             country   = country_preferred,
                             action    = Action,
                             year      = Year)%>%
                      filter(country != "Agreement") %>%
                      mutate(action_simple = action) %>% # for now take off the difficult ones
                      filter(action != "Entry Into Force (Tacit Acceptance) - Objection" &
                             action != "Status follows original agreement" |
                             action != "Waiver (Art.28)") %>% # deal with withdrawals later, relevant but not now
                      filter(action!= "Withdrawal or Similar" |
                             action!= "Withdrawal or Similar 2" |
                             action!= "Provisional application" )
  
# clean the action codes ----
  
  members_raw_all$action_simple[members_raw_all$action== "Signature" ] <- "signature"
  
  members_raw_all$action_simple[members_raw_all$action== "Ratification, Accession, Succession, or Similar" |
                                  members_raw_all$action== "Cooperating Non-Party or Similar" |
                                  members_raw_all$action== "Deposit of instrument"  ]  <- "accession"
  
  members_raw_all$action_simple[members_raw_all$action== "Entry Into Force" |
                                  members_raw_all$action== "Entry Into Force 2"  |
                                  members_raw_all$action== "Entry Into Force 3"   |
                                  members_raw_all$action== "Entry Into Force (Tacit Acceptance)"   |
                                  members_raw_all$action== "Entry Into Force (Tacit Acceptance); Status based on post-amendment agreement action of Entry into force"   |
                                  members_raw_all$action== "Entry Into Force (Tacit Acceptance) - Removal of Objection"  |
                                  members_raw_all$action== "Entry Into Force (Tacit Acceptance); Status based on pre-amendment agreement action of Entry into force"   |
                                  members_raw_all$action== "Entry Into Force (Tacit Acceptance); Status based on pre-amendment agreement action of Withdrawal or Similar"   |
                                  members_raw_all$action== "Entry Into Force (Tacit Acceptance); Status based on pre-amendment agreement action of Entry into force 2"   |
                                  members_raw_all$action== "Entry Into Force (Tacit Acceptance); Status based on pre-amendment agreement action of Withdrawal or Similar 2"    |
                                  members_raw_all$action== "Entry Into Force (Tacit Acceptance); Status based on pre-amendment agreement action of Entry into force 3"   |
                                  members_raw_all$action== "Entry Into Force (Tacit Acceptance); Status based on post-amendment agreement action of Withdrawal or Similar"   |
                                  members_raw_all$action== "Entry Into Force (Tacit Acceptance); Status based on post-amendment agreement action of Entry into force 2"    |
                                  members_raw_all$action== "Entry Into Force (Tacit Acceptance); Status based on post-amendment agreement action of Withdrawal or Similar 2"   |
                                  members_raw_all$action== "Entry Into Force (Tacit Acceptance); Status based on post-amendment agreement action of Entry into force 3"   |
                                  members_raw_all$action== "Entry Into Force (Tacit Acceptance); Status based on pre-amendment agreement action of Entry into force 3" |
                                  members_raw_all$action== "Non-provisional application"] <- "tacit acceptance / eif"
  
  
  
  members_raw     <- members_raw_all %>%
                     filter(year > 1960 & year < 2023)
  
# make a manageable test df ----
  
  treaty_codes <- unique(members_raw$treaty_id)
  
  
  
  treaty_codes_all <- unique(members_raw_all$treaty_id)  # 3735 if no upper limit, 3331
  
  length(treaty_codes) # 3466 unique treaties after 1960, 2875 if in 1960 to 2023
  
  countries    <- as.data.frame(unique(members_raw$country))
  
  nrow(countries) # 254, note historical names will vary
  
  # demo(gexfbuildfromscratch)
  # demo(gexftwitter)
  set.seed(2)
  size_pilot <- 5
  
  # create a sample vector with size_pilot treaties in there
  
  # pilot_indices <- sample(treaty_codes, size = size_pilot,
  #                         replace = FALSE)
  
  # create test dataframe 
  nrow(members_raw) # 75510 member decisions
  
  members_pilot_all_data  <- members_raw %>%
                    #filter(year > 1950) %>%
                    filter(treaty_id %in% sample(unique(treaty_id), size_pilot)) %>%
                    filter(country != "Agreement") 
  
  members_pilot <- members_pilot_all_data %>%
                    select(treaty_id, country) 
  
  # https://stackoverflow.com/questions/69171494/how-to-create-an-edgelist-from-a-dataframe-expand-group-by
  
  net <- graph_from_edgelist(as.matrix(members_pilot))
  
  
  V(net)$type <- bipartite_mapping(net)$type
  
  net_df <- as_data_frame(net)
  
  par(mfrow=c(1,3),mar=c(0,1,1,1))
  
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
  
  treaty_net  <- bipartite_projection(net,
                                      multiplicity = TRUE)$proj1

  # convert to a df to look
  
  country_df_v  <- as_data_frame(country_net,
                                 what = c("vertices"))
  
  country_df_e  <- as_data_frame(country_net,
                                 what = c("edges"))
  #old
  # graph.adjlist
  ?graph_from_adj_list
  
  # old
  ?graph.edgelist
  ?graph_from_edgelist
  
  # old 
  ?graph.adjacency
  
  ?graph_from_adjacency_matrix
  
  
# Showing manually how the cleaning works ----
  
  # Function Inputs 
  full_network            <- members_pilot_all_data
  running_var             <- "year"
  undesired_column        <- "treaty_id"
  desired_column          <- "country"
  path                    <- file.path(data_clean,"iea_matrices")
  df_name_root            <- "iea_country"
  
  
  # Start the Function
  
  # check that output path exists
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  # create the vector that's going to be iterating, here it's years
  running_vec <- sort(unique(full_network[[running_var]]))
  
  for (t in running_vec) {
    
    # back out just the edges we need
    
    # filter on the running var
    temp_network        <- full_network[full_network[running_var]<=t, ]
    
    # select only desired column
    temp_edgelist       <- temp_network[,c(desired_column,undesired_column)]
   
     temp_graph          <- graph_from_edgelist(as.matrix(temp_edgelist))
    
     # assign type so we can use bipartite projection
     V(temp_graph)$type <- bipartite_mapping(temp_graph)$type
     
    temp_projection     <- bipartite_projection(temp_graph,
                                                multiplicity = TRUE)$proj1 %>%
                            as_data_frame()
    
    names(temp_projection) <- c("from","to",paste0("ieas_",min(running_vec),"_to_",t))
    saveRDS(temp_projection,
            file = file.path(path,paste0(df_name_root,"_",min(running_vec),"_to_",t,".rds")))
    
  }
  
  

# show that they're the same as going through manually ----
  
  par(mfrow=c(1,2),mar=c(0,1,1,1))
  
  
  iea_country_1978_to_2003 <- readRDS(file = file.path(path,"iea_country_1978_to_2003.rds"))
  
  plot(bipartite_projection(net)$proj2,
       main="Country Ties",
       vertex.label.color = "black",
       edge.color = "black",
       vertex.color = yale_lblue,
       layout = m2)
  
  plot(graph_from_edgelist(as.matrix(iea_country_1978_to_2003)),
       main="Country Ties Test",
       vertex.label.color = "black",
       edge.color = "black",
       vertex.color = "orange",
       layout = layout_nicely(iea_country_1978_to_2003))
  
  
# do it through a function ----
  
  get_cumulative_projection_dfs(full_network = members_pilot_all_data,
                                           running_var = "year",
                                           undesired_column = "treaty_id", # treaty ID
                                           desired_column   = "country", # country ID
                                           path             = file.path(data_clean,"iea_matrices_test"),
                                           df_name_root     = "iea_country")
  
  
# do with matrices ----
  
  # cumulative
  
  # Function Inputs 
  full_network_df         <- members_pilot_all_data
  cumulative_var          <- "year"
  category_vars           <- "action_simple"
  undesired_column        <- "treaty_id"
  desired_column          <- "country"
  path                    <- file.path(data_clean,"iea_matrices")
  mat_name_root            <- "iea_country_matrix"
  
  
  # Start the Function
  
  # check that output path exists
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  # create the vector that's going to be iterating, here it's years
  running_vec <- sort(unique(full_network[[running_var]]))
  
  # full matrix
  
  M_full <- full_network_df[,c(desired_column,undesired_column)] %>%
            table() %>%
            as.matrix()
  
  
  for (t in running_vec) {
    
    # back out just the edges we need
    
    # filter on the running var
    temp_network        <- full_network[full_network[running_var]<=t, ]
    
    # select only desired column
    temp_edgelist       <- temp_network[,c(desired_column,undesired_column)]
    
    
    saveRDS(temp_adj_mat,
            file = file.path(path,paste0(df_name_root,"_",min(running_vec),"_to_",t,".rds")))
    
    
  }
  
  
# try out ----
  
  # Function Inputs 
  network_df               <- members_pilot_all_data #%>% filter(year <= 2010)
  running_var             <- "year"
  undesired_column        <- "treaty_id"
  desired_column          <- "country"
  path                    <- file.path(data_clean,"iea_matrices_test")
  df_name_root            <- "iea_full_matrix"
  
  
  # Start the Function
  
  # check that output path exists
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  # create the vector that's going to be iterating, here it's years
  running_vec <- sort(unique(network_df[[running_var]]))
  
  # the vars that you want turned into nas cumulatively, i.e all but the desired column
  na_vars <- names(network_df)[!names(network_df) %in% c(desired_column)]
                
  # this will stay the same so we can add matrices
  edgelist <- network_df[, c(desired_column, undesired_column)]
  
  # for testing
  #t <- 3
  
  for (t in 1:length(running_vec)) {
    
    # create a network which is all the correct nodes but edges / weights not added up
    
    temp_network <- network_df
    
    for (var in na_vars) {
      
      temp_network[[var]][network_df[running_var]>running_vec[t]] <- NA
    }
    
    # get the adj matrix for the full network
    M_full <- temp_network[,c(desired_column,undesired_column)] %>%
      table() %>%
      as.matrix()
    
    
    # matrix multiplication to get edge weights for just the projection network of interest
    Mrow <- tcrossprod(M_full)
    
    # divide by 2 and round up to account for double-counting
    
    Mrow_final <- ceiling(Mrow/2)

    # save the new cumulative matrix
    
    # if t==1 then make two versions, one annual with just that year and one that works with the subtracting format to get other annual matrices
    
    if(t==1){
      saveRDS(Mrow_final,
              file = file.path(path,paste0(df_name_root,"_",running_vec[t],".rds")))
      saveRDS(Mrow_final,
              file = file.path(path,paste0(df_name_root,"_",min(running_vec),"_to_",running_vec[t],".rds")))
    }
    else{
      saveRDS(Mrow_final,
              file = file.path(path,paste0(df_name_root,"_",min(running_vec),"_to_",running_vec[t],".rds")))
    }
    
    # bring in the old matrix
    
    if (t>1) {
    prior_M <- readRDS(file = file.path(path,paste0(df_name_root,"_",min(running_vec),"_to_",running_vec[t-1],".rds")))
                        
    # get the yearly addition by subtracting current matrix from prior matrix
    
    M_current <- Mrow_final - prior_M
    
    # save the one-year matrix
    saveRDS(M_current,
            file = file.path(path,paste0(df_name_root,"_",running_vec[t],".rds"))
            )
    }
    
   }
  
  saveRDS(edgelist,
          file = file.path(path,paste0(df_name_root,"_","full-edgelist.rds"))
          )
  

  
# Get annual matrices ----
  
  temp_df <- readRDS(file = file.path(path,paste0(df_name_root,"_",min(running_vec),"_to_",t,".rds"))
  )
  
  # give it the right name
  
  # get the name we want
  
  assign(paste0(df_name_root,"_",min(running_vec),"_to_",t), temp_df)
  
  
# With sparse matrices ----
  
  # Issue here is that edge list isn't preserved and I want to add matrices up
  
  # Function Inputs 
  network_df               <- members_pilot_all_data #%>% filter(year <= 2010)
  running_var             <- "year"
  undesired_column        <- "treaty_id"
  desired_column          <- "country"
  path                    <- file.path(data_clean,"iea_matrices_test")
  df_name_root            <- "iea_sparse"
  
  
  # Start the Function
  
  # check that output path exists
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  # create the vector that's going to be iterating, here it's years
  running_vec <- sort(unique(network_df[[running_var]]))
  
  
  for (t in running_vec) {
    
    
  #old 
  temp_network        <- network_df[network_df[running_var]<=t, ]
  # select only desired column
  temp_edgelist       <- temp_network[,c(desired_column,undesired_column)]
  
  # create a sparse matrix with this
  temp_A                   <- spMatrix(nrow = length(unique(temp_edgelist[[desired_column]])),
                                       ncol = length(unique(temp_edgelist[[undesired_column]])),
                                       i    = as.numeric(factor(temp_edgelist[[desired_column]])),
                                       j    = as.numeric(factor(temp_edgelist[[undesired_column]])),
                                       x    = rep(1, length(as.numeric(temp_edgelist[[desired_column]])))
  )
  
  row.names(temp_A)        <- levels(factor(temp_edgelist[[desired_column]]))
  colnames(temp_A)         <- levels(factor(temp_edgelist[[undesired_column]]))
  
  Arow <- tcrossprod(temp_A)
  
  # divide by 2 and round up to account for double-counting
  
  Arow_final <- ceiling(Arow/2)
  
  
  saveRDS(temp_edgelist,
          file = file.path(path,paste0(df_name_root,"_",min(running_vec),"_to_",t,".rds")))
  
  saveRDS(Arow_final,
          file = file.path(path,paste0(df_name_root,"_",min(running_vec),"_to_",t,".rds")))
  
  }
  