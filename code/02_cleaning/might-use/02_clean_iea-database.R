# _______________________________#
# International Agreements
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
  world                         <- readRDS(file.path(data_clean,"world.rds"))
  
  # # find out how many are, take the filter by year and action: 689 withdrawals BEFORE 1961
  # # just use the read CSV
  # nrow(members_raw_all[members_raw_all$Action == "Withdrawal or Similar",])
  # # 0
  # nrow(members_raw_all[members_raw_all$Action == "Status follows original agreement",])
  # # 0
  # nrow(members_raw_all[members_raw_all$Action == "Provisional application" ,])
  # # 0
  # nrow(members_raw_all[members_raw_all$Action == "Entry Into Force (Tacit Acceptance) - Objection" ,])
  # # 0
  # nrow(members_raw_all[members_raw_all$Action == "Entry Into Force (Tacit Acceptance) - Objection" ,])
  # nrow(members_raw_all[members_raw_all$Action == "Waiver (Art.28)" ,])
  # # 0
  
  members_raw_keep_all <- read.csv(file = data_path,
                              header = TRUE) %>% # handle pre-1960 later
    rename(treaty_id = Mitch.ID,
           country   = country_preferred,
           action    = Action,
           year      = Year)
  
  
  
  # for ease only take recent
  members_raw_all <- read.csv(file = data_path,
                              header = TRUE) %>% # handle pre-1960 later
                      rename(treaty_id = Mitch.ID,
                             country   = country_preferred,
                             action    = Action,
                             year      = Year)%>%
                      filter(year > 1960 & year <2023) %>%
                      filter(country != "Agreement") %>% # drop the treaties signed with international organizations, will need to adjust manually
                      filter(country!= "IO-World Health Organization" &
                               country!= "IO-UN Food and Agriculture Organization" &
                               country!= "IO-World Meteorological Organization" &
                               country!= "IO-European Atomic Energy Community" &
                               country!= "IO-African Intellectual Property Organization" &
                               country!= "IO-European Bank for Reconstruction and Development (EBRD)" &
                               country!= "IO-IAEA" &
                               country!= "IO-Nordic Environment Finance Corporation (NEFCO)" &
                               country!= "IO-International Bank for Reconstruction and Development") %>%
                      mutate(action_simple = action) %>% # for now take off the difficult ones
                      filter(action != "Entry Into Force (Tacit Acceptance) - Objection" &
                             action != "Status follows original agreement" &
                             action != "Waiver (Art.28)") %>% # deal with withdrawals later, relevant but not now
                      filter(action!= "Withdrawal or Similar" &
                             action!= "Withdrawal or Similar 2" &
                             action!= "Provisional application" ) %>% # add in European Union, means all EU countries at the time of signature, and then will have to apply to new EU entrants once they enter the EU
                      filter(country!= "European Union") %>%
                      filter(country!= "Yugoslavia") %>% # deal with yugoslavia later
                      filter(country!= "Organisation of Eastern Caribbean States") %>% # take out disputed territories
                      filter(country!= "South Ossetia" & # disputed in Russia/Georgia
                             country!= "Abkhazia" & # disputed in Georgia/Russia
                             country!= "Transkei" & # unrecognized to 1975, S Africa
                             country!= "Venda" & # a republic in S africa
                             country!= "Ciskei" & # republic in S Africa
                             country!= "Bophutatswana" & 
                             country!= "Sahrawi Arab Democratic Republic" & # W Sahara, Morocco / Spain
                             country!= "Netherlands Antilles" &
                             country!= "Tokelau" & # dependent territory in New Zealand
                             country!= "Aruba" & #5 obs and not in country codes 
                             country!= "Curacao" &# 9 obs and not in countrycode
                             country!= "Saint Martin" & # 3 obs and not in countrycodes
                             country!= "Saint Helena" & # 3 obs, not in countrycodes 
                             country!= "Turks and Caicos Islands" & # 3 obs, not in countrycodes
                             country!= "Micronesia" & # micronesias total 5 obs
                             country!= "Micronesia (pre-1986)" &
                             country!= "Macao" & # 4 obs
                             country!= "Greenland" & # 3 obs, could put in with Denmark?
                             country!= "British Virgin Islands" # 8 obs
                             )
  
  
  length(unique(members_raw_all$country)) # 222
  
# clean the country codes ----
  
  # for now pretend that the current country with the most part of the landmass is what's relevant----
  
  library(countrycode)
  
  # use 

  
  members_raw_all$country[members_raw_all$country== "Antigua and Barbuda"] <- "Antigua & Barbuda"
  members_raw_all$country[members_raw_all$country== "Belarus (Soviet period)"  ] <- "Belarus" 
  members_raw_all$country[members_raw_all$country== "Belgium (Brussels)" |
                            members_raw_all$country== "Belgium (Flemish)" | 
                            members_raw_all$country== "Belgium (Walloon)"] <- "Belgium" 
  
  members_raw_all$country[members_raw_all$country== "Bosnia and Herzegovina"] <- "Bosnia & Herzegovina"
  
  members_raw_all$country[members_raw_all$country== "Congo, Republic of the"  ] <- "Congo - Brazzaville"
  
  members_raw_all$country[members_raw_all$country== "Brunei Darussalam"  ] <- "Brunei"
  
  
  members_raw_all$country[members_raw_all$country== "Congo, Democratic Republic of"    ] <- "Congo - Kinshasa" 
  members_raw_all$country[members_raw_all$country== "Cote d'Ivoire"    ] <- "Côte d’Ivoire"  
  

  
  
  members_raw_all$country[members_raw_all$country== "Estonia (Soviet period)"  ] <- "Estonia" 
  
  
                            
  members_raw_all$country[members_raw_all$country== "Germany (Baden-Wurttemberg)" |
                            members_raw_all$country== "Prussia" |
                            members_raw_all$country== "Germany, Former GDR" |
                            members_raw_all$country== "Germany (Bavaria)"  |
                            members_raw_all$country== "Germany (pre-1949)"] <- "Germany"
  

  
  
  members_raw_all$country[members_raw_all$country== "Hong Kong"  ] <- "Hong Kong SAR China"
  
  members_raw_all$country[members_raw_all$country== "Kenya (pre-1963)"    ] <- "Kenya"  
  
  
  
  members_raw_all$country[members_raw_all$country== "Marshall Islands (pre-1986)"  ] <- "Marshall Islands" 
  
  members_raw_all$country[members_raw_all$country== "Myanmar"  ] <- "Myanmar (Burma)"
  
  
  
  # dropped bc obs count is 5
  members_raw_all$country[members_raw_all$country== "Micronesia (pre-1986)" |
                          members_raw_all$country== "Micronesia"] <- "Micronesia (Federated States of)" 
  
  members_raw_all$country[members_raw_all$country== "Namibia (pre-1990)"  ] <- "Namibia" 
  members_raw_all$country[members_raw_all$country== "Korea, Democratic People's Republic"  ] <- "North Korea"
  

  members_raw_all$country[members_raw_all$country== "Palestine, Occupied Territories"  ] <- "Palestinian Territories"
  
  
  members_raw_all$country[members_raw_all$country== "Qatar (pre-1971)"  ] <- "Qatar" 
  
  members_raw_all$country[members_raw_all$country== "Russian Federation"  ] <- "Russia"
  
  
  members_raw_all$country[members_raw_all$country== "Sao Tome and Principe"  ] <- "São Tomé & Príncipe"
  
  members_raw_all$country[members_raw_all$country== "Korea, Republic of"  ] <- "South Korea"
  
  
  members_raw_all$country[members_raw_all$country== "Saint Kitts and Nevis"  ] <- "St. Kitts & Nevis"
  members_raw_all$country[members_raw_all$country== "Saint Lucia"  ] <- "St. Lucia"
  members_raw_all$country[members_raw_all$country== "Saint Vincent and the Grenadines"  ] <- "St. Vincent & Grenadines"
  
  members_raw_all$country[members_raw_all$country== "Suriname (pre-1975)"  ] <- "Suriname" 
  
  
  members_raw_all$country[members_raw_all$country== "Trinidad and Tobago"  ] <- "Trinidad & Tobago" 
  
  members_raw_all$country[members_raw_all$country== "Tunisia (pre-1956)"  ] <- "Tunisia" 
  members_raw_all$country[members_raw_all$country== "Türkiye"  ] <- "Turkey"
  
  
  members_raw_all$country[members_raw_all$country== "Uganda (pre-1962)"  ] <- "Uganda" 
  
  members_raw_all$country[members_raw_all$country== "Ukraine (Soviet period)"   ] <- "Ukraine" 
  
  members_raw_all$country[members_raw_all$country== "Holy See"  ] <- "Vatican City"
  
  
  members_raw_all$country[members_raw_all$country== "Vietnam, South"  |
                            members_raw_all$country== "Viet Nam"] <- "Vietnam" 
  
  
  
  
  
  
  
  
  members_raw_all$country[members_raw_all$country== "Zimbabwe (pre-1965)"  ] <- "Zimbabwe" 
  
  
  
  
  
  members_raw_all$country[members_raw_all$country== "Myanmar (pre-1948)"  ] <- "Myanmar" 
  
  
  
  
  members_raw_all$country[members_raw_all$country== "Yemen (pre-1962)"  |
                            members_raw_all$country== "Yemen, Arab Republic"  |
                            members_raw_all$country== "Yemen, Democratic"  ] <- "Yemen"  
  
  
  
  length(unique(members_raw_all$country)) # 200, 195 officially sovereign but some self-governing in there for now
  
  
  
  # rename some to match with the country centroids ----
  
  members_raw_all$country[members_raw_all$country== "Faeroe Islands"  ] <- "Faroe Islands"
  
  
  


  
  
  
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
  
  
  unique(members_raw_all$action_simple) # signature, accession, tacit acceptance / eif

  
  
# make a manageable test df ----
  
  treaty_codes <- unique(members_raw_all$treaty_id)
  
  treaty_codes_all <- unique(members_raw_all$treaty_id)  # 3735 if no upper limit, 3331
  
  length(treaty_codes) # 3466 unique treaties after 1960, 2875 if in 1960 to 2023
  
  countries    <- as.data.frame(unique(members_raw_all$country))
  
  country_freq <- as.data.frame(table(members_raw_all$country))
  
  nrow(countries) # 254, note historical names will vary
  
  # demo(gexfbuildfromscratch)
  # demo(gexftwitter)
  set.seed(5)
  size_pilot <- 5

  # create test dataframe 
  nrow(members_raw_all) # 75510 member decisions
  
  members_pilot_all_data  <- members_raw_all %>%
                    filter(treaty_id %in% sample(unique(treaty_id), size_pilot)) 
  
  edgelist_pilot <- members_pilot_all_data %>%
                    select(treaty_id, country) 
  
 
  
# show how projection adjacency matrices will work ---- 
  
  # Function Inputs 
  network_df               <- members_pilot_all_data 
  running_var             <- "year"
  undesired_column        <- "treaty_id"
  desired_column          <- "country"
  path                    <- file.path(data_clean,"iea_matrices_pilot")
  mat_name_root            <- "iea_full_matrix"
  
  
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
              file = file.path(path,paste0(mat_name_root,"_",running_vec[t],".rds")))
      saveRDS(Mrow_final,
              file = file.path(path,paste0(mat_name_root,"_",min(running_vec),"_to_",running_vec[t],".rds")))
    }
    else{
      saveRDS(Mrow_final,
              file = file.path(path,paste0(mat_name_root,"_",min(running_vec),"_to_",running_vec[t],".rds")))
    }
    
    # bring in the old matrix
    
    if (t>1) {
    prior_M <- readRDS(file = file.path(path,paste0(mat_name_root,"_",min(running_vec),"_to_",running_vec[t-1],".rds")))
                        
    # get the yearly addition by subtracting current matrix from prior matrix
    
    M_current <- Mrow_final - prior_M
    
    # save the one-year matrix
    saveRDS(M_current,
            file = file.path(path,paste0(mat_name_root,"_",running_vec[t],".rds"))
            )
    }
    
   }
  
  saveRDS(edgelist,
          file = file.path(path,paste0(mat_name_root,"_","full-edgelist.rds"))
          )
  
 
  

# do with a function ----
  
  system.time(
           get_projection_matrices(network_df        = members_raw_all,
                                    running_var      = "year",
                                    undesired_column = "treaty_id", # treaty ID
                                    desired_column   = "country", # country ID
                                    path             = file.path(data_clean,"iea_matrices_year"),
                                    mat_name_root    = "iea_full_matrix")
  ) # end system time
  
  path <- file.path(data_clean,"iea_dfs")
  
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  saveRDS(members_raw_all,
          file = file.path(path,"iea_members.rds"))
  
  write.csv(members_raw_all,
            file = file.path(path, "iea_members.csv"))
  
  set.seed(9)
  size_pilot <- 5
  
  # create test dataframe 
  nrow(iea_members) 
  
  iea_members_pilot  <- iea_members %>%
    filter(treaty_id %in% sample(unique(treaty_id), size_pilot))
  
  saveRDS(iea_members_pilot, file.path(data_clean,"iea_members_pilot.rds"))
  
  iea_members_pilot <- readRDS(file.path(data_clean,"iea_members_pilot.rds"))

# Create cumulative and single-valued matrix lists ----
  
  
  cumulative_path <- file.path(data_clean,"iea_matrices_year","cumulative")
  
  cumulative_list_RDS            <- list.files(path = cumulative_path)
  cumulative_data_list           <- lapply(file.path(cumulative_path,cumulative_list_RDS), readRDS)
  names_cumulative_data          <- gsub(".rds","",cumulative_list_RDS)
  
  names(cumulative_data_list)    <- names_cumulative_data
  
  saveRDS(cumulative_data_list, file = file.path(data_clean,"iea_matrices_year","iea_list_all_cumulative_years.rds"))
  
  
  
  single_valued_path     <- file.path(data_clean,"iea_matrices_year","single_valued")
  
  sv_list_RDS            <- list.files(path = single_valued_path)
  sv_data_list           <- lapply(file.path(single_valued_path,sv_list_RDS), readRDS)
  names_sv_data          <- gsub(".rds","",sv_list_RDS)
  
  names(sv_data_list) <- names_sv_data
  
  saveRDS(sv_data_list, file = file.path(data_clean,"iea_matrices_year","iea_list_all_single_valued_years.rds"))
  
  
  cumulative_data_list <-readRDS(file = file.path(data_clean,"iea_matrices_year","iea_list_all_cumulative_years.rds"))
  
  sv_data_list <- readRDS( file = file.path(data_clean,"iea_matrices_year","iea_list_all_single_valued_years.rds"))
  

  