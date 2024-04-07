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

# bring in the original data----


  
  iea_members <- readRDS(file = file.path(data_clean,"iea_dfs","iea_members.rds"))
  
  
  cumulative_data_list <-readRDS(file = file.path(data_clean,"iea_matrices_year","iea_list_all_cumulative_years.rds"))
  
  sv_data_list <- readRDS( file = file.path(data_clean,"iea_matrices_year","iea_list_all_single_valued_years.rds"))

  iea_members_pilot <- readRDS(file.path(data_clean,"iea_members_pilot.rds"))
  
  #iea_members <- iea_members_pilot
  
# Treaty-level summary stats ----
  
  # look at years 
  
  num_agreements_trunc <- iea_members %>%
                       filter(year == 1961) %>%
                      length(unique(iea_members$treaty_id))
  
  # pilot is 5
  num_agreements <- length(unique(iea_members$treaty_id))
  
  by_treaty <- iea_members %>%
               group_by(treaty_id)
  

  var <- iea_members$Treaty.Name.from.Treaty.Table
  df      <- iea_members
  sample_size <- 5
  
  # get number unique
  
  set.seed(4)
  num_unique <- length(unique(var))
  examples   <- sample(unique(var),
                       size = sample_size)
  
  # get obs count
  obs_counts <- rep(NA, times = sample_size )
  examples_plus_counts <- rep(NA, times = sample_size)
  
  for (i in 1:sample_size) {
  obs_counts[i]        <- df[var==examples[i],] %>%
                nrow()
  
  if (i<sample_size) {
  examples_plus_counts[i] <- paste0(examples[i]," (N = ",obs_counts[i],"),\n")
  } else
  {examples_plus_counts[i] <- paste0(examples[i]," (N = ",obs_counts[i],").")
    }

  }
  
  
# types of agreements
  
  lineages <- unique(iea_members$Lineage) #362 lineages, 
  # ranging from "Polar Bear" to "Central Asia Environmental Cooperation" 
  # to "Canada-USSR Fisheries" to "Health in Mines" to
  # "Transboundary Effects of Industrial Accidents" 
  
  
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
  
  
  