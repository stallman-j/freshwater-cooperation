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


  node_level <- readRDS(file = file.path(data_clean,"node-level","countries_with_codes.rds"))
  
  iea_members <- readRDS(file = file.path(data_clean,"iea_dfs","iea_members.rds"))
  
  
  cumulative_data_list <-readRDS(file = file.path(data_clean,"iea_matrices_year","iea_list_all_cumulative_years.rds"))
  
  sv_data_list <- readRDS( file = file.path(data_clean,"iea_matrices_year","iea_list_all_single_valued_years.rds"))

  iea_members_pilot <- readRDS(file.path(data_clean,"iea_members_pilot.rds"))
  
  #iea_members <- iea_members_pilot
  
  
  # get bilateral v. multilateral agreements ----
  
  treaties <- iea_members %>%
              group_by(Treaty.Name.from.Treaty.Table) %>%
              count(Inclusion) 
  
  
  bea_v_mea <- table(treaties$Inclusion)
  
  
  # BEA  MEA 
  # 1858 1223
  
  
# Treaty-level summary stats ----
  
  sample_size <- 5
  
  countries_row <- get_examples_table_row(var =iea_members$country,
                                        df  = iea_members,
                                        sample_size   = sample_size,
                                        newline       = FALSE,
                                        notes         = "Assumes 2022 country borders, e.g. former Soviet Republic countries are labeled as present-day. \\newline An observation is a country by action by treaty, e.g. Guatemala signing the International Tropical Timber Extension Agreement, whereas ratification would be a new observation.",
                                        seed          = 5)
  
  agreements_row <- get_examples_table_row(var =iea_members$Treaty.Name.from.Treaty.Table,
                               df  = iea_members,
                               sample_size   = sample_size,
                               notes         = "",
                               newline       = TRUE,
                               seed          = 6)
  
  lineages_row   <- get_examples_table_row(var =iea_members$Lineage,
                                           df  = iea_members,
                                           sample_size   = 3,
                                           notes         = "",
                                           newline       = TRUE)
  
  action_types_row <-  get_examples_table_row(var =iea_members$Action.Type,
                                              df  = iea_members,
                                              sample_size   = 3,
                                              notes         = "",
                                              newline       = FALSE)
  
  
  inclusion_row <-  get_examples_table_row(var =iea_members$Inclusion,
                                              df  = iea_members,
                                              sample_size   = 2,
                                              notes         = "Bilateral Environmental Agreement (BEA) or Multilateral Environmental Agreement (MEA). \\newline There are 1858 BEAs and 1223 MEAs, whereas an observation below is a country by action by treaty.",
                                              newline       = FALSE)
  
  
  action_simple_row <-  get_examples_table_row(var =iea_members$action_simple,
                                              df  = iea_members,
                                              sample_size   = 3,
                                              notes         = "Entry into force (EIF).",
                                              newline       = FALSE)
  
  # put into one DF 
  
  examples_tab <- data.frame(rbind(countries_row,
                                   agreements_row,
                                   lineages_row,
                                   action_types_row,
                                   action_simple_row,
                                   inclusion_row))
  
  # rownames make null
  
  rownames(examples_tab) <- c("Countries","Agreements","Lineages","Actions (Detail)","Actions (Simplified)","Bilateral/Multilateral")
  colnames(examples_tab) <- c("N","Examples and Relevant Details")
  examples_tab
  
  tabpath <- file.path(output_tables,paste0("iea_examples_nobs.tex"))
  
  examples_tab_tex <- xtable(examples_tab, 
                       caption = "International Environmental Agreements Variables",
                       label = "iea_examples_nobs",
                       digits = c(0,0,0),
                       align = c("p{4.5cm}","p{1cm}","p{20cm}"))
  
  examples_tab_tex

  print(examples_tab_tex,
        file=tabpath,
        append=FALSE,
        table.placement = "H",
        caption.placement="top")
  
  
  # node-level summary stats ----
  node_level <- readRDS(file = file.path(data_clean,"node-level","countries_with_codes.rds"))
  distlist_2019 <- readRDS(file = file.path(data_clean,"edge-level","distlist_2019.rds"))
  
  tabpath <- file.path(output_tables,paste0("select_sum_stats.tex"))
  
  
  treaties_sum <- c(summary(node_level$number_treaties_joined)[1:6],length(unique(iea_members$treaty_id)))
  dists_sum    <- c(summary(distlist_2019$mindist)[1:6]/1000,nrow(distlist_2019))
  
  sums_tab <- rbind(treaties_sum,
                         dists_sum)
  
  rownames(sums_tab) <- c("Treaties per country, 1961-2022","Min.Distance(1000km)")
  colnames(sums_tab) <- c("Min","2.5\\%","Median","Mean","75\\%","Max","N")
  
  sums_tab_tex <- xtable(sums_tab,
                         caption = "Select Summary Statistics",
                         label = "tab:select_sum_stats",
                         digits = c(0,2,2,2,2,2,2,0))
  
  sums_tab_tex
  
  print(sums_tab_tex,
        file = tabpath,
        append = FALSE,
        table.placement = "H",
        caption.placement = "top")
  
  examples_tab <- data.frame(rbind(countries_row,
                                   agreements_row,
                                   lineages_row,
                                   action_types_row,
                                   action_simple_row,
                                   inclusion_row))
  
  # Other summary stats ----
  
  rownames(examples_tab) <- c("Countries","Agreements","Lineages","Actions (Detail)","Actions (Simplified)","Bilateral/Multilateral")
  colnames(examples_tab) <- c("N","Examples and Relevant Details")
  examples_tab
  
  tabpath <- file.path(output_tables,paste0("iea_examples_nobs.tex"))
  
  examples_tab_tex <- xtable(examples_tab, 
                             caption = "International Environmental Agreements Variables",
                             label = "tab:iea_examples_nobs",
                             digits = c(0,0,0),
                             align = c("p{4.5cm}","p{1cm}","p{20cm}"))
  
  examples_tab_tex
  
  print(examples_tab_tex,
        file=tabpath,
        append=FALSE,
        table.placement = "H",
        caption.placement="top")
  

  
   
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
  
  
  