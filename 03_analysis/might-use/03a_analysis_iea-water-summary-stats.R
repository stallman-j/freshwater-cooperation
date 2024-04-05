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

  home_folder <- file.path("P:","Projects","environment")
  
  source(file.path(home_folder,"code","00_startup_master.R"))

# bring in the original data----

  # this is from 1960 - 2023 and keywords pertain to water
  iea_water_clean <- readRDS(file = file.path(data_clean,"iea_dfs","iea_water_clean.rds"))
  
  iftd_treaties <- readRDS(file = file.path(data_clean,"edge-level","iftd_treaties.rds"))
  
  iftd_since_1960 <- iftd_treaties %>%
    filter(DateSigned > "1960-01-01")
  
  
  # get bilateral v. multilateral agreements ----
  
  treaties <- iea_water_clean %>%
              group_by(treaty_name) %>%
              count(Inclusion) 
  
  
  bea_v_mea <- table(treaties$Inclusion)
  
  
  # BEA  MEA 
  # 268 135
  
  
# Treaty-level summary stats ----
  
  sample_size <- 5
  
  countries_row <- get_examples_table_row(var =iea_water_clean$country,
                                        df  = iea_water_clean,
                                        sample_size   = sample_size,
                                        newline       = FALSE,
                                        notes         = "Assumes 2022 country borders, e.g. former Soviet Republic countries are labeled as present-day. \\newline An observation is a country by action by treaty, e.g. Guatemala signing the International Tropical Timber Extension Agreement, whereas ratification would be a new observation.",
                                        seed          = 5)
  
  agreements_row <- get_examples_table_row(var =iea_water_clean$treaty_name,
                               df  = iea_water_clean,
                               sample_size   = sample_size,
                               notes         = "",
                               newline       = TRUE,
                               seed          = 7)
  
  lineages_row   <- get_examples_table_row(var =iea_water_clean$lineage,
                                           df  = iea_water_clean,
                                           sample_size   = 3,
                                           notes         = "",
                                           newline       = TRUE,
                                           seed = 6)
  
  action_types_row <-  get_examples_table_row(var =iea_water_clean$Action.Type,
                                              df  = iea_water_clean,
                                              sample_size   = 3,
                                              notes         = "",
                                              newline       = FALSE)
  
  
  inclusion_row <-  get_examples_table_row(var =iea_water_clean$Inclusion,
                                              df  = iea_water_clean,
                                              sample_size   = 2,
                                              notes         = "Bilateral Environmental Agreement (BEA) or Multilateral Environmental Agreement (MEA). \\newline There are 268 BEAs and 135 MEAs, whereas an observation below is a country by action by treaty.",
                                              newline       = FALSE)
  
  
  action_simple_row <-  get_examples_table_row(var =iea_water_clean$action_simple,
                                              df  = iea_water_clean,
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
  
  tabpath <- file.path(output_tables,paste0("iea_water_examples_nobs.tex"))
  
  examples_tab_tex <- xtable(examples_tab, 
                       caption = "International Environmental Water Agreements Variables",
                       label = "iea_water_examples_nobs",
                       digits = c(0,0,0),
                       align = c("p{4.5cm}","p{1cm}","p{20cm}"))
  
  examples_tab_tex

  print(examples_tab_tex,
        file=tabpath,
        append=FALSE,
        table.placement = "H",
        caption.placement="top")
  
  
  # summary stats ----
  
  # addmargins adds the total at the bottom
  
  # N countries per treaty
  iea_water_by_treaty <- iea_water_clean %>%
    group_by(treaty_id) %>%
    summarize(number_countries = n()) %>%
    as.data.frame()
  
  # N treaties per country
  
  iea_water_by_country <- iea_water_clean %>%
    group_by(country) %>%
    summarize(number_treaties = n()) %>%
    as.data.frame()
  
  
  # N countries per treaty
  iftd_by_treaty <- iftd_since_1960 %>%
    group_by(treaty_id) %>%
    summarize(number_countries = n()) %>%
    as.data.frame()
  
  # N treaties per country
  
  iftd_by_country <- iftd_since_1960 %>%
    group_by(CCODE) %>%
    summarize(number_treaties = n()) %>%
    as.data.frame()
  
  
  
  
  treaties_sum <- c(summary(iea_water_by_treaty$number_countries),length(unique(iea_water_by_treaty$treaty_id)),nrow(iea_water_clean))
  countries_sum <- c(summary(iea_water_by_country$number_treaties), length(unique(iea_water_by_country$country)),nrow(iea_water_clean))
 
  treaties_sum_iftd <- c(summary(iftd_by_treaty$number_countries),length(unique(iftd_by_treaty$treaty_id)),nrow(iftd_since_1960))
  countries_sum_iftd <- c(summary(iftd_by_country$number_treaties), length(unique(iftd_by_country$CCODE)),nrow(iftd_since_1960))
  
  
   sums_tab <- rbind(treaties_sum,
                     countries_sum,
                     treaties_sum_iftd,
                     countries_sum_iftd)
  
  rownames(sums_tab) <- c("Country actions per treaty, 1961-2022 IEADB",
                          "Treaty actions per country, 1961-2022 IEADB",
                          "Country actions per treaty, 1960-2007 IFTD",
                          "Treaty actions per country, 1960-2007 IFTD")
  colnames(sums_tab) <- c("Min","2.5\\%","Median","Mean","75\\%","Max","N Unique","N Total")
  
  sums_tab_tex <- xtable(sums_tab,
                         caption = "Select Summary Statistics",
                         label = "tab:select_sum_stats_water",
                         digits = c(0,0,0,0,2,0,0,0,0))
  
  sums_tab_tex
  
  tabpath <- file.path(output_tables,paste0("select_sum_stats_water.tex"))
  
  print(sums_tab_tex,
        file = tabpath,
        append = FALSE,
        table.placement = "H",
        caption.placement = "top")
  
  
  
  
# Hist of number of treaties joined ----
  
  treaties_joined_hist <- ggplot(data  = countries_with_codes,
                                 aes(x = number_treaties_joined)) +
    geom_histogram(binwidth = 50,
                   fill = yale_lblue,
                   color = NA) +
    labs(title = "Country Distribution of Total Treaties Joined",
         caption = c("Data from International Environmental Agreements Database Project, (Mitchell 2022). ")
    )+
    geom_vline(xintercept = cutoff,
               linetype = "dashed",
               color = yale_blue) +
    annotate(geom = "text",
             x = cutoff+75,
             y = 30,
             label = paste0("Mean = ", round(cutoff,2)),
             color = yale_blue) +
    ylab("Count")+
    theme_plot()
  
  treaties_joined_hist
  
  save_map(output_folder = output_figures,
           plotname = treaties_joined_hist,
           filename = "treaties_joined_hist.png",
           width = 9,
           height = 5,
           dpi  = 300)
  

# document types breakdown
  
  treaty_codes <- c("Not coded",
                    "Not a treaty",
                    "Semi-international",
                    "Not TFDD compatible",
                    "Primary agreement",
                    "Replaces primary agreement",
                    "Amendment",
                    "Protocol",
                    "Financial agreement",
                    "Missing",
                    "Available, not translated",
                    "Available, not coded",
                    "Total")
  
  # since 1960
  iftd_treaties_by_treaty_1960 <- iftd_since_1960 %>%
    group_by(treaty_id, DocType) %>%
    summarize(number_countries = n()) %>%
    as.data.frame()
  
  # addmargins adds the total at the bottom
  
  treaty_bytreaty_tab_1960 <- table(iftd_treaties_by_treaty_1960$DocType) %>% addmargins() %>% as.data.frame()
  treaty_bytreaty_tab_1960 <- treaty_bytreaty_tab_1960[,2] %>% as.data.frame()
  
  
  treaty_tab_1960 <- table(iftd_since_1960$DocType) %>% addmargins() %>% as.data.frame() 
  treaty_tab_1960 <- treaty_tab_1960[,2] %>% as.data.frame()
  
  
  # all time 
  iftd_treaties_by_treaty <- iftd_treaties %>%
    group_by(treaty_id, DocType) %>%
    summarize(number_countries = n()) %>%
    as.data.frame()
  
  
  
  # addmargins adds the total at the bottom
  
  treaty_bytreaty_tab <- table(iea_water_clean$DocType) %>% addmargins() %>% as.data.frame()
  treaty_bytreaty_tab <- treaty_bytreaty_tab[,2] %>% as.data.frame()
  
  
  treaty_tab <- table(iftd_treaties$DocType) %>% addmargins() %>% as.data.frame() 
  treaty_tab <- treaty_tab[,2] %>% as.data.frame()
  
  
  treaty_tab <- cbind(treaty_tab,treaty_bytreaty_tab,treaty_tab_1960,treaty_bytreaty_tab_1960)
  
  colnames(treaty_tab) <- c("All Links","All Treaties",">1960 Links",">1960 Treaties")
  rownames(treaty_tab) <- treaty_codes
  
  treaty_tab
  
  treaty_tab_tex <- xtable(treaty_tab, 
                           caption = "International Freshwater Treaty Database: Document Types and Counts",
                           label = "doc_types_and_counts",
                           digits = c(0,0,0,0,0))
  
  tabpath <- file.path(output_tables,paste0("doc_types_and_counts.tex"))
  
  print(treaty_tab_tex,
        file=tabpath,
        append=FALSE,
        table.placement = "H",
        caption.placement="top")
  
  
  
   