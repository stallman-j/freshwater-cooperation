# _______________________________#
# Environment
# Plot 04 Plot Basin-Treaty Info
# Stallman
# Started 2023-03-16
# Last edited: 
#________________________________#
# Startup

rm(list = ls())


# bring in the packages, folders, paths ----

home_folder <- file.path("P:","Projects","environment")

source(file.path(home_folder,"code","00_startup_master.R"))

# international freshwater treaties database

  iftd_treaties <- readRDS(file = file.path(data_clean,"edge-level","iftd_treaties.rds"))

  iftd_since_1960 <- iftd_treaties %>%
    filter(DateSigned > "1960-01-01")
  
  
## input doctype labels ----


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
  
  treaty_bytreaty_tab <- table(iftd_treaties_by_treaty$DocType) %>% addmargins() %>% as.data.frame()
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



# show a histogram over the number of countries
  
  # over all treaties
  
  number_countries <- as.data.frame(table(iftd_treaties$treaty_id)) %>%
    rename(treaty_id = Var1, num_countries = Freq)
  
  cutoff <- mean(number_countries$num_countries)
  
  treaties_joined_hist <- ggplot(data  = number_countries,
                                 aes(x = num_countries))+
    geom_histogram(fill = yale_lblue,
                   color = NA) +
    labs(title = "Distribution of Number of Countries per Treaty (all)",
         caption = c("Data from International Freshwater Treaties Database, 2019 \n
                     Over 634 treaties from 1820 to 2007")
    ) +

  geom_vline(xintercept = cutoff,
             linetype = "dashed",
             color = yale_blue) +
  annotate(geom = "text",
           x = cutoff,
           y = 30,
           label = paste0("Mean = ", round(cutoff,2)),
           color = yale_blue) +
  ylab("Count")+
  theme_plot(title_size = 16)

  treaties_joined_hist
  
  save_map(output_folder = output_figures,
           plotname = treaties_joined_hist,
           filename = "treaties_joined_hist.png",
           width = 9,
           height = 5,
           dpi  = 300)


  # over all treaties since 1960
  
  number_countries <- as.data.frame(table(iftd_since_1960$treaty_id)) %>%
    rename(treaty_id = Var1, num_countries = Freq)
  
  cutoff <- mean(number_countries$num_countries)
  
  treaties_joined_hist_1960 <- ggplot(data  = number_countries,
                                 aes(x = num_countries))+
    geom_histogram(fill = yale_lblue,
                   color = NA) +
    labs(title = "Distribution of Number of Countries per Treaty (since 1960)",
         caption = c("Data from International Freshwater Treaties Database, 2019 \n
                     Over 373 treaties from 1960 to 2007")
    ) +
    
    geom_vline(xintercept = cutoff,
               linetype = "dashed",
               color = yale_blue) +
    annotate(geom = "text",
             x = cutoff,
             y = 30,
             label = paste0("Mean = ", round(cutoff,2)),
             color = yale_blue) +
    ylab("Count")+
    theme_plot(title_size = 16)
  
  treaties_joined_hist
  
  save_map(output_folder = output_figures,
           plotname = treaties_joined_hist_1960,
           filename = "treaties_joined_hist_1960.png",
           width = 9,
           height = 5,
           dpi  = 300)
  
  
  