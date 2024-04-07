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




# show a histogram over the number of countries
number_countries <- as.data.frame(table(water_treaties_tmp$treaty_id)) %>%
  rename(treaty_id = Var1, num_countries = Freq)

treaties_joined_hist <- ggplot(data  = number_countries,
                               aes(x = Freq))+
  geom_histogram(binwidth = 50,
                 fill = yale_lblue,
                 color = NA) +
  labs(title = "Distribution of Number of Countries per Treaty",
       caption = c("Data from International Freshwater Treaties Database, 2019 \n
                   Over xx treaties since 1960")
  )

+
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

