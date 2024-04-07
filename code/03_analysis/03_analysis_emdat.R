
# _______________________________#
# Environment
# analysis 04: EM-DAT Panel
# 
# Stallman
# Started: 2023-09-24
# Last edited: 
#________________________________#


# Startup

#rm(list = ls())

# packages ----

  if (!require("sf")) install.packages("sf")
  if (!require("lubridate")) install.packages("lubridate")
  if (!require("AER")) install.packages("AER")
  if (!require("plm")) install.packages("plm")

  library(lubridate) # for doing things with dates
  library(sf)
  library(AER) # standard errors 'n stuff
  library(plm) # panels
# paths ----

  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))

# parameters ----
  
  current_continent <- current_continent # set in 00_startup_master.R
  time_range <- time_range
  disaster_type <- disaster_type
  
  
  joined_path <- file.path(data_external_clean,"EM-DAT",current_continent)
  joined_filename <- paste0("emdat_panel_",current_continent,"_",year(first(time_range)),"_",year(last(time_range)),"_",disaster_type,".rds")
  
  
  saveRDS(joined_sf, file = file.path(joined_path,joined_filename))
  data <- readRDS( file = file.path(joined_path,joined_filename))  %>% filter(!is.na(data$precip))
  
  
  
# https://www.econometrics-with-r.org/10.1-panel-data.html
  
# declare panel data ----
  
 data <- pdata.frame(data, index = c("unit_id","year"))
  is.data.frame(data)
  dim(data)  
  # 32273 x 79
  
  # summarize "unit_id" and "year"
  
  summary(data[,c("unit_id","year")])
  

  
  # subset the data ----
  data_2015 <- subset(data, year == "2015")
  data_1982 <- subset(data, year == "1982")
  
  lm_1982 <- lm(disaster_dummy ~ precip, data = data_1982)
  lm_2015 <- lm(disaster_dummy ~ precip, data = data_2015)
  
  coeftest(lm_1982, vcov. = vcovHC, type = "HC1")
  coeftest(lm_2015, vcov. = vcovHC, type = "HC1")
  
  
  years <- seq(from = ymd(head(time_range,n=1)),
               to   = ymd(tail(time_range,n=1)),by = "1 year") %>%
    year() # pull out just the year
  
  
  for (y in years) {
    # https://stackoverflow.com/questions/15633714/adding-a-regression-line-on-a-ggplot
    
    #year <- 2015
    
    data_year_y <- data %>% filter(year == y)
    
    lm_out <- lm(disaster_dummy ~ precip, data = data_year_y)
    
    predicted_df <- data.frame(prediction = predict(lm_out, data_year_y), precip = data_year_y$precip)
    
    plot <- ggplot(data = data_year_y, aes(x = precip, y = disaster_dummy)) +
      geom_point(color = yale_blue) +
      geom_line(color = "darkred", data = predicted_df, aes(x = precip, y = prediction) )+
      labs(title = paste0("Precipitation and disaster records, ",y),
           caption = c("Data from CRU-TS Version 4.07 (2023) and EM-DAT (2023)"))+
      xlab("Precipitation (annual avg. of mm/month)") +
      ylab("Recorded disaster in EM-DAT") +
      theme_plot(title_size = 20,
                 axis_title_x = element_text(color = "black",size = 15),
                 axis_title_y = element_text(color = "black", size = 15))
    
    plot
    
    save_map(output_folder = file.path(output_figures,"CRU_TS"),
             plotname = plot,
             filename = paste0("precipitation_v_disaster_",y,".png"),
             width = 8,
             height = 6,
             dpi  = 400)
    
    
  }
  
  path_to_pngs <- file.path(output_figures,"CRU_TS",paste0("precipitation_v_disaster_",1960:2018,".png"))
  
  
  # make into a gif
  gifski(png_files = path_to_pngs,
         gif_file = file.path(output_figures,
                              "CRU_TS",
                              paste0("precipitation_v_disaster_",year(first(time_range)),"_",year(last(time_range)),".gif")
                              ),
         width = 1500,
         height = 1000,
         delay = .5)
  
  
  
  saveRDS(joined_sf, file = file.path(joined_path,joined_filename))
  joined_sf <- readRDS( file = file.path(joined_path,joined_filename))
  
  