# _______________________________#
# International Agreements
# Plot 04 Plot IEA Network
# # # https://stackoverflow.com/questions/42960248/how-to-plot-networks-over-a-map-with-the-least-overlap
# almost verbatim from the above
# Stallman
# Started 2022-12-18
# Last edited: 
#________________________________#
# Startup

rm(list = ls())


# bring in the packages, folders, paths ----

  home_folder <- file.path("P:","Projects","international-agreements")

  source(file.path(home_folder,"code","00_master_ia.R"))

  # temporarily unload packages that ggnet doesn't like
  # pacman::p_unload(
  #   purrr,
  #   terra,
  #   readr)
  #   

  library(tidyverse)
  library(sna)
  library(maps)
  #library(network)
  if (!require("ggnet")) devtools::install_github("briatte/ggnet")
  library(ggplot2)
  library(ggnet)
  
# bring in data ----

  countries_with_codes <- readRDS(file = file.path(data_clean,"node-level","countries_with_codes.rds"))
  
  # get ids on which to merge other stuff
  # ids <- seq(1, nrow(countries_with_codes))
  # 
  # countries <- cbind(id = ids,
  #                    countries_with_codes)
  # 
  distlist_2019        <- readRDS(file = file.path(data_clean,"edge-level","distlist_2019.rds"))
  iea_full_matrix_1961_to_2022 <- readRDS(file = file.path(data_clean,"iea_matrices_year","cumulative","iea_full_matrix_1961_to_2022.rds"))
  
  iea_full_matrix_1961_to_2022 <- set_diag_zero(iea_full_matrix_1961_to_2022)
  
  # get the names and make sure they match up with the countries list
  
  matrix_rownames <- rownames(iea_full_matrix_1961_to_2022)
  
  countrynames    <- countries_with_codes$country
  
  # iea_members<- readRDS(file = file.path(data_clean,"iea_dfs","iea_members.rds"))
  # country_economic_indicators <- readRDS(file = file.path(data_temp,"node-level","country_economic_indicators.rds"))
  # country_centroids_sf <- readRDS(file = file.path(data_temp,"node-level","country_centroids_sf.rds"))
  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  
  netty <- graph_from_adjacency_matrix(iea_full_matrix_1961_to_2022,
                   mode = c("directed"),
                   weighted = TRUE)
  
  
  edgelist <- as_edgelist(netty,
                          names = TRUE)
  
  edgelist <- data.frame(from_country = edgelist[,1],
                    to_country   = edgelist[,2],
                    weight = E(netty)$weight)
  
  edgelist %>% head()
  #  from_country        to_country weight
  # 1 Afghanistan       Afghanistan    148
  # 2 Afghanistan           Albania    129
  # 3 Afghanistan           Algeria    121
  # 4 Afghanistan           Andorra     55
  # 5 Afghanistan            Angola    102
  # 6 Afghanistan Antigua & Barbuda    128
  
  
# join by countries on the left by the in id to get the node name
  all.equal(sort(unique(edgelist$from_country)),sort(unique(countries_with_codes$country)))
  # TRUE, we're not getting any bad matches
  
  # the appended long and lat and all that other info is for the from_country
  
  dt <- left_join(edgelist, countries_with_codes, by = c('from_country'='country'),
                  keep = FALSE)

  
  
  # show some of it if keep = TRUE
  
  #dt %>%  select(from_country, weight, country, long, lat)  %>% head() %>% view()
  
  # check failes
  
  dt_fails <- anti_join(edgelist, countries_with_codes, by = c('from_country'='country'),
                  keep = TRUE)
  # 0 obs of 3 vars
  
  # now add on the to country
  
  dt_2 <- left_join(dt, countries_with_codes,
                    by = c('to_country' = 'country'),
                    suffix = c('.from','.to'))
  
  dt_2[6870:6899,] %>% select(from_country, to_country, long.from, lat.from, long.to, lat.to, weight) %>% view()
  # -71.35512 long
  # -37.69607 lat, should be named chile, look in google maps and that's right
  
  # then -77.3 long, 18.15 lat should be Jamaica and that's right (google maps write lat first)
  
  
  # add nice labels
  
  dt_3 <- dt_2 %>%
          mutate(edge_id = seq(1,nrow(.)),
                 from_coord_long = long.from,
                 from_coord_lat  = lat.from,
                 from_ar5        = ar5.from,
                 from_continent  = continent.from,
                 from_cown       = cown.from,
                 from_gwn        = gwn.from,
                 from_iso3c      = iso3c.from,
                 from_region     = region.from,
                 from_region23   = region23.from,
                 to_coord_long   = long.to,
                 to_coord_lat    = lat.to,
                 to_ar5          = ar5.to,
                 to_continent    = continent.to,
                 to_cown         = cown.to,
                 to_gwn          = gwn.to,
                 to_iso3c        = iso3c.to,
                 to_region       = region.to,
                 to_region23     = region23.to
                 )
  
  
  keep_vars <- c("from_country","to_country","weight","from_coord_long","from_coord_lat","to_coord_long","to_coord_lat",
                 "from_ar5","from_continent","to_ar5","to_continent","from_cown","to_cown","from_iso3c","to_iso3c",
                 "from_region","to_region","from_region23","to_region23")
  
  dt_4 <- dt_3[, keep_vars]
  
  # faster way but doesn't put them right next to each other
  # dt_4 <- dt_3 %>% select(edge_id, weight,
  #                         starts_with('from'),
  #                         starts_with('to'))
  
  dt_4 %>% head() %>% view()
  
# Choose how to graph weights -----
  
  # take the 95th percentile
  # with any weights, this ends up leading to a smudge in Europe
  
  # cutoff <- quantile(dt_4$weight, 0.998)
  # 
  # cutoff_weight <- ifelse(dt_4$weight > cutoff, dt_4$weight, 0)
  # 
  # cutoff_weight[cutoff_weight>0]
  # 
  # dt_cutoff <- dt_4[dt_4$weight > cutoff, ]
  # 
  # hist(dt_4$weight)
  # abline(v = cutoff,
  #        col = "red",
  #        lty = 3,
  #        lwd = 2)
  # 
  # hist(cutoff_weight)
  # 
  # cutoff_weight[cutoff_weight > 0]
  

# find the max partner for each country ----
  
  # take the weight that's the top weight for each country and only 
  # keep those obs
  
  # this gives the max for each country
  
  
  dt_max <- dt_4 %>%
            group_by(from_country) %>%
            filter(weight == max(weight)) # %>%
  # st_as_sf(coords = c("from_coord_long","from_coord_lat"),
  #          crs = 4326,
  #          remove = FALSE)  
  # 
  
  # get the right CRS
  #dt_max <- st_transform(dt_max, projection_crs)
  
  # showthe projection
  
  #st_crs(dt_max)
  
  
  cutoff_map <- quantile(dt_max$weight, 0.75)
  
  dt_map <- dt_max[dt_max$weight > cutoff_map, ]
  
  hist(dt_max$weight)
  abline(v = cutoff_map,
         col = "red",
         lty = 3,
         lwd = 2)
  
  
  
  # find countries ----
  
  # put data = all_countries in geom_text_repel() to find which countries to include
  all_countries <- dt_map %>% 
    select(from_country, 
           from_coord_long, 
           from_coord_lat) %>% unique()
  
  
  # names of countries to include once you have an idea
  
  countries <- c("Canada",
                 "United States",
                 "Mexico",
                 "Panama",
                 "Brazil",
                 "Uruguay",
                 "Iceland",
                 "Norway",
                 "Finland",
                 "Russia",
                 "Japan",
                 "Philippines",
                 "Australia",
                 "New Zealand",
                 "South Africa",
                 "Egypt",
                 "Germany",
                 "South Korea",
                 "China",
                 "Mauritius",
                 "Argentina",
                 "Ecuador",
                 "Jamaica",
                 "Cyprus",
                 "India",
                 "Morocco",
                 "Netherlands",
                 "Sweden")
  
  map_countries <- dt_map[dt_map$from_country %in% countries,] %>%
    select(from_country, 
           from_coord_long, 
           from_coord_lat) 
  
 
  
  library(ggrepel) # geom_text_repel
  library(ggalt) # change projection of maps
  
  world_map <- map_data('world') 
  
  myMap <- ggplot() +
    geom_map(data = world_map,
             map = world_map,
             aes(map_id = region),
             color = "gray85",
             fill = "gray93") +
    xlim(c(-180,180)) + ylim(c(-70,90))+
    geom_segment(data = dt_map,
                 alpha = 0.1,
                 color = "dodgerblue1",
                 aes(x = from_coord_long, y = from_coord_lat,
                     xend = to_coord_long, yend = to_coord_lat,
                     lwd = weight/max(dt_map$weight)),
                 show.legend = FALSE) +
     geom_point(data = dt_map,
               aes(x = from_coord_long,
                   y = from_coord_lat),
               size = (5*dt_map$weight)/max(dt_map$weight),
               color = yale_blue,
               alpha = 0.5) +
    geom_text_repel(data = map_countries,
                    colour = yale_blue,
                    aes(x = from_coord_long, y = from_coord_lat, label = from_country),
                    max.overlaps = 20) + 
    labs(title = "Top Partners in Environmental Agreements") +
    theme_map()
  myMap
  
  
  # try to change projections
  
  myMap <- ggplot() +
    geom_sf(data = world) +
    xlim(c(-180,180)) + ylim(c(-70,90))+
    # geom_segment(data = dt_map,
    #              alpha = 0.1,
    #              color = "dodgerblue1",
    #              aes(x = from_coord_long, y = from_coord_lat,
    #                  xend = to_coord_long, yend = to_coord_lat,
    #                  lwd = weight/max(dt_map$weight)),
    #              show.legend = FALSE) +
    geom_point(data = dt_map,
               aes(x = from_coord_long,
                   y = from_coord_lat),
               size = (5*dt_map$weight)/max(dt_map$weight),
               color = yale_blue,
               alpha = 0.5) +
    # geom_text_repel(data = map_countries,
    #                 colour = yale_blue,
    #                 aes(x = from_coord_long, y = from_coord_lat, label = from_country),
    #                 max.overlaps = 20) + 
    # labs(title = "Top Partners in Environmental Agreements") +
    theme_map()
  myMap
  
  
  map_submissions <- ggplot() +
    geom_sf(data = world, alpha = 0,
            size = .3) +
    geom_sf(data = submissions_data_with_geocode,
            aes(size = n),
            color = yale_blue,
            shape = 21,
            fill = yale_blue)+
    labs(x = NULL,
         y = NULL,
         title = "Current Institutions of Authors Submitting to NEUDC 2022",
         #subtitle = "With Predicted Capacity, for Plants with WRI Capacity Data",
         caption = "Georeferenced by institution or city, state and country as available from submitter information on Conference Maker on 2022-11-18 \n
                          621 total unique submissions" ) +
    scale_size(name = "Number",
               breaks = c(1,5,10,20,40),
               range = c(10,40)
    ) +
    theme_map()
    

# show frequencies in a bar chart ----
  
  cutoff_chart <- quantile(dt_max$weight, 0.75)
  
  dt_chart <- dt_max[dt_max$weight > cutoff_chart, ]
  
  # show how many countries rely on germany or the netherlands
  
  ggplot(data = dt_chart,
         aes(x = to_country)) +
    geom_bar(stat = "count") +
    labs(title = "Most Common Partners in IEAs",
         caption = c("Frequency count from top 75th percentile in number of agreements. \n
                     Data from International Environmental Agreements Database Project, (Mitchell 2022). ")
                     )+
    ylab("Count")+
    theme_plot()
  

  
  