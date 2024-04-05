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

  home_folder <- file.path("P:","Projects","environment")

  source(file.path(home_folder,"code","00_startup_master.R"))

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
  
  
  distlist_2019        <- readRDS(file = file.path(data_clean,"edge-level","distlist_2019.rds"))
  
  iea_full_matrix_1961_to_2022 <- readRDS(file = file.path(data_clean,"iea_matrices_year","cumulative","iea_full_matrix_1961_to_2022.rds"))
  
  # If you don't set diagonals to zero, we just get that the country itself is its most common partner
  iea_full_matrix_1961_to_2022 <- set_diag_zero(iea_full_matrix_1961_to_2022)
  
  # get the names and make sure they match up with the countries list
  
  matrix_rownames <- rownames(iea_full_matrix_1961_to_2022)
  
  countrynames    <- countries_with_codes$country
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
                 from_number_treaties_joined = number_treaties_joined.from,
                 from_coord_long = long.from,
                 from_coord_lat  = lat.from,
                 from_ar5        = ar5.from,
                 from_continent  = continent.from,
                 from_cown       = cown.from,
                 from_gwn        = gwn.from,
                 from_iso3c      = iso3c.from,
                 from_region     = region.from,
                 from_region23   = region23.from,
                 to_number_treaties_joined = number_treaties_joined.to,
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
  
  
  keep_vars <- c("from_country","to_country","weight","from_number_treaties_joined","to_number_treaties_joined", "from_coord_long","from_coord_lat","to_coord_long","to_coord_lat",
                 "from_ar5","from_continent","to_ar5","to_continent","from_cown","to_cown","from_iso3c","to_iso3c",
                 "from_region","to_region","from_region23","to_region23")
  
  dt_4 <- dt_3[, keep_vars]
  
  # faster way but doesn't put them right next to each other
  # dt_4 <- dt_3 %>% select(edge_id, weight,
  #                         starts_with('from'),
  #                         starts_with('to'))
  
  dt_4 %>% head() %>% view()
  
  hist(dt_4$from_number_treaties_joined)

  dt_final <- dt_4
# find the max partner for each country ----
  
  # take the weight that's the top weight for each country and only 
  # keep those obs
  
  # this gives the max for each country

  dt_max <- dt_4 %>%
            group_by(from_country) %>%
            filter(weight == max(weight))
  
  
  cutoff_map <- quantile(dt_max$weight, 0.75)
  
  dt_map <- dt_max[dt_max$weight > cutoff_map, ]
  
  hist(dt_max$weight)
  abline(v = cutoff_map,
         col = "red",
         lty = 3,
         lwd = 2)
  
  ggplot(data  = dt_max,
         aes(x = weight)) +
    geom_histogram(binwidth = 50,
                   fill = yale_lblue,
                   color = NA) +
    labs(title = "Distribution of Maximum Number of Agreements",
         caption = c("Data from International Environmental Agreements Database Project, (Mitchell 2022). ")
    )+
    geom_vline(xintercept = cutoff_map,
               linetype = "dashed",
               color = yale_blue) +
    annotate(geom = "text",
             x = cutoff_map,
              y = 30,
              label = "75th percentile",
             color = yale_blue) +
    ylab("Count")+
    theme_plot()
    

# Number of treaties joined, distribution ----
  
  cutoff <- mean(dt_4$from_number_treaties_joined)
  
  
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
  
# Now show the distribution over total number of agreements
  
  
  data_frame = dt_4
  counting_Var_index = 3 # index for weight
  title = "Distribution of Number of Common Agreements between Countries"
  caption = "Data from International Environmental Agreements Database Project, (Mitchell 2022). "
  

  
 degree_histogram <-  make_histogram(data_frame = dt_4,
                 counting_var_index = 3,
                 title = "Distribution of Number of Shared IEAs between Countries",
                 caption = "Data from International Environmental Agreements Database Project, (Mitchell 2022). ",
                 where_y = c(7000,8200,2000,2000),
                 where_x = c(.984,.85,.02,.86),
                 title_size = 15)
  
 degree_histogram
 
 save_map(output_folder = output_figures,
          plotname = degree_histogram,
          filename = "distribution-ieas_hist.png",
          width = 9,
          height = 5,
          dpi  = 300)
 
 

  # find countries ----
  
  # put data = all_countries in geom_text_repel() to find which countries to include
  all_countries <- dt_map %>% 
    select(from_country, 
           from_coord_long, 
           from_coord_lat) %>% unique()
  
  
  # names of countries to include once you have an idea
  
  # countries <- c("Canada",
  #                "United States",
  #                "Mexico",
  #                "Panama",
  #                "Brazil",
  #                "Uruguay",
  #                "Iceland",
  #                "Norway",
  #                "Finland",
  #                "Russia",
  #                "Japan",
  #                "Philippines",
  #                "Australia",
  #                "New Zealand",
  #                "South Africa",
  #                "Egypt",
  #                "Germany",
  #                "South Korea",
  #                "China",
  #                "Mauritius",
  #                "Argentina",
  #                "Ecuador",
  #                "Jamaica",
  #                "Cyprus",
  #                "India",
  #                "Morocco",
  #                "Netherlands",
  #                "Sweden")
  
 
 countries_with_codes %>%
   select(country,number_treaties_joined) %>%
   arrange(desc(number_treaties_joined)) %>%
   view()
 
 # highest weights for each country
 dt_map <- dt_final %>%
   group_by(from_country) %>%
   filter(weight == max(weight))
   #filter(weight > quantile(weight,0.99999))
 
 
 # take the top 20 countries by number of treaties joined
 countries <- c("Russia",
                "Germany",
                "United States",
                "France",
                "Netherlands",
                "Canada",
                "Japan",
                "Australia",
                "Brazil",
                "South Africa",
                "Mexico",
                "China",
                "Panama",
                "Morocco",
                "South Korea",
                "India",
                "New Zealand",
                "Uruguay",
               # "Tanzania",
                "France",
                "Philippines",
                "Kazakhstan")
  map_countries <- dt_map[dt_map$from_country %in% countries,] %>%
        select(from_country, 
           from_coord_long, 
           from_coord_lat) 
  
  # tanzania was duplicated
  map_countries <- map_countries[!duplicated(map_countries),]
  
 # testing it out
  # set.seed(9)
  # size_pilot <- 5
  # 
  # # create test dataframe 
  # nrow(iea_members) 
  # 
  # dt_pilot  <- dt_final %>%
  #             mutate(row_id = row_number()) %>%
  #   filter(row_id %in% sample(1:nrow(dt_final), size_pilot))
  # 
  # dt_pilot2 <- dt_pilot %>%
  #   group_by(to_region23) %>%
  #   filter(weight == max(weight))
  # 
  # test_dt <- sample(dt_final, size = 10)
  # 
  # 
  # 
  # # try out common regional maxes; most all of these are with the netherlands so not very interesting
  # 
  # dt_final %>%
  #     group_by(to_region23) %>%
  #     filter(weight == max(weight)) %>%
  #     view()

  world_map <- map_data('world') 
  
  
  library(ggrepel) # geom_text_repel
  test_map <- ggplot() +
    geom_map(data = world_map,
             map = world_map,
             aes(map_id = region),
             color = "gray70",
             fill = "gray99",
             alpha = .5,
             linewidth = .3) +
    xlim(c(-180,180)) + ylim(c(-70,90))+
    geom_segment(data = dt_map,
                 alpha = .2,
                 color = yale_lblue,
                 aes(x = from_coord_long, y = from_coord_lat,
                     xend = to_coord_long, yend = to_coord_lat),
                    size = 5*dt_map$weight/(max(dt_map$weight)),
                 show.legend = FALSE) +
    geom_point(data = countries_with_codes,
               aes(x = long,
                   y = lat),
               size = 5*countries_with_codes$number_treaties_joined/max(countries_with_codes$number_treaties_joined),
               color = yale_blue,
               alpha = 0.5) +
    geom_text_repel(data = map_countries,
                    colour = yale_blue,
                    aes(x = from_coord_long, y = from_coord_lat, label = from_country),
                    max.overlaps = 20) + 
    labs(title = "Most Common Shared Parties in International Environmental Agreements",
         caption = c("Data from International Environmental Agreements Database Project, (Mitchell 2022). ")
    )+
    theme_map()
  
  # "Edge width proportional to the number of IEAs between countries. \n
  #                   Node size proportional to maximum number of IEAs between the country and any other \n
  
  test_map
  
  save_map(output_folder = output_maps,
           plotname = test_map,
           filename = "most_common_partner_map.png",
           width = 9,
           height = 5,
           dpi  = 400)
  
  library(ggalt) # change projection of maps
  
  
  myMap <- ggplot() +
    geom_map(data = world_map,
             map = world_map,
             aes(map_id = region),
             color = "gray70",
             fill = "gray99",
             alpha = .5,
             linewidth = .3) +
    xlim(c(-180,180)) + ylim(c(-70,90))+
    geom_segment(data = dt_map,
                 alpha = 0.1,
                 color = yale_lblue,
                 aes(x = from_coord_long, y = from_coord_lat,
                     xend = to_coord_long, yend = to_coord_lat,
                     lwd = weight/max(dt_map$weight)),
                 show.legend = FALSE) +
     geom_point(data = countries_with_codes,
               aes(x = long,
                   y = lat),
               size = 5*countries_with_codes$number_treaties_joined/max(countries_with_codes$number_treaties_joined),
               color = yale_blue,
               alpha = 0.5) +
    geom_text_repel(data = map_countries,
                    colour = yale_blue,
                    aes(x = from_coord_long, y = from_coord_lat, label = from_country),
                    max.overlaps = 20) + 
    labs(title = "Most Common Shared Parties in International Environmental Agreements",
         caption = c("Data from International Environmental Agreements Database Project, (Mitchell 2022). ")
    )+
    theme_map()
  
  # "Edge width proportional to the number of IEAs between countries. \n
  #                   Node size proportional to maximum number of IEAs between the country and any other \n
                     
  myMap
  
  save_map(output_folder = output_maps,
           plotname = myMap,
           filename = "most_common_partner_map.png",
           width = 9,
           height = 5,
           dpi  = 300)
  

# show frequencies in a bar chart ----
  
  
  dt_table <- get_table(dt_max$to_country)
  
  keep_table <- dt_table[dt_table$count > 3,]
  
  
  
  bar_most_common_partners <- ggplot(data = keep_table,
                                     aes(x = data_call,
                                         y = count)) +
    geom_bar(stat = "identity",
             fill = yale_lblue,
             color = NA) +
    labs(title = "Most Common Partners in IEAs",
         caption = c("Data from International Environmental Agreements Database Project, (Mitchell 2022). ")
    )+
    ylab("Count")+
    theme_plot(title_size = 20)
  
  
  
  bar_most_common_partners
  
  save_map(output_folder = output_figures,
           plotname = bar_most_common_partners,
           filename = "most-common-partner_bar.png",
           width = 9,
           height = 5,
           dpi  = 300)


  

  
  