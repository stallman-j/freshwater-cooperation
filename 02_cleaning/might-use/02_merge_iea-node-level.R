# _______________________________#
# International Agreements
# Merge 02: IEA Database Node-level Data
# 
# Stallman
# Started 2022-12-17
# Last edited: 
#________________________________#
# Startup

rm(list = ls())


# bring in the packages, folders, paths ----

  home_folder <- file.path("P:","Projects","international-agreements")

  source(file.path(home_folder,"code","00_master_ia.R"))

# bring in data ----

  iea_members<- readRDS(file = file.path(data_clean,"iea_dfs","iea_members.rds"))
  country_economic_indicators <- readRDS(file = file.path(data_temp,"node-level","country_economic_indicators.rds"))
  country_centroids_sf <- readRDS(file = file.path(data_temp,"node-level","country_centroids_sf.rds"))
  world <- readRDS(file = file.path(data_clean,"world.rds"))
  
  total_trade_import <- readRDS(file.path(data_temp,"total_trade_import.rds"))
  total_trade_export <- readRDS(file.path(data_temp,"total_trade_export.rds"))
  
  
  # pilot counting the number of treaties they've joined ----
  iea_members_pilot <- readRDS(file.path(data_clean,"iea_members_pilot.rds"))
  
  iea_members_small <- iea_members_pilot %>%
                      select(country, treaty_id)
  
  data_count <- aggregate(data = iea_members_small,
                          treaty_id ~ country,
                          function(x) length(unique(x)))
  
  data_count
  
  
  # do this for the full thing
  
  iea_treaty_count <- aggregate(data = iea_members,
                                treaty_id ~ country,
                                function(x) length(unique(x))) 
  
  iea_treaty_count <- iea_treaty_count %>%
                      rename(number_treaties_joined = treaty_id)
  
  head(iea_treaty_count)
  
  
  library(countrycode)
  data(codelist_panel)
  
# check country codes ----

#  library(countrycode)
  
#   #codelist_panel %>% view()
#   codelist_df <- as.data.frame(table(codelist_panel$country.name.en))
#   countries_iea_df <- as.data.frame(table(iea_members$country))
# 
# # check that everything is matched up
#   
#   df_test <- full_join(codelist_df, countries_iea_df, keep = TRUE ,
#                        by = "Var1",
#                        suffix = c(".codelist",".iea"))
# 
# 

  codelist_panel_2019 <- codelist_panel %>%
                          filter(year == 2019)
# merge country centroids ----
  
  library(sf)
  
  # pull out nodelist
  countries_ia <- data.frame(country = sort(unique(iea_members$country)))
  
  # merge on the treaty counts
  countries_ia <- left_join(countries_ia, iea_treaty_count,
                            by = "country")
  
  ## test the join
  # countries_ia_centroids <- full_join(countries_ia,country_centroids_sf, by = c("country"),
  #                                     keep = TRUE,
  #                                     suffix= c(".iea",".centroids"))
  
  # right-join by countries_ia keeps the sf because the x version needs to be the SF object
  
  countries_ia_centroids <- right_join(country_centroids_sf, countries_ia, by = c("country"),
                                      keep = FALSE)

# merge country codes ----
  
  # test the join
  # countries_with_codes <- full_join(countries_ia_centroids,codelist_panel_2019,
  #                                   by = c("country"="country.name.en"),
  #                                   keep = TRUE,
  #                                   suffix = c(".iea","codelist"))
  

  countries_with_codes <- left_join(countries_ia_centroids,codelist_panel_2019,
                                    by = c("country"="country.name.en"),
                                    keep = FALSE)
  
  class(countries_with_codes) # keeps sf, dataframe
  
  saveRDS(countries_with_codes,file = file.path(data_clean,"node-level","countries_with_codes.rds"))
  
  # # example plot by the region of centroids
  # plot(countries_with_codes['ar5'])
  # 
  # 
  # 
  # map_centroids <- ggplot() +
  #   geom_sf(data = world, alpha = 0,
  #           size = .3) +
  #   geom_sf(data = countries_with_codes,
  #           aes(color = ar5),
  #           shape = 21)
  # 
  # map_centroids
  
  
# now slice the trade data by countries which are in the ia data ----
  
  # just relevant names 
  
  countries_codes_limited <- countries_with_codes %>%
                               select("country","long","lat","ar5","continent","gwn","iso3c")
  
  # names: location_code, partner_code, year, import_value
  
  data_frame <- total_trade_import
  countries_to_include <- countries_with_codes$iso3c
  
  add_info_to_trade_list <- function(data_frame,
                                   countries_to_include) {
  # delete anything with a name not in countries to include
  
  df_2 <- data_frame[data_frame$location_code %in% countries_to_include,]
  
  names_trade <- unique(df_2$location_code) #193 of them
  
  df_3 <- df_2[df_2$partner_code %in% countries_to_include,]
  
  names_trade_2 <- unique(df_2$location_code)
    
  
  df_4 <- left_join(df_3, countries_codes_limited, by = c('location_code'='iso3c'),
                  keep = TRUE)
  
  # then add on the other edge attributes
  df_5 <- left_join(df_4, countries_codes_limited,
                    by = c('partner_code' = 'iso3c'),
                    suffix = c('.from','.to'))
  }
  
  
  
  trade_imports_with_nodes <- add_info_to_trade_list(data_frame = total_trade_import,
                                                     countries_to_include = countries_with_codes$iso3c)
  
  trade_exports_with_nodes <- add_info_to_trade_list(data_frame = total_trade_export,
                                                     countries_to_include = countries_with_codes$iso3c)
  
  saveRDS(trade_imports_with_nodes,file = file.path(data_clean,"edge-level","trade_import_clean.rds"))
  
  saveRDS(trade_exports_with_nodes,file = file.path(data_clean,"edge-level","trade_export_clean.rds"))
  
  
# merge distances ----
  
  distlist_2019 <- readRDS(file = file.path(data_clean,"edge-level","distlist_2019.rds"))
  
  dl_1 <- right_join(distlist_2019,countries_codes_limited,
                     by = c('ccode1' = 'gwn'),
                     keep = TRUE)
  
  dl_2 <- right_join(dl_1, countries_codes_limited,
                     by = c('ccode2' = 'gwn'),
                     suffix = c('.from','.to'),
                     keep = TRUE) %>%
          filter(!is.na(country.from))
  
  
  # make a network object
  
  # get out the edgelist and merge 
  
  cumulative_data <- readRDS( file = file.path(data_clean,"iea_matrices_year","cumulative","iea_full_matrix_1961_to_2022.rds"))
  
  cumulative_data_net <- graph_from_adjacency_matrix(adjmatrix = cumulative_data,
                                             mode = c("undirected"),
                                             weighted = TRUE)
  
  # full edgelist
  total_edgelist <- as.data.frame(as_edgelist(cumulative_data_net,
                                                names = TRUE)) %>%
                    rename(country_from = V1,
                           country_to = V2)
  
  # merge with distances
  
  distance_edges <- left_join(total_edgelist, dl_2,
                              by = c("country_from" = "country.from",
                                     "country_to" = "country.to"))
  

  df <- distance_edges %>% select(country_from,country_to) %>% as.matrix()
        
  
  distance_net <- graph_from_edgelist(df,
                                      directed = FALSE)

  distance_net <- set_edge_attr(distance_net, "weight", value = distance_edges$mindist)

  is_weighted(distance_net)    

  distance_mat <- as_adjacency_matrix(graph = distance_net,
                                attr = "weight")  %>% as.matrix()
  
  saveRDS(distance_mat, 
          file = file.path(data_clean,"edge-level","distance_mat_clean.rds"))
  
# get a sumstats table ----
  
  
  