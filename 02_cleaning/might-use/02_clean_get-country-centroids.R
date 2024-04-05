# _______________________________#
# International Agreements
# Clean 02: get country centroids
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


## Can come back to this for city locations ----
# https://www.naturalearthdata.com/downloads/10m-cultural-vectors/

# populated_data <- st_as_sf(ne_download(scale = 110,
#                               type = 'populated_places',
#                               category = 'cultural'))
# 
# names(populated_data)
# 
# class(populated_data)
  library(rnaturalearth)
  library(sf)
  library(terra)
  library(rgdal) 
  
  all_countries <- st_as_sf(ne_download(scale = 110,
                                      type = "countries"))

  # look at names
  #all_countries %>% select(NAME) %>% arrange(NAME) %>% view()

  plot(st_geometry(all_countries))

  countries <- all_countries %>%
  dplyr::select(NAME, POP_EST, POP_YEAR, GDP_MD, GDP_YEAR, ECONOMY,
         INCOME_GRP,ISO_A3_EH) %>%
  filter(NAME!="Antarctica")

  countries <- countries %>%
  dplyr::mutate(gdp_billion_usd = GDP_MD / 1000,
         country = NAME) %>%
  dplyr::rename(estimated_pop = POP_EST)
  
  


# get centroids ----

# https://gis.stackexchange.com/questions/71921/list-of-central-coordinates-centroid-for-all-countries
# get world map
  
  library(rgeos)
  library(rworldmap)
  
  
  wmap <- getMap(resolution="high")

  # get centroids
  centroids <- gCentroid(wmap, byid=TRUE)

  # get a data.frame with centroids
  df <- as.data.frame(centroids)
  head(df)

  #>                     x         y
  #> Aruba       -69.97345  12.51678
  #> Afghanistan  66.00845  33.83627
  #> Angola       17.53646 -12.29118
  #> Anguilla    -63.06082  18.22560
  #> Albania      20.05399  41.14258
  #> Aland        20.03715  60.20733
  
  # plot
  plot(centroids)
  
  centroids_df <- cbind(country = rownames(df), data.frame(centroids, row.names = NULL)) %>%
                  rename(long = x,
                         lat  = y)
  
  # change in country centroids (use ISO codes there if possible?)
  centroids_df$country[centroids_df$country== "Antigua and Barbuda"  ] <- "Antigua & Barbuda"
  
  centroids_df$country[centroids_df$country== "The Bahamas"  ] <- "Bahamas"
  centroids_df$country[centroids_df$country== "Bosnia and Herzegovina"  ] <- "Bosnia & Herzegovina"
  centroids_df$country[centroids_df$country== "Democratic Republic of the Congo"     ] <- "Congo - Kinshasa"
  centroids_df$country[centroids_df$country== "Republic of the Congo"  ] <- "Congo - Brazzaville"
  centroids_df$country[centroids_df$country==  "Ivory Coast"   ] <- "Côte d’Ivoire"
  centroids_df$country[centroids_df$country== "Czech Republic"  ] <-  "Czechia"
  centroids_df$country[centroids_df$country== "East Timor"  ] <-  "Timor-Leste"
  centroids_df$country[centroids_df$country== "Gaza"  ] <- "Palestinian Territories"
  centroids_df$country[centroids_df$country== "Guinea Bissau"  ] <- "Guinea-Bissau"
  centroids_df$country[centroids_df$country== "Hong Kong S.A.R."  ] <- "Hong Kong SAR China"
  centroids_df$country[centroids_df$country== "Federated States of Micronesia"  ] <-  "Micronesia (Federated States of)"
  centroids_df$country[centroids_df$country== "Macedonia"  ] <- "North Macedonia"
  centroids_df$country[centroids_df$country== "Myanmar"  ] <- "Myanmar (Burma)"
  centroids_df$country[centroids_df$country== "Sao Tome and Principe"  ] <- "São Tomé & Príncipe"
  centroids_df$country[centroids_df$country== "Saint Kitts and Nevis"  ] <- "St. Kitts & Nevis"
  centroids_df$country[centroids_df$country== "Saint Lucia"  ] <- "St. Lucia"
  centroids_df$country[centroids_df$country== "Saint Vincent and the Grenadines"  ] <- "St. Vincent & Grenadines"
  centroids_df$country[centroids_df$country== "Trinidad and Tobago"  ] <- "Trinidad & Tobago"
  centroids_df$country[centroids_df$country== "United States of America"  ] <- "United States"
  centroids_df$country[centroids_df$country== "Vatican"  ] <- "Vatican City"
  centroids_df$country[centroids_df$country== "United Republic of Tanzania"   ] <- "Tanzania"
  centroids_df$country[centroids_df$country==  "Republic of Serbia"  ] <- "Serbia"
  centroids_df$country[centroids_df$country==  "Swaziland"   ] <- "Eswatini"

  
# save dfs ----
  
  path <- file.path(data_temp,"node-level")
  
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  saveRDS(centroids_df,
        file = file.path(path,"country_centroids_df.rds"))

  saveRDS(countries,
          file = file.path(path,"country_economic_indicators.rds"))
  
  
  centroids_sf <- st_as_sf(centroids_df, coords = c("long","lat"),
                 crs = 4326,
                 remove = FALSE)
  

  # get the right CRS
  centroids_sf <- st_transform(centroids_sf, projection_crs)
  
  saveRDS(centroids_sf,
          file = file.path(path,"country_centroids_sf.rds"))
  
  centroids_sf <- readRDS(file = file.path(path,"country_centroids_sf.rds"))
  