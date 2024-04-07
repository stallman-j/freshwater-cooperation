# _______________________________#
# International Agreements
# Clean 02: Merge Country Codes
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


# get country distances ----

# https://rdrr.io/cran/cshapes/man/distmatrix.html

  library(cshapes)
  
  # 2020 was out of range, 2019 most recent
  # this with min distance is an expensive algorithm b/c polygons are big
  
  # current_distmatrix <- distmatrix(date = as.Date("2019-01-01",
  #                           type = "mindist"),
  #                           keep = 0.5,
  #                          useGW = TRUE,
  #                          dependencies = FALSE)
   
  system.time(
  current_distlist   <- distlist(date = as.Date("2019-01-01",
                                                  type = "mindist"),
                                   keep = 0.1,
                                   useGW = TRUE,
                                   dependencies = FALSE)
  ) # end system time
  
   # saveRDS(current_distmatrix,
   #       file = file.path(data_clean,"distmatrix_2019.rds"))
   # 
  
  path <- file.path(data_clean,"edge-level")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  
   
   saveRDS(current_distlist,
           file = file.path(path,"distlist_2019.rds"))
   
   
  distlist_2019 <- readRDS(file = file.path(data_clean,"edge-level","distlist_2019.rds"))
  
  # 
  # # get world map
  # 
  # library(rgeos)
  # library(rworldmap)
  # 
  # 
  # wmap <- getMap(resolution="high")
  # 
  # # get centroids
  # centroids <- gCentroid(wmap, byid=TRUE)
  # 
  # # get a data.frame with centroids
  # df <- as.data.frame(centroids)
  # head(df)
  # 
  # #>                     x         y
  # #> Aruba       -69.97345  12.51678
  # #> Afghanistan  66.00845  33.83627
  # #> Angola       17.53646 -12.29118
  # #> Anguilla    -63.06082  18.22560
  # #> Albania      20.05399  41.14258
  # #> Aland        20.03715  60.20733
  # 
  # # plot
  # plot(centroids)
  # 
  # centroids_df <- cbind(country = rownames(df), data.frame(centroids, row.names = NULL)) %>%
  #                 rename(long = x,
  #                        lat  = y)
  # 
  # 
  # saveRDS(centroids_df,
  #       file = file.path(data_clean,"country_centroids_df.rds"))
  # 
  # centroids_sf <- st_as_sf(centroids_df, coords = c("long","lat"),
  #                crs = 4326)
  # 
  # 
  # # get the right CRS
  # centroids_sf <- st_transform(centroids_sf, projection_crs)
  # 
  # saveRDS(centroids_sf,
  #         file = file.path(data_clean,"country_centroids_sf.rds"))