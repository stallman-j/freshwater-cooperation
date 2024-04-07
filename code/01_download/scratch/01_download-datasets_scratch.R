# _______________________________#
# International Agreements
# download 01: download datasets and extract from their zip files
# 
# Stallman
# Started 2022-08-20
# Last edited: 
#________________________________#


# Startup

  rm(list = ls())

  # Only run this the first time, otherwise start from "cleaning!"
  
# bring in the packages, folders, paths ----
  
  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))
  

# Transboundary waters ----
  
  # transboundary waters shapefiles, basins + basin-country polygons with some attributes 
  download_data(data_subfolder = "transboundary-waters",
                data_raw = data_raw,
                url = "https://transboundarywaters.science.oregonstate.edu/sites/transboundarywaters.science.oregonstate.edu/files/TFDD_SpatialData_Public202203.zip",
                zip_file = TRUE,
                pass_protected = FALSE)
  
  # international freshwater treaties
  # https://transboundarywaters.science.oregonstate.edu/content/international-freshwater-treaties-database
  
  # last updated date
  
    
  download_data(data_subfolder = "transboundary-waters",
                data_raw = data_raw,
                url = "https://transboundarywaters.science.oregonstate.edu/sites/transboundarywaters.science.oregonstate.edu/files/Database/Data/Treaty/WebsiteTreatiesDB_20181124.xlsx",
                zip_file = TRUE,
                pass_protected = FALSE)
  
  
  # not sure why, but not having the "wb" was causing an error while downloading
  # get tbw_treaty_database_update_date from 00_startup_master since it's used in both download and cleaning
  
  download.file(url = "https://transboundarywaters.science.oregonstate.edu/sites/transboundarywaters.science.oregonstate.edu/files/Database/Data/Treaty/WebsiteTreatiesDB_20181124.xlsx",
                destfile = file.path(data_raw,"transboundary-waters",paste0("WebsiteTreatiesDB_",tbw_treaty_database_update_date,".xlsx")),
                quiet = TRUE,
                mode = "wb")

  download.file(url = "https://transboundarywaters.science.oregonstate.edu/sites/transboundarywaters.science.oregonstate.edu/files/Database/Data/Events/EventMaster111710%20%281%29.xls",
                destfile = file.path(data_raw,"transboundary-waters",paste0("EventMaster111710.xls")),
                quiet = TRUE,
                mode = "wb")
  

# International Environmental Agreements ----
  
  # https://iea.uoregon.edu/current_ieadb_dataset
  
  
  # bigger file, need to allow it more time
  
  download.file(url = "https://iea.uoregon.edu/sites/iea1.uoregon.edu/files/db_members.csv",
                destfile = file.path(data_raw,"iea-database-project","db_members.csv"),
                )
  
  filenames <- c("db_countries.csv",
                 "db_country_traits.csv",
                 "db_treaties.csv")
  
  download_multiple_files(data_subfolder = "iea-database-project",
                          data_raw = data_raw,
                          base_url = "https://iea.uoregon.edu/sites/iea1.uoregon.edu/files",
                          sub_urls = filenames,
                          filename = filenames)

# World Bank Projects ----
  
  

# Federico-Tena World Trade Historical Database: 1800-1938 ----
  
  # https://edatos.consorciomadrono.es/dataset.xhtml?persistentId=doi:10.21950/JKZFDP
  
  # https://edatos.consorciomadrono.es/dataset.xhtml?persistentId=doi:10.21950/JKZFDP
  
  #download.file(url = "https://edatos.consorciomadrono.es/dataset.xhtml?persistentId=doi:10.21950/JKZFDP",
  #              destfile = file.path(data_raw, "federico-tena-world-trade",))

# night lights ----
  
  # see 02_clean_noaa-satellite-lights, download+extract+clean all together was easiest

# shape files for countries ----
  # NOTE BIG DATASET
  
  
  
  # as gpkg, a single file
  download_data(data_subfolder = "gadm-global-shapefiles",
                data_raw = data_raw,
                url = "https://geodata.ucdavis.edu/gadm/gadm4.1/gadm_410-gpkg.zip",
                zip_file = TRUE,
                pass_protected = FALSE)
  
  # as separate subunits
  download_data(data_subfolder = "gadm-global-shapefiles",
                data_raw = data_raw,
                url = "https://geodata.ucdavis.edu/gadm/gadm4.1/gadm_410-levels.zip",
                zip_file = TRUE,
                pass_protected = FALSE)
  

# Global subnational river borders ----
  
  # https://zenodo.org/record/3906567#.XvN-GGhKjIU
  
  
  download_data(data_subfolder = "gsrb_global-subnational-river-borders",
                data_raw = data_raw,
                url = "https://zenodo.org/record/3906567/files/GSRB.zip?download=1",
                zip_file = TRUE,
                pass_protected = FALSE)
  

# Major rivers of the world, World Bank ----
  
  # https://datacatalog.worldbank.org/search/dataset/0042032
  
  # To do, it uses Javascript
  
# Water stress projections ----
  
  # TO DO
#   http://www.wri.org/resources/data-sets/aqueduct-water-stress-projections-data
  
  # Done
  download_data(data_subfolder = "resource-watch",
                data_raw = data_raw,
                url = "https://wri-public-data.s3.amazonaws.com/resourcewatch/wat_006_projected_water_stress.zip",
                zip_file = TRUE,
                pass_protected = FALSE)
  
# GeoDAR Georeferenced global dams and reservoirs ----
  
  #https://zenodo.org/record/6163413
  
  # https://zenodo.org/record/6163413/files/GeoDAR_beta_peer_review.zip?download=1
  # https://zenodo.org/record/6163413/files/GeoDAR_v10_v11.zip?download=1
  
  filenames <- c("GeoDAR_beta_peer_review","GeoDAR_v10_v11")
  sub_urls <- paste0(filenames,".zip?download=1")
  
  
  download_multiple_files(data_subfolder = "GeoDAR_georeferenced-global-dams-reservoirs",
                          data_raw = data_raw,
                          base_url = "https://zenodo.org/record/6163413/files",
                          sub_urls = sub_urls,
                          filename = filenames,
                          zip_file = TRUE)
  
# GRanD Global Reservoir and Dam ----
  
  # https://sedac.ciesin.columbia.edu/data/set/grand-v1-dams-rev01
  # 2011
  # https://sedac.ciesin.columbia.edu/downloads/data/grand-v1/grand-v1-dams-rev01/dams-rev01-global-shp.zip
  
  if (!require(httr)) install.packages("httr")
  library(httr)
  
  download_data(data_subfolder = "GRanD_global-reservoir-and-dam",
                data_raw = data_raw,
                url = "https://sedac.ciesin.columbia.edu/downloads/data/grand-v1/grand-v1-dams-rev01/dams-rev01-global-shp.zip",
                zip_file = TRUE,
                pass_protected = TRUE)
  
# GeoDAR-TopoCat: Drainage topology and catchment database (TopoCat) for Georeferenced global Dams And Reservoirs (GeoDAR) ----

  # https://zenodo.org/record/7750736
  # sikderGeoDARTopoCatDrainageTopology2023
  
  download_data(data_subfolder = "GeoDAR-TopoCat_drainage-topology-catchment-database",
                data_raw = data_raw,
                url = "https://zenodo.org/record/7750736/files/GeoDAR_TopoCat.zip?download=1",
                zip_file = TRUE,
                pass_protected = FALSE)
  
# Lake-TopoCat: Global Lake Drainage Topology and Catchment Database ----
  
  # https://zenodo.org/record/7420810
  # 
  

  # https://zenodo.org/record/7420810/files/Lake-TopoCat-v1.0_product_description.pdf?download=1
  
  download_multiple_files(data_subfolder = "GeoDAR_georeferenced-global-dams-reservoirs",
                          data_raw = data_raw,
                          base_url = "https://zenodo.org/record/6163413/files",
                          sub_urls = sub_urls,
                          filename = filenames,
                          zip_file = TRUE)
  
  filenames <- c("Lake-TopoCat-v1.0_product_description.pdf","https://zenodo.org/record/7420810/files/Pfaf2_basins.jpg")
  sub_urls <- paste0(filenames,"?download=1")
  
  
  download.file(url = "https://zenodo.org/record/7420810/files/Lake-TopoCat-v1.0_product_description.pdf?download=1",
                destfile = file.path(data_raw,"Lake-TopoCat","Lake-TopoCat-v1.0_product_description.pdf"),
                quiet = TRUE,
                mode = "wb")
  
  download.file(url = "https://zenodo.org/record/7420810/files/Pfaf2_basins.jpg?download=1",
                destfile = file.path(data_raw,"Lake-TopoCat","Pfaf2_basins.jpg"),
                quiet = TRUE,
                mode = "wb")
  

  filenames <- c("Lake_TopoCat_v1.0.gdb","Lake_TopoCat_v1.0.shp")
  sub_urls  <- c("Lake_TopoCat_v1.0.gdb.zip?download=1","Lake_TopoCat_v1.0.shp.zip?download=1")
 
   download_multiple_files(data_subfolder = "Lake-TopoCat",
                          data_raw = data_raw,
                          base_url = "https://zenodo.org/record/7420810/files",
                          sub_urls = sub_urls,
                          filename = filenames,
                          zip_file = TRUE)
  
  
  