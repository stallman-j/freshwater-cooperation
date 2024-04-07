# _______________________________#
# Environment
# download 01: download datasets and extract from their zip files
# 
# Stallman
# Started: 2023-04-13
# Last edited: 
#________________________________#

# Startup

  rm(list = ls()) # removes everything from environment

# bring in the packages, folders, paths ----
  
  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))
  
# packages ----
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    httr, # # for inputting username + password into a static sit
    rvest,# for getting urls
    reticulate, # running python in R
    httr,
    jsonlite,
    utils,
    R.utils, # to unzip a .gz file
    archive, # to unzip a .tar file
    stringr
  )
  
  devtools::install_github("JakobMie/nightlightstats")
  
# Satellite lights 2013-2022 ----

  # you need to 1) register with Earth Observation Group
  # https://eogdata.mines.edu/products/register/ for login and registration info
  # code for downloading from bottom of : https://eogdata.mines.edu/products/register/
    
  # Retrieve access token
    params <- list(
      client_id = 'eogdata_oidc',
      client_secret = '2677ad81-521b-4869-8480-6d05b9e57d48',
      username = "jillian.stallman@gmail.com",
      password = "Uh2y2Vr@M8tpT-o9",
      grant_type = 'password'
    )
    
    token_url <- 'https://eogauth.mines.edu/auth/realms/master/protocol/openid-connect/token'
    response <- POST(token_url, body = params, encode = "form")
    access_token_list <- fromJSON(content(response,as="text",encoding="UTF-8"))
    access_token <- access_token_list$access_token
    # Submit request with token bearer and write to output file
    ## Change data_url variable to the file you want to download
    data_url <-     "https://eogdata.mines.edu/nighttime_light/annual/v21/2013/VNL_v21_npp_2013_global_vcmcfg_c202205302300.average_masked.dat.tif.gz"

    auth <- paste('Bearer', access_token)
    ## You can either define the output file name directly
    # output_file <- 'EOG_sensitive_contents.txt'
    ## Or get the filename from the data_url variable
    
    extract_path <- file.path(data_external_raw,"EOG_satellite-lights")
    
    if (file.exists(extract_path)) {
      cat("The data subfolder",extract_path,"already exists. \n")
    } else{
      cat("Creating data subfolder",extract_path,".\n")
      dir.create(extract_path)
    }
    
    out_path <- file.path(data_external_temp,"EOG_satellite-lights")
    if (!file.exists(out_path)) dir.create(out_path,recursive = TRUE)
    
    urls <- c(
      "https://eogdata.mines.edu/nighttime_light/annual/v21/2013/VNL_v21_npp_2013_global_vcmcfg_c202205302300.average_masked.dat.tif.gz",
      "https://eogdata.mines.edu/nighttime_light/annual/v21/2014/VNL_v21_npp_2014_global_vcmslcfg_c202205302300.average_masked.dat.tif.gz",
      "https://eogdata.mines.edu/nighttime_light/annual/v21/2015/VNL_v21_npp_2015_global_vcmslcfg_c202205302300.average_masked.dat.tif.gz",
      "https://eogdata.mines.edu/nighttime_light/annual/v21/2016/VNL_v21_npp_2016_global_vcmslcfg_c202205302300.average_masked.dat.tif.gz",
      "https://eogdata.mines.edu/nighttime_light/annual/v21/2017/VNL_v21_npp_2017_global_vcmslcfg_c202205302300.average_masked.dat.tif.gz",
      "https://eogdata.mines.edu/nighttime_light/annual/v21/2018/VNL_v21_npp_2018_global_vcmslcfg_c202205302300.average_masked.dat.tif.gz",
      "https://eogdata.mines.edu/nighttime_light/annual/v21/2019/VNL_v21_npp_2019_global_vcmslcfg_c202205302300.average_masked.dat.tif.gz",
      "https://eogdata.mines.edu/nighttime_light/annual/v21/2020/VNL_v21_npp_2020_global_vcmslcfg_c202205302300.average_masked.dat.tif.gz",
      "https://eogdata.mines.edu/nighttime_light/annual/v21/2021/VNL_v21_npp_2021_global_vcmslcfg_c202205302300.average_masked.dat.tif.gz"
    )
    
   
    # download the rasters
    for(data_url in urls) {
      output_file <- basename(data_url) # takes out all path up to and including the final separator

    download.file(data_url,
                  destfile = file.path(extract_path,output_file),
                  mode = "wb", 
                  headers = list(Authorization = auth))
    
    }
    
    # unzip to data/02_temp
    # 
    for (data_url in urls){
      output_file <- basename(data_url)
      
      # unzip from .gz so we have a .tif
      
    gunzip(filename = file.path(extract_path,output_file),
           destname = file.path(out_path,str_remove(output_file,".gz")),
           remove = FALSE) # otherwise eremoves input file
    
    }
    
# Satellite Lights, NOAA 1992-2013 ----
    
    # http://hydro.iis.u-tokyo.ac.jp/~yamadai/MERIT_Hydro/
    # takes quite a long time
    
    url <- "https://ngdc.noaa.gov/eog/dmsp/downloadV4composites.html#AVSLCFC"
    
    #Grab filenames from separate URL
    helplinks <- read_html(url) %>% 
      html_elements("a") %>% 
      html_attr('href') # pulls out the things that are the href, not the content of the <a href = ""> thing </a>
    
    #https://hydro.iis.u-tokyo.ac.jp/~yamadai/MERIT_Hydro/distribute/v1.0/dir_n60w180.tar
    
    head(helplinks, n=10)
    
    
    #Keep only filenames relevant for download: the ones 
    # \\. says match the period
    # tar match the letters tar
    # $ says that ".tar" should appear at the string's end
    
    tar_sub_urls <- str_subset(helplinks, "\\.tar$") %>% str_replace_all("\n","") # remove some newlines that got stuck in there
    tgz_sub_urls <- str_subset(helplinks, "\\.tgz$") %>% str_replace_all("\n","") # remove some newlines that got stuck in there
    
    # equivalently
    #helplinks <- helplinks[grepl("\\.tar", helplinks)]
    
    current_sub_urls <- tar_sub_urls[32:34]

    
    #https://ngdc.noaa.gov/eog/data/web_data/v4composites/F101992.v4.tar
    
    download_multiple_files(data_subfolder = "NOAA_satellite-lights",
                            data_raw = data_external_raw,
                            base_url = "https://ngdc.noaa.gov",
                            sub_urls = tar_sub_urls,
                            filename = basename(tar_sub_urls),
                            zip_file = FALSE,
                            pass_protected = FALSE) 
    # get files
    
    dirs_to_unzip <- list.files(file.path(data_external_raw,"NOAA_satellite-lights"))
    
    
    # create output path
    out_path <- file.path(data_external_temp,"NOAA_satellite-lights","stable_avg_vis")
    
    if (!file.exists(out_path)) dir.create(out_path, recursive = TRUE)
    
    # extract the .tif.gz files for stable_lights average visibility into a new folder
    
    for (dir in dirs_to_unzip){
      
      path <- file.path(data_external_raw,"NOAA_satellite-lights",dir)
      
      # get a list of all the files in the .tar folder
      all_files <- untar(path, list = TRUE)
      
      archive::archive_extract(file.path(path),
                               dir = out_path,
                               files = str_subset(all_files,"\\.stable_lights.avg_vis.tif.gz$"))
      
    }
    
    # unzip to get TIF files
    # 
    files_to_unzip <- list.files(out_path)
    
    for (file in files_to_unzip){
      gunzip(filename = file.path(out_path,file),
             remove = TRUE)
    }
    
    
  
# MERIT Hydro ----

  # http://hydro.iis.u-tokyo.ac.jp/~yamadai/MERIT_Hydro/
  
  
  url <- "http://hydro.iis.u-tokyo.ac.jp/~yamadai/MERIT_Hydro/"
  
  #Grab filenames from separate URL
  helplinks <- read_html(url) %>% 
    html_nodes("a") %>% 
    html_text(trim = T)
  
  #https://hydro.iis.u-tokyo.ac.jp/~yamadai/MERIT_Hydro/distribute/v1.0/dir_n60w180.tar
  
  head(helplinks, n=10)
  
  
  #Keep only filenames relevant for download: the ones 
  # \\. says match the period
  # tar match the letters tar
  # $ says that ".tar" should appear at the string's end

  helplinks <- str_subset(helplinks, "\\.tar$")
  
  # equivalently
  #helplinks <- helplinks[grepl("\\.tar", helplinks)]
  
  helplinks[1:5]
  # [1] "dir_n60w180.tar" "dir_n60w150.tar" "dir_n60w120.tar" "dir_n60w090.tar"
  # [5] "dir_n60w060.tar"  
  # 
  
  
  download_multiple_files(data_subfolder = "MERIT_Hydro",
                          data_raw = data_external_raw,
                          base_url = "https://hydro.iis.u-tokyo.ac.jp/~yamadai/MERIT_Hydro/distribute/v1.0",
                          sub_urls = helplinks,
                          filename = helplinks,
                          zip_file = FALSE,
                          pass_protected = TRUE,
                          username = "hydrography",
                          password = "rivernetwork") 
  

  

# GLOW global long term river width ----
  
  # Doesn't distinguish canals but does have time variation
  
  # DOI
  # https://zenodo.org/record/6425657
  
  # https://zenodo.org/record/6425657/files/Cross_section.zip?download=1
  
  
  filenames <- c("Cross_section","Environmental_parameter_global","Width")
  sub_urls  <- paste0(filenames,".zip?download=1")
  
  download_multiple_files(data_subfolder = "GLOW_global-long-term-river-width",
                          data_raw = data_external_raw,
                          base_url = "https://zenodo.org/record/6425657/files",
                          sub_urls = sub_urls,
                          filename = filenames,
                          zip_file = FALSE) # zip files are really large, unzip separately with decompress_file a better function for files >4GB
  
  # Width is 8 G big and needs to be manually unzipped using like 7zip
  # 
  for (filename in filenames){
  decompress_file(directory = file.path(data_external_raw,"GLOW_global-long-term-river-width"),
                  file = paste0(filename,".zip"))
  }
  

  

# ERA 5 ----
  
  # 
  
  # 1) register for an account at https://cds.climate.copernicus.eu/#!/home
  # 2) once logged in, copy from this URL https://cds.climate.copernicus.eu/api-how-to
  # the code which is two lines consisting of
  # url: YOUR_URL_HERE
  # key: YOUR_KEY_HERE
  # into a .txt file  
  # save file with "File name" as ".cdsapirc" and "Save as Type" to "All Files"
  # into (most likely) C:\Users\Username folder
  # otherwise your %USERPROFILE$\.cdsapirc file
  
  # https://confluence.ecmwf.int/display/CKB/How+to+install+and+use+CDS+API+on+Windows
  #   # (from https://confluence.ecmwf.int/pages/viewpage.action?pageId=139068264 )

  # 3) go to your request for Copernicus, e.g. 
  # https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=form
  # use NETCdf since that's easier to bring into R with metadata
  # and at the bottom click "Show API request" and copy that code

  # 4) paste that API request code into a .py file (or a .txt which is saved as e.g. "import_era5_shortname.py" and in files "All File Types")
  # if using more than one variable, rename the 'download.nc' to make clearer which name it is
  # alternatively
  # use https://confluence.ecmwf.int/display/CUSF/Download+CDS+ERA5+data+using+R
  
  library(reticulate)
  #create new environment
  # install latest python version
  reticulate::install_python()
  
  virtualenv_create("r-reticulate") # create a virtual environment
  virtualenv_install("r-reticulate", packages = "cdsapi") # install the CDS API package into this virtual environment
  
  path <- file.path(data_external_raw,"ERA_5")
  
  if (!file.exists(path)) dir.create(path, recursive = TRUE)
    
  os <- import("os")
  os$getcwd() # get current directory
  os$chdir(path) # change current directory (so that a file downloaded will go there)
  
  # import_era5_precip
  # import_era5_surface_temperature
  
  py_path <- file.path(code_download,"import_era5_surface_temperature.py")
  
  py_run_file(py_path)
  
  py_path <- file.path(code_download,"import_era5_precip.py")
  
  py_run_file(py_path)
  
  
# # GADM Country Shapefiles ----
  
  # lots and lots of data, takes quite some time to run
  
  # https://gadm.org/download_world.html
  
  # https://geodata.ucdavis.edu/gadm/gadm4.1/gadm_410-gpkg.zip
  # https://geodata.ucdavis.edu/gadm/gadm4.1/gadm_410-gdb.zip
  # https://geodata.ucdavis.edu/gadm/gadm4.1/gadm_410-levels.zip
  
  # the geopackage is the standard format
  
  filenames <- c("gadm_410-gpkg","gadm_410-gdb","gadm_410-levels")
  
  sub_urls <- paste0(filenames,".zip")
  
  
  
  download_multiple_files(data_subfolder = "GADM",
                          data_raw = data_external_raw,
                          base_url = "https://geodata.ucdavis.edu/gadm/gadm4.1",
                          sub_urls = sub_urls,
                          filename = filenames,
                          zip_file = TRUE,
                          pass_protected = FALSE)
  

# GDAT Global Dam Tracker ----
  

  #   https://zenodo.org/records/7616852/files/GDAT_data_v1.zip?download=1

  
  filenames <- c("GDAT_data_v1")
  sub_urls <- paste0(filenames,".zip?download=1")
  
  
  download_multiple_files(data_subfolder = "GDAT_global-dam-tracker",
                          data_raw = data_external_raw,
                          base_url = "https://zenodo.org/records/7616852/files",
                          sub_urls = sub_urls,
                          filename = filenames,
                          zip_file = TRUE)
  
  
# ADHI African Database of Hydrometric Indices
# currently just download from here:
# https://dataverse.ird.fr/dataset.xhtml?persistentId=doi:10.23708/LXGXQ9
# need to get clicking through to okay the terms of download
