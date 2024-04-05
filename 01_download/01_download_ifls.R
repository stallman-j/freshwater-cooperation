# _______________________________#
# Environment
# download 01: download datasets and extract from their zip files
# 
# Stallman
# Started: 2023-04-13
# Last edited: 
#________________________________#


# Startup

rm(list = ls())

# Only run this the first time, otherwise start from "cleaning!"

# bring in the packages, folders, paths ----

code_folder <- file.path("P:","Projects","environment","code")
source(file.path(code_folder,"00_startup_master.R"))

# Indonesia Family Life Panel Survey  ----


#https://www.rand.org/well-being/social-and-behavioral-policy/data/FLS/IFLS/download.html

#https://sites.rand.org/labor/family/software_and_data/FLS/IFLS/IFLS2/data/stata/hh97dta.zip

## Wave 1 1993 ----
# 
filenames <- c("hh93dta","cf93dta")
sub_urls <- paste0(filenames,".zip")

#https://sites.rand.org/labor/family/software_and_data/FLS/IFLS/IFLS1-RR/data/stata/hh93dta.zip
download_multiple_files(data_subfolder = "IFLS",
                        data_raw = data_raw,
                        base_url = "https://sites.rand.org/labor/family/software_and_data/FLS/IFLS/IFLS1-RR/data/stata",
                        sub_urls = sub_urls,
                        filename = filenames,
                        zip_file = TRUE)


## Wave 2 1997 ----
filenames <- c("hh97dta","cf97dta")
sub_urls <- paste0(filenames,".zip")

#https://sites.rand.org/labor/family/software_and_data/FLS/IFLS/IFLS3/data/stata/hh00_all_dta.zip

download_multiple_files(data_subfolder = "IFLS",
                        data_raw = data_raw,
                        base_url = "https://sites.rand.org/labor/family/software_and_data/FLS/IFLS/IFLS2/data/stata",
                        sub_urls = sub_urls,
                        filename = filenames,
                        zip_file = TRUE)

## Wave 3 2000 ----
filenames <- c("hh00_all_dta","cf00_all_dta")
sub_urls <- paste0(filenames,".zip")


download_multiple_files(data_subfolder = "IFLS",
                        data_raw = data_raw,
                        base_url = "https://sites.rand.org/labor/family/software_and_data/FLS/IFLS/IFLS3/data/stata",
                        sub_urls = sub_urls,
                        filename = filenames,
                        zip_file = TRUE)

## Wave 4 2007 ----
# https://sites.rand.org/labor/family/software_and_data/FLS/IFLS/IFLS4/data/stata/hh07_all_dta.zip

filenames <- c("hh07_all_dta","cf07_all_dta")
sub_urls <- paste0(filenames,".zip")


download_multiple_files(data_subfolder = "IFLS",
                        data_raw = data_raw,
                        base_url = "https://sites.rand.org/labor/family/software_and_data/FLS/IFLS/IFLS4/data/stata",
                        sub_urls = sub_urls,
                        filename = filenames,
                        zip_file = TRUE)


## Wave 5 2014 ----
#  https://sites.rand.org/labor/family/software_and_data/FLS/IFLS/IFLS5/hh14_all_dta.zip
filenames <- c("hh14_all_dta","cf14_all_dta")
sub_urls <- paste0(filenames,".zip")


download_multiple_files(data_subfolder = "IFLS",
                        data_raw = data_raw,
                        base_url = "https://sites.rand.org/labor/family/software_and_data/FLS/IFLS/IFLS5",
                        sub_urls = sub_urls,
                        filename = filenames,
                        zip_file = TRUE)

