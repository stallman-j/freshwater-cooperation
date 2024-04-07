# _______________________________#
# Environment
# Download 01: Scrape AllAfrica for Water
# 
# Stallman
# Started 2023-05-29
# Last edited: 
#________________________________#


# See 02_log_gdelt-relevant-themes-and-countries.R for theme and country names
# SEE 00_startup_parameters for the vector include_themes which has all the themes

# filter by a smaller set of countries (those sharing the Zambezi river basin)
# and also merge together the formal agreements by URL

# Startup

rm(list = ls())


# bring in the packages, folders, paths

home_folder <- file.path("P:","Projects","environment")

source(file.path(home_folder,"code","00_startup_master.R"))



# packages
library(RSelenium)

library(rvest)

library(tidyverse)

library(netstat)

driver <- remoteDriver( port = sample(x = netstat::free_ports(), size = 1),
                        browserName = "firefox")
driver$open()
driver$navigate("https://www.google.com/")

# for running R in parallel ignore this part
rD <- RSelenium::rsDriver(browser = "firefox",
                          chromever = NULL,
                          port = sample(x = netstat::free_ports(), size = 1))

remDr <- rD[["client"]]

url <- "https://allafrica.com/commerce/user/manage/"

# open
# navigate to login page
remDr$navigate(url)
Sys.sleep(sample(x = 4:10, size = 1)) # Give page time to load

webElem1 <- remDr$findElement(using = "xpath", "//input[@name = 'login_username']")
webElem1$sendKeysToElement(list("jillian.stallman@yale.edu"))
Sys.sleep(sample(x = 3:9, size = 1)) # Give page time to load

# Find 'password' element and send 'saved_pass' and 'enter' keystroke as input
webElem2 <- remDr$findElement(using = "xpath", "//input[@name = 'login_password']")
webElem2$sendKeysToElement(list("brt_DZA0xrd*xyu.yup"))
Sys.sleep(sample(x = 4:12, size = 1)) # Give page time to load


webElem3 <- remDr$findElement(using = "xpath", "//input[@name = 'login']")
webElem3$clickElement()
Sys.sleep(sample(x = 6:17, size = 1)) # Give page time to load

#print(paste0("Working on ",year))
print(paste0("Current time is ",Sys.time()))
# for 2016 gives 97k obs
# 2017: 62k obs
# 2022: 25.8k obs

