# _______________________________#
# Environment
# Download Allafrica : Download Allafrica Water News: text
# 
# Stallman
# Started 2023-08-31
# Last edited: 2024-03-03
# Edits: made it more clearly linked to 01_download_allafrica-water_01_headlines
#________________________________#


# to use after "01_download_allafrica-water_headlines-summaries.R
# Startup

rm(list = ls())


# bring in the packages, folders, paths

home_folder <- file.path("P:","Projects","environment")

source(file.path(home_folder,"code","00_startup_master.R"))

type <- "eau" # use "water" for accessing the English version of stuff

# packages ----


if (!require("pacman")) install.packages("pacman")
pacman::p_load(
 openxlsx,
 stringr,
 tidyverse,
 Rselenium,
 rvest,
 netstat,
 lubridate, 
 tictoc
)

# parameters ----
# 

date <- "2002_to_2024-03-02"
category <- "eau"
headlines_filename <- paste0(date,"_",category,"_category_allafrica.xlsx")


# start RSelenium on a new computer

# follow the first answer in this:
# https://stackoverflow.com/questions/45395849/cant-execute-rsdriver-connection-refused/74735571#comment131986973_74735571


 
# 2004: 2156
# filtered: 1798

# 2004 start time: 18:01
# finish "Current time is 2023-09-05 03:07:18.149072"
#[1] "Current time is 2023-09-05 16:08:08.340498"

# packages


# note that the "Terminal" tab can be used to execute terminal commands

## FIRST open "Docker Desktop"
shell('docker pull selenium/standalone-firefox')
shell('docker run -d -p 4445:4444 selenium/standalone-firefox')


# the second answer suggests that RSelenium is looking for a Chrome version and that's why it might be failing
# so set chromever to null and we're good to go

rD <- RSelenium::rsDriver(
  browser = "firefox",
  chromever = NULL,
  port      = free_port(random = TRUE))

remDr <- rD[["client"]]

url <- "https://allafrica.com/commerce/user/manage/"

# open
# navigate to login page and log in
remDr$navigate(url)

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


#years <- c(2011:2021)
year_to_scrape <- 2023

# you need to download Docker and initialize it

#for (year_to_scrape in years){

path <- file.path("E:","data","01_raw","allafrica","water",headlines_filename)
data <- openxlsx::read.xlsx(path, 
                            sheet = "Sheet1") %>%
        mutate(year = str_sub(dates,start = -4)) %>% # extract the last 4 characters from the date string
        filter(year == year_to_scrape) %>%
  #filter(relevant!= "W" & relevant != "Q" & relevant!="N") %>% # remove the "wastewater" and "quality". for 2023 this results in a drop from 460 to 337
  mutate(full_url = paste0("https://allafrica.com",urls),
         title_from_page = headlines,
         text = summaries)


# loop through and get the info
i <- 1 

start_time <- Sys.time()

print(paste0("Current time is ",Sys.time()))

# for testing purposes
#url_test <- data$full_url[1:10]
#for (url in url_test){ 


sleep_vec <- c(8:21,43,23,37,22,33)
# display estimated time
est_time <- round(mean(sleep_vec)*nrow(data)/(60*60),3)

print(paste0("Mean time will be ", round(mean(sleep_vec),3)," seconds, so for ",nrow(data)," records that will be about ",est_time," hours."))

for (url in data$full_url){
 
  if (i%%29==0){
  print(paste0("This is iteration ",i," of ",nrow(data),"."))
  }
  
  # for testing purposes
  #print(paste0("This is iteration ",i," of ",length(url_test),"."))
  
  remDr$navigate(url)
  
  html <- remDr$getPageSource()[[1]]
  
  title <- read_html(html) %>% # parse html
    html_nodes("h2.headline") %>% # get the headline
    html_text() # remove the extra info
  Sys.sleep(sample(x = c(8:21,43,23,37,22,33), size = 1)) # Give page time to load
  
  text <- read_html(html) %>%
    html_nodes("p.story-body-text") %>%
    html_text() %>%
    unlist() %>%
    paste(collapse = " \n\n -**-")
  
  data$title[data$full_url==url] <- ifelse(length(nchar(title)) !=0 , title, 0)
  data$text[data$full_url==url]  <- ifelse(length(nchar(text)) !=0 , text, 0)
  
  i <- i+1
  
}

# deal with long strings ----

# sometimes output doesn't work if the article is over a certain number of characters (32767), 
# it won't input in excel and throws an error and messes with the csv file

# flag which articles were truncated, and truncate at that point

data$too_long <- as.character((nchar(data$text)>32767))
# which indices is it
#which(too_long==TRUE)

data$text <- stringr::str_trunc(data$text,32766)



# for English Version ----

# if (type == "water") {
#   
# if (year_to_scrape == 2006){
#   data <- data[-c(402,1056),]
# }
# 
# if (year_to_scrape == 2016){
#   data <- data[-c(1117),]
# }
# 
# }
# # for French
# 
# if (type == "eau"){
# if (year_to_scrape == 2012) data <- data[-c(1165),]
# if (year_to_scrape == 2013) data <- data[-c(1165),]







allafrica_water <- data %>%
  mutate(relevant = "N",
         relationship = NA,
         location     = NA,
         actor_01     = NA,
         actor_02     = NA,
         impact       = NA,
         impact_count = NA,
         notes        = NA,
         country      = str_extract(title,"^[^\\:]+")) %>%# ^ matches beginning of string, at least one thing before a colon 
  dplyr::select(country,title,text,relevant,relationship,dates,year,full_url,location,actor_01,actor_02,impact,impact_count,notes,too_long)


print(paste0("Current time is ",Sys.time()," and we started at ",start_time))
end_time <- Sys.time()

allafrica_water <- save_rds_csv(data = data,
                                output_path = file.path("E:","data","02_temp","laborious",type),
                                output_filename = paste0("_allafrica_",type,"_text.rds"),
                                remove = FALSE,
                                date = year_to_scrape,
                                csv_vars = names(data),
                                format = "csv")



allafrica_water <- save_rds_csv(data = data,
                                     output_path = file.path("E:","data","02_temp","laborious",type),
                                     output_filename = paste0("_allafrica_",type,"_text.rds"),
                                     remove = FALSE,
                                     date = year_to_scrape,
                                     csv_vars = names(data),
                                     format = "xlsx")




   #} #if you want to loop over years


remDr$close()
# stop the selenium server
rD[["server"]]$stop()
gc(rD)
rm(rD)
