# _______________________________#
# Environment
# Download 01: Download Allafrica Water News
# 
# Stallman
# Started 2023-08-22
# Last edited: 
#________________________________#


# Startup

rm(list = ls())


# bring in the packages, folders, paths

home_folder <- file.path("P:","Projects","environment")

source(file.path(home_folder,"code","00_startup_master.R"))


# start RSelenium on a new computer
  # if this is your first time,
  # follow the first answer in this:
  # https://stackoverflow.com/questions/45395849/cant-execute-rsdriver-connection-refused/74735571#comment131986973_74735571

# you need to download Docker and initialize it



if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  RSelenium, # advanced web scraping
  rvest, # medium web scraping
  tidyverse, # data wrangling
  netstat, # 
  tictoc # timing
)


# note that the "Terminal" tab can be used to execute terminal commands

# the second answer suggests that RSelenium is looking for a Chrome version and that's why it might be failing
# so set chromever to null and we're good to go

rD <- RSelenium::rsDriver(
                      browser = "firefox",
                      chromever = NULL)

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


# navigate to the premium search page
remDr$navigate("https://allafrica.com/search/advanced.html")

# get into the search box
category_search_box <- remDr$findElement(using = "class name", value = "search-field")
category_search_box$clickElement() # click on the "Category" search box
Sys.sleep(sample(x = 5:10, size = 1)) # Give page time to load

# find the input portion of the search
# https://stackoverflow.com/questions/64154120/rselenium-input-value-in-text-box

input <- category_search_box$findChildElement(using = "xpath", value = "input")

# input water into the search box and hit enter
input$sendKeysToElement(list("Water", key = "enter"))

# find the "search" button to submit the search

submit_button <- remDr$findElement(using = "xpath", "//input[@name = 'submit']")
# click on the search button
submit_button$clickElement()
Sys.sleep(sample(x = 10:22, size = 1)) # Give page time to load

## get information ----
# adapted from 
# https://joshuamccrain.com/tutorials/web_scraping_R_selenium.html

# uncomment this if you want just a three-page test version
#remDr$navigate("https://allafrica.com/search/advanced.html?q=cahora+bassa&g=all&c=&c=water&p=&sd=19961017&ed=20230822&st=3&di=1&os=0#results")


# try catch find element
#https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r


i <- 1
next_button <- 0

while ((i>0)&(class(next_button)!="character")){
  Sys.sleep(sample(x = c(2:12,32), size = 1)) # Give page time to load
  
  
  next_button <- tryCatch({
    remDr$findElement(using = "xpath",
                      "//table/tbody/tr/td[3]/ul/li/a/b")
  }, error = function(err){
    print(paste( "This is the error message we got: ",err))
    
  })
  Sys.sleep(sample(x = c(2:3), size = 1)) # Give page time to load
  
  html <- remDr$getPageSource()[[1]]
  
  
  headlines <- read_html(html) %>%
    html_nodes("p.title") %>% # extract the nodes with class "headline"
    html_text()
  
  summaries <- read_html(html) %>%
    html_nodes("p.summary") %>%
    html_text()
  
  dates <- read_html(html) %>%
    html_nodes("span.date") %>%
    html_text()
  
  urls   <- read_html(html) %>%
    html_elements("ul.stories") %>% # restrict to the "stories" to avoid getting ALL links on the page
    html_elements("a") %>% # restrict to elements that have an "a"
    html_attr("href") # just the part with the href
  
  # first round through make the df
  if (i == 1){
  df <- data.frame(headlines = headlines,
                   summaries = summaries,
                   dates     = dates,
                   urls      = urls)
  } else { # later rounds through just rbind
    temp <- data.frame(headlines = headlines,
                      summaries = summaries,
                      dates     = dates,
                      urls      = urls)
    df <- rbind(df,temp)
  }
  
  if (class(next_button)=="character"){
    break
  }  else {
    next_button$clickElement()
    
    
    i <- i+1
  }
}


output_data <- save_rds_csv(data = df,
               output_path = file.path("E:","data","01_raw","allafrica","water"),
               output_filename = "_water_category_allafrica.rds",
               remove = FALSE,
               date = "2002_to_2024-03-02",
               csv_vars = c("headlines","summaries","dates","urls"),
               format = "xlsx")


# do it for French now ----


# navigate to the premium search page
remDr$navigate("https://fr.allafrica.com/search/advanced.html")

# get into the search box
category_search_box <- remDr$findElement(using = "class name", value = "search-field")
category_search_box$clickElement() # click on the "Category" search box
Sys.sleep(sample(x = 5:10, size = 1)) # Give page time to load

# find the input portion of the search
# https://stackoverflow.com/questions/64154120/rselenium-input-value-in-text-box

input <- category_search_box$findChildElement(using = "xpath", value = "input")

# input water into the search box and hit enter
input$sendKeysToElement(list("Eau", key = "enter"))

# find the "search" button to submit the search

submit_button <- remDr$findElement(using = "xpath", "//input[@name = 'submit']")
# click on the search button
submit_button$clickElement()
Sys.sleep(sample(x = 10:22, size = 1)) # Give page time to load

## get information ----
# adapted from 
# https://joshuamccrain.com/tutorials/web_scraping_R_selenium.html

# uncomment this if you want just a three-page test version
#remDr$navigate("https://allafrica.com/search/advanced.html?q=cahora+bassa&g=all&c=&c=water&p=&sd=19961017&ed=20230822&st=3&di=1&os=0#results")


# try catch find element
#https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r


i <- 1
next_button <- 0

while ((i>0)&(class(next_button)!="character")){
  Sys.sleep(sample(x = c(2:12,32), size = 1)) # Give page time to load
  
  
  next_button <- tryCatch({
    remDr$findElement(using = "xpath",
                      "//table/tbody/tr/td[3]/ul/li/a/b")
  }, error = function(err){
    print(paste( "This is the error message we got: ",err))
    
  })
  Sys.sleep(sample(x = c(2:3), size = 1)) # Give page time to load
  
  html <- remDr$getPageSource()[[1]]
  
  
  headlines <- read_html(html) %>%
    html_nodes("p.title") %>% # extract the nodes with class "headline"
    html_text()
  
  summaries <- read_html(html) %>%
    html_nodes("p.summary") %>%
    html_text()
  
  dates <- read_html(html) %>%
    html_nodes("span.date") %>%
    html_text()
  
  urls   <- read_html(html) %>%
    html_elements("ul.stories") %>% # restrict to the "stories" to avoid getting ALL links on the page
    html_elements("a") %>% # restrict to elements that have an "a"
    html_attr("href") # just the part with the href
  
  # first round through make the df
  if (i == 1){
    df <- data.frame(headlines = headlines,
                     summaries = summaries,
                     dates     = dates,
                     urls      = urls)
  } else { # later rounds through just rbind
    temp <- data.frame(headlines = headlines,
                       summaries = summaries,
                       dates     = dates,
                       urls      = urls)
    df <- rbind(df,temp)
  }
  
  if (class(next_button)=="character"){
    break
  }  else {
    next_button$clickElement()
    
    
    i <- i+1
  }
}


output_data <- save_rds_csv(data = df,
                            output_path = file.path("E:","data","01_raw","allafrica","water"),
                            output_filename = "_eau_category_allafrica.rds",
                            remove = FALSE,
                            date = "2002_to_2024-03-02",
                            csv_vars = c("headlines","summaries","dates","urls"),
                            format = "xlsx")


# click on the button

#next_button <- remDr$findElement(using = "link text", "Next ›")

# table tbody tr td[3] ul li a
remDr$close()
# stop the selenium server
rD[["server"]]$stop()
gc(rD)
rm(rD)
