# scrape from allafrica 

#https://stackoverflow.com/questions/45395849/cant-execute-rsdriver-connection-refused
#https://www.r-bloggers.com/2021/05/r-selenium/


if (!require("RSelenium")) install.packages("RSelenium")
library(RSelenium)

if (!require("rvest")) install.packages("rvest")
library(rvest)

if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# if (!require("keyring")) install.packages("keyring")
# library(keyring)

#rD <- RSelenium::rsDriver() # This might throw an error

rD <- RSelenium::rsDriver(browser = "firefox",
                          chromever = NULL) # alt: chromever = NULL

remDr <- rD[["client"]]

url <- "https://allafrica.com/commerce/user/manage/"

# open
# navigate to login page
remDr$navigate(url)

webElem1 <- remDr$findElement(using = "xpath", "//input[@name = 'login_username']")
webElem1$sendKeysToElement(list("jillian.stallman@yale.edu"))

# Find 'password' element and send 'saved_pass' and 'enter' keystroke as input
webElem2 <- remDr$findElement(using = "xpath", "//input[@name = 'login_password']")
webElem2$sendKeysToElement(list("brt_DZA0xrd*xyu.yup"))
Sys.sleep(5) # Give page time to load


webElem3 <- remDr$findElement(using = "xpath", "//input[@name = 'login']")
webElem3$clickElement()


# Navigate to desired page and download source
remDr$navigate("http://allafrica.com/stories/201601051011.html")

html <- remDr$getPageSource()[[1]]

title <- read_html(html) %>% # parse html
        html_nodes("h2.headline") %>% # get the headline
        html_text() # remove the extra info

text <- read_html(html) %>%
        html_nodes("p.story-body-text") %>%
        html_text() %>%
        unlist()




names <- html_nodes(site, "h2")

%>% # parse html
          html_nodes("")
titles <- read_html()
Sys.sleep(5) # Give page time to load
html <- remDr$getPageSource()[[1]] %>% read_html()
# Use further rvest commands to extract required data
# ...
# End Selenium Session
remDr$close()







rD$server$stop()

#
remDr$close()
# stop the selenium server
rD[["server"]]$stop()








library(RSelenium)
startServer()
remDr <- remoteDriver(browserName = "chrome")
#remDr <- remoteDriver()

remDr$open()
appURL <- "https://allafrica.com/commerce/user/manage"
remDr$navigate(appURL)

remDr$findElement("id", "id_username")$sendKeysToElement(list("user"))
remDr$findElement("id", "id_password")$sendKeysToElement(list("password", key='enter'))

appURL <- 'https://www.optionslam.com/earnings/stocks/MSFT?page=-1'
remDr$navigate(appURL)