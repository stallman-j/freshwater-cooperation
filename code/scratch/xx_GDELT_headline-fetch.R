# explore GDELT

# http://blog.rolffredheim.com/2013/04/mapping-gdelt-data-in-r-and-some.html

# r data mining 
# https://www.rdatamining.com/examples/other-examples


# Biden v trump coverage tutorial
# recent

# https://rpubs.com/drkblake/1007662


# GDELT headline fetch

# https://rpubs.com/drkblake/1007551

#gdeltproject.org/data/lookups/CAMEO.eventcodes.txt

# there's a video here which shows how to query with google big query

# https://hoffa.medium.com/understanding-the-world-with-sql-the-gdelt-bigquery-video-6070daf9f865


# install packages ----

  # if (!require("tidyverse")) install.packages("tidyverse")
  # if (!require("readr")) install.packages("readr")
  # if (!require("dplyr")) install.packages("dplyr")
  # if (!require("tidytext")) install.packages("tidytext")
    
  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))



  library(tidyverse)
  library(readr) # reading file types eg csv
  library(dplyr)
  library(tidytext)
  library(stringr) # Part of the tidyverse package, good for strings ops

# search parameters ----

  query <- "'Joe Biden' " #Enter search term(s)
  startdate <- "20230102" #Enter preferred start date
  enddate <- "20230219" #Enter preferred end date
 
   sources <- c("washingtonpost.com",
               "nytimes.com") #Enter sources to search

#Generating a sequence of dates ----
   
  # get a list of dates b/w start and end date specified above, in YYYYMMDD
   
  startdate2 <- as.Date(startdate,"%Y%m%d")
  enddate2 <- as.Date(enddate,"%Y%m%d")
  dates <- seq(as.Date(startdate2), as.Date(enddate2), "days")
  dates <- format(dates, "%Y%m%d")
  
#Estimating run time for query ----
  # each fetch about 2.5 seconds; 2 seconds from a pause in code to avoid GDELT's API rate limit
  # and number of fetches = # sources x # days in date range, so run times can lengthen pretty quickly
  
  Minutes <- round((length(sources)*(length(dates)*2.5/60)), digits = 1)
  Hours <- round((length(sources)*(length(dates)*2.5/3600)), digits = 1)
  print(paste0("Est. runtime is ",Minutes," minutes, i.e. ",Hours," hours."))

#Creating the dataframe ----
  
  # code displays info about each fetch as it cycles through the loops
  # however service disruption can really wreck it
  
Headlines = data.frame(
  Source = character(),
  URL = character(),
  MobileURL = character(),
  Date = character(),
  Title = character(),
  stringsAsFactors = FALSE)

  
  
# loop through the queries ----
  
for (thissource in sources)
{  
  for (thisdate in dates)
  {
    URL_p1 <- "https://api.gdeltproject.org/api/v2/doc/doc?query="
    URL_p2 <- " domainis:"
    URL_p3 <- "&mode=artlist&maxrecords=250&sort=datedesc&startdatetime="
    URL_p4 <- "000000&enddatetime="
    URL_p5 <- "235959&format=CSV"
    URL_raw <- paste0(URL_p1,
                      query,
                      URL_p2,
                      thissource,
                      URL_p3,
                      thisdate,
                      URL_p4,
                      thisdate,
                      URL_p5)
    URL_encoded <- URLencode(URL_raw)
    print("Getting data for")
    print(paste0(thisdate," ",thissource))
    print(URL_encoded)
    Thisfetch <- read_csv(URL_encoded, show_col_types = FALSE)
    Thisfetch$Source <- thissource
    print(paste0(nrow(Thisfetch)," rows"))
    Headlines <- rbind(Headlines,Thisfetch)
    Sys.sleep(2)
  }
}
  
#Deduplicate, check, and export data ----
  
  Headlines <- Headlines[!duplicated(Headlines$URL),]
  
  # give the number of hits by source
  CountsBySource <- Headlines %>% 
    group_by(Source) %>% 
    summarize(HeadlineCount = n())
  
  View(CountsBySource)
  
  filename <- paste0(query,startdate,"to",enddate,".csv")
  
  write_excel_csv(Headlines,
                  file = file.path(data_raw,"GDELT",filename))

# clean old query ----
  rm(Thisfetch,
     dates,
     enddate,
     enddate2,
     filename,
     query,
     sources,
     startdate,
     startdate2,
     thisdate,
     thissource,
     URL_encoded,
     URL_p1,
     URL_p2,
     URL_p3,
     URL_p4,
     URL_p5,
     URL_raw)

#View raw data frame ----
  View(Headlines)


#Headline word counts ----
  WordCounts <- Headlines %>% 
    unnest_tokens(word,Title) %>% 
    count(word, sort = TRUE)

# Deleting standard stop words
  data("stop_words")
  WordCounts <- WordCounts %>%
    anti_join(stop_words)

# Deleting custom stop words
  my_stopwords <- tibble(word = c("and",
                                "the",
                                "etc."))
  
WordCounts <- WordCounts %>% 
  anti_join(my_stopwords)

rm(stop_words,
   my_stopwords)

#Viewing word counts
  View(WordCounts)

#Headline coding ----
  searchterms <- "trump|maga"
  
  Headlines$Trump <- ifelse(grepl(searchterms,
                                  Headlines$Title,
                                  ignore.case = TRUE),1,0)
  
  searchterms <- "house|gop|republican|mccarthy"
  
  Headlines$Republicans <- ifelse(grepl(searchterms,
                                        Headlines$Title,
                                        ignore.case = TRUE),1,0)
  
  searchterms <- "classified documents"
  
  Headlines$Documents <- ifelse(grepl(searchterms,
                                      Headlines$Title,
                                      ignore.case = TRUE),1,0)
  searchterms <- "debt|ceiling"
  
  Headlines$Debt <- ifelse(grepl(searchterms,
                                 Headlines$Title,
                                 ignore.case = TRUE),1,0)
  searchterms <- "Jan. 6|capitol riot"
  
  Headlines$Jan6 <- ifelse(grepl(searchterms,
                                 Headlines$Title,
                                 ignore.case = TRUE),1,0)
  searchterms <- "China|Chinese"
  
  Headlines$China <- ifelse(grepl(searchterms,
                                  Headlines$Title,
                                  ignore.case = TRUE),1,0)
  searchterms <- "Ukraine|Ukranian|Russia"
  
  Headlines$Ukraine <- ifelse(grepl(searchterms,
                                    Headlines$Title,
                                    ignore.case = TRUE),1,0)
  searchterms <- "border|migra"
  
  
  Headlines$Immigration <- ifelse(grepl(searchterms,
                                        Headlines$Title,
                                        ignore.case = TRUE),1,0)

# Categorizing by day and also by week ----
  
  if (!require("lubridate")) install.packages("lubridate")
  library(lubridate)
  
  Headlines$Day <- round_date(Headlines$Date,
                              unit = "day")
  Headlines$WeekOf <- round_date(Headlines$Date,
                                 unit = "week",
                                 week_start = getOption("lubridate.week.start",1))
#Aggregating by week
AggByWeek <- Headlines %>%
    group_by(WeekOf) %>% 
    summarize(Trump = sum(Trump, na.rm=TRUE),
              Republicans = sum(Republicans, na.rm=TRUE),
              Documents = sum(Documents, na.rm=TRUE),
              Debt = sum(Debt, na.rm=TRUE),
              Jan6 = sum(Jan6, na.rm=TRUE),
              China = sum(China, na.rm=TRUE),
              Ukraine = sum(Ukraine, na.rm=TRUE),
              Immigration = sum(Immigration, na.rm=TRUE),
              ArticleCount = n())
#Aggregating by day
  AggByDay <- Headlines %>%
    group_by(Day) %>% 
    summarize(Trump = sum(Trump, na.rm=TRUE),
              Republicans = sum(Republicans, na.rm=TRUE),
              Documents = sum(Documents, na.rm=TRUE),
              Debt = sum(Debt, na.rm=TRUE),
              Jan6 = sum(Jan6, na.rm=TRUE),
              China = sum(China, na.rm=TRUE),
              Ukraine = sum(Ukraine, na.rm=TRUE),
              Immigration = sum(Immigration, na.rm=TRUE),
              ArticleCount = n())


#Graphing by week and by day ----
  
if (!require("plotly")) install.packages("plotly")
library(plotly)


# Color palettes: https://coolors.co/palettes/trending
# Graphing by week
  fig <- plot_ly(AggByWeek, x = ~WeekOf, y = ~Trump, 
                 name = 'Trump', type = 'scatter', 
                 mode = 'none', stackgroup = 'one', 
                 fillcolor = '#F94144')
  fig <- fig %>% add_trace(y = ~Republicans, 
                           name = 'Republicans', 
                           fillcolor = '#F3722C')
  fig <- fig %>% add_trace(y = ~Documents, name = 'Documents', 
                           fillcolor = '#F8961E')
  fig <- fig %>% add_trace(y = ~Debt, name = 'Debt', 
                           fillcolor = '#F9844A')
  fig <- fig %>% add_trace(y = ~Jan6, name = 'Jan. 6', 
                           fillcolor = '#90BE6D')
  fig <- fig %>% add_trace(y = ~China, name = 'China', 
                           fillcolor = '#43AA8B')
  fig <- fig %>% add_trace(y = ~Ukraine, name = 'Ukraine', 
                           fillcolor = '#4D908E')
  fig <- fig %>% add_trace(y = ~Immigration, name = 'Immigration', 
                           fillcolor = '#577590')
  fig <- fig %>% layout(title = 'Headline counts, by topic and week',
                        xaxis = list(title = "Week of",
                                     showgrid = FALSE),
                        yaxis = list(title = "Count",
                                     showgrid = TRUE))

fig

# Graphing by day
  fig <- plot_ly(AggByDay, x = ~Day, y = ~Trump, 
                 name = 'Trump', type = 'scatter', 
                 mode = 'none', stackgroup = 'one', 
                 fillcolor = '#F94144')
  fig <- fig %>% add_trace(y = ~Republicans, 
                           name = 'Republicans', 
                           fillcolor = '#F3722C')
  fig <- fig %>% add_trace(y = ~Documents, name = 'Documents', 
                           fillcolor = '#F8961E')
  fig <- fig %>% add_trace(y = ~Debt, name = 'Debt', 
                           fillcolor = '#F9844A')
  fig <- fig %>% add_trace(y = ~Jan6, name = 'Jan. 6', 
                           fillcolor = '#90BE6D')
  fig <- fig %>% add_trace(y = ~China, name = 'China', 
                           fillcolor = '#43AA8B')
  fig <- fig %>% add_trace(y = ~Ukraine, name = 'Ukraine', 
                           fillcolor = '#4D908E')
  fig <- fig %>% add_trace(y = ~Immigration, name = 'Immigration', 
                           fillcolor = '#577590')
  fig <- fig %>% layout(title = 'Headline counts, by topic and week',
                        xaxis = list(title = "Week of",
                                     showgrid = FALSE),
                        yaxis = list(title = "Count",
                                     showgrid = TRUE))

fig
