# _______________________________#
# 00 Rework the gdeltr2 package to gdeltr3
# 2023 05 31
# _______________________________#

# based on https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

if (!require("devtools")) install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")

# create package directory ----
# create directory with bare min folders of R packages
setwd(file.path("P:","Projects","coding","r-code","packages"))

# creates the package ----
devtools::create("gdeltr3")

# add a function ----


# add documentation ----
# do this in the DESCRIPTION file in the package folder just created

setwd("./gdeltr3")

# Process documentation ----
# adds the .Rd files to the man directory, and adds NAMESPACE file to the main directory
devtools::document()

# Install ----

setwd("..")
# goes up to the WD above

devtools::install("gdeltr3")

# Make the package a Github Repo ----

# https://stackoverflow.com/questions/21171142/how-to-install-r-package-from-private-repo-using-devtools-install-github

#install_github('jillianstallman/gdeltr3')


#set config
#usethis::use_git_config(user.name = "YourName", user.email = "your@mail.com")

#Go to github page to generate token
#usethis::create_github_token() 

#paste your PAT into pop-up that follows...
#credentials::set_github_pat()

#now remotes::install_github() will work
#remotes::install_github("username/privaterepo")

setwd(file.path("P:","Projects","coding","r-code","packages","gdeltr3"))

use_git()
usethis::use_github(private = TRUE,
                    visibility = c("private"))
