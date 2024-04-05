# _______________________________#
# Environment
# Clean 02: DHS Child Mortality Testing
# 
# Stallman
# Started: 2023-10-23
# Last edited: 
#________________________________#


# https://dhsprogram.com/data/Guide-to-DHS-Statistics/Adult_Mortality_Rates.htm

# Startup

  rm(list = ls())

  # Only run this the first time, otherwise start from "cleaning!"
  
# bring in the packages, folders, paths ----
  
  code_folder <- file.path("P:","Projects","environment","code")
  source(file.path(code_folder,"00_startup_master.R"))
  
  if (!require(naniar)) install.packages("naniar")
  if (!require(sjlabelled)) install.packages("sjlabelled")
  if (!require(expss)) install.packages("expss")
  if (!require(xlsx)) install.packages("xlsx")
  if (!require(DHS.rates)) install.packages("DHS.rates")
  if (!require(stringr)) install.packages("stringr")
  
  if (!require(haven)) install.packages("haven")
  
  library(naniar)
  library(sjlabelled)
  library(expss)
  library(xlsx)
  library(DHS.rates)
  library(stringr)
  library(haven)
  
  
# DHS Data ----
   
   # Walkthrough 
   # https://docs.ropensci.org/rdhs/articles/introduction.html
   
  
  path <- file.path(data_external_raw,"DHS","UGBR7BSV.rds")
  
  BRdata <- readRDS(path)
  
# copied from 
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap08_CM/CM_CHILD.R
  
  # -----------------------------------------------------------------------------#
  # # Indicators created in this file:
  # NNMR		"Neonatal Mortality Rate"
  # PNNMR		"Post-neonatal Mortality Rate"
  # IMR			"Infant Mortality Rate"
  # CMR			"Child Mortality Rate"
  # U5MR		"Under-5 Mortality Rate"
  # -----------------------------------------------------------------------------#
  #
  
  # important indicators:
  # b3: date of birth
  # b7: age at death
  
  BRdata <- BRdata %>%
    mutate(child_sex = b4) %>%
    mutate(child_sex = set_label(child_sex, label = "Sex of child"))  %>%
    mutate(months_age = b3-v011) %>%
    mutate(mo_age_at_birth =
             case_when(
               months_age < 20*12   ~ 1 ,
               months_age >= 20*12 & months_age < 30*12 ~ 2,
               months_age >= 30*12 & months_age < 40*12 ~ 3,
               months_age >= 40*12 & months_age < 50*12 ~ 4)) %>%
    mutate(mo_age_at_birth = factor(mo_age_at_birth, levels = c(1,2,3,4), labels = c("Mother's age at birth < 20", "Mother's age at birth 20-29", "Mother's age at birth 30-39","Mother's age at birth 40-49"))) %>%
    mutate(mo_age_at_birth = set_label(mo_age_at_birth, label = "Mother's age at birth")) %>% # from sjlabelled
    # set_label: adds varlabels as an attribute named "label" to the variable x
    mutate(birth_order =
             case_when(
               bord == 1  ~ 1,
               bord >= 2 & bord <= 3 ~ 2,
               bord >= 4 & bord <= 6 ~ 3,
               bord >= 7  ~ 4,
               bord == NA ~ 99)) %>%
    replace_with_na(replace = list(birth_order = c(99))) %>%
    mutate(birth_order = factor(birth_order, levels = c(1,2,3,4), labels = c("Birth order:1", "Birth order:2-3", "Birth order:4-6","Birth order:7+"))) %>%
    mutate(birth_order = set_label(birth_order, label = "Birth order"))  %>%
    mutate(prev_bint =
             case_when(
               b11 <= 23 ~ 1,
               b11 >= 24 & b11 <= 35 ~ 2,
               b11 >= 36 & b11 <= 47 ~ 3,
               b11 >= 48 ~ 4)) %>%
    mutate(prev_bint = set_label(prev_bint, label = "Preceding birth interval"))   %>%
    mutate(birth_size =
             case_when(
               m18 >= 4 & m18 <= 5 ~ 1,
               m18 <= 3 ~ 2,
               m18 > 5 ~ 99)) %>%
    mutate(birth_size = set_label(birth_size, label = "Birth size"))
  
  BRdata[["prev_bint"]] <- ifelse(is.na(BRdata[["prev_bint"]]), 999, BRdata[["prev_bint"]])
  BRdata[["birth_size"]] <- ifelse(is.na(BRdata[["birth_size"]]), 999, BRdata[["birth_size"]])
  
  BRdata <- BRdata %>%
    mutate(prev_bint = factor(prev_bint, levels = c(1,2,3,4,999), labels = c("Previous birth interval <2 years", "Previous birth interval 2 years", "Previous birth interval 3 years","Previous birth interval 4+ years", "missing"))) %>%
    mutate(birth_size = factor(birth_size, levels = c(1,2,99,999), labels = c("Birth size: Small/very small","Birth size: Average or larger", "Birth size: Don't know/missing", "missing" )))
  
  
  ##################################################################################
  # MORTALITY RATES ################################################################
  ##################################################################################
  
  BRdata_CMORT <- (BRdata[, c("v001","v021", "v022","v024", "v025", "v005", "v008","v011", 
                              "b3", "b7", "v106", "v190", "child_sex", "mo_age_at_birth", "birth_order", "prev_bint","birth_size")])
  
  # NNMR, PNNMR, IMR, CMR & U5MR
  # TABLES 8.1, 8.2 and 8.3
  
  # Different datasets for period ends: 5-9 and 10-14 (0-4 years before survey data, 5-9 yrs before, 10-14 yrs before)
  
  

  BRdata_CMORT1 <- BRdata_CMORT
  BRdata_CMORT2 <- BRdata_CMORT
  BRdata_CMORT1$v008 <- BRdata_CMORT$v008 - 12 * (5)
  BRdata_CMORT2$v008 <- BRdata_CMORT$v008 - 12 * (10)
  
  resn1 <- as.data.frame(chmort(BRdata_CMORT))
  resn1$period <- "0-4"
  
  resn2 <- as.data.frame(chmort(BRdata_CMORT1))
  resn2$period <- "5-9"
  resn3 <- as.data.frame(chmort(BRdata_CMORT2))
  resn3$period <- "10-14"
  
  # calculates child mortality
  ?chmort
  
  # resc <- rbind(
  #                             chmort(BRdata_CMORT, Class="v025",Period = 120),
  #                             #chmort(BRdata_CMORT, Class="v106",Period = 120),
  #                             chmort(BRdata_CMORT, Class="v190",Period = 120),
  #                             chmort(BRdata_CMORT, Class="child_sex",Period = 120))
  #                             #chmort(BRdata_CMORT, Class="mo_age_at_birth",Period = 120),
  #                             #chmort(BRdata_CMORT, Class="birth_order",Period = 120),
  #                             #chmort(BRdata_CMORT, Class="prev_bint",Period = 120),
  #                             #chmort(BRdata_CMORT, Class="birth_size",Period = 120))
  # 
  # resc$period <- "0-9"
  
  
  
  CHMORT <- vector("list", 4)
  CHMORT <- rbindlist(list(resn1,resn2,resn3,resc), fill = TRUE)
  
  CHMORT[["Class"]] <- ifelse(is.na(CHMORT[["Class"]]), "National", CHMORT[["Class"]])
  CHMORT <- CHMORT[CHMORT[["Class"]]!="missing",] 
  
  rownames(CHMORT) <- paste(seq(1:nrow(CHMORT)) , rep(row.names(resn1),times = nrow(CHMORT)/5))
  
  path <- file.path(output_tables,"DHS","scratch")
  
    if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  write.xlsx(CHMORT, 
             file = file.path(output_tables,"DHS","scratch","Tables_child_mort.xlsx"), 
             sheetName = "CMR",
             append=TRUE)
  
  ########################################################################################