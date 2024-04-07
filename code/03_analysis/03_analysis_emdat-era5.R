# _______________________________#
# Environment
# Analysis 03 Emdat and ERA5
# 
# Stallman
# Started: 2023-10-05
# Last edited: 2023-10-07
# Edits made: added additional regressions and more description
#________________________________#


# Startup

# uncomment the three lines below if you're running file-by-file 
#rm(list = ls())
# 

# SET THE BELOW TO BE WHATEVER YOUR HOME_FOLDER IS
# ONCE YOU'VE FINISHED CODING YOU CAN COMMENT OUT THE HOME_FOLDER
# AND JUST SET IT IN THE 00_master_run_of_show
home_folder <- file.path("P:","Projects","environment")
source(file.path(home_folder,"code","00_startup_master.R"))

# startup -------------
## bring in the packages, folders, paths ----

if (!require("AER")) install.packages("AER")
if (!require("stargazer")) install.packages("stargazer")
if (!require("modelsummary")) install.packages("modelsummary")
if (!require("kableExtra")) install.packages("kableExtra")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("fixest")) install.packages("fixest")

library(kableExtra) # use with modelsummary to output tables to latex
library(ggrepel) # repel overlapping text labels away from each other in a plot
library(AER) # for outputting nicely
library(fixest) # for fixed effects estimation regressions
# You can also use the package plm for panel linear models, but I was having a hard
# time getting the correct standard errors to output to latex with that, plus fixest is faster
# and the syntax seems a little cleaner to read out

library(stargazer) # outputting to latex. less customizable than modelsummary but the 
# help documentation is easier to use
library(modelsummary) # more ability to customize to output to latex. use with kableExtra to output tables
# to console, latex, Rmarkdown, html etc.

## bring in the data ----

data <- readRDS(file = file.path(data_external_clean,"merged",paste0("GDIS_emdat_era5.rds"))) %>%
        ungroup() %>%
        mutate(is_disaster_all = ifelse(!is.na(is_disaster), yes = is_disaster, no = 0),
               length_disaster_months_all = ifelse(!is.na(length_disaster_months),
                                               yes = length_disaster_months,
                                               no  = 0),
               deaths_dummy_all  = ifelse(!is.na(deaths_dummy),
                                      yes = deaths_dummy,
                                      no  = 0))


# see the varnames we have here
names(data)

# Summary Statistics ----
## Select data to summarize ----
# NOTE 
# if you use a gcb_ variable, you may run into problems with logs! Why is this?
# you can still use the outcome, but you might not be able to run all the regressions
# If you can use it without error, 
# you might want to look in the 02_clean_gcb to see if there's anything weird about 
# the variables in there.

data_to_summarize <- data %>% select(precip,
                                     lr_monthly_avg_precip,
                                     lr_monthly_sd_precip,
                                     monthly_zscore_precip,
                                     current_annual_avg_precip,
                                     current_annual_sd_precip,
                                     annual_zscore_precip,
                                     lr_mean_precip,
                                     lr_sd_precip,
                                     zscore_precip,
                                     sd_deviation_precip,
                                     length_disaster_months,
                                     is_disaster_all,
                                     deaths_dummy,
                                     length_disaster_months_all,
                                     deaths_dummy_all
                                     ) %>% as.data.frame()

## FIND UNITS FOR GDP PER CAPITA IN THE 02_clean_merge file
var_labels <- c("Precipitation (million m)",
                "Long-run monthly average precip",
                "Long-run monthly sd precip",
                "Monthly precip zscore",
                "Current Annual Avg. Precip",
                "Current annual sd precip",
                "Annual precip zscore",
                "Long-run mean precip",
                "Long-run precip SD",
                "Precip z-score",
                "SD Deviation (annual SD - LR SD)",
                "Disaster duration | is disaster (months) ",
                "Is Disaster (1 if in EM-DAT)",
                "Deaths Dummy (1 if deaths occurred) | is disaster",
                "Duration of disaster (months)(unconditional)",
                "Deaths Dummy (unconditional)"
                )

## generate summary statistics table ----

# https://www.jakeruss.com/cheatsheets/stargazer/#the-default-summary-statistics-table

# you can also use datasummary() from the modelsummary package if you want more customizability

stargazer(data_to_summarize, 
          type = "text",
          style = "qje",
          summary = TRUE,
          covariate.labels = var_labels,
          summary.stat = c("n","min","mean","median","max","sd"),
          digits = 2 # round to 2 digits
)

#out_path <- file.path(output_tables,"le_summary_stats.tex")
out_path <- file.path(output_tables,"emdat_gdis_summary_stats.tex")

# output to latex
stargazer(data_to_summarize, 
          type = "latex",
          style = "qje", # also has aer style
          out = out_path,
          summary = TRUE,
          covariate.labels = var_labels,
          summary.stat = c("n","min","mean","median","max","sd"),
          digits = 2, # round to 2 digits
          title = "Summary Statistics",
          label = "tab:emdat_sumstats", # this label now redundant with float = FALSE
          float = FALSE # do this so I can use threeparttable in latex
          # and have very pretty notes
          # this removes the exterior "table" environment
)


#dim(data)

# https://www.geo.fu-berlin.de/en/v/soga-r/Basics-of-statistics/Linear-Regression/Polynomial-Regression/Polynomial-Regression---An-example/index.html
# note there are two ways of fitting polynomial regression in R: with poly(predictor, degree = k) or with I(predictor^k).
# The difference is that poly() uses orthogonal polynomials, so that you can better see which polynomial degree is helping
# add explanatory value, but you lose a straightforward interpretation of the beta regression coefficients
# I() allows you to read off the betas, but makes it harder to assess which are significant powers since you'll run into issues
# of multicollinearity

# see here for a description of nonlinear regression functions:
# https://www.econometrics-with-r.org/8.1-a-general-strategy-for-modelling-nonlinear-regression-functions.html



# subset the data ----

year_a <- 1970
year_b <- 2018

data_year_a  <- data %>% filter(year(date) == year_a)
data_year_b  <- data %>% filter(year(date) == year_b)

## set up regression formulas ----

# Regression Equation Function ----
# creating regression equations was really formulaic, if we want we can make a function
# that does it for us

#' @param outcome_var a string vector with the outcome variable name
#' @param regressor_vars a character vector with all the regressors (not fixed effects) variable names
#' @param fe_vars if fixed effects desired, a character vector of the variable names to take fixed effects of. 
#' defaults to null

reg_equation <- function(outcome_var = "lge_15",
                         regressor_vars = c("gdp_pc","tfr"),
                         fe_vars        = NULL){
  
  reg_string <- paste(outcome_var, paste(regressor_vars, collapse = " + "), sep = " ~ ")
  
  if (!is.null(fe_vars)){ 
    reg_form <- paste(reg_string,paste(fe_vars,collapse = " + "), sep = "|") %>% as.formula()
  } else
    
    reg_form   <- reg_string %>% as.formula()
  
  return(reg_form)
  
  
}

# show an example of it
reg_1_form <- reg_equation(outcome_var = "deaths_dummy_all",
                                 regressor_vars = c("precip","sd_deviation_precip","tfr","I(gdp_pc)^2"),
                                 fe_vars     = c("iso3c"))

reg_form_example

# and without FEs
reg_form_2 <- reg_equation(outcome_var = "le_15",
                           regressor_vars = c("gdp_pc","tfr"))


## Set Covariate Labels ----
# this is the variable labels you want for eqs 1 to 3 (i.e. Table 2)
# change this so that the variables that show up are the same order
# as they appear in reg_1_vars reg_2_vars then reg_3_vars

# you'll want to manually examine the output of lm() to see that
# you've labeled everything correctly in the right order

cov_labels_1_to_3 <- c("Intercept","GDP per capita","Log GDP per capita","TFR", "GDP (unit) per (unit)")

# variable labels you want for Eqs 4 and 5 (i.e. Table 3)
cov_labels_4_to_5 <- c("Intercept","GDP (units) per (units) capita","(GDP(units) per (units))$$^2$$","(GDP (units) per (units))$$^3$$")



## output using modelsummary ----

  # Change this title and notes to fit what you need
  title_crosssection <- "Life Expectancy Cross-Section Comparisons, 1960 and 2019 \\label{tab:hw03crosssection}"
  my_notes <- "Robust standard errors given in parentheses. Population and life expectancy are obtained from \\\\citet{undesaWorldPopulationProspects2022}. Gross domestic product (GDP) is (what units) from \\\\citet{feenstraNextGenerationPenn2015}."

# https://modelsummary.com/articles/modelsummary.html
  
  options(modelsummary_format_numeric_latex = "plain") # there was a "\num{}# argument wrapping around the latex tables
  options(modelsummary_factory_html = 'kableExtra')

# output to viewer

models <- list(
  "(1)" = lm(reg_1_form, data = data_year_a),
  "(2)" = lm(reg_1_form, data = data_year_b),
  
  
  "(3)" = lm(reg_2_form, data = data_year_a),
  "(4)" = lm(reg_2_form, data = data_year_b),
  
  "(5)" = lm(reg_3_form, data = data_year_a),
  "(6)" = lm(reg_3_form, data = data_year_b)
)

# get the LM output in the console so we can check that the covariate labels are correct
reg_1_form
lm(reg_1_form, data = data_year_a)
lm(reg_1_form, data = data_year_b)
reg_2_form
lm(reg_2_form, data = data_year_a)
lm(reg_2_form, data = data_year_b)
reg_3_form
lm(reg_3_form, data = data_year_a)
lm(reg_3_form, data = data_year_b)

# check that we're getting standard errors right
# this should be column (6)
coeftest(lm(reg_1_form, data = data_year_a),   vcov. = vcovHC, type = "HC1")
coeftest(lm(reg_1_form, data = data_year_b),   vcov. = vcovHC, type = "HC1")
coeftest(lm(reg_2_form, data = data_year_a),   vcov. = vcovHC, type = "HC1")
coeftest(lm(reg_2_form, data = data_year_b),   vcov. = vcovHC, type = "HC1")
coeftest(lm(reg_3_form, data = data_year_a),   vcov. = vcovHC, type = "HC1")
coeftest(lm(reg_3_form, data = data_year_b),   vcov. = vcovHC, type = "HC1")


coeftest(lm(le_65 ~ gdp_pc, data = data_year_a),   vcov. = vcovHC, type = "HC1")


# it's good practice to put in the mean of the outcome variable
# as a way to look at a glance whether the coefficients are "economically significant"

# another common way to talk about economic significance is to look at
# the coefficients relative to the standard deviation of the outcome variable

# IMPORTANT
# IF YOU ARE GETTING MEAN NA IN YOUR OUTPUT, YOU NEED TO EXAMINE WHY
# It's most likely that your data has log(x) for x<0 which does not exist
# since log(0) -> -infinity
 
# get length of the number of unique regressors
n_total_regressor_vars <- c(reg_1_vars,reg_2_vars,reg_3_vars) %>% unique() %>% length()

rows <- data.frame("term" = c("Mean"),
                   "(1)"  = c(round(mean(data_year_a[[outcome_vars[1]]]),2)),
                   "(2)"  = c(round(mean(data_year_b[[outcome_vars[1]]]),2)),
                   "(3)"  = c(round(mean(data_year_a[[outcome_vars[2]]]),2)),
                   "(4)"  = c(round(mean(data_year_b[[outcome_vars[2]]]),2)),
                   "(5)"  = c(round(mean(data_year_a[[outcome_vars[3]]]),2)),
                   "(6)"  = c(round(mean(data_year_b[[outcome_vars[3]]]),2)))

attr(rows, 'position') <- c(2*n_total_regressor_vars+4) # this should put it right after the number of observations
# there are 2 rows per regressor var, plus the Number of Observations, plus 2 for the intercept, and then put the Num Obs at 
# the next one down


# output to console to check that we're getting what we want
# if you are getting an output of 0.000, examine if you need to adjust the units
# of that regressor. It might be the case that your units are just too small, not that
# you have a true zero



modelsummary(models,
             stars = FALSE,
             vcov = "HC1", # stata's heteroskedasticity robust standard errors
             #statistic = "conf.int",
             coef_rename = cov_labels_1_to_3,
             #output = "latex",
             add_rows = rows,
             title = title_crosssection,
             gof_omit = "AIC|BIC|RMSE|Log.Lik|Std.Errors" # omit several of the goodness of fit stats
)   %>% 
  add_header_above(c(" "=1, "1960" =1, "2019"=1, "1960"=1, "2019"=1, "1960"=1,"2019"=1)) %>% # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns
  add_header_above(c(" "=1, "Birth"=2, "Age 15" = 2, "Age 65"=2))  # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns



## output to latex to put in overleaf ----
# maybe change this to like "hw03_reg_cross_sections.tex"
#out_path <- file.path(output_tables,"le_reg_cross_sections.tex")
out_path <- file.path(output_tables,"hw03_reg_cross_sections.tex")

modelsummary(models,
             stars = FALSE,
             vcov = "HC1", # stata's robust standard errors
             #statistic = "conf.int",
             coef_rename = cov_labels_1_to_3,
             output = "latex",
             add_rows = rows,
             booktabs = T,  # adding booktabs = TRUE allows us to call out specific elements of the latex table and change them
             title = title_crosssection,
             gof_omit = "AIC|BIC|RMSE|Log.Lik|Std.Errors") %>% # omit several of the goodness of fit stats
  kableExtra::footnote(escape = FALSE, # this allows us to use \\\\citet{} and put citations in this end-of-table note
           general = my_notes,
           threeparttable = TRUE,
           general_title = "Notes: "
  )%>%
  add_header_above(c(" "=1, "1960" =1, "2019"=1, "1960"=1, "2019"=1, "1960"=1,"2019"=1)) %>% # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns
  add_header_above(c(" "=1, "Birth"=2, "Age 15" = 2, "Age 65"=2)) %>%  # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns
  kable_styling(latex_options = "HOLD_position") %>% # changes the latex placement to [H] (here), put this table in the latex doc where I say it goes
  save_kable(
    file = out_path,
    format = "latex"
  )


# Analysis: panel regression ----

## set titles and notes ----

  title_paneltab <- "Panel Regression, Life Expectancy \\label{tab:hw03regpanel}"
  notes_paneltab <- "Heteroskedascicity-robust standard errors clustered at the country (for within) and country and year (for twoways) levels given in parentheses. Population and life expectancy data are from \\\\citet{undesaWorldPopulationProspects2022}. Gross domestic product (GDP) is (what units) from \\\\citet{feenstraNextGenerationPenn2015}."
  n_total_regressor_vars <- c(reg_4_vars,reg_5_vars) %>% unique() %>% length()
  
## set formulas ----


  # test it out 
  # feols: fixed effect ordinary least squares, from the fixest package
  reg_5_within_form
  
  fe_ols_5 <- feols(reg_5_within_form,
                    data = data)

# check that it squares away with what we think
  fe_ols_5 
  
  reg_5_twoway_form
  
  fe_ols_6 <- feols(reg_5_twoway_form,
                    data = data)

  # notice this has fixed effects for iso3c and year, but it's clustered at just the
  # country level. What's the interpretation?
  # That the errors are correlated within but not across countries,
  # and that the errors are not correlated within years
  # We'll adjust a little below to cluster at the year and country level just to be paranoid
  # the standard errors will rise a tad

  fe_ols_6

# how do we decide what to cluster? Look at the residuals plot and see what 
# groups there's correlation within. If there's correlation in the residuals according
# to country, then cluster by country

# what to do with the standard errors?
# cluster by group and time?
# https://stackoverflow.com/questions/8389843/double-clustered-standard-errors-for-panel-data


models <- list(
  "(1)" = lm(reg_4_form,
              data = data_year_a),
  "(2)" = lm(reg_4_form,
              data = data_year_b),
  "(3)" = feols(reg_5_within_form,
                data = data),
  "(4)" = feols(reg_5_twoway_form,
              data = data)
)

reg_4_form
lm(reg_4_form,
   data = data_year_a)
lm(reg_4_form,
   data = data_year_b)
reg_5_within_form
feols(reg_5_within_form,
      data = data)
reg_5_twoway_form
feols(reg_5_twoway_form,
      data = data)

# declare some extra rows to add 
rows <- data.frame("term" = c("Mean","Country FE", "Time FE"),
                   "(1)"  = c(round(mean(data_year_a[[outcome_vars[4]]]),2),"N","N"),
                   "(2)"  = c(round(mean(data_year_b[[outcome_vars[4]]]),2),"N","N"),
                   "(3)"  = c(round(mean(data[[outcome_vars[5]]]),2),"Y","N"),
                   "(4)"  = c(round(mean(data[[outcome_vars[5]]]),2),"Y","Y"))

# you'll want to check if this is the right location
attr(rows, 'position') <- c(2*n_total_regressor_vars+3,2*n_total_regressor_vars+4,2*n_total_regressor_vars+5) # this should put it right after the number of observations
# there are 2 rows per regressor var, plus the Number of Observations, and then the Country FE is the next one down


# output to console to make sure we've got the right placement

modelsummary(models,
             stars = FALSE,
             vcov =
             list("HC1","HC1",~iso3c,~iso3c+year),
             # heteroskedasticity robust standard errors, for the first three clustered at the country, 
             # for the second three double clustered at country and time
             coef_rename = cov_labels_4_to_5,
             title = title_paneltab,
             add_rows = rows,
             gof_omit = "AIC|BIC|RMSE|Log.Lik|Std.Errors|Adj.|FE:|F" # omit several of the goodness of fit stats
) %>%
  add_header_above(c(" "=1, "1960"=1, "2019" = 1, "Within"=1,"Twoway"=1))  # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns


## output to latex ----
out_path <- file.path(output_tables,"hw03_reg_panel.tex") # maybe hw03_reg_panel.tex

modelsummary(models,
             stars = FALSE,
             vcov =
               list("HC1","HC1",~iso3c,~iso3c+year),
             # heteroskedasticity robust standard errors, for 1 and 2
             # then robust clustered at the entity, then robust double clustered by year and country
             coef_rename = cov_labels_4_to_5,
             output = "latex",
             booktabs = TRUE,
             title = title_paneltab,
             add_rows = rows,
             gof_omit = "AIC|BIC|RMSE|Log.Lik|Std.Errors|FE:|Adj.|F" # omit several of the goodness of fit stats
) %>% 
  kableExtra::footnote(escape = FALSE, # so that KableExtra knows to just pass through the \
           general= notes_paneltab,
           threeparttable = TRUE,
           general_title = "Notes: "
  )%>%
  add_header_above(c(" "=1, "1960"=1, "2019" = 1, "Within"=1,"Twoway"=1)) %>% # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns
  add_header_above(c(" "=1, "Life Expectancy at Birth"=4)) %>% # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns
  kable_styling(latex_options = "HOLD_position") %>% # changes the latex placement to [H] (here), put this table in the latex doc where I say it goes
  save_kable(
    file = out_path,
    format = "latex"
  )


## output to word ----

# We need a different package to output to Word
# See help here: https://ardata-fr.github.io/flextable-book/

if (!require("flextable")) install.packages("flextable")

library(flextable)

out_path <- file.path(output_tables,"le_reg_panel.docx")

modelsummary(models,
             stars = FALSE,
             vcov =
               list("HC1","HC1",~iso3c,~iso3c+year),
             # heteroskedasticity robust standard errors, for 1 and 2
             # then robust clustered at the entity, then robust double clustered by year and country
             coef_rename = cov_labels_4_to_5,
             output = out_path,
             booktabs = TRUE,
             title = title_paneltab,
             add_rows = rows,
             gof_omit = "AIC|BIC|RMSE|Log.Lik|Std.Errors|FE:|Adj.|F" # omit several of the goodness of fit stats
)


