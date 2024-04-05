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

home_folder <- file.path("P:","Projects","environment")
source(file.path(home_folder,"code","00_startup_master.R"))

# packages ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  kableExtra, # use with modelsummary to output tables to latex
  ggrepel, # repel overlapping text labels away from each other in a plot
  AER, #  for outputting nicely
  fixest, # for fixed effects estimation regressions
  # You can also use the package plm for panel linear models, but I was having a hard
  # time getting the correct standard errors to output to latex with that, plus fixest is faster
  # and the syntax seems a little cleaner to read out
  stargazer, # outputting to latex. less customizable than modelsummary but the 
  # help documentation is easier to use
  sf, # vector spatial geometry
  modelsummary # more ability to customize to output to latex. use with kableExtra to output tables
  # to console, latex, Rmarkdown, html etc.
)

# parameters ----

  min_time      <- "1940-01-01"
  max_time      <- "2023-09-01"
  current_file  <- "total_precipitation"
  level         <- 2
  levels        <- 1:5
  country       <- "UGA"
  dhs_br_filename  <- "UGBR7BSV"
  
  # gadm_1 <- readRDS(file = file.path(data_external_clean,"GADM","global","GADM_global_ADM_1.rds"))
  # 
  # countries_df <- gadm_1 %>% st_drop_geometry() %>% select(GID_0,COUNTRY) %>%
  #              group_by(GID_0) %>% filter(row_number()==1)
  #                         
  # 
  # countries_iso3 <- countries_df$GID_0
  
  #rm(gadm_1)
  gc()
  
  

## bring in the data ----

  in_path      <- file.path(data_external_clean,"merged","country-level")
  in_filename  <- paste0(min_time,"_to_",max_time,"_GADM_ADM_",level,"_",country,"_panel_",
                                        current_file,"_monthly_df_with_vars.rds")
  
  
    
  precip_data <- readRDS(file = file.path(in_path,in_filename))
  
  names(precip_data)
  
  dhs_data <- readRDS(file.path(data_external_clean,"DHS",
                          paste0(dhs_br_filename,"_child_mortality","_GADM_ADM_",level,".rds")))
  
  names(dhs_data)
  
# join the precip and dhs data 
  
  merged_data <- left_join(precip_data,dhs_data,
                           by = join_by(vector_cast_id,date)) %>%
                 filter(!is.na(R_CMR)) %>% ungroup()

# Summary Statistics ----
## Select data to summarize ----

data_to_summarize <- merged_data %>% select(
                                     precip,
                                     precip_003m_ra,
                                     precip_013m_ra,
                                     precip_025m_ra,
                                     precip_037m_ra,
                                     precip_lr_monthly_avg,
                                     precip_lr_monthly_sd,
                                     precip_lr_mean,
                                     precip_zscore,
                                     R_U5MR,
                                     N_U5MR,
                                     WN_U5MR,
                                     R_IMR,
                                     N_IMR,
                                     WN_IMR,
                                     R_CMR,
                                     N_CMR,
                                     WN_CMR,
                                     

                                     ) %>% as.data.frame()

var_labels <- c("Precipitation (depth, million m)",
                "Precipitation (3 month avg)",
                "Precipitation (13 month avg)",
                "Precipitation (25 month avg)",
                "Precipitation (37 month avg)",
                "Precipitation (LR monthly avg)",
                "Precipitation (LR monthly sd)",
                "Precipitation (LR monthly zscore)",
                "Precipitation (LR avg)",
                "Under-5 Mortality Rate (U5MR) (37-month avg)",
                "N to calculate U5MR",
                "Weighted N for U5MR",
                "Infant Mortality Rate (IMR) (37-month avg)",
                "N to calculate IMR",
                "Weighted N for IMR",
                "Child Mortality Rate (CMR) (37-month avg)",
                "N to calculate CMR",
                "Weighted N for CMR"
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
out_path <- file.path(output_tables,"dhs_era5_summary_stats.tex")

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
          label = "tab:dhsera5summarystats", # this label now redundant with float = FALSE
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

date_a <- "2013-11-01"
date_b <- "2015-11-01"

data_a  <- merged_data %>% filter(date == date_a)
data_b  <- merged_data %>% filter(date == date_b)

## set up regression formulas ----

chosen_dates <- c(date_a,date_b)

#chosen_years <- years  # if you uncomment this, you can get a gif from start to end which is cool

# set colors for a legend
# https://community.rstudio.com/t/adding-manual-legend-to-ggplot2/41651/2

# I have set these yale_lblue in the file 00_startup_palette.R in folder code/00_startup
# the HEX codes are taken from Yale's web services. It's unnecessarily fancy but a good default
colors <- c("Under 5 Mortality Rate (U5MR)"     = yale_lblue,
            "Infant Mortality Rate (IMR)"      = yale_blue,
            "Child Mortality Rate (CMR)"      = yale_medblue,
            "U5MR Quadratic"     = yale_lblue,
            "IMR Quadratic"      = yale_blue,
            "CMR Quadratic"      = yale_medblue)


#y <- 2019 # uncomment this if you want to examine within the loop to see what's happening
for (y in chosen_dates) {
  
  # choose just the data for the current year
  data_y <- merged_data %>% filter(date == y)
  
  plot <- ggplot(data = data_y,
                 aes(x = precip)) +
    geom_point(aes(y =R_U5MR, color = "Under 5 Mortality Rate (U5MR)")) +
    geom_smooth(aes(x = precip,
                    y = R_U5MR,
                    color = "U5MR Quadratic"),
                data = data_y,
                formula = y~ x,
                method  = lm)+
    geom_point(aes(y =R_IMR, color = "Infant Mortality Rate (IMR)")) +
    geom_smooth(aes(x = precip,
                    y = R_IMR,
                    color = "IMR Quadratic"),
                data = data_y,
                formula = y~ x,
                method  = lm)+
    geom_point(aes(y =R_CMR, color = "Child Mortality Rate (CMR)")) +
    geom_smooth(aes(x = precip,
                    y = R_CMR,
                    color = "CMR Quadratic"),
                data = data_y,
                formula = y~ x,
                method  = lm)+
     labs(title = paste0("Mortality Rates and Precipitation Ages and GDP, ",y),
         caption = c("Both measures are 37-month rolling averages. Mortality rates from DHS Uganda (2016); precipitation from ERA5 (2023)"),
         x ="Precipitation (depth over polygon, million m)" ,
         y = "Mortality Rate",
         color = "" # sets legend name
    )+
    # xlab() +
    # ylab() +
    theme_plot(title_size = 20,
               axis_title_x = element_text(color = "black",size = 15),
               axis_title_y = element_text(color = "black", size = 15),
               legend.key = element_rect(fill = "white", # box fill for the legend
                                         colour = "white" # box outlines for the legend
               ),
               legend.position = c(.15,.85) #"none" # sets legend position, x from [0,1] to y [0,1].
               # remove legend with writing legend.position = "none" instead
    ) +
    #scale_x_continuous(trans = "log10", limits = c(400,100000)) +
    #scale_y_continuous(limits = c(0,100)) +
    scale_color_manual(values = colors) # this sets the legend colors as yale colors
  #scale_y_continuous(trans = "log10", limits = c(.05,50)) +
  #scale_linetype_manual("",values = c("Predicted Values"))
  
  plot
  
  # I have a save_map and a save_plot function, but the save_map gets used 
  # more often so it's less buggy at the moment
  # good example of "don't let the perfect be the enemy of the `it works by golly I'll take it`"
  
  save_map(output_folder = file.path(output_figures,"DHS_ERA5"),
           plotname = plot,
           filename = paste0("DHS_ERA5_CMR_",y,".png"),
           width = 9,
           height = 6,
           dpi  = 400)
  
}


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

outcome_vars <- c("R_CMR","R_IMR","R_U5MR")

# show an example of it
reg_1_form <- reg_equation(outcome_var = "R_CMR",
                                 regressor_vars = c("precip","precip_037m_ra"))

reg_2_form <- reg_equation(outcome_var = "R_IMR",
                           regressor_vars = c("precip","precip_037m_ra"))

reg_3_form <- reg_equation(outcome_var = "R_U5MR",
                           regressor_vars = c("precip","precip_037m_ra"))


## Set Covariate Labels ----
# this is the variable labels you want for eqs 1 to 3 (i.e. Table 2)
# change this so that the variables that show up are the same order
# as they appear in reg_1_vars reg_2_vars then reg_3_vars

# you'll want to manually examine the output of lm() to see that
# you've labeled everything correctly in the right order

cov_labels_1_to_3 <- c("Intercept",
                       "Precip (current month, million m depth)",
                       "Precip (37-month rolling avg)")

# variable labels you want for Eqs 4 and 5 (i.e. Table 3)
cov_labels_4_to_5 <- c("Intercept","GDP (units) per (units) capita","(GDP(units) per (units))$$^2$$","(GDP (units) per (units))$$^3$$")



## output using modelsummary ----

  # Change this title and notes to fit what you need
  title_crosssection <- "Child Mortality, Nov 2013 and 2015\\label{tab:dhscrosssection}"
  my_notes <- "Robust standard errors given in parentheses. (add notes)."

# https://modelsummary.com/articles/modelsummary.html
  
  options(modelsummary_format_numeric_latex = "plain") # there was a "\num{}# argument wrapping around the latex tables
  options(modelsummary_factory_html = 'kableExtra')

# output to viewer

models <- list(
  "(1)" = lm(reg_1_form, data = data_a),
  "(2)" = lm(reg_1_form, data = data_b),
  
  
  "(3)" = lm(reg_2_form, data = data_a),
  "(4)" = lm(reg_2_form, data = data_b),
  
  "(5)" = lm(reg_3_form, data = data_a),
  "(6)" = lm(reg_3_form, data = data_b)
)

# get the LM output in the console so we can check that the covariate labels are correct
reg_1_form
lm(reg_1_form, data = data_a)
lm(reg_1_form, data = data_b)
reg_2_form
lm(reg_2_form, data = data_a)
lm(reg_2_form, data = data_b)
reg_3_form
lm(reg_3_form, data = data_a)
lm(reg_3_form, data = data_b)

# check that we're getting standard errors right
# this should be column (6)
coeftest(lm(reg_1_form, data = data_a),   vcov. = vcovHC, type = "HC1")
coeftest(lm(reg_1_form, data = data_b),   vcov. = vcovHC, type = "HC1")
coeftest(lm(reg_2_form, data = data_a),   vcov. = vcovHC, type = "HC1")
coeftest(lm(reg_2_form, data = data_b),   vcov. = vcovHC, type = "HC1")
coeftest(lm(reg_3_form, data = data_a),   vcov. = vcovHC, type = "HC1")
coeftest(lm(reg_3_form, data = data_b),   vcov. = vcovHC, type = "HC1")



# it's good practice to put in the mean of the outcome variable
# as a way to look at a glance whether the coefficients are "economically significant"

# another common way to talk about economic significance is to look at
# the coefficients relative to the standard deviation of the outcome variable

# IMPORTANT
# IF YOU ARE GETTING MEAN NA IN YOUR OUTPUT, YOU NEED TO EXAMINE WHY
# It's most likely that your data has log(x) for x<0 which does not exist
# since log(0) -> -infinity
 
# get length of the number of unique regressors
n_total_regressor_vars <- 2 # c(reg_1_vars,reg_2_vars,reg_3_vars) %>% unique() %>% length()

rows <- data.frame("term" = c("Mean"),
                   "(1)"  = c(round(mean(data_a[[outcome_vars[1]]]),2)),
                   "(2)"  = c(round(mean(data_b[[outcome_vars[1]]]),2)),
                   "(3)"  = c(round(mean(data_a[[outcome_vars[2]]]),2)),
                   "(4)"  = c(round(mean(data_b[[outcome_vars[2]]]),2)),
                   "(5)"  = c(round(mean(data_a[[outcome_vars[3]]]),2)),
                   "(6)"  = c(round(mean(data_b[[outcome_vars[3]]]),2)))

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
  add_header_above(c(" "=1, "2013" =1, "2015"=1, "2013"=1, "2015"=1, "2013"=1,"2015"=1)) %>% # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns
  add_header_above(c(" "=1, "CMR"=2, "IMR" = 2, "U5MR"=2))  # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns



## output to latex to put in overleaf ----
# maybe change this to like "hw03_reg_cross_sections.tex"
#out_path <- file.path(output_tables,"le_reg_cross_sections.tex")
out_path <- file.path(output_tables,"dhs_reg_cross_sections.tex")

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
  add_header_above(c(" "=1, "2013" =1, "2015"=1, "2013"=1, "2015"=1, "2013"=1,"2015"=1)) %>% # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns
  add_header_above(c(" "=1, "CMR"=2, "IMR" = 2, "U5MR"=2)) %>% # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns
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
              data = data_a),
  "(2)" = lm(reg_4_form,
              data = data_b),
  "(3)" = feols(reg_5_within_form,
                data = data),
  "(4)" = feols(reg_5_twoway_form,
              data = data)
)

reg_4_form
lm(reg_4_form,
   data = data_a)
lm(reg_4_form,
   data = data_b)
reg_5_within_form
feols(reg_5_within_form,
      data = data)
reg_5_twoway_form
feols(reg_5_twoway_form,
      data = data)

# declare some extra rows to add 
rows <- data.frame("term" = c("Mean","Country FE", "Time FE"),
                   "(1)"  = c(round(mean(data_a[[outcome_vars[4]]]),2),"N","N"),
                   "(2)"  = c(round(mean(data_b[[outcome_vars[4]]]),2),"N","N"),
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


