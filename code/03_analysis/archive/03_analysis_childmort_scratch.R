# _______________________________#
# Environment
# Analysis 03: Summary Stats and Regressions 
# 
# Stallman
# Started 2023-10-11
# Last edited: 
#________________________________#


# need 02_merge_a_era5_dhs


# Startup

rm(list = ls())


# bring in the packages, folders, paths

home_folder <- file.path("P:","Projects","environment")

source(file.path(home_folder,"code","00_startup_master.R"))

# requires having run
# 02_merge_a_era5_dhs
# 02_clean_dhs_child-mortality_annual

# packages ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tictoc, #measuring time to run operations
  countrycode, # for translating between country names
  #rdhs, # for dealing with DHS in R
  sf, # vector operations
  terra, # raster operations
  #zoo, # time series stuff, easy calc of rolling averages
  AER, # outputting stuff nicely
  stargazer, # for output directly to latex
  modelsummary, # for outputting nicely, more customizable than stargazer
  kableExtra, # for manipulating tables
  ggrepel, # for plotting and shifting labels around
  fixest # for fixed effects estimation regressions
)


# parameters ----
period_length <- 60
countries_iso3c <- c("SEN")
countries_dhs <- c("SN","GM","MR")

countries_names <- c("SEN","Africa_all_countries")

# bring in data ----
era5_gps_childmort <- readRDS(file.path(data_external_clean,"merged",paste0("Africa_all_years_DHS_",period_length,"_month_window_child_mortality_GPS_ERA5.rds"))) %>%
  filter(DHSCC %in% countries_dhs) %>%
  st_drop_geometry()

  
i <- 1

  in_path <- file.path(data_external_clean,"merged","DHS_GPS_childmort_ERA5","country-level","annual")
  
  data_full <- readRDS(file = file.path(in_path,paste0(countries_names[i],"_",period_length,"_month_window_child_mortality_GPS_ERA5_annual.rds")))
  
  data_cut  <- readRDS(file = file.path(in_path,paste0(countries_names[i],"_",period_length,"_month_window_child_mortality_GPS_ERA5_annual_cut.rds")))
  

  
  # see the varnames we have here
  names(data_full)
  names(data_cut)
  
# # see how many total dhs surveys are being used ----
#   
#   full_dhs_surveys_u <- unique(data_full$dhs_gps_filename_u)
#   full_dhs_surveys_d <- unique(data_full$dhs_gps_filename_d)
#   
#   total_full_dhs_surveys <- unique(c(full_dhs_surveys_d,full_dhs_surveys_u)) %>% length()
# 
#   
#   full_dhs_cc_u <- unique(data_full$DHSCC_u)
#   full_dhs_cc_d <- unique(data_full$DHSCC_d)
#   
#   total_full_dhs_surveys <- unique(c(full_dhs_cc_u,full_dhs_cc_d)) %>% length()
#   
  
# Summary Statistics ----

  # https://www.jakeruss.com/cheatsheets/stargazer/#the-default-summary-statistics-table
  
  
  ## Node statistics ----
  
  node_df_to_summarize <- era5_gps_childmort %>%
                          select(year,
                                 precip_current_annual_avg_mm_month,
                                 precip_current_annual_sd_mm_month,
                                 precip_annual_zscore_mm_month,
                                 R_IMR,
                                 R_U10MR,
                                 WN_IMR,
                                 WN_U10MR
                                 ) %>% as.data.frame()
  
  
  var_labels <- c("Year",
                  "Average annual precipitation (mm/month)",
                  "Annual precipitation SD (mm/month)",
                  "Annual precipitation Z-score (mm/month)",
                  "Infant Mortality Rate (IMR) (/1000 live births)",
                  "Under-10 Mort. Rate (U10MR) (/1000 live births)",
                  "IMR Weighted Number Exposed",
                  "U10MR Weighted Number Exposed")

  stargazer(node_df_to_summarize, 
            type = "text",
            style = "qje",
            summary = TRUE,
            covariate.labels = var_labels,
            summary.stat = c("n","min","mean","median","max","sd"),
            digits = 2 # round to 2 digits
  )
  
  path <- file.path(output_tables,"summary_statistics")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  out_path <- file.path(path,paste0("node_sum_stats_",countries_names[i],".tex"))
  
  stargazer(node_df_to_summarize, 
            type = "latex",
            style = "qje", # also has aer style
            out = out_path,
            summary = TRUE,
            covariate.labels = var_labels,
            summary.stat = c("n","min","mean","median","max","sd"),
            digits = 2, # round to 2 digits
            title = "Summary Statistics",
            label = "tab:node_sumstats", # this label now redundant with float = FALSE
            float = FALSE # do this so I can use threeparttable in latex
            # and have very pretty notes
            # this removes the exterior "table" environment
  )
  
  ## Full Dyad Statistics ----
  
  full_df_to_summarize <- data_full %>%
    select(year,
           distance_m,
           cross_country,
           cross_adm_1,
           cross_adm_2,
           urban_u_urban_d,
           urban_u_rural_d,
           rural_u_urban_d,
           rural_u_rural_d,
           R_U10MR_ud,
           WN_U10MR_u,
           WN_U10MR_d
    ) %>% as.data.frame()
  
  
  var_labels <- c("Year",
                  "River distance b/w clusters(m)",
                  "Crosses country border",
                  "Crosses Adm 1 border (e.g. state)",
                  "Cross Adm 2 border (e.g. county) ",
                  "Urban Up, Urban Down (UU-UD)",
                  "Urban Up, Rural Down (UU-RD)",
                  "Rural Up, Urban Down (RU-UD)",
                  "Rural Up, Rural Down (RU-RD)",
                  "(U10MR Up)/(U10MR Down)",
                  "U10MR Weighted Number Exposed (Up)",
                  "U10MR Weighted Number Exposed (Down)")
  
  stargazer(full_df_to_summarize, 
            type = "text",
            style = "qje",
            summary = TRUE,
            covariate.labels = var_labels,
            summary.stat = c("n","min","mean","median","max","sd"),
            digits = 2 # round to 2 digits
  )
  
  path <- file.path(output_tables,"summary_statistics")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  out_path <- file.path(path,paste0("full_sum_stats_",countries_names[i],".tex"))
  
  # output to latex
  stargazer(full_df_to_summarize, 
            type = "latex",
            style = "qje", # also has aer style
            out = out_path,
            summary = TRUE,
            covariate.labels = var_labels,
            summary.stat = c("n","min","mean","median","max","sd"),
            digits = 2, # round to 2 digits
            title = "Dyad Summary Statistics (All)",
            label = "tab:full_df_sumstats", # this label now redundant with float = FALSE
            float = FALSE # do this so I can use threeparttable in latex
            # and have very pretty notes
            # this removes the exterior "table" environment
  )
  
  
  ## Cut Dyad Statistics ----

  cut_df_to_summarize <- data_cut %>%
    select(year,
           distance_m,
           cross_country,
           cross_adm_1,
           cross_adm_2,
           urban_u_urban_d,
           urban_u_rural_d,
           rural_u_urban_d,
           rural_u_rural_d,
           R_U10MR_ud,
           WN_U10MR_u,
           WN_U10MR_d
    ) %>% as.data.frame()
  
  
  var_labels <- c("Year",
                  "River distance b/w clusters(m)",
                  "Crosses country border",
                  "Crosses Adm 1 border (e.g. state)",
                  "Cross Adm 2 border (e.g. county) ",
                  "Urban Up, Urban Down (UU-UD)",
                  "Urban Up, Rural Down (UU-RD)",
                  "Rural Up, Urban Down (RU-UD)",
                  "Rural Up, Rural Down (RU-RD)",
                  "(U10MR Up)/(U10MR Down)",
                  "U10MR Weighted Number Exposed (Up)",
                  "U10MR Weighted Number Exposed (Down)")
  
  stargazer(cut_df_to_summarize, 
            type = "text",
            style = "qje",
            summary = TRUE,
            covariate.labels = var_labels,
            summary.stat = c("n","min","mean","median","max","sd"),
            digits = 2 # round to 2 digits
  )
  
  path <- file.path(output_tables,"summary_statistics")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  
  out_path <- file.path(path,paste0("cut_sum_stats_",countries_names[i],".tex"))
  
  # output to latex
  stargazer(cut_df_to_summarize, 
            type = "latex",
            style = "qje", # also has aer style
            out = out_path,
            summary = TRUE,
            covariate.labels = var_labels,
            summary.stat = c("n","min","mean","median","max","sd"),
            digits = 2, # round to 2 digits
            title = "Dyad Summary Statistics (Cut)",
            label = "tab:cut_df_sumstats", # this label now redundant with float = FALSE
            float = FALSE # do this so I can use threeparttable in latex
            # and have very pretty notes
            # this removes the exterior "table" environment
  )
  
  
## set up regression formulas ----

reg_1_vars  <- c("precip_annual_zscore_mm_month_u","precip_annual_zscore_mm_month_d")
reg_2_vars  <- c("precip_annual_zscore_mm_month_u","precip_annual_zscore_mm_month_d","precip_lr_sd_deviation_u","precip_lr_sd_deviation_d")
reg_3_vars  <- c("precip_annual_zscore_mm_month_u","precip_annual_zscore_mm_month_d","cross_country","cross_adm_1","cross_adm_2","urban_u_rural_d","urban_u_urban_d","rural_u_rural_d","rural_u_urban_d")

## Set Covariate Labels ----
# this is the variable labels you want for eqs 1 to 3 (i.e. Table 2)
# change this so that the variables that show up are the same order
# as they appear in reg_1_vars reg_2_vars then reg_3_vars

# you'll want to manually examine the output of lm() to see that
# you've labeled everything correctly in the right order

cov_labels_tab_01 <- c("Intercept","GDP per capita","Log GDP per capita","TFR", "GDP (unit) per (unit)")

# variable labels you want for Eqs 4 and 5 (i.e. Table 3)
cov_labels_tab_02 <- c("Intercept","GDP (units) per (units) capita","(GDP(units) per (units))$$^2$$","(GDP (units) per (units))$$^3$$")



  options(modelsummary_format_numeric_latex = "plain") # there was a "\num{}# argument wrapping around the latex tables
  options(modelsummary_factory_html = 'kableExtra')

# output to viewer

models <- list(
  "(1)" = feols(R_U10MR_ud ~ paste(reg_1_vars,collapse = "+") | dyad_id + year , data = data_cut),
  "(2)" = feols(R_U10MR_ud ~ paste(reg_2_vars,collapse = "+") | dyad_id + year , data = data_cut),
  "(3)" = feols(R_U10MR_ud ~ paste(reg_3_vars,collapse = "+") | dyad_id + year , data = data_cut),
  "(4)" = feols(R_U10MR_ud ~ paste(reg_1_vars,collapse = "+") | dyad_id + year , data = data_cut),
  "(5)" = feols(R_U10MR_ud ~ paste(reg_2_vars,collapse = "+") | dyad_id + year , data = data_cut),
  "(6)" = feols(R_U10MR_ud ~ paste(reg_3_vars,collapse = "+") | dyad_id + year , data = data_cut))

# get the LM output in the console so we can check that the covariate labels are correct
models[[1]]

# add dependent variable means ----
# get length of the number of unique regressors
n_total_regressor_vars <- c(reg_1_vars,reg_2_vars,reg_3_vars) %>% unique() %>% length()

rows <- data.frame("term" = c("Mean"),
                   "(1)"  = c(round(mean(data_cut[[outcome_vars[1]]]),2)),
                   "(2)"  = c(round(mean(data_cut[[outcome_vars[1]]]),2)),
                   "(3)"  = c(round(mean(data_cut[[outcome_vars[2]]]),2)),
                   "(4)"  = c(round(mean(data_cut[[outcome_vars[2]]]),2)),
                   "(5)"  = c(round(mean(data_cut[[outcome_vars[3]]]),2)),
                   "(6)"  = c(round(mean(data_cut[[outcome_vars[3]]]),2)))

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
             coef_rename = cov_labels_tab_01,
             #output = "latex",
             add_rows = rows,
             title = title_tab_01,
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
             coef_rename = cov_labels_tab_01,
             output = "latex",
             add_rows = rows,
             booktabs = T,  # adding booktabs = TRUE allows us to call out specific elements of the latex table and change them
             title = title_tab_01,
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


# Analysis: Table 02: Panel Regression ----

## set titles and notes ----

  title_paneltab <- "Panel Regression, Life Expectancy \\label{tab:hw03regpanel}"
  notes_paneltab <- "Heteroskedascicity-robust standard errors clustered at the country (for within) and country and year (for twoways) levels given in parentheses. Population and life expectancy data are from \\\\citet{undesaWorldPopulationProspects2022}. Gross domestic product (GDP) is (what units) from \\\\citet{feenstraNextGenerationPenn2015}."
  n_total_regressor_vars <- c(reg_4_vars,reg_5_vars) %>% unique() %>% length()
  
## set formulas ----
  
  fe_ols_5 <- feols(le_birth ~ paste(reg_5_vars,collapse = "+") | iso3c,
                    data = data)


  fe_ols_6 <- feols(le_birth ~ paste(reg_5_vars,collapse = "+") | iso3c + year,
                    data = data)

  # notice this has fixed effects for iso3c and year, but it's clustered at just the
  # country level. What's the interpretation?
  # That the errors are correlated within but not across countries,
  # and that the errors are not correlated within years
  # We'll adjust a little below to cluster at the year and country level just to be paranoid
  # the standard errors will rise a tad

  fe_ols_5
  
  fe_ols_6

# how do we decide what to cluster? Look at the residuals plot and see what 
# groups there's correlation within. If there's correlation in the residuals according
# to country, then cluster by country

# what to do with the standard errors?
# cluster by group and time?
# https://stackoverflow.com/questions/8389843/double-clustered-standard-errors-for-panel-data
reg_4_form

models <- list(
  "(1)" = lm(le_birth ~ paste(reg_4_vars,collapse = "+"),
              data = data_year_a),
  "(2)" = lm(le_birth ~ paste(reg_4_vars,collapse = "+"),
              data = data_year_b),
  "(3)" = feols(le_birth ~ paste(reg_5_vars,collapse = "+") | iso3c,
                data = data),
  "(4)" = feols(le_birth ~ paste(reg_5_vars,collapse = "+") | iso3c + year,
                data = data)
)


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
             coef_rename = cov_labels_tab_02,
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
             coef_rename = cov_labels_tab_02,
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
