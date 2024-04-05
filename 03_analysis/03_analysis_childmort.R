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


countries_DHS <-  dhs_datasets() %>% 
  dplyr::filter(DatasetType == "GPS Datasets") %>% 
  dplyr::filter(FileType == "Geographic Data") %>%
  mutate(continent = countrycode(DHS_CountryCode, origin = "dhs",destination = "continent"))%>%
  dplyr::filter(continent == "Africa") %>%
  dplyr::select(DHS_CountryCode) %>% 
  dplyr::filter(DHS_CountryCode!="LB") %>% # something off with Liberia
  dplyr::filter(DHS_CountryCode!="TG") %>% # something off with Togo
  unique() %>%
  .[,1] # get just the character


# bring in data ----

i <- 1

countries_names <- c("SEN","Africa_all_countries")

for (i in 1:2){
if (i==1){
  
  countries <- "SN"
  countries_title <- "Senegal"

} else {
  countries <- countries_DHS
  countries_title <- "All Africa"
  }

era5_gps_childmort <- readRDS(file.path(data_external_clean,"merged",paste0("Africa_all_years_DHS_",period_length,"_month_window_child_mortality_GPS_ERA5.rds"))) %>%
  filter(DHSCC %in% countries) %>%
  st_drop_geometry()


era5_gps_childmort <- readRDS(file.path(data_external_clean,"merged",paste0("DHS_HH_infant_mortality_GPS_ERA5_rivers_1_to_3_towns.rds")))



# sum stats for nodes

## Node statistics

# node_df_to_summarize <- era5_gps_childmort %>%
#   select(year,
#          precip_current_annual_avg_mm_month,
#          precip_current_annual_sd_mm_month,
#          precip_annual_zscore_mm_month,
#          R_NNMR,
#          R_IMR,
#          R_U5MR,
#          R_U10MR,
#          WN_IMR,
#          WN_U10MR
#   ) %>% as.data.frame()
# 
# 
# var_labels <- c("Year",
#                 "Average annual precip. (mm/month)",
#                 "Annual precipitation SD (mm/month)",
#                 "Annual precipitation Z-score",
#                 "Neonatal Mort. Rate (NNMR)",
#                 "Infant Mortality Rate (IMR)",
#                 "Under-5 Mort. Rate (U5MR)",
#                 "Under-10 Mort. Rate (U10MR)",
#                 "NNMR Weighted Number (WN)",
#                 "IMR WN",
#                 "U5MR WN",
#                 "U10MR WN")
# 
# stargazer(node_df_to_summarize, 
#           type = "text",
#           style = "qje",
#           summary = TRUE,
#           covariate.labels = var_labels,
#           summary.stat = c("n","min","mean","median","max","sd"),
#           digits = 2 # round to 2 digits
# )
# 
# path <- file.path(output_tables,"summary_statistics")
# if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
# 
# out_path <- file.path(path,paste0("node_sum_stats_",countries_names[i],".tex"))
# 
# stargazer(node_df_to_summarize, 
#           type = "latex",
#           style = "qje", # also has aer style
#           out = out_path,
#           summary = TRUE,
#           covariate.labels = var_labels,
#           summary.stat = c("n","min","mean","median","max","sd"),
#           digits = 2, # round to 2 digits
#           title = paste0("Node Summary Statistics ",countries_title),
#           label = paste0("tab:node_sumstats_",countries), # this label now redundant with float = FALSE
#           float = FALSE # do this so I can use threeparttable in latex
#           # and have very pretty notes
#           # this removes the exterior "table" environment
# )

# dyad-level data 
  


  in_path <- file.path(data_external_clean,"merged","DHS_GPS_childmort_ERA5","country-level","annual")
  
  data_full <- readRDS(file = file.path(in_path,paste0(countries_names[i],"_",period_length,"_month_window_child_mortality_GPS_ERA5_annual.rds")))
  
  data_cut  <- readRDS(file = file.path(in_path,paste0(countries_names[i],"_",period_length,"_month_window_child_mortality_GPS_ERA5_annual_cut.rds")))
  

  
  # see the varnames we have here
  names(data_full)
  names(data_cut)
  

  
# Summary Statistics 

  # https://www.jakeruss.com/cheatsheets/stargazer/#the-default-summary-statistics-table

  ## Full Dyad Statistics
  
  # full_df_to_summarize <- data_full %>%
  #   select(year,
  #          distance_m,
  #          cross_country,
  #          cross_adm_1,
  #          cross_adm_2,
  #          urban_u_urban_d,
  #          urban_u_rural_d,
  #          rural_u_urban_d,
  #          rural_u_rural_d,
  #          R_NNMR_ud,
  #          WN_NNMR_u,
  #          WN_NNMR_d,
  #          R_IMR_ud,
  #          WN_IMR_u,
  #          WN_IMR_d
  #   ) %>% as.data.frame()
  # 
  # 
  # var_labels <- c("Year",
  #                 "Cluster distance (m)",
  #                 "International",
  #                 "Cross-Adm 1 (state)",
  #                 "Cross-Adm 2 (county) ",
  #                 "Urban U, Urban D",
  #                 "Urban U, Rural D",
  #                 "Rural U, Urban D",
  #                 "Rural U, Rural D",
  #                 "(NNMR U)/(NNMR D)",
  #                 "NNMR Weighted N (U)",
  #                 "NNMR Weighted N (D)",
  #                 "(IMR U)/(IMR D)",
  #                 "IMR Weighted N (U)",
  #                 "IMR Weighted N (D)")
  # 
  # stargazer(full_df_to_summarize, 
  #           type = "text",
  #           style = "qje",
  #           summary = TRUE,
  #           covariate.labels = var_labels,
  #           summary.stat = c("n","min","mean","median","max","sd"),
  #           digits = 2 # round to 2 digits
  # )
  # 
  # path <- file.path(output_tables,"summary_statistics")
  # if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  # 
  # out_path <- file.path(path,paste0("full_sum_stats_",countries_names[i],".tex"))
  # 
  # # output to latex
  # stargazer(full_df_to_summarize, 
  #           type = "latex",
  #           style = "qje", # also has aer style
  #           out = out_path,
  #           summary = TRUE,
  #           covariate.labels = var_labels,
  #           summary.stat = c("n","min","mean","median","max","sd"),
  #           digits = 2, # round to 2 digits
  #           title = paste0("Dyad Summary Statistics ",countries_title, " (Full)"),
  #           label = paste0("tab:full_df_sumstats_",countries), # this label now redundant with float = FALSE
  #           float = FALSE # do this so I can use threeparttable in latex
  #           # and have very pretty notes
  #           # this removes the exterior "table" environment
  # )
  # 
  # 
  # ## Cut Dyad Statistics 
  # 
  # cut_df_to_summarize <- data_cut %>%
  #   select(year,
  #          distance_m,
  #          cross_country,
  #          cross_adm_1,
  #          cross_adm_2,
  #          urban_u_urban_d,
  #          urban_u_rural_d,
  #          rural_u_urban_d,
  #          rural_u_rural_d,
  #          R_NNMR_ud,
  #          WN_NNMR_u,
  #          WN_NNMR_d,
  #          R_IMR_ud,
  #          WN_IMR_u,
  #          WN_IMR_d
  #   ) %>% as.data.frame()
  # 
  # 
  # stargazer(cut_df_to_summarize, 
  #           type = "text",
  #           style = "qje",
  #           summary = TRUE,
  #           covariate.labels = var_labels,
  #           summary.stat = c("n","min","mean","median","max","sd"),
  #           digits = 2 # round to 2 digits
  # )
  # 
  # path <- file.path(output_tables,"summary_statistics")
  # if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories
  # 
  # out_path <- file.path(path,paste0("cut_sum_stats_",countries_names[i],".tex"))
  # 
  # # output to latex
  # stargazer(cut_df_to_summarize, 
  #           type = "latex",
  #           style = "qje", # also has aer style
  #           out = out_path,
  #           summary = TRUE,
  #           covariate.labels = var_labels,
  #           summary.stat = c("n","min","mean","median","max","sd"),
  #           digits = 2, # round to 2 digits
  #           title = paste0("Dyad Summary Statistics ",countries_title," (Cut)"),
  #           label = paste0("tab:cut_df_sumstats_",countries), # this label now redundant with float = FALSE
  #           float = FALSE # do this so I can use threeparttable in latex
  #           # and have very pretty notes
  #           # this removes the exterior "table" environment
  # )
  
  
  
## set up regression formulas


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

  reg_1_vars  <- c("precip_current_annual_avg_mm_month_u","precip_current_annual_avg_mm_month_d")
  reg_2_vars  <- c("precip_current_annual_avg_mm_month_u","precip_current_annual_avg_mm_month_d","precip_lr_sd_deviation_u","precip_lr_sd_deviation_d")
  reg_3_vars  <- c("precip_current_annual_avg_mm_month_u","precip_current_annual_avg_mm_month_d","precip_current_annual_avg_mm_month_u:cross_country")
  reg_4_vars  <- c("precip_current_annual_avg_mm_month_u","precip_current_annual_avg_mm_month_d","urban_u_urban_d","urban_u_rural_d","rural_u_rural_d","rural_u_urban_d")
  
  
reg_1_form <- reg_equation(outcome_var = "R_NNMR_ud",
                           regressor_vars = reg_1_vars,
                           fe_vars = c("dyad_id","year"))

reg_2_form <- reg_equation(outcome_var = "R_NNMR_ud",
                           regressor_vars = reg_2_vars,
                           fe_vars = c("dyad_id","year"))

reg_3_form <- reg_equation(outcome_var = "R_NNMR_ud",
                           regressor_vars = reg_3_vars,
                           fe_vars = c("year"))

reg_4_form <- reg_equation(outcome_var = "R_IMR_ud",
                           regressor_vars = reg_1_vars,
                           fe_vars = c("dyad_id","year"))

reg_5_form <- reg_equation(outcome_var = "R_IMR_ud",
                           regressor_vars = reg_2_vars,
                           fe_vars = c("dyad_id","year"))

reg_6_form <- reg_equation(outcome_var = "R_IMR_ud",
                           regressor_vars = reg_3_vars,
                           fe_vars = c("year"))
# output to viewer

models <- list(
  "(1)" = feols(reg_1_form , data = data_cut),
  "(2)" = feols(reg_2_form , data = data_cut),
  "(3)" = feols(reg_4_form,  data = data_cut),
  "(4)" = feols(reg_5_form,  data = data_cut)
  #"(5)" = feols(reg_5_form , data = data_cut),
  #"(6)" = feols(reg_6_form , data = data_cut)
  )

# get the LM output in the console so we can check that the covariate labels are correct
models[[1]]
models[[2]]
models[[3]]
models[[4]]
# models[[5]]
# models[[6]]


## Set  Labels

title_tab_01 <- paste0("Precipitation and Mortality ",countries_title," \\label{tab:",countries,"-reg-tab}")

cov_labels <- c("Annual Avg Precip (mm/month) (U)","Annual Avg Precip (D)","Precip (Current - Long-run SD) (U)","Precip (Current - Long-run SD) (D)")
my_notes <- c("(U): upstream; (D) downstream. Long-run SD: annual standard deviation of precipitation from January 1940 to July 2023. Data from ERA5 (2023); DHS surveys from 1988 to 2019. Mortality rates were truncated above median, below 90th percentile because of small exposed population sizes.")



options(modelsummary_format_numeric_latex = "plain") # there was a "\num{}# argument wrapping around the latex tables
options(modelsummary_factory_html = 'kableExtra')


# add dependent variable means 
# get length of the number of unique regressors
n_total_regressor_vars <- c(reg_1_vars,reg_2_vars) %>% unique() %>% length()

rows <- data.frame("term" = c("Mean","Dyad FE","Year FE"),
                   "(1)"  = c(round(mean(data_cut$R_NNMR_ud),2),"Y","Y"),
                   "(2)"  = c(round(mean(data_cut$R_NNMR_ud),2),"Y","Y"),
                   "(3)"  = c(round(mean(data_cut$R_NNMR_ud),2),"Y","Y"),
                   "(4)"  = c(round(mean(data_cut$R_IMR_ud),2),"Y","Y"))
                  # "(5)"  = c(round(mean(data_cut$R_IMR_ud),2),"Y","Y"),
                  # "(6)"  = c(round(mean(data_cut$R_IMR_ud),2),"N","Y"))

attr(rows, 'position') <- c(2*n_total_regressor_vars+2,2*n_total_regressor_vars+3,2*n_total_regressor_vars+4) # this should put it right after the number of observations
# there are 2 rows per regressor var, plus the Number of Observations, plus 2 for the intercept, and then put the Num Obs at 
# the next one down


# output to console to check that we're getting what we want
# if you are getting an output of 0.000, examine if you need to adjust the units
# of that regressor. It might be the case that your units are just too small, not that
# you have a true zero


modelsummary(models,
             stars = FALSE,
             vcov =
               list(~dyad_id+year,~dyad_id+year,~dyad_id+year,
                    ~dyad_id+year), # stata's heteroskedasticity robust standard errors
             #statistic = "conf.int",
             coef_rename = cov_labels,
             #output = "latex",
             add_rows = rows,
             title = title_tab_01,
             gof_omit = "AIC|BIC|RMSE|Log.Lik|Std.Errors|:|Within|Adj." # omit several of the goodness of fit stats
)  %>%
  add_header_above(c(" "=1, "(NNMR-U)/(NNMR-D)" = 2, "(IMR-U)/(IMR-D)"=2))  # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns


path <- file.path(output_tables,"regressions")
if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories

out_path <- file.path(path,paste0("reg-tab-01_",countries_names[i],".tex"))



modelsummary(models,
             stars = FALSE,
             vcov =
               list(~dyad_id+year,~dyad_id+year,~dyad_id+year,
                    ~dyad_id+year), # stata's heteroskedasticity robust standard errors
             #statistic = "conf.int",
             coef_rename = cov_labels,
             output = "latex",
             add_rows = rows,
             #title = title_tab_01,
             #escape = FALSE, # what allows the math to go through
             gof_omit = "AIC|BIC|RMSE|Log.Lik|Std.Errors|:|Within|Adj." # omit several of the goodness of fit stats
)  %>%  # omit several of the goodness of fit stats
  kableExtra::footnote(escape = FALSE, # this allows us to use \\\\citet{} and put citations in this end-of-table note
           general = my_notes,
           threeparttable = TRUE,
           general_title = "Notes: "
  )%>%
  add_header_above(c(" "=1, "(NNMR-U)/(NNMR-D)" = 2, "(IMR-U)/(IMR-D)"=2)) %>% # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns
 kable_styling(latex_options = "HOLD_position") %>% # changes the latex placement to [H] (here), put this table in the latex doc where I say it goes
  save_kable(
    file = out_path,
    format = "latex"
  )


# Regression Table 02


reg_1_vars  <- c("precip_current_annual_avg_mm_month_u","precip_current_annual_avg_mm_month_d","precip_lr_sd_deviation_u","precip_lr_sd_deviation_d","precip_current_annual_avg_mm_month_u:cross_country")
reg_2_vars  <- c("precip_current_annual_avg_mm_month_u","precip_current_annual_avg_mm_month_d","precip_lr_sd_deviation_u","precip_lr_sd_deviation_d","precip_current_annual_avg_mm_month_u:cross_adm_1")
reg_3_vars  <- c("precip_current_annual_avg_mm_month_u","precip_current_annual_avg_mm_month_d","precip_lr_sd_deviation_u","precip_lr_sd_deviation_d","precip_current_annual_avg_mm_month_u:cross_adm_2")
reg_4_vars  <- c("precip_current_annual_avg_mm_month_u","precip_current_annual_avg_mm_month_d","urban_u_urban_d","urban_u_rural_d","rural_u_rural_d","rural_u_urban_d")


reg_1_form <- reg_equation(outcome_var = "R_NNMR_ud",
                           regressor_vars = reg_1_vars,
                           fe_vars = c("year"))

reg_2_form <- reg_equation(outcome_var = "R_NNMR_ud",
                           regressor_vars = reg_2_vars,
                           fe_vars = c("year"))

reg_3_form <- reg_equation(outcome_var = "R_NNMR_ud",
                           regressor_vars = reg_3_vars,
                           fe_vars = c("year"))

reg_4_form <- reg_equation(outcome_var = "R_IMR_ud",
                           regressor_vars = reg_1_vars,
                           fe_vars = c("year"))

reg_5_form <- reg_equation(outcome_var = "R_IMR_ud",
                           regressor_vars = reg_2_vars,
                           fe_vars = c("year"))

reg_6_form <- reg_equation(outcome_var = "R_IMR_ud",
                           regressor_vars = reg_3_vars,
                           fe_vars = c("year"))
# output to viewer

models <- list(
  "(1)" = feols(reg_1_form , data = data_cut),
  "(2)" = feols(reg_2_form , data = data_cut),
  "(3)" = feols(reg_3_form,  data = data_cut),
  "(4)" = feols(reg_4_form,  data = data_cut),
  "(5)" = feols(reg_5_form , data = data_cut),
  "(6)" = feols(reg_6_form , data = data_cut)
)

# get the LM output in the console so we can check that the covariate labels are correct
models[[1]]
models[[2]]
models[[3]]
models[[4]]
models[[5]]
models[[6]]


## Set  Labels

title_tab_02 <- paste0("Precipitation and Mortality: Border Crossings ",countries_title[i]," \\label{tab:",countries,"-reg-tab}")

cov_labels <- c("Annual Avg Precip (mm/month) (U)","Annual Avg Precip (D)","Precip (Current - Long-run SD) (U)","Precip (Current - Long-run SD) (D)","Precip (U) x Cross-Country","Precip (U) x Cross Adm 1","Precip (U) x Cross Adm 2")
my_notes <- c("(U): upstream; (D) downstream. Long-run SD: annual standard deviation of precipitation from January 1940 to July 2023. Data from ERA5 (2023); DHS surveys from 1988 to 2019. Mortality rates were truncated above median, below 90th percentile because of small exposed population sizes.")



options(modelsummary_format_numeric_latex = "plain") # there was a "\num{}# argument wrapping around the latex tables
options(modelsummary_factory_html = 'kableExtra')


# add dependent variable means 
# get length of the number of unique regressors
n_total_regressor_vars <- c(reg_1_vars,reg_2_vars,reg_3_vars) %>% unique() %>% length()

rows <- data.frame("term" = c("Mean","Dyad FE","Year FE"),
                   "(1)"  = c(round(mean(data_cut$R_NNMR_ud),2),"N","Y"),
                   "(2)"  = c(round(mean(data_cut$R_NNMR_ud),2),"N","Y"),
                   "(3)"  = c(round(mean(data_cut$R_NNMR_ud),2),"N","Y"),
                   "(4)"  = c(round(mean(data_cut$R_IMR_ud),2),"N","Y"),
                   "(5)"  = c(round(mean(data_cut$R_IMR_ud),2),"N","Y"),
                   "(6)"  = c(round(mean(data_cut$R_IMR_ud),2),"N","Y"))

attr(rows, 'position') <- c(2*n_total_regressor_vars+2,2*n_total_regressor_vars+3,2*n_total_regressor_vars+4) # this should put it right after the number of observations
# there are 2 rows per regressor var, plus the Number of Observations, plus 2 for the intercept, and then put the Num Obs at 
# the next one down


# output to console to check that we're getting what we want
# if you are getting an output of 0.000, examine if you need to adjust the units
# of that regressor. It might be the case that your units are just too small, not that
# you have a true zero


modelsummary(models,
             stars = FALSE,
             vcov =
               list(~dyad_id+year,~dyad_id+year,~dyad_id+year,
                    ~dyad_id+year,~dyad_id+year,~dyad_id+year), # stata's heteroskedasticity robust standard errors
             #statistic = "conf.int",
             coef_rename = cov_labels,
             #output = "latex",
             add_rows = rows,
             #title = title_tab_02,
             gof_omit = "AIC|BIC|RMSE|Log.Lik|Std.Errors|:|Within|Adj." # omit several of the goodness of fit stats
)  %>%
  add_header_above(c(" "=1, "(NNMR-U)/(NNMR-D)" = 3, "(IMR-U)/(IMR-D)"=3))  # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns


path <- file.path(output_tables,"regressions")
if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories

out_path <- file.path(path,paste0("reg-tab-02_",countries_names[i],".tex"))



modelsummary(models,
             stars = FALSE,
             vcov =
               list(~dyad_id+year,~dyad_id+year,~dyad_id+year,
                    ~dyad_id+year,~dyad_id+year,~dyad_id+year), # stata's heteroskedasticity robust standard errors
             #statistic = "conf.int",
             coef_rename = cov_labels,
             output = "latex",
             add_rows = rows,
             #title = title_tab_02,
             #escape = FALSE, # what allows the math to go through
             gof_omit = "AIC|BIC|RMSE|Log.Lik|Std.Errors|:|Within|Adj." # omit several of the goodness of fit stats
)  %>%  # omit several of the goodness of fit stats
  kableExtra::footnote(escape = FALSE, # this allows us to use \\\\citet{} and put citations in this end-of-table note
                       general = my_notes,
                       threeparttable = TRUE,
                       general_title = "Notes: "
  )%>%
  add_header_above(c(" "=1, "(NNMR-U)/(NNMR-D)" = 3, "(IMR-U)/(IMR-D)"=3)) %>% # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns
  kable_styling(latex_options = "HOLD_position") %>% # changes the latex placement to [H] (here), put this table in the latex doc where I say it goes
  save_kable(
    file = out_path,
    format = "latex"
  )


## Set  Labels

title_tab_03 <- paste0("Precipitation and Mortality: Border Crossings ",countries_title[i]," \\label{tab:",countries,"-reg-tab}")

cov_labels <- c("Annual Avg Precip (mm/month) (U)","Annual Avg Precip (D)","Precip (Current - Long-run SD) (U)","Precip (Current - Long-run SD) (D)","Precip (U) x Cross-Country","Precip (U) x Cross Adm 1","Precip (U) x Cross Adm 2")
my_notes <- c("(U): upstream; (D) downstream. Long-run SD: annual standard deviation of precipitation from January 1940 to July 2023. Data from ERA5 (2023); DHS surveys from 1988 to 2019. Mortality rates were truncated above median, below 90th percentile because of small exposed population sizes.")



options(modelsummary_format_numeric_latex = "plain") # there was a "\num{}# argument wrapping around the latex tables
options(modelsummary_factory_html = 'kableExtra')


# add dependent variable means 
# get length of the number of unique regressors
n_total_regressor_vars <- c(reg_1_vars,reg_2_vars,reg_3_vars) %>% unique() %>% length()

rows <- data.frame("term" = c("Mean","Dyad FE","Year FE"),
                   "(1)"  = c(round(mean(data_cut$R_NNMR_ud),2),"N","Y"),
                   "(2)"  = c(round(mean(data_cut$R_NNMR_ud),2),"N","Y"),
                   "(3)"  = c(round(mean(data_cut$R_NNMR_ud),2),"N","Y"),
                   "(4)"  = c(round(mean(data_cut$R_IMR_ud),2),"N","Y"),
                   "(5)"  = c(round(mean(data_cut$R_IMR_ud),2),"N","Y"),
                   "(6)"  = c(round(mean(data_cut$R_IMR_ud),2),"N","Y"))

attr(rows, 'position') <- c(2*n_total_regressor_vars+2,2*n_total_regressor_vars+3,2*n_total_regressor_vars+4) # this should put it right after the number of observations
# there are 2 rows per regressor var, plus the Number of Observations, plus 2 for the intercept, and then put the Num Obs at 
# the next one down


# output to console to check that we're getting what we want
# if you are getting an output of 0.000, examine if you need to adjust the units
# of that regressor. It might be the case that your units are just too small, not that
# you have a true zero


modelsummary(models,
             stars = FALSE,
             vcov =
               list(~dyad_id+year,~dyad_id+year,~dyad_id+year,
                    ~dyad_id+year,~dyad_id+year,~dyad_id+year), # stata's heteroskedasticity robust standard errors
             #statistic = "conf.int",
             coef_rename = cov_labels,
             #output = "latex",
             add_rows = rows,
             #title = title_tab_02,
             gof_omit = "AIC|BIC|RMSE|Log.Lik|Std.Errors|:|Within|Adj." # omit several of the goodness of fit stats
)  %>%
  add_header_above(c(" "=1, "(NNMR-U)/(NNMR-D)" = 3, "(IMR-U)/(IMR-D)"=3))  # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns


path <- file.path(output_tables,"regressions")
if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories

out_path <- file.path(path,paste0("reg-tab-03_",countries_names[i],".tex"))



modelsummary(models,
             stars = FALSE,
             vcov =
               list(~dyad_id+year,~dyad_id+year,~dyad_id+year,
                    ~dyad_id+year,~dyad_id+year,~dyad_id+year), # stata's heteroskedasticity robust standard errors
             #statistic = "conf.int",
             coef_rename = cov_labels,
             output = "latex",
             add_rows = rows,
             #title = title_tab_02,
             #escape = FALSE, # what allows the math to go through
             gof_omit = "AIC|BIC|RMSE|Log.Lik|Std.Errors|:|Within|Adj." # omit several of the goodness of fit stats
)  %>%  # omit several of the goodness of fit stats
  kableExtra::footnote(escape = FALSE, # this allows us to use \\\\citet{} and put citations in this end-of-table note
                       general = my_notes,
                       threeparttable = TRUE,
                       general_title = "Notes: "
  )%>%
  add_header_above(c(" "=1, "(NNMR-U)/(NNMR-D)" = 3, "(IMR-U)/(IMR-D)"=3)) %>% # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns
  kable_styling(latex_options = "HOLD_position") %>% # changes the latex placement to [H] (here), put this table in the latex doc where I say it goes
  save_kable(
    file = out_path,
    format = "latex"
  )


} # end of looping over regressions

