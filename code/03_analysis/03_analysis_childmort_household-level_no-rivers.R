# _______________________________#
# Environment
# Analysis 03: Summary Stats and Regressions 
# 
# Stallman
# Started 2023-10-11
# Last edited: 2024-04-08
#________________________________#


# need 02_merge_a_era5_dhs


# Startup

rm(list = ls())


# bring in the packages, folders, paths

home_folder <- file.path("P:","Projects","freshwater-cooperation")

source(file.path(home_folder,"code","00_startup_master.R"))

# requires having run
# 02_merge_a_era5_dhs
# 02_clean_dhs_child-mortality_annual
# 02_merge_d_era5_dhs-gps_dhs-child-mortality.R

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
  data.table,
  fixest # for fixed effects estimation regressions
)


# parameters ----

current_seed <- 2024

# bring in data ----


system.time(
  era5_gps_childmort <- readRDS(file.path(file.path(data_external_clean,"merged",paste0("DHS_HH_infant_mortality_GPS_ERA5_river_points_under_100_towns.rds")))) %>% st_drop_geometry()
)
  

  
# make a training dataset of 2/3 of the obs
  set.seed(current_seed)
  
  n_dhsid_full <- length(unique(era5_gps_childmort$DHSID))
  n_slice_sample <- ceiling(.4*n_dhsid_full)
  
  system.time(
  era5_gps_childmort_train <- era5_gps_childmort %>% 
                              group_by(DHSID) %>% # want 60% of the DHS ID, not 60% of total obs
                              slice_sample(prop = 0.6) %>%
                              ungroup()
)
  
  # 1.58s elapsed
 n_dhsid_train <- length(unique(era5_gps_childmort_train$DHSID))

  
  saveRDS(era5_gps_childmort_train,file.path(data_external_clean,"merged",paste0("Africa_all_years_DHS_HH_infant_mortality_GPS_ERA5_training-data.rds")))
  
  
  era5_gps_childmort_train <- readRDS(file.path(data_external_clean,"merged",paste0("Africa_all_years_DHS_HH_infant_mortality_GPS_ERA5_training-data.rds")))
  
  
  
era5_gps_childmort_train2 <- era5_gps_childmort_train %>%
                      filter(!is.na(precip_annual_mean)) %>%
                      mutate(infant_death_1000 = infant_death*1000
                             ) %>%
                      group_by(DHSID) %>%
                      mutate(n_children_in_DHSID = n()/2) %>%
                      ungroup()%>%
                      group_by(year) %>%
                      mutate(n_children_in_year = n()/2) %>% ungroup() %>%
                      ungroup() %>%
                      group_by(year,DHSID) %>%
                      mutate(n_children_town_year = n()/2) %>% ungroup() %>%
                      mutate(
                             infant_death_1000_exposure = infant_death_1000*share_year_t) %>%
                      mutate(rural = if_else(URBAN_RURA == "R",
                                             1,
                                             0)) %>%
                      filter(DHSCC!="MD") %>% # Madagascar is weird
                      group_by(MAIN_RIV) %>%
                      mutate(main_riv_mean_precip = mean(precip_annual_mean)) %>%
                      ungroup() %>%
                      mutate(median_precip = median(precip_annual_mean)) %>%
                      filter(main_riv_mean_precip<=median_precip)

  # create a variable that's 

era5_gps_childmort_train_namibia <- era5_gps_childmort_train2 %>%
                                filter(DHSCC == "NM")

n_dhsid_namibia <- length(unique(era5_gps_childmort_train_namibia$DHSID))


share_dhsid_lost <- (n_dhsid_full-n_dhsid_train)/(n_dhsid_full)

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

# sum stats for nodes

## Node statistics

i <- 2
for (i in 1:2){
  if (i==1) {
    current_data     <- era5_gps_childmort_train2
    filestub <- "no_rivers_training"
    name_stub     <- "DHS Data: 3/5 Random Sample"
  } else {
    current_data = era5_gps_childmort_train_namibia
    filestub <- "no_rivers_namibia"
    name_stub     <- "DHS Data: 3/5 Random Sample, Namibia"
  }
node_df_to_summarize <- current_data %>%
  select(year,
         precip_annual_mean,
         precip_lr_avg,
         precip_lr_zscore, 
         precip_rolling_avg_3_years,
         precip_rolling_avg_5_years,
         infant_death_1000,
         infant_death_1000_exposure,
         rural,
         has_dam,
         has_glow,
         had_adhi,
         n_children_in_DHSID,
         n_children_in_year,
         n_children_town_year
  ) %>% as.data.frame()

var_labels <- c("Year",
                "Average annual precip. (mm/month)",
                "Long-run avg. precip (mm/month)",
                "Annual precipitation Z-score",
                "3-year avg. precip.",
                "5-year avg. precip.",
                "Infant Mortality (/1000 births)",
                "Infant Mort., Exposure-weighted",
                "Rural",
                "On a Dammed River",
                "On a River with Width Obs",
                "On a River with Hydro Station",
                "N infants per town",
                "N infants per year",
                "N infants/town/year"
)

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

out_path <- file.path(path,paste0("node_",filestub,"_sum_stats.tex"))

stargazer(node_df_to_summarize,
          type = "latex",
          style = "qje", # also has aer style
          out = out_path,
          summary = TRUE,
          covariate.labels = var_labels,
          summary.stat = c("n","min","mean","median","max","sd"),
          digits = 2, # round to 2 digits
          title = paste0("Summary Statistics",name_stub),
          float = FALSE # do this so I can use threeparttable in latex
          # and have very pretty notes
          # this removes the exterior "table" environment
)

}

# data exploration ----

# get country names
# 
countries <- era5_gps_childmort_train2$DHSCC %>% unique()
country_names <- countrycode(sourcevar = countries,
                             origin = "dhs",
                             destination = "country.name")

countries_df <- data.frame(country_dhs = era5_gps_childmort_train2$DHSCC %>% unique(),
                           country_names = countrycode(sourcevar = era5_gps_childmort_train2$DHSCC %>% unique(),
                                                       origin = "dhs",
                                                       destination = "country.name")) %>%
                arrange(countries)

plot <- ggplot(mapping = aes(x = era5_gps_childmort_train2$DHSCC)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", aes(label = after_stat(count)/2), vjust = -0.3, size = 3.5) +
  labs(title = paste0("Observations per country, below-median rain"),
       caption = c("Data from DHS (1984-2023)")) +
  theme_map()

plot

save_map(output_folder = output_figures,
         plotname = plot,
         filename = paste0("obs-per-country_under-median.png"),
         width = 10,
         height = 4,
         dpi  = 300)

## set up regression formulas


  reg_1_vars  <- c("precip_annual_mean")
  reg_2_vars  <- c("precip_annual_mean","I(precip_annual_mean^2)")
  reg_3_vars  <- c("precip_annual_mean","precip_rolling_avg_5_years")
  reg_4_vars  <- c("precip_annual_mean","precip_rolling_avg_5_years","rural")
  reg_5_vars  <- c("precip_annual_mean","precip_rolling_avg_5_years","DIST_UP_KM")
  reg_6_vars  <- c("precip_annual_mean","precip_rolling_avg_5_years","DIST_UP_KM","precip_annual_mean:DIST_UP_KM")
  reg_7_vars  <- c("precip_annual_mean","precip_rolling_avg_5_years","precip_rolling_avg_5_years:DIST_UP_KM")
  
reg_1_form <- reg_equation(outcome_var = "infant_death_1000_exposure",
                           regressor_vars = reg_3_vars,
                           fe_vars = NULL)

reg_2_form <- reg_equation(outcome_var = "infant_death_1000_exposure",
                           regressor_vars = reg_3_vars,
                           fe_vars = "DHSID")

reg_3_form <- reg_equation(outcome_var = "infant_death_1000_exposure",
                           regressor_vars = reg_3_vars,
                           fe_vars = c("DHSID","year"))

reg_4_form <- reg_equation(outcome_var = "infant_death_1000_exposure",
                           regressor_vars = reg_7_vars,
                           fe_vars = NULL)

reg_5_form <- reg_equation(outcome_var = "infant_death_1000_exposure",
                           regressor_vars = reg_7_vars,
                           fe_vars = c("DHSID"))

reg_6_form <- reg_equation(outcome_var = "infant_death_1000_exposure",
                           regressor_vars = reg_7_vars,
                           fe_vars = c("DHSID","year"))

#for (i in 1:2){
if (i==1) {
  current_data     <- era5_gps_childmort_train2
  filestub <- "no_rivers_training"
  name_stub     <- "DHS Data: 2/3 Random Sample"
} else {
  current_data = era5_gps_childmort_train_namibia
  filestub <- "no_rivers_namibia"
  name_stub     <- "DHS Data: 2/3 Random Sample of Namibia"
}

models <- list(
  "(1)" = feols(reg_1_form , data = current_data),
  "(2)" = feols(reg_2_form , data = current_data),
  "(3)" = feols(reg_3_form,  data = current_data),
  "(4)" = feols(reg_4_form,  data = current_data),
  "(5)" = feols(reg_5_form,  data = current_data),
  "(6)" = feols(reg_6_form,  data = current_data)
  
  
  
)


# get the LM output in the console so we can check that the covariate labels are correct
models[[1]]
models[[2]]
models[[3]]
models[[4]]
models[[5]]
models[[6]]


## Set  Labels

title_tab_01 <- paste0("Precipitation and Mortality", name_stub, "\\label{tab:basic-reg-tab}")

cov_labels <- c("Intercept",
                "Annual avg precip",
                "5-year avg precip",
                "5-yr precip x Dist to source")

if (i == 1){
my_notes <- c(paste0("Outcome is infant deaths per 1000 infants observed. Precipitation data from ERA5 (2023), in millimeters per month (averaged over the year); river data from HydroSHEDS (2022); DHS surveys from 1988 to 2023, covering ",length(unique(era5_gps_childmort_train2$DHSID)), " towns on ",length(unique(era5_gps_childmort_train2$MAIN_RIV))," rivers over 1957-2022. Omits Madagascar and any rivers with rainfall above the median of river-level mean precipitation."))
} else {
  my_notes <- c("Outcome is infant deaths per 1000 infants observed. Precipitation data from ERA5 (2023); river data from HydroSHEDS (2022); DHS surveys from 1988 to 2023, covering 538 towns on 576 rivers over 1958-2022. ")
  
}


options(modelsummary_format_numeric_latex = "plain") # there was a "\num{}# argument wrapping around the latex tables
options(modelsummary_factory_html = 'kableExtra')



modelsummary(models,
             stars = FALSE,
             vcov = "HC1",
               # list(~dyad_id+year,~dyad_id+year,~dyad_id+year,
               #      ~dyad_id+year), # stata's heteroskedasticity robust standard errors
             #statistic = "conf.int",
             #coef_rename = cov_labels,
             #output = "latex",
             #add_rows = rows,
             title = title_tab_01,
             gof_omit = "AIC|BIC|RMSE|Log.Lik|Std.Errors|:|Within|Adj." # omit several of the goodness of fit stats
) # %>%
  #add_header_above(c(" "=1, "(NNMR-U)/(NNMR-D)" = 2, "(IMR-U)/(IMR-D)"=2))  # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns


path <- file.path(output_tables,"regressions")
if (!dir.exists(path)) dir.create(path, recursive = TRUE) # recursive lets you create any needed subdirectories

out_path <- file.path(path,paste0("reg-tab-01_",filestub,".tex"))

# add dependent variable means 
# get length of the number of unique regressors
n_total_regressor_vars <- c(reg_3_vars,reg_7_vars) %>% unique() %>% length()

rows <- data.frame("term" = c("Mean","Cluster FE","Year FE"),
                   "(1)"  = c(round(mean(current_data$infant_death_1000_exposure),2),"N","N"),
                   "(2)"  = c(round(mean(current_data$infant_death_1000_exposure),2),"Y","N"),
                   "(3)"  = c(round(mean(current_data$infant_death_1000_exposure),2),"Y","Y"),
                   "(4)"  = c(round(mean(current_data$infant_death_1000_exposure),2),"N","N"),
                   "(5)"  = c(round(mean(current_data$infant_death_1000_exposure),2),"Y","N"),
                   "(6)"  = c(round(mean(current_data$infant_death_1000_exposure),2),"Y","Y"))

attr(rows, 'position') <- c(2*n_total_regressor_vars+3,2*n_total_regressor_vars+4,2*n_total_regressor_vars+5) # this should put it right after the number of observations
# there are 2 rows per regressor var, plus the Number of Observations, plus 2 for the intercept, and then put the Num Obs at 
# the next one down
# 
# 
modelsummary(models,
             stars = FALSE,
             vcov = "HC1",
             #   list(~dyad_id+year,~dyad_id+year,~dyad_id+year,
             #        ~dyad_id+year), # stata's heteroskedasticity robust standard errors
             # #statistic = "conf.int",
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
  #add_header_above(c(" "=1, "(NNMR-U)/(NNMR-D)" = 2, "(IMR-U)/(IMR-D)"=2)) %>% # add a header that gives the years, a blank over the varnames then the other two spanning 3 columns
 kable_styling(latex_options = "HOLD_position") %>% # changes the latex placement to [H] (here), put this table in the latex doc where I say it goes
  save_kable(
    file = out_path,
    format = "latex"
  )

}

# Regression Table 02



## Set  Labels

title_tab_02 <- paste0("Precipitation and Mortality: Border Crossings ",countries_title[i]," \\label{tab:",countries,"-reg-tab}")

cov_labels <- c("Annual Avg Precip (mm/month) (U)","Annual Avg Precip (D)","Precip (Current - Long-run SD) (U)","Precip (Current - Long-run SD) (D)","Precip (U) x Cross-Country","Precip (U) x Cross Adm 1","Precip (U) x Cross Adm 2")
my_notes <- c("(U): upstream; (D) downstream. Long-run SD: annual standard deviation of precipitation from January 1940 to July 2023. Data from ERA5 (2023); DHS surveys from 1988 to 2019. Mortality rates were truncated above median, below 90th percentile because of small exposed population sizes.")



options(modelsummary_format_numeric_latex = "plain") # there was a "\num{}# argument wrapping around the latex tables
options(modelsummary_factory_html = 'kableExtra')





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

