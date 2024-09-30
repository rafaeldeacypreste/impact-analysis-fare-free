# Impact analysis of the zero-fare program on public transport
# Author: Rafael de Acypreste
# Advisor: Profa. Thais Carvalho Valadares Rodrigues


#___________________________________________________#
#             Summary
# In this file, we will estimate the impact of the zero-fare
# program on public transport in Brazil.

# We will use the difference-in-differences (DID) model
# to estimate the impact of the program on the collection
# of the ISS tax and the total tax collection. 
#___________________________________________________#



# Load the necessary libraries ----
load_packages_did <- function(){ 
  
library(did)       # Difference-in-differences
library(gtsummary) # Summary tables
library(labelled)  # Label data

options(scipen = 999)  # To avoid scientific notation
}

load_packages_did()

  # Sourcing R files ----

  # Functions and libraries
source("src/functions.r")

  # Data importation
  # Warning: check if the database is updated
if(!file.exists("data/fare_free_municipalities_2003_2021.csv")){
  
  source(file.path(getwd(), "src", "data-importation.r"))  
  
} else {
  
  tbl_fare_free <- read_csv("data/fare_free_municipalities_2003_2021.csv")
  
}



#___________________________________________________#
#          Tranforming variables ---- 
#___________________________________________________#

tbl_did <- 
tbl_fare_free |> 
    mutate(treated = if_else(is.na(start_year) | start_year > year, 0, 1), # Variable to descriptive analysis
           ) |> 
    filter(!is.na(gdp_current),
           total_employment > 0,           # Negative gdp is not reasonable and prohibits log transformation
           gdp_current > 0 ) |>  # Zero employment prohibits log transformation
    mutate(log_iss       = log(iss_collection),
           iss_per_capita = iss_collection / pop_datasus,
           tax_minus_iss = total_tax_collection - iss_collection,
           log_res_taxes = log(tax_minus_iss),
           log_total_tax = log(total_tax_collection),
           log_emp       = log(total_employment),
           log_mean_wage = log(mean_wage),
           log_gdp       = log(gdp_current),
           log_pop       = log(pop_datasus),
           log_pop_ibge  = log(pop_estimation),
           log_area      = log(territorial_area_2010),
           log_auto      = log(AUTOMÓVEL),
           ) |> 
   replace_na(list(start_year = 0)) |>    # 0 to represent never treated (did package requirement)
   mutate(any_treated = if_else(start_year >= 2003 &
                                start_year <= 2019, "Treated", "Untreated")) 
  


#___________________________________________________#
#          Pre-treatment of the variables ---- 
#___________________________________________________#



## Mean wage outliers ----

  # Checking the treated units
treated_units <-
  tbl_did |> 
  filter(treated == 1) |> 
  select(ibge_code) |>    # At least one treated
  distinct() |> 
  pull()

  # Rule for considering the units as outliers
wage_outliers <- 
tbl_did |> 
  mutate(IQR = IQR(mean_wage),
         wage_upper = quantile(mean_wage, probs = 0.75) + 7.5 * IQR,
         wage_lower = quantile(mean_wage, probs = 0.25) - 1.5 * IQR,
         outlier = if_else(mean_wage > wage_upper | mean_wage < wage_lower, 1, 0),
         .by = c(year)) |> 
  select(ibge_code, year, outlier, mean_wage, wage_upper, IQR) |> 
  filter(outlier == 1) |> 
  pull(ibge_code) |> 
  unique()


treated_lost <- wage_outliers[wage_outliers %in% treated_units]

cat("Treated units lost due to wage outliers (IBGE codes):", str_c(treated_lost, collapse = ", "))


  # Adjusted table
tbl_did_wage_outliers <- 
  tbl_did |> 
  filter(!ibge_code %in% wage_outliers)



#___________________________________________________#
#          Data exploratory analysis  ----
#___________________________________________________#

  # Units by tretament status (staggered adoption)
# plot_staggered <- 
# panelview(data             = tbl_applied_to_did,
#           Y                = "log_iss",
#           D                = "treated",
#           X                = c("log_gdp", "log_pop", "log_area"),
#           index            = c("ibge_code", "year"),
#           pre.post         = TRUE,
#           display.all      = TRUE,
#           collapse.history = TRUE,
#           by.timing        = TRUE,
#           xlab             = "Year")



  # Identification of the unique federation units with at least one treated municipality
federations_units_with_treated <- 
  tbl_did |> 
  filter(treated == 1) |> 
  select(federation_unit) |> # At least one treated
  distinct() |> 
  pull()


  # Labeling variables to show in the summary tables
var_label(tbl_did) <- list(
  log_iss = "ISS collection",
  log_pop = "Population",
  log_gdp = "GDP",
  log_area = "Territorial area"
)

year_ref <- 2010

  # Example of descriptive table
tbl_descriptive(year_ref = year_ref)

  # Example of descriptive density plot
density_plot(year_ref = year_ref)



#___________________________________________________#
#          DiD estimations  ----
#___________________________________________________#


# The did package allows for incorporating covariates so that the parallel trends assumption
# holds only after conditioning on these covariates. 
# The did package requires that covariates be time-invariant. For time varying covariates,
# the did package sets the value of the covariate to be equal to the value of the covariate
# in the “base period” where, in post-treatment periods the base period is the period immediately 
# before observations in a particular group become treated (when there is anticipation, it is before
# anticipation effects start too), and in pre-treatment periods the base period is the period right
# before the current period. Covariates are passed as a formula to the did package by setting, 
# for example, xformla = ~lpop. For estimators under unconditional parallel trends, the xformla
# argument can be left blank or can be set as xformla = ~1 to only include a constant.


# The logaritmic transformation is necessary to avoid the problem of heteroscedasticity 
# and to make the interpretation of the coefficients easier. 
# In the log-log model, the coefficient of the treatment variable is the elasticity of the outcome variable.
# The log transformation is a common practice in the literature of public finance and economics.

# Furthermore, the linear model turns IPW harder or even impossible to estimate.


alpha_applied <- 0.05

#___________________________________________________#
##          DiD in ISS   ----
#___________________________________________________#


did_iss <- model_did(
  data           = tbl_did,
  outcome        = "log_iss",
  group_name     = "start_year",
  covariates     = c("log_gdp", "log_pop", "log_area"),
  time           = "year",
  dummy_treated  = "treated",
  period         = c(2003, 2019),
  id             = "ibge_code",
  only_fed_units = TRUE,
  bootstrap      = FALSE,
  base_period    = "universal",
  federation_col = "federation_unit",
  control        = "notyettreated",
  digits         = 3,
  alpha          = alpha_applied
)




# Comparison of the three aggregation methods
models_to_get <- list(did_iss$group_aggte,
                      did_iss$dyn_aggte,
                      did_iss$calendar_aggte)

# Names of the aggregation methods
aggregation_to_test <- c("Group", "Dynamic", "Calendar-time")

# Comparison of the three aggregation methods
tbl_comparison <-
  map2_dfr(
    models_to_get,
    aggregation_to_test,
    \(x, y) get_overall_att_ci_comparison(
      did_model   = x,
      aggregation = y,
      covariates  = "GDP, Population and Area"
    )
  ) 




#___________________________________________________#
##          DiD in ISS (Pop and GDP)   ----
#___________________________________________________#


did_iss_pop_gdp <- model_did(
  data           = tbl_did,
  outcome        = "log_iss",
  group_name     = "start_year",
  covariates     = c("log_gdp", "log_pop"),
  time           = "year",
  dummy_treated  = "treated",
  period         = c(2003, 2019),
  id             = "ibge_code",
  only_fed_units = TRUE,
  bootstrap      = FALSE,
  base_period    = "universal",
  federation_col = "federation_unit",
  control        = "notyettreated",
  digits         = 3,
  alpha          = alpha_applied
)


  # Comparison of the three aggregation methods
models_to_get <- list(did_iss_pop_gdp$group_aggte,
                      did_iss_pop_gdp$dyn_aggte,
                      did_iss_pop_gdp$calendar_aggte)


  # Comparison of the three aggregation methods
tbl_comparison <-
  map2_dfr(
    models_to_get,
    aggregation_to_test,
    \(x, y) get_overall_att_ci_comparison(
      did_model   = x,
      aggregation = y,
      covariates  = "GDP and Population"
    )
  ) |>
  bind_rows(tbl_comparison)


#___________________________________________________#
##          DiD in ISS (Pop and Area)   ----
#___________________________________________________#


did_iss_pop_area <- model_did(
  data           = tbl_did,
  outcome        = "log_iss",
  group_name     = "start_year",
  covariates     = c("log_gdp", "log_area"),
  time           = "year",
  dummy_treated  = "treated",
  period         = c(2003, 2019),
  id             = "ibge_code",
  only_fed_units = TRUE,
  bootstrap      = FALSE,
  base_period    = "universal",
  federation_col = "federation_unit",
  control        = "notyettreated",
  digits         = 3,
  alpha          = alpha_applied
)


models_to_get_pop_area <- list(did_iss_pop_area$group_aggte,
                      did_iss_pop_area$dyn_aggte,
                      did_iss_pop_area$calendar_aggte)


tbl_comparison <-
  map2_dfr(
    models_to_get_pop_area,
    aggregation_to_test,
    \(x, y) get_overall_att_ci_comparison(
      did_model   = x,
      aggregation = y,
      covariates  = "Population and Area"
    )
  ) |>
  bind_rows(tbl_comparison)



#___________________________________________________#
##          DiD in ISS (without covariates)   ----
#___________________________________________________#


did_iss_without <- model_did(
  data           = tbl_did,
  outcome        = "log_iss",
  group_name     = "start_year",
  covariates     = NULL,
  time           = "year",
  dummy_treated  = "treated",
  period         = c(2003, 2019),
  id             = "ibge_code",
  only_fed_units = TRUE,
  bootstrap      = FALSE,
  base_period    = "universal",
  federation_col = "federation_unit",
  control        = "notyettreated",
  digits         = 3,
  alpha          = alpha_applied
)


models_to_get_intercept <- list(
  did_iss_without$group_aggte,
  did_iss_without$dyn_aggte,
  did_iss_without$calendar_aggte
)


tbl_comparison <- 
map2_dfr(models_to_get_intercept,
         aggregation_to_test,
         \(x,y) get_overall_att_ci_comparison(did_model = x,
                                              aggregation = y)) |> 
  bind_rows(tbl_comparison) |> 
  arrange(desc(Aggregation))



#___________________________________________________#
##           Prototypes ----

##          DiD in employment   ----
#___________________________________________________#

eval_did_emp <- FALSE

if(eval_did_emp){
did_emp <- model_did(
  data           = tbl_did,
  outcome        = "log_emp",
  group_name     = "start_year",
  covariates     = c("log_gdp", "log_pop", "log_area"),
  time           = "year",
  dummy_treated  = "treated",
  period         = c(2003, 2021),
  id             = "ibge_code",
  only_fed_units = FALSE,
  bootstrap      = FALSE,
  federation_col = "federation_unit",
  base_period    = "universal",
  digits         = 3,
  alpha          = alpha_applied
)

}

#___________________________________________________#
##          DiD in wages   ----
#___________________________________________________#

eval_did_wages <- FALSE

if(eval_did_wages){

did_wages <- model_did(
  data           = tbl_did_wage_outliers,
  outcome        = "log_mean_wage",
  group_name     = "start_year",
  covariates     = c("log_gdp", "log_pop", "log_area"),
  time           = "year",
  dummy_treated  = "treated",
  period         = c(2003, 2021),
  id             = "ibge_code",
  only_fed_units = TRUE,
  bootstrap      = FALSE,
  federation_col = "federation_unit",
  base_period    = "universal",
  digits         = 3,
  alpha          = alpha_applied
)

ggdid(did_wages$group_aggte)
ggdid(did_wages$dyn_aggte)

}
#___________________________________________________#
#          Parallel trends testing  ----
#___________________________________________________#



iss_event_study <- test_PT_event_study(
  data_to_study = tbl_did,
  outcome       = "log_iss",
  covariates    = c("log_gdp", "log_pop", "log_area"),
  id            = "ibge_code",
  time          = "year",
  alpha         = alpha_applied,
  n_periods     = 15
)






#___________________________________________________#
#          Pre-test for parallel trends with honestDiD ----
#___________________________________________________#




iss_sensi_analysis <- apply_sensitivity_analysis(dyn_model_summary = did_iss$dyn_aggte,
                                                 type_sensitivity  = "smoothness")






