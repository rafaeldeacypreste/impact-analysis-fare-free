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
           # AUTOMÓVEL > 0,
           # pop_estimation < 100000, # It does not change the results significantly
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






#___________________________________________________#
#          Backlog  ----
#___________________________________________________#
 


# 1. Check the parallel trends assumption
# 2. Add total tax
# 3. Write analysis
# 4. Add other outcome variables
# 5. Test some grouping units to use as control


# Potential problemas

# Small group sizes can sometimes cause estimation problems
# in the did package. To give an example, if there are any
# groups that have fewer observations than the number of covariates
# in the model, the code will error. 

# https://bcallaway11.github.io/did/articles/did-basics.html

























#___________________________________________________#
#          Draft before model_did() function  ----
#___________________________________________________#




run_draft <- FALSE


if(run_draft){
  



### Balancing data ----

treated_units <-
  tbl_did |> 
  filter(treated == 1) |> 
  select(ibge_code) |> # At least one treated
  distinct() |> 
  pull()


missing_iss_values <-
  map(
    c("log_iss", "log_gdp", "log_pop", "log_area"), # Variables in the model
    \(x) get_unbalanced_units(variable = x)
  ) |>
  unlist()


# Test if some treated units are excluded
sum(treated_units %in% missing_iss_values )


tbl_did <-
  tbl_did |>
  filter(year >= 2003, 
         year <= 2019,
         !ibge_code %in% missing_iss_values)


# Defaulf option
tbl_applied_to_did <- tbl_did


## Treating control group ----
# Reducing the sample to control for some grouping units (test if the results are robust to the sample)
reduce_sample <- FALSE

if(reduce_sample){  # Test some grouping units to use as control 
  max_extra <- 1.01
  min_extra <- 0.99
  
  tbl_did_threshold <-
    tbl_did |>
    summarise(
      max_log_iss = max(log_iss, na.rm = TRUE) * max_extra,
      min_log_iss = min(log_iss, na.rm = TRUE) * min_extra,
      max_log_total_tax = max(log_total_tax, na.rm = TRUE) * max_extra,
      min_log_total_tax = min(log_total_tax, na.rm = TRUE) * min_extra,
      max_log_gdp = max(log_gdp, na.rm = TRUE) * max_extra,
      min_log_gdp = min(log_gdp, na.rm = TRUE) * min_extra,
      max_log_pop = max(log_pop, na.rm = TRUE) * max_extra,
      min_log_pop = min(log_pop, na.rm = TRUE) * min_extra,
      max_log_area = max(log_area, na.rm = TRUE) * max_extra,
      min_log_area = min(log_area, na.rm = TRUE) * min_extra,
      .by = treated
    )  |>
    filter(treated == 1)
  
  
  tbl_did_reduced  <-
    tbl_did |>
    filter(
      log_iss < tbl_did_threshold$max_log_iss,
      log_iss > tbl_did_threshold$min_log_iss,
      log_pop < tbl_did_threshold$max_log_pop,
      log_pop > tbl_did_threshold$min_log_pop,
      log_gdp < tbl_did_threshold$max_log_gdp,
      log_gdp > tbl_did_threshold$min_log_gdp,
      log_area < tbl_did_threshold$max_log_area,
      log_area > tbl_did_threshold$min_log_area
    )
  
  # Checking the number of grouping units  
  dim(tbl_did_reduced)
}


## Reducing the sample by federative unit with at least one treated ----

reduce_by_federative_unit <- TRUE


if(reduce_by_federative_unit){
  
  
  
  federations_units_with_treated <-
    tbl_did |> 
    filter(treated == 1) |> 
    select(federation_unit) |> # At least one treated
    distinct() |> 
    pull()
  
  
  tbl_did_reduced  <-
    tbl_did |>
    filter(federation_unit %in% federations_units_with_treated)
  
  # Checking the number of grouping units
  
  
  # Change the table to be applied
  tbl_applied_to_did <- tbl_did_reduced
  
  
}






### First DiD model ----

# Significance level
alpha <- 0.05

### Covariates: log_pop and log_gdp ----
did_gdp_pop <- att_gt(yname   = "log_iss",           # Outcome variable
                      tname   = "year",
                      gname   = "start_year",        # Group by year of implementation
                      idname  = "ibge_code",
                      xformla = ~ log_gdp + log_pop, # Covariates
                      alp     = alpha, 
                      bstrap  = TRUE,
                      cband   = TRUE,
                      data    = tbl_applied_to_did
)


# summarize the results
summary(did_gdp_pop)
names(did_gdp_pop)


# Details of the aggregate procedures in 
# https://bcallaway11.github.io/did/articles/did-basics.html

#### Simple aggregation of the group-time average treatment effects ----
# Weighted average of all group-time average treatment effects with weights
# proportional to the group size. 

# Limitations:  aggregation tends to overweight the effect of early-treated 
# groups simply because we observe more of them during post-treatment periods. 
# Probably, it is not the best way to aggregate the results.

did_gdp_pop_simple <- aggte(did_gdp_pop, type = "simple", alp  = alpha)
summary(did_gdp_pop_simple)

# Overall average treatment effect
did_gdp_pop_simple$overall.att

# Confidence interval
(ci_gdp_pop_simple <- get_overall_att_ci(did_gdp_pop_simple, alpha = alpha))

# round(ci_gdp_pop_simple, 3)


#### Dynamic aggregation of the group-time average treatment effects ----

# Aggregates the group-time average treatment effects, i.e,
# average treatment effects at different lengths of exposure to the treatment

# Length of exposure equal to 0 provides the average effect of participating 
# in the treatment across groups in the time period when they first participate
# in the treatment (instantaneous treatment effect).

# Length of exposure equal to -1  corresponds to the time period before groups
# first participate in the treatment, and length of exposure
# equal to 1 corresponds to the first time period after initial exposure to the treatment.

did_gdp_pop_dyn <- aggte(did_gdp_pop, type = "dynamic")
summary(did_gdp_pop_dyn)


# Table of the results
summary_did_gdp_pop_dyn <- get_avg_summary_table(did_gdp_pop_dyn, digits = 3)

# Overall ATT: averages the average treatment effects across all lengths of exposure to the treatment.
did_gdp_pop_dyn$overall.att
(ci_gdp_pop_dyn <- get_overall_att_ci(did_gdp_pop_dyn, alpha = alpha))




# aggregate the group-time average treatment effects
# One way to combat this is to balance the sample by (i) only including groups
# that are exposed to the treatment for at least a certain number of time periods and
# (ii) only look at dynamic effects in those time periods.
# In the did package, one can do this by specifying the balance_e option. 
did_gdp_pop_dyn_bal_i <- aggte(did_gdp_pop,
                               type      = "dynamic",
                               balance_e = 5)

summary(did_gdp_pop_dyn_bal_i)

# Table of the results
(summary_did_gdp_pop_dyn_bal_i <- get_avg_summary_table(did_gdp_pop_dyn_bal_i,
                                                        digits = 3)
)



#### Group aggregation of the group-time average treatment effects ----
# ggregate group-time average treatment effects into 
# group-specific average treatment effects

did_gdp_pop_group <- aggte(did_gdp_pop, type = "group")



# Overall ATT: averages the group-specific treatment effects across groups.
# In our view, this parameter is a leading choice as an overall summary
# effect of participating in the treatment
# It is the average effect of participating in the treatment that was
# experienced across all units that participate in the treatment in any period.


summary(did_gdp_pop_group)
(summary_did_gdp_pop_group <- get_avg_summary_table(did_gdp_pop_group))

get_overall_att_ci(did_gdp_pop_group)



#### Calendar aggregation of the group-time average treatment effects ----
# average effect of participating in the treatment in a particular time period
# for all groups that participated in the treatment in that time period.

did_gdp_pop_calendar <- aggte(did_gdp_pop, type = "calendar")
summary(did_gdp_pop_calendar)

(summary_did_gdp_pop_calendar <- get_avg_summary_table(did_gdp_pop_calendar))


# Overall ATT: averages the group-time average treatment effects across all time periods.
get_overall_att_ci(did_gdp_pop_calendar)




did_pop_gdp_area_comaring <- model_did()


## Covariates: log_pop, log_gdp, log_area ----
did_pop_gdp_area <- 
  att_gt(yname   = "log_iss",
         tname   = "year",
         gname   = "start_year",
         idname  = "ibge_code",
         xformla = ~ log_pop + log_gdp + log_area,
         alp     = alpha, 
         bstrap  = FALSE,
         cband   = TRUE,
         data    = tbl_applied_to_did
  )

teste$panel_data


teste <- model_did()


tbl_did |> colnames()

teste_emp <- model_did(outcome = "log_emp",
                       period = c(2003, 2019))

teste_wage <- model_did(outcome = "mean_wage",
                        period = c(2003, 2019))

# summarize the results
did_pop_gdp_area_simple <- aggte(did_pop_gdp_area, type = "simple")
summary(did_pop_gdp_area_simple)
teste$simple_aggte



### aggregate the group-time average treatment effects ----
did_pop_gdp_area_dyn <- aggte(did_pop_gdp_area, type = "dynamic")
summary(did_pop_gdp_area_dyn)

teste$dyn_aggte |> summary()
teste_emp$dyn_aggte |> summary()
teste_wage$dyn_aggte |> summary()


(summary_did_pop_gdp_area_dyn <- 
    get_avg_summary_table(did_pop_gdp_area_dyn, digits = 3))


# Overall ATT
get_overall_att_ci(did_pop_gdp_area_dyn)



### aggregate the group-time average treatment effects (balance) ----

did_pop_gdp_area_dyn_bal_i <- aggte(did_pop_gdp_area,
                                    type = "dynamic",
                                    balance_e = 5)
summary(did_pop_gdp_area_dyn_bal_i)

# Overall ATT
get_overall_att_ci(did_pop_gdp_area_dyn_bal_i)
ggdid(did_pop_gdp_area_dyn_bal_i)


### Group aggregation of the group-time average treatment effects ----

did_pop_gdp_area_group <- aggte(did_pop_gdp_area, type = "group")
summary(did_pop_gdp_area_group)

(summary_did_pop_gdp_area_group <- 
    get_avg_summary_table(did_pop_gdp_area_group))

# Overall ATT
get_overall_att_ci(did_pop_gdp_area_group)
ggdid(did_pop_gdp_area_group)

# Calendar aggregation of the group-time average treatment effects
did_pop_gdp_area_calendar <- aggte(did_pop_gdp_area, type = "calendar")
summary(did_pop_gdp_area_calendar)

(summary_did_pop_gdp_area_calendar <- 
    get_avg_summary_table(did_pop_gdp_area_calendar))

# Overall ATT
get_overall_att_ci(did_pop_gdp_area_calendar)

ggdid(did_pop_gdp_area_calendar)


}




