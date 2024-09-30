# Impact analysis of the zero-fare program on public transport
# Author: Rafael de Acypreste
# Advisor: Profa. Thais Carvalho Valadares Rodrigues


# Function to load the necessary libraries ----

load_packages <- function(){ 
  
  
# Load the necessary libraries ----
library(readxl)    # To read Excel files
library(dplyr)     # To manipulate data
library(readr)     # To read CSV files
library(tidyr)     # To manipulate data
library(purrr)     # To manipulate data
library(sidrar)    # To access IBGE data
library(stringr)   # To manipulate strings
library(writexl)   # To write Excel files
library(did)       # To estimate the DID model
library(ggplot2)   # To plot data
library(Matrix)    # To manipulate matrices
library(panelView) # To create interactive tables
library(ggrepel)   # For adjust labels in ggplot2
library(gtsummary) # Summary tables
library(labelled)  # Label data
library(gt)        # To create tables in latex format
library(kableExtra)# To create tables

  
  
# Non-CRAN packages
if(!require("roadtrafficdeaths")){
  devtools::install_github("pabsantos/roadtrafficdeaths")
}
library(roadtrafficdeaths) # To access the road traffic deaths data in Brazil

options(scipen = 999)  # To avoid scientific notation
  

}

# Load the necessary libraries 
load_packages()



#___________________________________________________#
#    Unzip fleet functions ----
#___________________________________________________#


#' Unzip the fleet data from 2003 to 2009
#'
#' @param year_tbl           The year of the table
#' @param zip_specific       The name of the zip file
#' @param zip_specific_table The name of the table inside the zip file  
#' @param sheet              The sheet number or name to read
#' @param skip               The number of rows to skip before reading
#' @param colnames           The column names aiming to further bind the tables by row
#'
#' @return A tibble with the fleet data
#' @export
#'
#' @examples
unzip_fleet_2003_2009 <-
  function(year_tbl,
           zip_specific,
           zip_specific_table,
           sheet = 1,
           skip  = 1,
           colnames) {
    
    
    
    # Unzip the first layer
    zip_path <- file.path("data", "raw_fleet", paste0(year_tbl, ".zip"))
    exdir_path <- file.path("data", "raw_fleet")
    
    unzip(zip_path, files = zip_specific, exdir = exdir_path)
    
    # Unzip the second layer
    zip_second_layer <- file.path("data", "raw_fleet", zip_specific)
    exdir_path <- file.path("data", "fleet")
    
    unzip(zip_second_layer, files = zip_specific_table, exdir = exdir_path)
    
    
    # Read the Excel file
    if(year_tbl == 2003){  # The column names are going to be the same of 2003

      table <-
        read_xls(
          file.path("data", "fleet", zip_specific_table),
          skip  = skip,
          sheet = sheet
        ) |>
        mutate(year = year_tbl)
      
    } else {
      table <-
        read_xls(
          file.path("data", "fleet", zip_specific_table),
          skip      = skip,
          sheet     = sheet,
          col_names = colnames
        ) |>
        mutate(year = year_tbl)
      }
    
    return(table)
    
    
  }


#' Unzip the fleet data from 2010 to 2012
#'
#' @param year_tbl            The year of the table 
#' @param zip_specific_table  The name of the table inside the zip file
#' @param sheet               The sheet number or name to read
#' @param skip                The number of rows to skip before reading
#' @param colnames            The column names aiming to further bind the tables by row
#'
#' @return                    A tibble with the fleet data
#' @export
#'
#' @examples
unzip_fleet_2010_2012 <-
  function(year_tbl,
           zip_specific_table,
           sheet = 1,
           skip = 1,
           colnames) {
    
    
    
    # Unzip the first layer
    zip_path <- file.path("data", "raw_fleet", paste0(year_tbl, ".zip"))
    exdir_path <- file.path("data", "fleet")
    
    unzip(zip_path, files = zip_specific_table, exdir = exdir_path)
    
      # Reorginize the folder structure (to further store the main files)
    old_path <- file.path("data", "fleet", zip_specific_table)
    new_path <- file.path("data", "fleet", paste0("Frota_", year_tbl, ".xls"))
    file.rename(old_path, new_path)
    
    
    # Read the Excel file
    table <-
      read_xls(new_path,
               skip      = skip,
               sheet     = sheet,
               col_names = colnames) |>
      mutate(year = year_tbl) 
    
    return(table)
    
  }


#___________________________________________________#
#       Replace Portuguese special characters ----
#___________________________________________________#


#' Replace Portuguese special characters
#'
#' @param variable A character vector (or a column)
#'
#' @return A character vector with the special characters replaced
#' @export 
#'
#' @examples

replace_special_char <- function(variable) {

  variable |>
    str_replace_all("[áàâãä]", "a") |>
    str_replace_all("[éèêë]", "e") |>
    str_replace_all("[íìîï]", "i") |>
    str_replace_all("[óòôõö]", "o") |>
    str_replace_all("[úùûü]", "u") |>
    str_replace_all("[ç]", "c")
  
}




#___________________________________________________#
#       Get unbalaced units ----
#___________________________________________________#



#' Get unbalanced units
#'
#' @param data       Complete dataset
#' @param year_min   Minimum year of intereste
#' @param year_max   Maximum year of interest
#' @param variable   Variable to check completeness
#' @param to_filter  Variable to be further used as filter
#'
#' @return           A vector with the unbalanced units
#' @export
#'
#' @examples
get_unbalanced_units <- function(data = tbl_did,
                                 year_min  = 2003,
                                 year_max  = 2019,
                                 variable  = "iss_collection",
                                 to_filter = "ibge_code")  {
  
  
  
  variable <- sym(variable)
  to_filter <- sym(to_filter)
  n_completeness <- year_max - year_min + 1
  
  data |>
    filter(year >= year_min, year <= year_max) |> # Considering the period of the program
    filter(!is.na(!!variable)) |>
    count(!!to_filter) |>
    filter(n < n_completeness) |>                          # Variable to be used as filter
    select(!!to_filter) |>
    pull()
  
}



#___________________________________________________#
#       Summary table for DiD ----
#___________________________________________________#



## Overall ATT Confidence interval ----

get_overall_att_ci <- function(did_model,
                               alpha = 0.05,
                               digits = 3,
                               percent = FALSE) {
  
  if(percent){
  
    ci <-  c(
      (did_model$overall.att - qnorm(1 - alpha / 2) * did_model$overall.se) * 100,
      (did_model$overall.att + qnorm(1 - alpha / 2) * did_model$overall.se) * 100
    )
    
    ci <- str_c(round(ci, digits), collapse = "%, ")
    ci_label <- str_c("[", ci, "%]")
    
    
  } else {
  
    ci <-  c(
      did_model$overall.att - qnorm(1 - alpha / 2) * did_model$overall.se,
      did_model$overall.att + qnorm(1 - alpha / 2) * did_model$overall.se
    )
    
    ci <- str_c(round(ci, digits), collapse = ", ")
    ci_label <- str_c("[", ci, "]")
      
  }
  

  
  return(ci_label)
  
}





## Overall ATT Confidence interval ----

get_overall_att_ci_comparison <- function(did_model,
                                          covariates = "Intercept only",
                                          aggregation = "Dynamic",
                                          digits = 3,
                                          alpha = 0.05) {
  ci <-  c(
    (
      did_model$overall.att - qnorm(1 - alpha / 2) * did_model$overall.se
    ),
    (
      did_model$overall.att + qnorm(1 - alpha / 2) * did_model$overall.se
    )
  )
  
  tbl_comparsicon <- 
  tibble("Covariate(s)" = covariates,
         Aggregation = aggregation,
         "Lower bound" = round(ci[1], digits),
         "Upper bound" = round(ci[2], digits),
         " " = if_else(ci[1] * ci[2] > 0, "*", ""))   # It does not cross the zero
  
  
  return(tbl_comparsicon)
  
}

## Average g(t) summary table ----


get_avg_summary_table <- function(did_model,
                                  aggregation_type = "none",
                                  digits = 3) {
  
  # Critical z value
  z_crit_value <- did_model$crit.val.egt
  
  
  # Summary table
  summary_table <-
    tibble(
      event_time   = did_model$egt,
      estimate_att = did_model$att,
      std_error    = did_model$se,
      lower_bound  = did_model$att - z_crit_value * did_model$se,
      upper_bound  = did_model$att + z_crit_value * did_model$se
    ) |>
    mutate(across(everything(), ~ round(., digits))) 
  
  


if(aggregation_type == "group"){

  n_units <- 
  did_model$DIDparams$data |> 
    filter(year == max(year)) |> 
    count(start_year, name = "number_units") |> 
    filter(start_year != 0) 
  
  summary_table  <-
  summary_table |> 
    left_join(n_units, by = c("event_time" = "start_year")) 
  
  
  }
  
  
if(aggregation_type == "calendar"){

  
  n_units  <- 
    did_model$DIDparams$data |> 
    filter(start_year != 0) |> 
    mutate(logic_year = start_year <= year) |> 
    summarise("number_units" = sum(logic_year), .by = year) |> 
    filter(number_units > 0) 
  
  summary_table  <-
    summary_table |> 
    left_join(n_units, by = c("event_time" = "year")) 
  
  
  }
  
summary_table  <- 
    summary_table |>
    mutate(statistical_significance = if_else(lower_bound * upper_bound > 0,
                                              "*",   # It does not cross the zero
                                              ""))
  return(summary_table)  
  
  
}






## Average g(t) summary table to latex ----


print_summary_latex <- function(table,
                                col_names = c("Event time",
                                              "Estimate (ATT)",
                                              "Std. Error",
                                              "95\\% Lower Bound",
                                              "95\\% Upper Bound",
                                              "Number of units",
                                              ""),
                                col_width = "1.5cm") {
  
  table |>
    kbl(booktabs  = TRUE,
        #longtable = TRUE,
        escape    = FALSE,
        col.names = col_names) |>
    footnote(general = "Signif. codes: `*' confidence band does not cover 0.") |>
    column_spec(1:6, width = col_width) |>
    kable_styling(bootstrap_options = "striped",
                  latex_options     = "hold_position")
  
  
}



#___________________________________________________#
#       Modeling DiD ----
#___________________________________________________#

#' Modeling DiD from DiD package
#'
#' @param data           dataset to be used
#' @param outcome        outcome variable
#' @param group_name     group variable
#' @param covariates     covariates to be used
#' @param time           time variable
#' @param dummy_treated  dummy variable for treated units
#' @param period         period of interest (default is 2003-2019)
#' @param id             id variable
#' @param only_fed_units =TRUE if only federation units with at least on municipality treated
#' @param bootstrap      =TRUE if bootstrap is to be used
#' @param base_period    "varying" or "universal" (read DiD package documentation)
#' @param control        control group (default is "nevertreated")
#' @param federation_col column with federation unit  
#' @param digits         number of digits to be used in the summary tables
#' @param alpha          alpha level for the confidence intervals
#'
#' @return A list with the results (overall ATT, average g(t) and summary table)
#' @export
#'
#' @examples
model_did <- function(data           = tbl_did,
                      outcome        = "log_iss",
                      group_name     = "start_year",
                      covariates     = c("log_gdp", "log_pop", "log_area"),
                      time           = "year",
                      dummy_treated  = "treated",
                      period         = c(2003, 2019),
                      id             = "ibge_code",
                      only_fed_units = TRUE,
                      bootstrap      = TRUE,
                      base_period    = "universal",
                      control        = "nevertreated",
                      federation_col = "federation_unit",
                      balance_agg    = 5,
                      digits         = 3,
                      alpha          = 0.05) {
  
  

  
  
  set.seed(2024)
  

  # Create the list of results
  
  results <- list()
  
  
  # Negate the `%in%` function
  `%not_in%` <- Negate(`%in%`)
  
  ## Symbolic variables ----
  id_var <- sym(id)
  time_var <- sym(time)
  dummy_treated <- sym(dummy_treated)
  federation_col <- sym(federation_col)
  
  
  
  
  ## Balancing data ----
treated_units <-
  data |> 
  filter(treated == 1) |> 
  select(id_var) |> # At least one treated
  distinct() |> 
  pull()


missing_values <- NA  
  
  if (!is.null(covariates)) {  

missing_values <-
  map(
    c(outcome, covariates),                 # Variables in the model
    \(x) get_unbalanced_units(variable  = x,
                              data      = data,
                              year_min  = period[1],
                              year_max  = period[2],
                              to_filter = id)
  ) |>
  unlist()
}

# Test if some treated units are excluded
cat("Quantity of treated units excluded: ", sum(treated_units %in% missing_values ))


# Filter the data
tbl_balanced <- 
  data |>
  filter(!!id_var %not_in% missing_values,
         !!time_var >= period[1],
         !!time_var <= period[2]) 

 # Filter only federations units with at least one treated unit
if(only_fed_units) {
  
  federations_units_with_treated <- 
    data |> 
    filter(!!dummy_treated == 1) |> 
    select(federation_col) |> # At least one treated
    distinct() |> 
    pull()
  
  
  tbl_balanced <- 
  tbl_balanced |>
    filter(!!federation_col %in% federations_units_with_treated)
}


results[["panel_data"]] <- tbl_balanced

results[["excluded_by_missing_values"]] <- paste(length(missing_values),
                                                 " units were excluded due to missing values.")



xformula_final <- NULL

  if (!is.null(covariates)) {

xformula_final <- as.formula(paste("~ ",
                                   paste(covariates,
                                         collapse = " + ")))

}
  ## Did modeling ----

did_model <-
att_gt(yname   = outcome,           # Outcome variable
       tname   = time,
       gname   = group_name,        # Group by year of implementation
       idname  = id,
       xformla = xformula_final, # Covariates
       alp           = alpha, 
       bstrap        = bootstrap,
       base_period   = base_period,
       cband         = TRUE,
       data          = tbl_balanced,
       control_group = control
)


results[["did_model"]] <- did_model

  ## Aggregates and summary tables ----

### Simple aggregation of the group-time average treatment effects ----
# Weighted average of all group-time average treatment effects with weights
# proportional to the group size. 

# Limitations:  aggregation tends to overweight the effect of early-treated 
# groups simply because we observe more of them during post-treatment periods. 
# Probably, it is not the best way to aggregate the results.

results[["simple_aggte"]] <- aggte(did_model, type = "simple", alp  = alpha)

### Dynamic aggregation of the group-time average treatment effects ----

# Aggregates the group-time average treatment effects, i.e,
# average treatment effects at different lengths of exposure to the treatment

# Length of exposure equal to 0 provides the average effect of participating 
# in the treatment across groups in the time period when they first participate
# in the treatment (instantaneous treatment effect).

# Length of exposure equal to -1  corresponds to the time period before groups
# first participate in the treatment, and length of exposure
# equal to 1 corresponds to the first time period after initial exposure to the treatment.

dyn_aggte <- aggte(did_model, type = "dynamic")


results[["dyn_aggte"]] <- dyn_aggte

 
# Table of the results
summary_dyn_aggte <- get_avg_summary_table(dyn_aggte,
                                           digits = digits)

results[["summary_dyn_aggte"]] <- summary_dyn_aggte


### Dynamic balanced aggregation of the group-time average treatment effects ----



dyn_aggte_bal <- aggte(did_model,
                       type      = "dynamic", 
                       balance_e = balance_agg)


results[["dyn_aggte_bal"]] <- dyn_aggte_bal

  # Calculate the number of balanced units
n_balanced <- 
dyn_aggte_bal$DIDparams$data |> 
  mutate(threshold_year = max(year) - balance_agg) |> 
  filter(year == threshold_year,
         start_year != 0,
         start_year <= threshold_year) |> 
  nrow() 
  
results[["n_balanced"]] <- n_balanced

# Table of the results
summary_dyn_aggte_bal <- get_avg_summary_table(dyn_aggte_bal, digits = digits)

results[["summary_dyn_aggte_bal"]] <- summary_dyn_aggte_bal



### Group aggregation of the group-time average treatment effects ----
# ggregate group-time average treatment effects into 
# group-specific average treatment effects

group_aggte <- aggte(did_model, type = "group")

results[["group_aggte"]] <- group_aggte

# Overall ATT: averages the group-specific treatment effects across groups.
# In our view, this parameter is a leading choice as an overall summary
# effect of participating in the treatment
# It is the average effect of participating in the treatment that was
# experienced across all units that participate in the treatment in any period.

summary_group_aggte <- get_avg_summary_table(group_aggte,
                                             aggregation_type = "group")

results[["summary_group_aggte"]] <- summary_group_aggte


### Calendar aggregation of the group-time average treatment effects ----
# average effect of participating in the treatment in a particular time period
# for all groups that participated in the treatment in that time period.

calendar_aggte <- aggte(did_model, type = "calendar")

results[["calendar_aggte"]] <- calendar_aggte


 # Summary tables
summary_calendar_aggte <- get_avg_summary_table(calendar_aggte,
                                                aggregation_type = "calendar")
results[["summary_calendar_aggte"]] <- summary_calendar_aggte



return(results)
}




#___________________________________________________#
#       Event - study ----
#___________________________________________________#


#' Event study to test the parallel trends assumption
#'
#' @param data_to_study dataset to study
#' @param outcome       outcome variable
#' @param covariates    covariates
#' @param id            id variable
#' @param time          time variable
#' @param alpha         significance level
#' @param n_periods     number of periods to study (max post-treatment periods)
#'
#' @return list with the results of the event study (summary statistics and plot)
#' @export
#'
#' @examples
test_PT_event_study <- function(data_to_study = tbl_did,
                                outcome       = "log_iss",
                                covariates    = c("log_gdp", "log_pop", "log_area"),
                                id            = "ibge_code",
                                time          = "year",
                                alpha         = 0.05,
                                n_periods     = 15) {
  
  
  list_of_results <- list()
  
  
  # Load the necessary libraries
  library(plm) # To estimate the event study
  
  
  time_of_exposure <- -(n_periods - 1):(n_periods - 2)
  
  # generate leads and lags of the treatment
  Dtl <- sapply(time_of_exposure, function(l) {
    dtl <- 1 * (
      (data_to_study$year == data_to_study$start_year + l) &
        (data_to_study$start_year > 0)
    )
    dtl
  }) |>
    as.data.frame()
  
  
  # Adjust of colnames
  pre_treat_names <- paste0("Dtmin", (n_periods - 1):1)
  
  new_variables_names <- c(pre_treat_names, paste0("Dt", 0:(n_periods - 2)))
  colnames(Dtl) <- c(pre_treat_names, paste0("Dt", 0:(n_periods - 2)))
  
  # Bind the data
  data <- cbind.data.frame(data_to_study, Dtl)
  row.names(data) <- NULL
  
  # Create the formula
  if (!is.null(covariates)) {
    # If there are covariates
    
    # Formula to estimate the event study (two-ways fixed effects model)
    formula <- as.formula(paste(
      outcome,
      " ~ ",
      paste(new_variables_names, collapse = " + "),
      " + ",
      paste(covariates, collapse = " + ")
    ))
    
    # Title of the plot
    title <- "Event study for conditional parallel trends"
    
    
  } else {
    # Formula to estimate the event study (two-ways fixed effects model)
    formula <- as.formula(paste(outcome, " ~ ", paste(new_variables_names, collapse = " + ")))
    
    # Title of the plot
    title <- "Event study for parallel trends"
    
  }
  
  
  
  
  
  # Event study two-way fixed effects ----
  event_study <- plm(
    formula,
    data   = data,
    model  = "within",
    effect = "twoways",
    index  = c(id, time)
  )
  
  
  summary_event_study <- summary(event_study)
  
  
  
  list_of_results[["summary"]] <- summary_event_study
  
  
  
  # Data to plot
  
  # Coefficients and standard errors
  all_coefs <- coef(event_study)[1:length(time_of_exposure)]
  all_se <- sqrt(diag(summary(event_study)$vcov))[1:length(time_of_exposure)]
  
  # Excluding the zero length of exposure
  idx_pre <- 1:(n_periods - 2)
  idx_post <- (n_periods - 1):length(all_coefs)
  coefs <- c(all_coefs[idx_pre], all_coefs[idx_post])
  ses <- c(all_se[idx_pre], all_se[idx_post])
  
  
  # Create the tibble
  tbl_plot_es <- tibble(coefs = coefs,
                        ses   = ses,
                        time_of_exposure = time_of_exposure)
  
  
  
  # Plot the results of the event study ----
  event_study_plot <-
    tbl_plot_es |>
    ggplot(mapping = aes(y = coefs, x = time_of_exposure)) +
    geom_line(linetype = "dashed") +
    geom_hline(yintercept = 0) +
    geom_point(aes(color = ifelse(time_of_exposure >= 0, TRUE, FALSE)), # Color the points according
               size = 1.5) +                                      # to the treatment status
    geom_errorbar(
      aes(
        ymin = (coefs - qnorm(1 - alpha / 2) * ses),
        ymax = (coefs + qnorm(1 - alpha / 2) * ses)
      ),
      color = ifelse(time_of_exposure >= 0, "#1874CD", "grey"),
      width = 0.3
    ) +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid = element_blank()) +
    scale_color_manual(values = c("grey", "#1874CD")) +  # Color of the points
    labs(x     = "Length of Exposure",
         y     = "Event-study coefficient",
         title = title)
  
  
  list_of_results[["plot"]] <- event_study_plot
  
  
  
  
  return(list_of_results)
  
  
}
  
  





#___________________________________________________#
#       Descriptive tables ----
#___________________________________________________#

# Mean and median tables using a year as reference
tbl_descriptive <- function(year_ref, mean = TRUE){
  
if(mean){
  
  tbl_desciptive <- 
  tbl_did |>
    filter(year == year_ref,
           # Filtering to the only federation unitis with treated municipalities
           federation_unit %in% federations_units_with_treated) |>
    select(any_treated, log_iss, log_pop, log_gdp, log_area) |>
    tbl_summary(
      statistic = list(all_continuous() ~ c("{mean} ({sd})")),
      by = any_treated
    ) |>
    add_difference() |>   # Mean difference
    modify_header(label ~ "**Variable (log)**")  
}  else {
  
  tbl_desciptive <- 
   tbl_did |>
    filter(year == year_ref,
           federation_unit %in% federations_units_with_treated) |>
    select(any_treated, log_iss, log_pop, log_gdp, log_area) |>
    tbl_summary(
      by = any_treated
    ) |>
    modify_header(label ~ "**Variable (log)**")  
  
}
  
  return(tbl_desciptive)
  
  
  
}





density_plot <- function(year_ref){
  
  tbl_did |> 
    filter(year == year_ref,
           federation_unit %in% federations_units_with_treated) |>
    select(any_treated, log_iss, log_pop, log_gdp, log_area) |> 
    pivot_longer(cols = -any_treated,
                 names_to = "Variable",
                 values_to = "Value") |> 
    mutate(Variable = factor(Variable,
                             levels = c("log_iss", "log_pop", "log_gdp", "log_area"),
                             labels = c("ISS collection",
                                        "Population",
                                        "GDP",
                                        "Territorial area"))) |>
    ggplot(aes(x = Value,
               fill = any_treated)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~Variable,
               scales = "free_x") +
    common_theme() +
    theme(legend.position = "bottom") +
    labs(title = paste("Density plot of the variables by treatment status in the base year of",
                       year_ref),
         x    = "Variable Value (log)",
         fill = NULL,
         y    = "Density") +
    scale_fill_manual(values = c("#1874CD", "grey"))
  
  
}


#___________________________________________________#
#       Sensitivity analysis (HonestDID) ----
#___________________________________________________#
if(!require(HonestDiD)){
  
  remotes::install_github("asheshrambachan/HonestDiD")  
  
}
library(HonestDiD)
## Auxiliary functions ----
# Details in https://github.com/asheshrambachan/HonestDiD 
# and        https://github.com/pedrohcgs/CS_RR
#' @title honest_did
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021)
honest_did <- function(...) UseMethod("honest_did")
#' @title honest_did.AGGTEobj
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021) when
#'  the event study is estimating using the `did` package
#'
#' @param e event time to compute the sensitivity analysis for.
#'  The default value is `e=0` corresponding to the "on impact"
#'  effect of participating in the treatment.
#' @param type Options are "smoothness" (which conducts a
#'  sensitivity analysis allowing for violations of linear trends
#'  in pre-treatment periods) or "relative_magnitude" (which
#'  conducts a sensitivity analysis based on the relative magnitudes
#'  of deviations from parallel trends in pre-treatment periods).
#' @inheritParams HonestDiD::createSensitivityResults
#' @inheritParams HonestDid::createSensitivityResults_relativeMagnitudes
honest_did.AGGTEobj <- function(es,
                                e          = 0,
                                type       = c("smoothness", "relative_magnitude"),
                                gridPoints = 100,
                                ...) {
  
  type <- match.arg(type)
  
  # Make sure that user is passing in an event study
  if (es$type != "dynamic") {
    stop("need to pass in an event study")
  }
  
  # Check if used universal base period and warn otherwise
  if (es$DIDparams$base_period != "universal") {
    stop("Use a universal base period for honest_did")
  }
  
  # Recover influence function for event study estimates
  es_inf_func <- es$inf.function$dynamic.inf.func.e
  
  # Recover variance-covariance matrix
  n <- nrow(es_inf_func)
  V <- t(es_inf_func) %*% es_inf_func / n / n
  
  # Remove the coefficient normalized to zero
  referencePeriodIndex <- which(es$egt == -1)
  V    <- V[-referencePeriodIndex,-referencePeriodIndex]
  beta <- es$att.egt[-referencePeriodIndex]
  
  nperiods <- nrow(V)
  npre     <- sum(1*(es$egt < -1))
  npost    <- nperiods - npre
  baseVec1 <- basisVector(index=(e+1),size=npost)
  orig_ci  <- constructOriginalCS(betahat        = beta,
                                  sigma          = V,
                                  numPrePeriods  = npre,
                                  numPostPeriods = npost,
                                  l_vec          = baseVec1)
  
  if (type=="relative_magnitude") {
    robust_ci <- createSensitivityResults_relativeMagnitudes(betahat        = beta,
                                                             sigma          = V,
                                                             numPrePeriods  = npre,
                                                             numPostPeriods = npost,
                                                             l_vec          = baseVec1,
                                                             gridPoints     = gridPoints,
                                                             ...)
    
  } else if (type == "smoothness") {
    robust_ci <- createSensitivityResults(betahat        = beta,
                                          sigma          = V,
                                          numPrePeriods  = npre,
                                          numPostPeriods = npost,
                                          l_vec          = baseVec1,
                                          ...)
  }
  
  return(list(robust_ci=robust_ci, orig_ci=orig_ci, type=type))
}


## Sensitivity analysis ----
#' Sensitivity analysis (using HonestDiD)
#'
#' @param dyn_model_summary  A dynamic model summary object (from DiD package)
#' @param type_sensitivity   "smoothness" or "relative_magnitude" Type of sensitivity analysis to be conducted.
#' @param e_0                Initial time of exposure to be considered in the sensitivity analysis.
#'
#' @return A list with table summary and plot
#' @export
#'
#' @examples
apply_sensitivity_analysis <- function(dyn_model_summary,
                                       type_sensitivity = "smoothness",
                                       e_0              = 0) {
  
  
  # List to store values
  hd_list <- list()
  
  
  if(type_sensitivity == "relative_magnitude"){
    
    ### Sensitivity analysis summary ----
    honest_did_estimate <- honest_did(es   = dyn_model_summary,
                                      e    = e_0,
                                      type = type_sensitivity)
    
    # Drop 0 as that is not really allowed.
    honest_did_estimate$robust_ci <- honest_did_estimate$robust_ci[-1,]
    
    
    ### ### Sensitivity analysis plot ----
    sensitivity_plot <- 
      createSensitivityPlot_relativeMagnitudes(robustResults   = honest_did_estimate$robust_ci,
                                               originalResults = honest_did_estimate$orig_ci) +
      theme_bw() +
      theme(panel.grid =  element_blank(),
            legend.position = "bottom") +
      labs(
        title = "Sensitivity analysis for the ATT (relative magnitude)",
        x     = "M",
        y     = "ATT",
        color = NULL
      )
    
    
  } else {
    
    honest_did_estimate <- honest_did(es   = dyn_model_summary,
                                      e    = e_0,
                                      type = type_sensitivity)
    
    
    sensitivity_plot <- 
      createSensitivityPlot(robustResults   = honest_did_estimate$robust_ci,
                            originalResults = honest_did_estimate$orig_ci) +
      theme_bw() +
      theme(panel.grid =  element_blank(),
            legend.position = "bottom") +
      labs(
        title = "Sensitivity analysis for the ATT (smoothness)",
        x     = "M",
        y     = "ATT",
        color = NULL
      )
    
  }
  
  
  
  hd_list[["honest_did_estimate"]] <- honest_did_estimate
  hd_list[["sensitivity_plot"]] <- sensitivity_plot
  
  return(hd_list)
  
  
  
}

#___________________________________________________#
#       Plot default theme ----
#___________________________________________________#



common_theme <- function(axis_size = 10) {
  theme_bw() +
  theme(
    plot.title = element_text(
      hjust = 0,
      size  = 11,
      face  = "plain",
      color = "black"
    ),
    axis.title.y = element_text(
      size  = axis_size,
      face  = "plain",
      color = "black"
    ),
    axis.title.x = element_text(
      size  = axis_size,
      face  = "plain",
      color = "black"
    ),
    plot.title.position = "panel",
    panel.grid = element_blank()
  )
  
  
}



















