# ==============================================================================
# File:          21b_Robustness_IV.R
# Project:       Immigration, Crime, and Prosperity:The Economic Drivers of Growth in German Districts
# Author:        Ömer Furkan Çoban
#
# Category:      Robustness Check (IV)
#
# Description:   Addresses potential endogeneity between GDP per capita and 
#                Capital Stock per capita using Instrumental Variables (IV)
#                as a robustness check for the main analysis.
#                Uses the first lag of Log Capital Stock as an instrument.
#
# Inputs:        Final Panel: 01_datasets/final/panel_data_final_2003_2021.csv
# Outputs:       IV regression tables exported to 03_results/tables/
# ==============================================================================

# 1. Setup & Utilities ---------------------------------------------------------
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))
source(here::here("02_scripts", "04_tools", "utils.R"))

# Script-specific configuration
force_process <- TRUE 

input_file <- here::here("01_datasets", "final", "panel_data_final_2003_2021.csv")
output_dir <- here::here("03_results", "tables")

# 2. Data Preparation ----------------------------------------------------------
# Uses prepare_panel_data from utils.R (now includes log_capital_l1)
df <- prepare_panel_data(input_file)
message(glue::glue("Data Ready: {nrow(df)} observations."))

# 3. Model Estimation ----------------------------------------------------------

# Specification: Final specification from 15_Main_Econometric_Analysis.R
# Endogenous Variable: log_capital
# Instrument: log_capital_l1

# [1] OLS Benchmark (Fixed Effects)
mod_ols <- fixest::feols(
    log_gdp_pc ~ log_unemp_l1 + log_capital + log_property_damage_l1 + 
                 FE_share_children + FE_share_youth + foreigner_share + 
                 FE_foreign_labor_supply_pressure_l1 + FE_log_labor_efficacy_l1 | 
                 district_id + year, 
    data = df
)

# [2] IV Estimation (2SLS)
# Syntax: y ~ exogenous_vars | fixed_effects | endogenous ~ instruments
mod_iv <- fixest::feols(
    log_gdp_pc ~ log_unemp_l1 + log_property_damage_l1 + 
                 FE_share_children + FE_share_youth + foreigner_share + 
                 FE_foreign_labor_supply_pressure_l1 + FE_log_labor_efficacy_l1 | 
                 district_id + year | 
                 log_capital ~ log_capital_l1, 
    data = df
)

models <- list(
    "OLS (Benchmark)" = mod_ols,
    "IV (Capital Instrumented)" = mod_iv
)

# 4. Table Construction ---------------------------------------------------------

coef_map <- c(
    "fit_log_capital" = "Log Capital Stock per Capita (IV)",
    "log_capital" = "Log Capital Stock per Capita (OLS)",
    "log_unemp_l1" = "Log Unemployment (t-1)",
    "log_property_damage_l1" = "Log Property Damage Rate (t-1)",
    "FE_share_children" = "Demographic Share: Children (%)",
    "FE_share_youth" = "Demographic Share: Youth (%)",
    "foreigner_share" = "Foreigner Share (%)",
    "FE_foreign_labor_supply_pressure_l1" = "Foreign Labor Supply Pressure (t-1)",
    "FE_log_labor_efficacy_l1" = "Log Labor Integration Efficacy (t-1)"
)

gof_mapping <- list(
    list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
    list("raw" = "r2.within", "clean" = "Within R\u00b2", "fmt" = 3),
    list("raw" = "iv_f_stat", "clean" = "First-stage F-stat", "fmt" = 2)
)

# 5. Export results ------------------------------------------------------------

save_standard_table(
    models = models,
    coef_map = coef_map,
    gof_map = gof_mapping,
    add_rows = data.frame(term = c("District FE", "Year FE"), "OLS" = c("Yes", "Yes"), "IV" = c("Yes", "Yes"), check.names = FALSE),
    title = "Instrumental Variables (IV) Analysis: Regional Prosperity",
    subtitle = "Instrumenting Capital Stock with its First Lag (2003-2021)",
    file_path_html = file.path(output_dir, "html", "regression_results_iv.html"),
    file_path_txt = file.path(output_dir, "txt", "regression_results_iv.txt"),
    force = force_process
)

# Print Summary to Console for verification
message("\n--- OLS vs IV Comparison ---")
print(modelsummary::modelsummary(models, stars = TRUE, gof_map = "nobs"))

message("\n✓ IV Analysis complete. Results saved to 03_results/tables/html/regression_results_iv.html")
