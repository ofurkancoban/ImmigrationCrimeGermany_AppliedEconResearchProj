# ==============================================================================
# File:          15_Main_Econometric_Analysis.R
# Project:       Immigration, Crime, and Prosperity:The Economic Drivers of Growth in German Districts
# Author:        Ömer Furkan Çoban
#
# University:    Carl von Ossietzky University of Oldenburg
# Department:    Applied Economics and Data Science
# Course:        Applied Economics
# Semester:      WiSe 25/26
# Lecturers:     Prof. Dr. Jürgen Bitzer, Dr. Bernhard Christopher Dannemann, 
#                Abigail Opokua Asare
#
# Category:      Econometric Analysis
#
# Description:   Constructs and estimates the core empirical model for the research paper. 
#                This version uses a one-year lag for Log Property Damage (t-1)
#                to ensure temporal consistency in the exclusion of endogeneity.
#
# Inputs:        Final Panel: 01_datasets/final/panel_data_final_2003_2021.csv
# Outputs:       Main regression tables ((.html, .txt)) exported to 03_results/tables/
# ==============================================================================

# 1. Setup & Utilities ---------------------------------------------------------
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))
source(here::here("02_scripts", "04_tools", "utils.R"))

# Script-specific configuration
force_process <- TRUE # Set to TRUE to overwrite existing files

input_file <- here::here("01_datasets", "final", "panel_data_final_2003_2021.csv")
output_dir <- here::here("03_results", "tables")

# 2. Data Preparation ----------------------------------------------------------
# Uses prepare_panel_data from utils.R for standard mutations
df <- prepare_panel_data(input_file)
message(glue::glue("Data Ready: {nrow(df)} observations."))

# 3. Model Estimation (Stepwise) -----------------------------------------------

# Base Model
mod1 <- fixest::feols(log_gdp_pc ~ log_unemp_l1 + log_capital + log_property_damage_l1 | district_id + year, data = df)

# Stepwise Inclusion of Controls
mod2 <- update(mod1, . ~ . + FE_share_children)
mod3 <- update(mod2, . ~ . + FE_share_youth)
mod4 <- update(mod3, . ~ . + foreigner_share)
mod5 <- update(mod4, . ~ . + FE_foreign_labor_supply_pressure_l1)
mod6 <- update(mod5, . ~ . + FE_log_labor_efficacy_l1)

models <- list(
    "(1) Base" = mod1,
    "(2) + Child" = mod2,
    "(3) + Youth" = mod3,
    "(4) + Foreign" = mod4,
    "(5) + Supply Pressure" = mod5,
    "(6) Final" = mod6
)

# 4. Table Construction (Metadata) ---------------------------------------------

coef_map <- c(
    "log_unemp_l1" = "Log Unemployment (t-1)",
    "log_capital" = "Log Capital Stock per Capita",
    "log_property_damage_l1" = "Log Property Damage Rate (t-1)",
    "FE_share_children" = "Demographic Share: Children (0-17) (%)",
    "FE_share_youth" = "Demographic Share: Youth (18-24) (%)",
    "foreigner_share" = "Foreigner Share (%)",
    "FE_foreign_labor_supply_pressure_l1" = "Foreign Labor Supply Pressure (t-1) (%)",
    "FE_log_labor_efficacy_l1" = "Log Labor Integration Efficacy (t-1)"
)

rows <- data.frame(
    term = c("District FE", "Year FE"),
    "(1) Base" = rep("Yes", 2),
    "(2) + Child" = rep("Yes", 2),
    "(3) + Youth" = rep("Yes", 2),
    "(4) + Foreign" = rep("Yes", 2),
    "(5) + Pressure" = rep("Yes", 2),
    "(6) Final" = rep("Yes", 2),
    check.names = FALSE
)

gof_mapping <- list(
    list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
    list("raw" = "r2.within", "clean" = "Within R\u00b2", "fmt" = 3)
)

# 5. Export Tables -------------------------------------------------------------

# Main results table
save_standard_table(
    models = models,
    coef_map = coef_map,
    gof_map = gof_mapping,
    add_rows = rows,
    title = "Regional Prosperity & Foreign Labor Supply Pressure Analysis",
    subtitle = "Sensitivity Analysis: One-Year Lag for Property Damage (2003-2021)",
    file_path_html = file.path(output_dir, "html", "regression_results.html"),
    file_path_txt = file.path(output_dir, "txt", "regression_results.txt"),
    force = force_process
)

# Final Model Only (Single column for 05_presentation/focused review)
save_standard_table(
    models = list("Final Specification" = mod6),
    coef_map = coef_map,
    gof_map = gof_mapping,
    add_rows = data.frame(term = c("District FE", "Year FE"), "Final Spec" = c("Yes", "Yes"), check.names = FALSE),
    title = "Main Specification: Integration Efficacy",
    subtitle = "Property Damage as Macroeconomic Stability Proxy",
    file_path_html = file.path(output_dir, "html", "regression_results_final.html"),
    force = force_process
)

message("✓ Main Econometric Analysis (Lagged Property Damage) complete.")
