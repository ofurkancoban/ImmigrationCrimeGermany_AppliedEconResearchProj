# ==============================================================================
# File:          21_Robustness_EastWest.R
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
# Description:   Performs spatial split-sample robustness checks. Re-estimates the core TWFE regression independently for East and West German districts to test if historical structural legacies or contemporary agglomeration dynamics bias the national-level integration assumptions.
#
# Inputs:        Final Panel: 01_datasets/final/panel_data_final_2003_2021.csv
# Outputs:       East vs West spatial split regression tables exported to 03_results/tables/
# ==============================================================================

# 1. Setup & Utilities ---------------------------------------------------------
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))
source(here::here("02_scripts", "04_tools", "utils.R"))

# Script-specific configuration
force_process <- TRUE # Set to TRUE to overwrite existing files

input_file <- here::here("01_datasets", "final", "panel_data_final_2003_2021.csv")
output_dir <- here::here("03_results", "tables")

# 3. Preparation ---------------------------------------------------------------
df <- prepare_panel_data(input_file) %>%
    mutate(
        # Regional Mapping (11-16 = East Germany states)
        state_code = substr(district_id, 1, 2),
        region = if_else(as.numeric(state_code) >= 11, "East", "West")
    )

# 4. Run Models ----------------------------------------------------------------

# Model 1: Full Sample
mod_full <- feols(
    log_gdp_pc ~ log_unemp_l1 +
        log_capital +
        log_property_damage_l1 +
        FE_share_children +
        FE_share_youth +
        foreigner_share +
        FE_foreign_labor_supply_pressure_l1 +
        FE_log_labor_efficacy_l1 |
        district_id + year,
    data = df
)

# Model 2: West Germany
df_west <- df %>% filter(region == "West")
mod_west <- feols(
    log_gdp_pc ~ log_unemp_l1 +
        log_capital +
        log_property_damage_l1 +
        FE_share_children +
        FE_share_youth +
        foreigner_share +
        FE_foreign_labor_supply_pressure_l1 +
        FE_log_labor_efficacy_l1 |
        district_id + year,
    data = df_west
)

# Model 3: East Germany
df_east <- df %>% filter(region == "East")
mod_east <- feols(
    log_gdp_pc ~ log_unemp_l1 +
        log_capital +
        log_property_damage_l1 +
        FE_share_children +
        FE_share_youth +
        foreigner_share +
        FE_foreign_labor_supply_pressure_l1 +
        FE_log_labor_efficacy_l1 |
        district_id + year,
    data = df_east
)

models <- list(
    "Full Sample" = mod_full,
    "West Germany" = mod_west,
    "East Germany" = mod_east
)

# 5. Table Construction (Metadata) ---------------------------------------------

coef_map <- c(
    "log_unemp_l1" = "Log Unemployment (t-1)",
    "log_capital" = "Log Capital Stock per Capita",
    "log_property_damage_l1" = "Log Property Damage Rate (t-1)",
    "FE_share_children" = "Share Children (0-17)",
    "FE_share_youth" = "Share Youth (18-24)",
    "foreigner_share" = "Foreigner Share",
    "FE_foreign_labor_supply_pressure_l1" = "Foreign Labor Supply Pressure (t-1)",
    "FE_log_labor_efficacy_l1" = "Labor Integration Efficacy (t-1)"
)

rows <- data.frame(
    term = c("District FE", "Year FE"),
    "Full Sample" = rep("Yes", 2),
    "West Germany" = rep("Yes", 2),
    "East Germany" = rep("Yes", 2),
    check.names = FALSE
)

gof_mapping <- list(
    list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
    list("raw" = "r2.within", "clean" = "Within R\u00b2", "fmt" = 3)
)

# 6. Export Tables -------------------------------------------------------------

save_standard_table(
    models = models,
    coef_map = coef_map,
    gof_map = gof_mapping,
    add_rows = rows,
    title = "Robustness Check: East vs. West Germany Comparison",
    subtitle = "Consistency of results across historical regional divisions",
    file_path_html = file.path(output_dir, "html", "regression_east_west.html"),
    file_path_txt = file.path(output_dir, "txt", "regression_east_west.txt"),
    force = force_process
)

message("✓ East-West Comparison complete.")
