# ==============================================================================
# File:          20_Robustness_LagSensitivity.R
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
# Description:   Conducts rigorous lag sensitivity robustness checks on the baseline model. Evaluates if the observed positive effects of Integration Efficacy on Regional Prosperity hold or compound when independent variables are lagged by multiple periods (L1 to L3).
#
# Inputs:        Final Panel: 01_datasets/final/panel_data_final_2003_2021.csv
# Outputs:       Lag sensitivity regression tables exported to 03_results/tables/
# ==============================================================================

# 1. Setup & Utilities ---------------------------------------------------------
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))
source(here::here("02_scripts", "04_tools", "utils.R"))

# Script-specific configuration
force_process <- TRUE # Set to TRUE to overwrite existing files

input_file <- here::here("01_datasets", "final", "panel_data_final_2003_2021.csv")
output_dir <- here::here("03_results", "tables")

# 3. Preparation ---------------------------------------------------------------
message("Preparing data...")
df <- prepare_panel_data(input_file) %>%
    group_by(district_id) %>%
    mutate(
        # Variable lags
        flsp_l0 = flsp,
        # FE_foreign_labor_supply_pressure_l1 is already in prepare_panel_data
        flsp_l2 = dplyr::lag(flsp, 2),
        flsp_l3 = dplyr::lag(flsp, 3),

        # Efficacy lags
        log_labor_efficacy_l0 = log(labor_efficacy),
        # FE_log_labor_efficacy_l1 is already in prepare_panel_data
        log_labor_efficacy_l2 = dplyr::lag(log(labor_efficacy), 2),
        log_labor_efficacy_l3 = dplyr::lag(log(labor_efficacy), 3)
    ) %>%
    ungroup()

# 4. Run Models ----------------------------------------------------------------
message("Estimating models...")

# Model 0: No Lag
mod_l0 <- feols(
    log_gdp_pc ~ log_unemp_l1 + log_capital + log_property_damage_l1 +
        FE_share_children + FE_share_youth + foreigner_share +
        flsp_l0 + log_labor_efficacy_l0 | district_id + year,
    data = df
)

# Model 1: Lag 1
mod_l1 <- feols(
    log_gdp_pc ~ log_unemp_l1 + log_capital + log_property_damage_l1 +
        FE_share_children + FE_share_youth + foreigner_share +
        FE_foreign_labor_supply_pressure_l1 + FE_log_labor_efficacy_l1 | district_id + year,
    data = df
)

# Model 2: Lag 2
mod_l2 <- feols(
    log_gdp_pc ~ log_unemp_l1 + log_capital + log_property_damage_l1 +
        FE_share_children + FE_share_youth + foreigner_share +
        flsp_l2 + log_labor_efficacy_l2 | district_id + year,
    data = df
)

# Model 3: Lag 3
mod_l3 <- feols(
    log_gdp_pc ~ log_unemp_l1 + log_capital + log_property_damage_l1 +
        FE_share_children + FE_share_youth + foreigner_share +
        flsp_l3 + log_labor_efficacy_l3 | district_id + year,
    data = df
)

models <- list(
    "(1) No Lag" = mod_l0,
    "(2) 1-Year Lag" = mod_l1,
    "(3) 2-Year Lag" = mod_l2,
    "(4) 3-Year Lag" = mod_l3
)

# 5. Table Construction --------------------------------------------------------

coef_map <- c(
    "log_unemp_l1" = "Log Unemployment (t-1)",
    "log_capital" = "Log Capital Stock per Capita",
    "log_property_damage_l1" = "Log Property Damage Rate (t-1)",
    "FE_share_children" = "Share Children (0-17)",
    "FE_share_youth" = "Share Youth (18-24)",
    "foreigner_share" = "Foreigner Share",
    "flsp_l0" = "Foreign Labor Supply Pressure (t)",
    "FE_foreign_labor_supply_pressure_l1" = "Foreign Labor Supply Pressure (t-1)",
    "flsp_l2" = "Foreign Labor Supply Pressure (t-2)",
    "flsp_l3" = "Foreign Labor Supply Pressure (t-3)",
    "log_labor_efficacy_l0" = "Log Labor Integration Efficacy (t)",
    "FE_log_labor_efficacy_l1" = "Log Labor Integration Efficacy (t-1)",
    "log_labor_efficacy_l2" = "Log Labor Integration Efficacy (t-2)",
    "log_labor_efficacy_l3" = "Log Labor Integration Efficacy (t-3)"
)

rows <- data.frame(
    term = c("District FE", "Year FE"),
    "(1) No Lag" = rep("Yes", 2),
    "(2) 1-Year Lag" = rep("Yes", 2),
    "(3) 2-Year Lag" = rep("Yes", 2),
    "(4) 3-Year Lag" = rep("Yes", 2),
    check.names = FALSE
)

gof_mapping <- list(
    list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
    list("raw" = "r2.within", "clean" = "Within Rsq", "fmt" = 3)
)

# 6. Export Tables -------------------------------------------------------------
outfile_txt <- file.path(output_dir, "txt", "regression_lag_sensitivity.txt")
modelsummary(
    models,
    coef_map = coef_map,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_map = gof_mapping,
    add_rows = rows,
    title = "Robustness Check: Lag Sensitivity Analysis",
    notes = "Standard errors clustered at the district level.",
    output = outfile_txt
)

outfile_html <- file.path(output_dir, "html", "regression_lag_sensitivity.html")
table_styled <- modelsummary(
    models,
    coef_map = coef_map,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_map = gof_mapping,
    add_rows = rows,
    output = "gt"
) %>%
    apply_academic_theme(
        title = "Robustness Check: Lag Sensitivity Analysis",
        subtitle = "Alternative Temporal Specifications (L0-L3)"
    )

gt::gtsave(table_styled, outfile_html)

message("✓ Lag Sensitivity Analysis complete.")
