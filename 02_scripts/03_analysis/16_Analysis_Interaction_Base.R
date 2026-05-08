# ==============================================================================
# File:          16_Analysis_Interaction_Base.R
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
# Description:   Introduces moderating interaction terms into the TWFE model. Specifically evaluates the interaction between regional Property Damage rates and Labor Integration Efficacy to discern if social instability weakens the economic returns of foreign workforce integration.
#
# Inputs:        Final Panel: 01_datasets/final/panel_data_final_2003_2021.csv
# Outputs:       Interaction regression tables ((.html, .txt)) exported to 03_results/tables/
# ==============================================================================

source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))

# 1. Setup & Utilities ---------------------------------------------------------
# Source utilities (which already sources 00_import.R)
source(here::here("02_scripts", "04_tools", "utils.R"))

input_file <- here("01_datasets", "final", "panel_data_final_2003_2021.csv")
output_dir <- here("03_results", "tables")

# 2. Data Preparation ----------------------------------------------------------
df <- prepare_panel_data(input_file)
message(glue::glue("Data Ready: {nrow(df)} observations."))

# 3. Model Estimation (Stepwise with Interaction) -----------------------------

# Stepwise build
mod1 <- feols(
    log_gdp_pc ~ log_unemp_l1 +
        log_capital +
        log_property_damage_l1 |
        district_id + year,
    data = df
)
mod2 <- update(mod1, . ~ . + FE_share_children)
mod3 <- update(mod2, . ~ . + FE_share_youth)
mod4 <- update(mod3, . ~ . + foreigner_share)
mod5 <- update(mod4, . ~ . + FE_foreign_labor_supply_pressure_l1)
mod6 <- update(mod5, . ~ . + FE_log_labor_efficacy_l1)

# Interaction Model
mod7 <- update(mod6, . ~ . + FE_foreign_labor_supply_pressure_l1:FE_log_labor_efficacy_l1)

models <- list(
    "(1) Base" = mod1,
    "(2) + Child" = mod2,
    "(3) + Youth" = mod3,
    "(4) + Foreign" = mod4,
    "(5) + Supply Pressure" = mod5,
    "(6) + Efficacy" = mod6,
    "(7) Interaction" = mod7
)

# 4. Table Construction (Metadata) ---------------------------------------------

coef_map <- c(
    "log_unemp_l1" = "Log Unemployment (t-1)",
    "log_capital" = "Log Capital Stock per Capita",
    "log_property_damage_l1" = "Log Property Damage Rate (t-1)",
    "FE_share_children" = "Share Children (0-17)",
    "FE_share_youth" = "Share Youth (18-24)",
    "foreigner_share" = "Foreigner Share",
    "FE_foreign_labor_supply_pressure_l1" = "Foreign Labor Supply Pressure (t-1)",
    "FE_log_labor_efficacy_l1" = "Labor Integration Efficacy (t-1)",
    "FE_foreign_labor_supply_pressure_l1:FE_log_labor_efficacy_l1" = "Interaction: FLSP * Efficacy"
)

rows <- data.frame(
    term = c("District FE", "Year FE"),
    "(1) Base" = rep("Yes", 2),
    "(2) + Child" = rep("Yes", 2),
    "(3) + Youth" = rep("Yes", 2),
    "(4) + Foreign" = rep("Yes", 2),
    "(5) + Supply Pressure" = rep("Yes", 2),
    "(6) + Efficacy" = rep("Yes", 2),
    "(7) Interaction" = rep("Yes", 2),
    check.names = FALSE
)

gof_mapping <- list(
    list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
    list("raw" = "r2.within", "clean" = "Within R\u00b2", "fmt" = 3)
)

# 5. Export Tables -------------------------------------------------------------

save_standard_table(
    models = models,
    coef_map = coef_map,
    gof_map = gof_mapping,
    add_rows = rows,
    title = "Regional Prosperity & Integration Interaction",
    subtitle = "Interaction between Foreign Labor Supply Pressure and Labor Efficacy (2003-2021)",
    file_path_html = file.path(
        output_dir,
        "html",
        "regression_results_interaction.html"
    ),
    file_path_txt = file.path(
        output_dir,
        "txt",
        "regression_results_interaction.txt"
    )
)
