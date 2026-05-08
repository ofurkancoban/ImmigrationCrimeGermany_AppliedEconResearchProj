# ==============================================================================
# File:          25_Subtype_Bodily_Harm.R
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
# Description:   Modifies the TWFE approach to measure the effect of Bodily Harm and Violent Crimes (Körperverletzung) on regional aggregate output, acting as a profound measure of severe social destabilization.
#
# Inputs:        Final Panel: 01_datasets/final/panel_data_final_2003_2021.csv
# Outputs:       Bodily harm regression tables exported to 03_results/tables/crime_subtypes/
# ==============================================================================

source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))
source(here::here("02_scripts", "04_tools", "utils.R"))

# Define paths
input_file <- here(
    "01_datasets",
    "final",
    "panel_data_final_2003_2021.csv"
)
output_dir <- here("03_results", "tables")

if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
}

# 2. Data Preparation ----------------------------------------------------------
df <- prepare_panel_data(input_file)

message(glue::glue("Data Ready: {nrow(df)} observations."))

# 2. Model Estimation (Stepwise) -----------------------------------------------

# Stage 1: Base (GDP ~ Lag Unemp + Capital + Street Crime)
mod1 <- feols(
    log_gdp_pc ~ log_unemp_l1 +
        log_capital +
        log_bodily_l1 |
        district_id + year,
    data = df
)

# Stage 2: + Children
mod2 <- feols(
    log_gdp_pc ~ log_unemp_l1 +
        log_capital +
        log_bodily_l1 +
        FE_share_children |
        district_id + year,
    data = df
)

# Stage 3: + Youth
mod3 <- feols(
    log_gdp_pc ~ log_unemp_l1 +
        log_capital +
        log_bodily_l1 +
        FE_share_children +
        FE_share_youth |
        district_id + year,
    data = df
)

# Stage 4: + Foreigner Share
mod4 <- feols(
    log_gdp_pc ~ log_unemp_l1 +
        log_capital +
        log_bodily_l1 +
        FE_share_children +
        FE_share_youth +
        foreigner_share |
        district_id + year,
    data = df
)

# Stage 5: + Foreign Labor Supply Pressure (t-1)
mod5 <- feols(
    log_gdp_pc ~ log_unemp_l1 +
        log_capital +
        log_bodily_l1 +
        FE_share_children +
        FE_share_youth +
        foreigner_share +
        FE_foreign_labor_supply_pressure_l1 |
        district_id + year,
    data = df
)

# Stage 6: Final (Full Model with Labor Efficacy)
mod6 <- feols(
    log_gdp_pc ~ log_unemp_l1 +
        log_capital +
        log_bodily_l1 +
        FE_share_children +
        FE_share_youth +
        foreigner_share +
        FE_foreign_labor_supply_pressure_l1 +
        FE_log_labor_efficacy_l1 |
        district_id + year,
    data = df
)

# 4. Table Construction --------------------------------------------------------

# Define Coefficient Mapping
coef_map <- c(
    "log_unemp_l1" = "Log Unemployment (t-1)",
    "log_capital" = "Log Capital Stock per Capita",
    "log_bodily_l1" = "Log Bodily Harm Rate (t-1)",
    "FE_share_children" = "Demographic Share: Children (0-17) (%)",
    "FE_share_youth" = "Demographic Share: Youth (18-24) (%)",
    "foreigner_share" = "Foreigner Share (%)",
    "FE_foreign_labor_supply_pressure_l1" = "Foreign Labor Supply Pressure (t-1) (%)",
    "FE_log_labor_efficacy_l1" = "Log Labor Integration Efficacy (t-1)"
)

models <- list(
    "(1) Base" = mod1,
    "(2) + Child" = mod2,
    "(3) + Youth" = mod3,
    "(4) + Foreign" = mod4,
    "(5) + Supply Pressure" = mod5,
    "(6) Final" = mod6
)

# Add FE Rows
rows <- data.frame(
    term = c("District FE", "Year FE"),
    "(1) Base" = rep("Yes", 2),
    "(2) + Child" = rep("Yes", 2),
    "(3) + Youth" = rep("Yes", 2),
    "(4) + Foreign" = rep("Yes", 2),
    "(5) + Supply Pressure" = rep("Yes", 2),
    "(6) Final" = rep("Yes", 2),
    check.names = FALSE
)

# Custom GOF Mapping
gof_mapping <- list(
    list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
    list("raw" = "r2.within", "clean" = "Within Rsq", "fmt" = 3)
)

# Export TXT
outfile_txt <- file.path(
    output_dir,
    "txt",
    "regression_results_bodily_harm.txt"
)
modelsummary(
    models,
    coef_map = coef_map,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_map = gof_mapping,
    add_rows = rows,
    title = "Econometric Analysis: Prosperity & Foreign Labor Supply Pressure (Dep. Var: Log GDP per Capita)",
    notes = "Standard errors clustered at the district level.",
    output = outfile_txt
)

# Export HTML (Styled)
outfile_html <- file.path(
    output_dir,
    "html",
    "regression_results_bodily_harm.html"
)

table_styled <- modelsummary(
    models,
    coef_map = coef_map,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_map = gof_mapping,
    add_rows = rows,
    output = "gt"
) %>%
    apply_academic_theme(
        title = "Regional Prosperity & Foreign Labor Supply Pressure Analysis",
        subtitle = "Linear Specification with Two-Way Fixed Effects: Bodily Harm (2003-2021)"
    )

gt::gtsave(table_styled, outfile_html)

message(
    "\u2713 Econometric Analysis results saved to 03_results/tables/regression_results_bodily_harm.txt and .html"
)
